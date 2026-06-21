library('rmarkdown')
library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')
library('tidyjson')
library('jsonlite')

reportPath <- "analysis/plots"
#source("/mnt/hollandC/stockviz/r/config.r")
#source("/mnt/data/blog/common/plot.common.r")

source("/mnt/hollandr/config.r")
source("/mnt/hollandr/plot.common.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host = 'sweden', user = ldbuser2, password = ldbpassword2, dbname = 'StockVizDyn', sslmode = 'allow')

startDate <- as.Date("2015-01-01")
getMonthlyIndexRets <- function(iNames){
  rets <- NULL
  for(i in 1:length(iNames)){
    pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s'", iNames[i], startDate))
    rets <- merge.xts(rets, monthlyReturn(xts(pDf[,1], pDf[,2])))
  }
  
  index(rets) <- as.Date(sprintf("%d-%d-%d", year(index(rets)), month(index(rets)), 
                                                days_in_month(as.Date(sprintf("%d-%d-1", year(index(rets)), month(index(rets)))))))
  
  names(rets) <- iNames
  return(rets)
}

getMonthlyAvgMf <- function(schemeCodes){
  rets <- NULL
  for(i in 1:length(schemeCodes)){
    pDf <- sqlQuery(lcon, sprintf("select nav, as_of from mf_nav_history where scheme_code = %d and as_of >= '%s'", schemeCodes[i], startDate))
    rets <- merge.xts(rets, monthlyReturn(xts(pDf[,1], pDf[,2])))
  }
  
  index(rets) <- as.Date(sprintf("%d-%d-%d", year(index(rets)), month(index(rets)), 
                                 days_in_month(as.Date(sprintf("%d-%d-1", year(index(rets)), month(index(rets)))))))
  
  rets <- xts(rowMeans(rets, na.rm = TRUE), index(rets))
  return(rets)
}

mktBenches <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY MICROCAP 250 TR")
factorBenches <- c("NIFTY500 VALUE 50 TR", "NIFTY500 QUALITY 50 TR", "NIFTY500 MOMENTUM 50 TR")
hybridFunds <- c(119609, #SBI EQUITY HYBRID FUND - DIRECT PLAN - Growth
                 120251, #ICICI Prudential Equity & Debt Fund - Direct Plan - Growth
                 119062 #HDFC Hybrid Equity Fund - Growth Plan
                 )
debtFunds <- c(118987, #HDFC Corporate Bond Fund - Growth Option - Direct Plan 
               120692, #ICICI Prudential Corporate Bond Fund - Direct Plan - Growth
               119533 #Aditya Birla Sun Life Corporate Bond Fund - Growth - Direct Plan
              )
multiAssetFunds <- c(120334, #ICICI Prudential Multi-Asset Fund - Direct Plan - Growth
                     119840, #SBI MULTI ASSET ALLOCATION FUND - DIRECT PLAN - GROWTH
                     148457 #Nippon India Multi Asset Allocation Fund - Direct Plan - Growth Option
                     )
rfBench <- c('NIFTY 1D RATE INDEX')

monthlyMktBenchRets <- getMonthlyIndexRets(mktBenches)
monthlyFactorBenchRets <- getMonthlyIndexRets(factorBenches)
monthlyHybridBenchRets <- getMonthlyAvgMf(hybridFunds)
names(monthlyHybridBenchRets) <- c('HYBRID_MF')
monthlyCorpDebtBenchRets <- getMonthlyAvgMf(debtFunds)
names(monthlyCorpDebtBenchRets) <- c('CORP_DEBT_MF')
monthlyMultiAssetBenchRets <- getMonthlyAvgMf(multiAssetFunds)
names(monthlyMultiAssetBenchRets) <- c('MULTI_ASSET')
rfBenchRets <- getMonthlyIndexRets(rfBench)

today <- Sys.Date()

idCnts <- dbGetQuery(pgCon, "select id,  count(val) cnt, max(yr) yr from sebi_pms_data group by id")
activePms <- idCnts |> filter(cnt >= 12 & yr >= year(today) -1)

getStats <- function(){
  fileTracker <- data.frame(ID = 0, SEBI_ID = "", PMS_NAME = "", STRAT_FILE = "", STRAT_TITLE = "", ASSET_CLASS = "", AUM = 0.0, SHARPE = 0.0, IR = 0.0, RET = 0.0, ST_DT = "", AS_OF = "")
  
  for(i in 1:nrow(activePms)){
    metaId <- activePms$id[i]
    print(paste("processing:", metaId))
    metaDf <- dbGetQuery(pgCon, "select sebi_id, pms_name from sebi_pms_meta where id = $1", params=list(metaId))
    sebiId <- metaDf$sebi_id[1]
    pmsName <- metaDf$pms_name[1]
    
    strategyReturnsDf1 <- dbGetQuery(pgCon, "select yr, mth, ln->>'investmentApproach' name, ln->>'aumINRCr' aumINRCr, ln->'returns'->>'oneMonth' ret_pct 
                                    from sebi_pms_data spd, 
                                      jsonb_array_elements(val->'discretionaryServices'->'performanceData') as ln 
                                    where id = $1
                                    and yr >= 2022
                                    order by yr, mth",
                                  params = list(metaId))
    
    stratRetTb1 <- as_tibble(strategyReturnsDf1) |> filter(!is.na(name) & !is.na(ret_pct))
    
    strategyReturnsDf2 <- dbGetQuery(pgCon, "select yr, mth, val->'discretionaryServices'->'performanceData' pd 
                                    from sebi_pms_data spd 
                                    where id = $1
                                    and yr >= 2022
                                    order by yr, mth",
                                  params = list(metaId))
    
    strategyReturnsDf3 <- dbGetQuery(pgCon, "select yr, mth, ln->>'strategy' name, ln->>'ret1m' ret_pct, ln->>'aum' auminrcr 
                                    from sebi_pms_data spd, 
                                    jsonb_array_elements(val) as ln
                                    where id = $1
                                    and yr = 2021  
                                    order by yr, mth",
                                     params = list(metaId))
    
    strategyReturnsDf3 <- strategyReturnsDf3 |> filter(!if_all(c(name, ret_pct, auminrcr), is.na))
    
    fundReturnsDf <- dbGetQuery(pgCon, "select yr, mth, val->>'wavg_ret' ret_pct, val->>'aum' auminrcr 
                                    from sebi_pms_data spd 
                                    where id = $1
                                    and yr <= 2020  
                                    order by yr, mth",
                                     params = list(metaId))
    
    fundReturnsDf$ret_pct <- as.numeric(fundReturnsDf$ret_pct)
    fundReturnsDf$auminrcr <- as.numeric(fundReturnsDf$auminrcr)
    
    stratRetTb <- tibble()
    for(j in 1:nrow(strategyReturnsDf2)){
      tryCatch({
        tj <- as.tbl_json(strategyReturnsDf2$pd[j]) %>% gather_array
        if(nrow(tj) == 0) next
        
        temp <- tj %>% spread_all %>%
          enter_object(approaches) %>% gather_array %>%
          spread_all
        
        if(nrow(temp) == 0) next
        
        if(!('strategy' %in% colnames(temp))){
          temp$strategy <- NA
        }
  
        tjrets <- temp %>%
          select(strategy, name, aumINRCr, returns.1Month)
        
        retTb <- as_tibble(tjrets)
        retTb$yr <- strategyReturnsDf2$yr[j]
        retTb$mth <- strategyReturnsDf2$mth[j]
        
        stratRetTb <- rbind(stratRetTb, retTb)
      }, error = \(x) {
          #print(j)
          #print(x)
        })
    }
    
    stratRetTb1$strategy <- NA
    if(nrow(stratRetTb1) > 0){
      for(j in 1:nrow(stratRetTb1)){
        if(!is.na(stratRetTb1$strategy[j])) next
        tryCatch({
          stratRetTb1$strategy[j] <- ((stratRetTb |> filter(name == stratRetTb1$name[j]) |> select(strategy))[1,1])[[1]]
        }, error = \(x){})
      }
    }
    
    if(nrow(stratRetTb) > 0){
      stratRetTb <- rbind(stratRetTb1, stratRetTb |> rename(auminrcr = aumINRCr, ret_pct = returns.1Month))
    } else {
      stratRetTb <- stratRetTb1
    }
    
    if(nrow(strategyReturnsDf3) > 0){
      strategyReturnsDf3$strategy <- NA
      stratRetTb <- rbind(strategyReturnsDf3, stratRetTb)
    }
    
    stratNameAlias <- dbGetQuery(pgCon, "select strategy_alias from sebi_pms_aux where id = $1", params = list(metaId))
    if(nrow(stratNameAlias) > 0){
      aliases <- jsonlite::fromJSON(stratNameAlias$strategy_alias[1])
      for(ai in 1:nrow(aliases)){
        stratRetTb <- stratRetTb |> mutate(name = str_replace(name, regex(paste(aliases$alias[[ai]], collapse="|"), ignore_case = TRUE), aliases$primary[[ai]]))
      }
    }
    
    fundRets <- tibble()
    fundAum <- tibble()
    if(nrow(fundReturnsDf) > 0){
      fundRets <- fundReturnsDf |> mutate(ret = as.numeric(ret_pct)/100, 
                                           dt = as.Date(sprintf("%d-%d-%d", yr, mth,
                                                                days_in_month(as.Date(sprintf("%d-%d-1", yr, mth)))))) |> 
        distinct(dt, .keep_all = TRUE) |> 
        select(dt, ret)
      
      
      fundAum <- fundReturnsDf |> mutate(aum = as.numeric(auminrcr), 
                                          dt = as.Date(sprintf("%d-%d-%d", yr, mth,
                                                               days_in_month(as.Date(sprintf("%d-%d-1", yr, mth)))))) |> 
        distinct(dt, .keep_all = TRUE) |> 
        select(dt, aum)
    }
    
    strategyRets <- stratRetTb |> mutate(ret = as.numeric(ret_pct)/100, 
                                         dt = as.Date(sprintf("%d-%d-%d", yr, mth,
                                                              days_in_month(as.Date(sprintf("%d-%d-1", yr, mth)))))) |> 
      filter(!is.na(name) & length(name) > 2 & name != "") |>
      select(dt, name, ret) |> distinct(dt, name, .keep_all = TRUE) |> 
      pivot_wider(id_cols = dt, names_from = name, values_from = ret)
    
    strategyAum <- stratRetTb |> mutate(aum = as.numeric(auminrcr), 
                                         dt = as.Date(sprintf("%d-%d-%d", yr, mth,
                                                              days_in_month(as.Date(sprintf("%d-%d-1", yr, mth)))))) |> 
      filter(!is.na(name) & length(name) > 2 & name != "") |>
      select(dt, name, aum) |> distinct(dt, name, .keep_all = TRUE) |>
      pivot_wider(id_cols = dt, names_from = name, values_from = aum) |>
      arrange(dt) 
    
    strategyAumAvg <- strategyAum |>
      slice_max(dt, n = 2) |>
      pivot_longer(cols=-dt) |>
      group_by(name) |>
      summarise(aum = mean(value, na.rm=TRUE)) |>
      mutate(aum = if_else(is.nan(aum) | is.na(aum) | is.infinite(aum), 0, aum))
    
    lastIsNa <- strategyRets |> filter(dt == max(dt)) |> summarise(across(everything(), ~sum(is.na(.)))) |> pivot_longer(cols = everything()) |> rename(v1 = value)
    naCount <- strategyRets |> summarise(across(everything(), ~sum(is.na(.)))) |> pivot_longer(cols = everything()) |> rename(v2 = value)
    
    names2Remove <- (lastIsNa |> inner_join(naCount, by = join_by(name)) |>
      filter(v2 > nrow(strategyRets)*0.9 | (v1 != 0 & v2 > nrow(strategyRets)*0.5)) |>
      select(name))$name
                                                                                               
    strategyRets <- strategyRets |> select(-all_of(names2Remove))
    strategyAum <- strategyAum |> select(-all_of(names2Remove))
    
    if(nrow(strategyRets) == 0 || ncol(strategyRets) < 2) next
    
    if (nrow(fundRets) > 0 && nrow(fundAum) > 0) {
      srXts <- xts(fundRets$ret, fundRets$dt)
      srXts <- srXts[srXts < 5]
      maxDt <- max(index(srXts))
      minDt <- min(index(srXts))
      names(srXts)[1] <- "PMS"
      retXts <- merge(srXts, monthlyMktBenchRets)
      retXts <- na.omit(retXts)
      ret <- as.numeric(Return.annualized(srXts)) * 100
      fundSr <- as.numeric(SharpeRatio.annualized(srXts, rfBenchRets))
      fundIr <- as.numeric(InformationRatio(retXts[,1], retXts[,2]))*100
      
      sr <- paste(round(SharpeRatio.annualized(retXts, rfBenchRets), 2), collapse="/")
      Common.PlotCumReturns(retXts, "PMS Returns (wavg)", sprintf("SR: %s", sr), #NULL)
                            sprintf("%s/pms-%d.cum.pre.png", reportPath, metaId))
      
      retXts <- merge(srXts, monthlyFactorBenchRets)
      retXts <- na.omit(retXts)
      
      sr <- paste(round(SharpeRatio.annualized(retXts, rfBenchRets), 2), collapse="/")
      Common.PlotCumReturns(retXts, "PMS Returns", sprintf("SR: %s", sr), #NULL)
                            sprintf("%s/pms-%d.factor.cum.pre.png", reportPath, metaId))
      
      barColor <- viridis(n=2)[1]

      aumTp <- fundAum |> 
        drop_na() |>
        filter(aum > 0) |>
        mutate(aum_chg = round(100*(aum/lag(aum) -1), 2)) |>
        select(dt, aum, aum_chg) 
      
      p1 <- ggplot(aumTp, aes(y = aum, x=dt)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_line(color = barColor, linewidth=1.1) +
        scale_x_date(date_breaks = "3 month", date_label = "%Y-%b") +
        labs(x = '', y = 'AUM Rs. (cr)', title = "Assets Under Management")
      
      p2 <- ggplot(aumTp, aes(y = aum_chg, x = dt)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_bar(stat='identity', fill=barColor) +
        scale_x_date(date_breaks = "3 month", date_label = "%Y-%b") +
        labs(x = '', y = 'AUM Growth (%)', title = "Growth in AUM")
      
      p1/p2 + plot_layout(axes = 'collect_x') +
        plot_annotation(title = "PMS AUM",
                        subtitle = sprintf("%s:%s", minDt, maxDt),
                        caption = '@StockViz',
                        theme = theme_economist())
      
      ggsave(sprintf("%s/pms-%d.aum.pre.png", reportPath, metaId), width = 12, height = 12, units = "in")
    }
    
    for(j in 2:ncol(strategyRets)){
      stratTname <- colnames(strategyRets)[j]
      
      if(nrow(strategyRets |> select(all_of(stratTname)) |> drop_na()) < 12 
         || all(strategyRets[,j] == 0, na.rm=TRUE)) next
      
      aum <- (strategyAumAvg |> filter(name == stratTname) |> select(aum))[[1]]
      if(aum == 0) next
      
      stratFname <- str_replace_all(stratTname, '[ .()/&]', '_')
  
      sebiClass <- ""
      tryCatch({
        sebiClass <- (stratRetTb |> filter(name == stratTname & !is.na(strategy) & !is.null(strategy)) |> select(strategy))[1, 1][[1]]
      }, error = \(x) {})
      
      srXts <- xts(strategyRets[,j], strategyRets$dt)
      srXts <- srXts[srXts < 5]
      
      maxDt <- max(index(srXts))
      minDt <- min(index(srXts))
  
      names(srXts)[1] <- "Strategy"

      if(is.na(sebiClass)){
        retXts <- merge(srXts, monthlyMktBenchRets[,1])
        retXts <- na.omit(retXts)
      } else if(sebiClass == "EQUITY"){
        retXts <- merge(srXts, monthlyMktBenchRets)
        retXts <- na.omit(retXts)
      } else if (sebiClass == "HYBRID"){
        retXts <- merge(srXts, monthlyHybridBenchRets)
        retXts <- na.omit(retXts)
      } else if (sebiClass == "DEBT"){
        retXts <- merge(srXts, monthlyCorpDebtBenchRets)
        retXts <- na.omit(retXts)
      } else if (sebiClass == "MULTI ASSET"){
        retXts <- merge(srXts, monthlyMultiAssetBenchRets)
        retXts <- na.omit(retXts)
      } else {
        retXts <- merge(srXts, monthlyMktBenchRets[,1])
        retXts <- na.omit(retXts)
      }
      
      ret <- as.numeric(Return.annualized(srXts)) * 100
      fundSr <- as.numeric(SharpeRatio.annualized(srXts, rfBenchRets))
      fundIr <- as.numeric(InformationRatio(retXts[,1], retXts[,2]))*100
      sr <- paste(round(SharpeRatio.annualized(retXts, rfBenchRets), 2), collapse="/")
      Common.PlotCumReturns(retXts, sprintf("%s Returns", stratTname), sprintf("SR: %s", sr), #NULL)
                            sprintf("%s/pms-%d.%s.cum.png", reportPath, metaId, stratFname))
      
      if(!is.na(sebiClass) && sebiClass == "EQUITY"){
        retXts <- merge(srXts, monthlyFactorBenchRets)
        retXts <- na.omit(retXts)
      
        sr <- paste(round(SharpeRatio.annualized(retXts, rfBenchRets), 2), collapse="/")
        Common.PlotCumReturns(retXts, sprintf("%s Returns", stratTname), sprintf("SR: %s", sr), #NULL)
                            sprintf("%s/pms-%d.%s.factor.cum.png", reportPath, metaId, stratFname))
      }
      
      barColor <- viridis(n=2)[1]
      aumTp <- strategyAum |> select(dt, all_of(stratTname)) |>
        drop_na() |>
        mutate(aum_chg = round(100*(.data[[stratTname]]/lag(.data[[stratTname]]) -1), 2)) |>
        select(dt, all_of(stratTname), aum_chg) |>
        rename_with(~"strategy", 2)
      
      p1 <- ggplot(aumTp, aes(y = strategy, x=dt)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_line(color = barColor, linewidth=1.1) +
        scale_x_date(date_breaks = "3 month", date_label = "%Y-%b") +
        labs(x = '', y = 'AUM Rs. (cr)', title = "Assets Under Management")
      
      p2 <- ggplot(aumTp, aes(y = aum_chg, x = dt)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_bar(stat='identity', fill=barColor) +
        scale_x_date(date_breaks = "3 month", date_label = "%Y-%b") +
        labs(x = '', y = 'AUM Growth (%)', title = "Growth in AUM")
      
      p1/p2 + plot_layout(axes = 'collect_x') +
        plot_annotation(title = stratTname,
                        subtitle = sprintf("%s:%s", minDt, maxDt),
                        caption = '@StockViz',
                        theme = theme_economist())
      
      ggsave(sprintf("%s/pms-%d.%s.aum.png", reportPath, metaId, stratFname), width = 12, height = 12, units = "in")
      
      fileTracker <- rbind(fileTracker, c(metaId, sebiId, pmsName, stratFname, str_to_title(stratTname), sebiClass, aum, fundSr, fundIr, ret, as.character(minDt), as.character(maxDt)))
    }
  }
  
  fileTracker <- fileTracker[-1,]
  return(fileTracker)
}

print("computing stats...")
fileTracker <- getStats()
save(fileTracker, file=sprintf("%s/fileTracker.Rdata", reportPath))
#load(file=sprintf("%s/fileTracker.Rdata", reportPath)) #fileTracker

createPmsPages <- function(){
  pmsDf <- (fileTracker |> select(SEBI_ID, PMS_NAME, ID) |> distinct())
  for(i in 1:nrow(pmsDf)){
    pmsId <- pmsDf$SEBI_ID[i]
    pmsTitle <- pmsDf$PMS_NAME[i]
    metaId <- pmsDf$ID[i]
    
    render("rp-PMS.Rmd", output_file=sprintf("analysis/rp-PMS.%s.html", pmsId), params=list(metaId = metaId, pmsTitle = pmsTitle))
  }
}

print("rendering PMS pages...")
createPmsPages()

print("rendering master page...")

render("rp-IN.Rmd", output_file="rp-IN.html")




