library('rmarkdown')
library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')
library('jsonlite')

reportPath <- "analysis/plots"
#source("/mnt/hollandC/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pgCon <- dbConnect(RPostgres::Postgres(), host = 'sweden', user = ldbuser2, password = ldbpassword2, dbname = 'StockVizDyn', sslmode = 'allow')

mktBenches <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR", "NIFTY MICROCAP 250 TR")
monthlyBenchRets <- NULL
startDate <- as.Date("2021-01-01")
for(i in 1:length(mktBenches)){
  pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name = '%s' and time_stamp >= '%s'", mktBenches[i], startDate))
  monthlyBenchRets <- merge.xts(monthlyBenchRets, monthlyReturn(xts(pDf[,1], pDf[,2])))
}

index(monthlyBenchRets) <- as.Date(sprintf("%d-%d-%d", year(index(monthlyBenchRets)), month(index(monthlyBenchRets)), 
                                           days_in_month(as.Date(sprintf("%d-%d-1", year(index(monthlyBenchRets)), month(index(monthlyBenchRets)))))))

names(monthlyBenchRets) <- mktBenches

today <- Sys.Date()

idCnts <- dbGetQuery(pgCon, "select id, count(val) cnt, max(yr) yr from sebi_pms_data group by id")
activePms <- idCnts |> filter(cnt >= 12 & yr >= year(today) -1)

fileTracker <- data.frame(ID = 0, STRAT_FILE = "", STRAT_TITLE = "")

for(i in 1:nrow(activePms)){
  metaId <- activePms$id[i]
  strategyReturnsDf <- dbGetQuery(pgCon, "select yr, mth, di.value strategy, spdData ->> 'TwrrReturns1 Month' ret_pct
                                from sebi_pms_data spd, 
                                  jsonb_array_elements(spd.val) spdMain, 
                                  jsonb_array_elements(spdMain -> 'data') spdData, 
                                  jsonb_each_text(spdData) as di(key, value) 
                                where id = $1 
                                  and spdMain ->> 'main' like '%data for disc%' 
                                  and spdMain ->> 'sub' like '%performance data%' 
                                  and di.key like '%Strategy%' 
                                  and di.value not like '%benchmark%' 
                                  and spdData -> 'TwrrReturns1 Month' is not null;",
                                params = list(metaId))
  
  strategyReturns <- strategyReturnsDf |> mutate(ret = as.numeric(ret_pct)/100, 
                                               dt = as.Date(sprintf("%d-%d-%d", yr, mth, 
                                                                    days_in_month(as.Date(sprintf("%d-%d-1", yr, mth))))))
  
  stratNameAlias <- dbGetQuery(pgCon, "select strategy_alias from sebi_pms_aux where id = $1", params = list(metaId))
  if(nrow(stratNameAlias) > 1){
    aliases <- jsonlite::fromJSON(stratNameAlias$strategy_alias[1])
    for(ai in seq_along(aliases)){
      strategyReturns <- strategyReturns |> mutate(strategy = str_replace(strategy, paste(aliases$alias[[ai]], collapse="|"), aliases$primary[[ai]]))
    }
  }
  strategyRets <- strategyReturns |> select(-c(yr, mth, ret_pct)) |>
    pivot_wider(id_cols = dt, names_from = strategy, values_from = ret)
  
  names2Remove <- (strategyRets |> summarise(across(everything(), ~sum(is.na(.)))) |>
    pivot_longer(cols = everything()) |>
    filter(value > nrow(strategyRets)*0.9) |>
    select(name))$name
  
  strategyRets <- strategyRets |> select(-all_of(names2Remove))
  
  for(j in 2:ncol(strategyRets)){
    srXts <- xts(strategyRets[,j], strategyRets$dt)
    srXts <- tail(srXts, 24)
    
    names(srXts)[1] <- "Strategy"
    retXts <- merge(srXts, monthlyBenchRets)
    retXts <- na.omit(retXts)
    
    stratFname <- gsub(' ', '_', colnames(strategyRets)[j])
    stratTname <- str_to_title(colnames(strategyRets)[j])
    sr <- paste(round(SharpeRatio.annualized(retXts), 2), collapse="/")
    Common.PlotCumReturns(retXts, sprintf("%s Returns", stratTname), sprintf("SR: %s", sr), #NULL)
                          sprintf("%s/pms-%d.%s.cum.png", reportPath, metaId, stratFname))
    
    toPlot <- data.frame(retXts*100)
    toPlot$T <- index(retXts)
    
    ggplot(toPlot |> pivot_longer(cols=-T), aes(x=T, y=value, fill = name)) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b") +
      scale_fill_viridis_d() +
      labs(x='', y='returns (%)', fill = '',
           title = sprintf("%s Returns", stratTname),
           subtitle = sprintf("%s:%s", min(toPlot$T), max(toPlot$T)),
           caption = '@StockViz')
    
    ggsave(sprintf("%s/pms-%d.%s.monthly.png", reportPath, metaId, stratFname), 
           width=12, height=6, units="in")
    
    fileTracker <- rbind(fileTracker, c(metaId, stratFname, stratTname))
  }
}

print("rendering master page...")

render("rp-IN.Rmd", output_file="rp-IN.html")

#q()


