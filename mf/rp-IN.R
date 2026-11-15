library('RODBC')
library('rmarkdown')

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/Pandoc")

#source("C:/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

asofDt<-sqlQuery(lcon, "select max(as_of) from mf_nav_history")[[1]]
startDate <- Sys.Date() - 5*365

print("subsetting mutual funds...")

largecapFunds <- sqlQuery(lcon, sprintf("select scheme_code, min(as_of) stdt, max(as_of) ltdt from mf_nav_history where 
										(scheme_name like '%%large%%direct%%' or scheme_name like '%%bluechip%%direct%%')
										and scheme_name like '%%growth%%' 
										and scheme_name not like '%%idcw%%' 
										and scheme_name not like '%%bonus%%' 
										and scheme_name not like '%%mid%%' 
										and scheme_name not like '%%small%%' 
										and scheme_name not like '%%tax%%' 
										and scheme_name not like '%%emerging%%' 
										and scheme_name not like '%% US %%'
										and as_of >= '%s' 
										group by scheme_code", startDate))
largecapFunds <- largecapFunds[largecapFunds$stdt <= startDate+180 & largecapFunds$ltdt >= asofDt-5,]

save(largecapFunds, file="largecapFunds.RData")

midcapFunds <- sqlQuery(lcon, sprintf("select scheme_code, min(as_of) stdt, max(as_of) ltdt from mf_nav_history where 
										scheme_name like '%%mid%%cap%%direct%%' 
										and scheme_name like '%%growth%%' 
										and scheme_name not like '%%idcw%%' 
										and scheme_name not like '%%bonus%%' 
										and scheme_name not like '%%large%%' 
										and scheme_name not like '%%small%%' 
										and scheme_name not like '%%tax%%' 
										and as_of >= '%s' 
										group by scheme_code", startDate))
midcapFunds <- midcapFunds[midcapFunds$stdt <= startDate+180 & midcapFunds$ltdt >= asofDt-5,]

save(midcapFunds, file="midcapFunds.RData")

#q()

print("rendering master page...")

render("rp-IN.Rmd", output_file="rp-IN.html")

#q()

print("rendering large caps...")
for(sc in largecapFunds$scheme_code){
	print(paste(sc, 'LARGECAP'))
	tryCatch({
		render("analysis/rp-mf.Rmd", output_file=paste0("rp-", sc, ".html"), params=list(scheme_code = sc, benchmark = 'LARGECAP'))
	}, error=function(e){print(e)})
}

print("rendering midcaps caps...")
for(sc in midcapFunds$scheme_code){
	print(paste(sc, 'MIDCAP'))
	tryCatch({
		render("analysis/rp-mf.Rmd", output_file=paste0("rp-", sc, ".html"), params=list(scheme_code = sc, benchmark = 'MIDCAP'))
	}, error=function(e){print(e)})
}