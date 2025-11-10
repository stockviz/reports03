library('rmarkdown')
library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('patchwork')

#source("/mnt/hollandC/stockviz/r/config.r")
source("/mnt/hollandr/config.r")

print("subsetting...")

pgCon <- dbConnect(
  RPostgres::Postgres(),
  host = 'sweden',
  user = ldbuser2,
  password = ldbpassword2,
  dbname = 'StockVizDyn',
  sslmode = 'allow'
)

today <- Sys.Date()
idCnts <- dbGetQuery(pgCon, "select id, count(val) cnt, max(yr) yr from sebi_pms_data group by id")
activePms <- idCnts |> filter(cnt >= 12 & yr >= year(today) -1)

save(activePms, file="active_pms.RData")

print("rendering master page...")

render("rp-IN.Rmd", output_file="rp-IN.html")

#q()


