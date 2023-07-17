library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "amur22_non_iwp", 
                 host = "192.168.5.219", port = 5432,
                 user = "postgres", password = "qq")
dbListTables(con, schema = "data")
dbListFields(con, "data.catalog")
q <- "SELECT * FROM data.catalog"
df <- dbGetQuery(con, q)
