source("functions.R")

host <- Sys.getenv("MONTAGU_DB_HOST", "localhost")
port <- as.integer(Sys.getenv("MONTAGU_DB_PORT", 8888))
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "montagu",
                      host = host,
                      port = port,
                      password = "changeme",
                      user = "vimc")

igme <- "E:/Data/Demography/ChildMortality2015/UN IGME Total U5MR, IMR and NMR Database 2015.xlsx"

cm_comparison_graphs(con, igme)

