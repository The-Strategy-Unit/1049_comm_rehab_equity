# this script needs to be run in the NCDR data science environment

library(tidyverse)
library(here)
library(odbc)


# connect to NCDR SQL server
con_ncdr <- dbConnect(odbc(),
                      Driver = "SQL Server",
                      Server = "PRODNHSESQL101",
                      Database = "NHSE_Sandbox_StrategyUnit",
                      Trusted_Connection = "True")


script <- 'select * from [NHSE_Sandbox_StrategyUnit].dbo.sw__CommRehabEqu_ipDischarges_priorPostCommContacts'

 
comRehabEquity <- dbGetQuery(con_ncdr, script)
 

saveRDS(comRehabEquity, file = here('dataRDS', 'comRehabEquity.RDS'))







script2 <- 'select * from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts_lsoa_counts'

commContactsLsoa <- dbGetQuery(con_ncdr, script2)

saveRDS(commContactsLsoa, file = here('dataRDS', 'commContactsLsoa.RDS'))




rm(con_ncdr, script, comRehabEquity, script2, commContactsLsoa)
