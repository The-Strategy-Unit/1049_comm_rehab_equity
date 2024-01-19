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





script3 <- 'select AttendOrNot, Consultation_MediumUsed, COUNT(*) as atts from [NHSE_Sandbox_StrategyUnit].dbo.sw_CommRehabEqu_CommContacts group by AttendOrNot, Consultation_MediumUsed'

commContactsCompleteness2vars <- dbGetQuery(con_ncdr, script3)

saveRDS(commContactsCompleteness2vars, file = here('dataRDS', 'commContactsCompleteness2vars.RDS'))



rm(con_ncdr, script, comRehabEquity, script2, commContactsLsoa, script3, commContactsCompleteness2vars)





