# 1049_comm_rehab_equity
Exploring equity of access to community services after hospital discharge

Following a stay in hospital, many patients, particularly older adults, require support at home from a healthcare professional such as a district nurse, an occupational therapist, or a physiotherapist.  This report explores the extent to which these post-discharge community healthcare services are equitably distributed.  It addresses this question over four potential axes of inequality: socio-economic group, ethnicity, sex, and geography.  

The analysis uses four datasets:
  1. The Community Servuices Data Set (access via NCDR)
  2. The admitted patient care dataset (access via NCDR)
  3. The Index of Deprivation 2019 (From the Ministry of Housing, Communities & Local Government website)
  4. A lookup from Lower Super Output Area to Integrated care Board (from the Office for National Statistics - Open Geography Portal)
  
The analysis consiste of a series TSQL and R scripts whoch must be run in the following order.

 A  extract_data_for_model.tsql 
 B  1 drawDownCommRehanData.R
    2 exploreData.R
    3 mapDataCoverage.R
    4 createModelDataFrame.R
    5 descrivbeStudyPopulation.R
    6 drescribeoUtcomeFrequency.R
    7 builModel.R
    8 exploreModelResults.R
    
The R scruipts require a number of packages inclusding tidyverse, here, broom, odbc. mgcv, stringr, lubridate, scales, maptools, rgdal, greos, ggpubr.

The scripts produce a series of RDS files, charts images and csv tables.

  
