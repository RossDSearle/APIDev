


msqlDriver   = "ODBC Driver 17 for SQL Server"
msqlServer   = "asris-sql-stage.it.csiro.au\\sql2017"
msqlDatabase = "NatSoil_Ross"
msqlUID      = 'rosssearle'
msqlPWD      = 'Ads@2*&5cv'


models <- c(SOC='SOC_Model_V2.rds', BD='BD_Model_V2.rds')
modelNames <- c('SOC', 'BD')
availAtts <- data.frame(modelNames, models)

DEF_agency_code='601' 
DEF_proj_code='spectest'

