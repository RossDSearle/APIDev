


msqlDriver   = "ODBC Driver 17 for SQL Server"
msqlServer   = "asris-sql-stage.it.csiro.au\\sql2017"
msqlDatabase = "NatSoil_Ross"
msqlUID      = 'rosssearle'
msqlPWD      = 'Ads@2*&5cv'


models <- c(SOC='SOC_Model_V2.rds', BD='BD_Model_V2.rds')
modelNames <- c('SOC', 'BD')
labcodes <- c('6A1', '4A1')
availAtts <- data.frame(modelNames, models, labcodes)

DEF_agency_code='601' 
DEF_proj_code='SpecDemo'

DEF_User='DemoUser'

