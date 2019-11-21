library(DBI)
library(odbc)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "asris-sql-stage.it.csiro.au\\sql2017",
                      Database = "NatSoil",
                      UID      = 'NEXUS\\sea084',
                      PWD      = 'Joan4066',
                      Trusted_Connection = "True"
                      )


dbListTables(con)

att <- "Organic carbon"


sql <- paste0("SELECT ARCHIVE_SAMPLES.spec_id, OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no, OBSERVATIONS.o_date_desc, OBSERVATIONS.o_longitude_GDA94, OBSERVATIONS.o_latitude_GDA94, SAMPLES.samp_upper_depth, SAMPLES.samp_lower_depth, LAB_RESULTS.labm_code, LAB_METHODS.LABM_SHORT_NAME, LAB_METHODS.LABM_NAME, LAB_RESULTS.labr_date, LAB_RESULTS.labr_value, LAB_METHODS.LABM_UNITS 
              FROM (((((SITES INNER JOIN OBSERVATIONS ON (SITES.s_id = OBSERVATIONS.s_id) AND (SITES.proj_code = OBSERVATIONS.proj_code) AND (SITES.agency_code = OBSERVATIONS.agency_code)) INNER JOIN HORIZONS ON (OBSERVATIONS.o_id = HORIZONS.o_id) AND (OBSERVATIONS.s_id = HORIZONS.s_id) AND (OBSERVATIONS.proj_code = HORIZONS.proj_code) AND (OBSERVATIONS.agency_code = HORIZONS.agency_code)) INNER JOIN SAMPLES ON (HORIZONS.h_no = SAMPLES.h_no) AND (HORIZONS.o_id = SAMPLES.o_id) AND (HORIZONS.s_id = SAMPLES.s_id) AND (HORIZONS.proj_code = SAMPLES.proj_code) AND (HORIZONS.agency_code = SAMPLES.agency_code)) INNER JOIN LAB_RESULTS ON (SAMPLES.samp_no = LAB_RESULTS.samp_no) AND (SAMPLES.h_no = LAB_RESULTS.h_no) AND (SAMPLES.o_id = LAB_RESULTS.o_id) AND (SAMPLES.s_id = LAB_RESULTS.s_id) AND (SAMPLES.proj_code = LAB_RESULTS.proj_code) AND (SAMPLES.agency_code = LAB_RESULTS.agency_code)) INNER JOIN ARCHIVE_SAMPLES ON (SAMPLES.samp_no = ARCHIVE_SAMPLES.samp_no) AND (SAMPLES.h_no = ARCHIVE_SAMPLES.h_no) AND (SAMPLES.o_id = ARCHIVE_SAMPLES.o_id) AND (SAMPLES.s_id = ARCHIVE_SAMPLES.s_id) AND (SAMPLES.proj_code = ARCHIVE_SAMPLES.proj_code) AND (SAMPLES.agency_code = ARCHIVE_SAMPLES.agency_code)) INNER JOIN LAB_METHODS ON LAB_RESULTS.labm_code = LAB_METHODS.LABM_CODE  
              WHERE ARCHIVE_SAMPLES.spec_id=10027 AND LAB_METHODS.LABM_SHORT_NAME='Organic carbon' 
              ORDER BY OBSERVATIONS.agency_code, OBSERVATIONS.proj_code, OBSERVATIONS.s_id, OBSERVATIONS.o_id, HORIZONS.h_no, SAMPLES.samp_no")

#sql <- 'select * from ARCHIVE_SAMPLES'

qry <- dbSendQuery(con, sql)
res <- dbFetch(qry)
head(res)
dbClearResult(qry)
