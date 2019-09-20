# =============================================================================
# Read-in and wrangle data from FM Pro DB
# Created: 2019-09-20
# =============================================================================

library(dplyr)
source("make_posixct.R") # Used to convert dates to POSIXct

# Open the Connection to the FM database
# Keyring notes: https://db.rstudio.com/best-practices/managing-credentials/
con <- DBI::dbConnect(
  odbc::odbc(),
  driver   = "/Library/ODBC/FileMaker ODBC.bundle/Contents/MacOS/FileMaker ODBC",
  server   = "spsqlapwv003.sph.uthouston.edu",
  database = "DETECT",
  uid      = keyring::key_list("detect_fm_db_readonly")[1,2],
  pwd      = keyring::key_get("detect_fm_db_readonly")
)