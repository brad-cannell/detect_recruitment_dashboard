# =============================================================================
# DETECT Recruiting Dashboard Update
# Created: 2019-09-30
# Stopped working on 2019-10-29 after upgrading to macOS Catalina. 
# Error message: pandoc version 1.12.3 or higher is required and was not found (see the help page ?rmarkdown::pandoc_available).
# =============================================================================

# Make sure you are connected to the VPN

# Change directory to recruitment dashboard folder
cd "/Users/bradcannell/Dropbox/Research/Elder Abuse/DETECT NIH RFA-AG-18-010/detect_recruitment_dashboard"

# Checking error
Rscript -e 'rmarkdown::pandoc_version()'

# Knit index.Rmd
Rscript -e 'rmarkdown::render("index.Rmd")'
