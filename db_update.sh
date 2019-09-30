# =============================================================================
# DETECT Recruiting Dashboard Update
# Created: 2019-09-30
# =============================================================================

# Make sure you are connected to the VPN

# Change directory to recruitment dashboard folder
cd "/Users/bradcannell/Dropbox/Research/Elder Abuse/DETECT NIH RFA-AG-18-010/detect_recruitment_dashboard"

# Knit index.Rmd
Rscript -e 'rmarkdown::render("index.Rmd")'
