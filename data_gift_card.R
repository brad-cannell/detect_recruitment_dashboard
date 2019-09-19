# =============================================================================
# Read-in and wrangle gift card data
# Created: 2019-09-12
# =============================================================================

library(dplyr)

# Read-in data
# Download to desktop to refresh
gift_card <- readr::read_csv("/Users/bradcannell/Desktop/Gift Card.csv")

# Data wrangling

## Convert variable names to snake case
names(gift_card) <- names(gift_card) %>% stringr::str_replace_all('(\\B)([A-Z])', '_\\2') 
names(gift_card) <- names(gift_card) %>% tolower()
gift_card <- rename(gift_card, medstar_id = medstar_i_d)

## All we need at this point is the number of gift cards given out, i.e. rows
## in this data
n_completed <- nrow(gift_card)

# Clean up
rm(gift_card)