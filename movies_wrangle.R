
# Install dplyr, tidyr, and lubridate
library(dplyr)
library(tidyr)
library(lubridate)

# Bring in the movies dataset
library(readr)
movies_original <- read_csv("~/Downloads/movies_original.csv", 
                            col_types = cols(movie_theatrical_release_release_date = col_date(format = "%Y-%m-%d")))

# Create new, simpler name for columns
movies_columns <- c("Title", "Prod_year", "Is_sequel", "Run_time", "Creative_type", "Source", "Prod_method", "Genre", "Prod_budget",
                    "Dom_BO", "Intl_BO", "Total_BO", "Infl_Adj_Dom_BO", "MPAA_rating", "Rel_date_TH", "Keywords", "Prod_company",
                    "Opening_rev", "Opening_theaters")
colnames(movies_original) <- movies_columns

# Examine columns and determine steps to take with missing values (using table(), length(), etc.)
# Columns without missing values = Title, Prod_year, Run_time, Prod_budget, Dom_BO, Intl_BO, Rel_date_TH
# Columns with missing values, but decided to do nothing = Creative_type, Source, Prod_method, Genre, Total_BO, MPAA_rating, Keywords,
# Prod_company, Opening_rev, Opening_theaters

# Five movies missing this value, none of which were sequels
movies_original$Is_sequel <- ifelse(is.na(movies_original$Is_sequel), 0, movies_original$Is_sequel)

# Movies without Dom_BO are NA for Inf_Adj_Dom_B).  Setting to 0 (as 0 adjusted for inflation is still 0)
movies_original$Infl_Adj_Dom_BO <- ifelse(is.na(movies_original$Infl_Adj_Dom_BO), 0, movies_original$Infl_Adj_Dom_BO)

# Things to consider for analysis
# 1. Remove movies without any Total_BO
# 2. Remove movies with future release date (only about 15 of these)
# 3. Remove any movie without opening rev or theaters (if important for analysis)

# Upload BLS datasets, Surv_resp = respondent level data, BLS_summary = summary information about the responses,
# and Cur_Pop = current population information about the respondents
Surv_resp <- atusact_0316 <- read_csv("~/Desktop/atusact_0316/atusact_0316.dat", 
                                      col_types = cols(TUCASEID = col_character()))
BLS_summary <- atussum_0316 <- read_csv("~/Desktop/atussum_0316/atussum_0316.dat", 
                                        col_types = cols(TUCASEID = col_character()))
Cur_pop <- atuscps_0316 <- read_csv("~/Desktop/atuscps_0316/atuscps_0316.dat", 
                                    col_types = cols(TUCASEID = col_character()))
