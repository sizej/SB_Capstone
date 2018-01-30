
# Install dplyr, tidyr, and lubridate
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyr)

# Bring in the movies dataset
library(readr)
movies_original <- read_csv("~/Downloads/movies_original.csv", 
                            col_types = cols(movie_theatrical_release_release_date = col_date(format = "%Y-%m-%d")))

# Create new, simpler name for columns
m_columns <- c("Title", "Prod_year", "Is_sequel", "Run_time", "Creative_type", "Source", "Prod_method", "Genre", "Prod_budget",
                    "Dom_BO", "Intl_BO", "Total_BO", "Infl_Adj_Dom_BO", "MPAA_rating", "Rel_date_TH", "Keywords", "Prod_company",
                    "Opening_rev", "Opening_theaters")
colnames(movies_original) <- m_columns

# Examine columns and determine steps to take with missing values (using table(), length(), etc.)
# Columns without missing values = Title, Prod_year, Run_time, Prod_budget, Dom_BO, Intl_BO, Rel_date_TH
# Columns with missing values, but decided to do nothing = Creative_type, Source, Prod_method, Genre, Total_BO, MPAA_rating, Keywords,
# Prod_company, Opening_rev, Opening_theaters

# Change NA to "Unknown" for Genre
movies_original$Genre <- ifelse(is.na(movies_original$Genre), "Unknown", movies_original$Genre)

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
Surv_resp <- read_csv("~/Desktop/atusact_0316/atusact_0316.dat", 
                                      col_types = cols(TUCASEID = col_character()))
BLS_summary <- read_csv("~/Desktop/atussum_0316/atussum_0316.dat", 
                                        col_types = cols(TUCASEID = col_character()))
Cur_pop <- read_csv("~/Desktop/atuscps_0316/atuscps_0316.dat", 
                                    col_types = cols(TUCASEID = col_character()))

# Get survey down to monthly summary and index it for comparison
# THIS ONE WORKS A LITTLE BIT
Cons_time_spent <- Surv_resp %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303" | TRCODEP == '120403') %>% 
  left_join(y = select(Cur_pop, TUCASEID, HRMONTH), by = 'TUCASEID') %>% 
  distinct() %>% 
  group_by(HRMONTH, TRCODEP) %>% 
  summarize("ave" = mean(TUACTDUR)) %>% 
  spread(key = TRCODEP, value = ave)

colnames(Cons_time_spent) <- c('HRMONTH', 'tv_mean', 'film_mean')

tv_total <- sum(Cons_time_spent$tv_mean)
film_total <- sum(Cons_time_spent$film_mean)
ent_total <- tv_total + film_total

Cons_time_spent_summ <- Cons_time_spent %>% 
  mutate("tv_index" = tv_mean/tv_total, "film_index" = film_mean/film_total) %>% 
  mutate("ent_mean" = tv_mean + film_mean) %>%  
  select(HRMONTH, tv_index, film_index, ent_index) %>% 
  gather(key = "measure", value ="value", tv_index, film_index)

Cons_time_spent_raw <- Surv_resp %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303" | TRCODEP == '120403') %>% 
  left_join(y = select(Cur_pop, TUCASEID, HRMONTH, HRYEAR4), by = 'TUCASEID') %>% 
  distinct()

ggplot(Cons_time_spent2, aes(x = factor(HRMONTH), y = TUACTDUR, group = TRCODEP, fill = TRCODEP)) + geom_point(alpha = 0.4, size = 2)

#import FRED data for indexing inflation (Dec 2017 = 100)
FRED_infl <- read_csv("~/Downloads/CPIAUCNS (2).csv", 
                      col_types = cols(CPIAUCNS_NBD20171201 = col_number(), 
                                       DATE = col_date(format = "%Y-%m-%d")))
colnames(FRED_infl) <- c("Date", "Infl_index")

# bring in zoo package for yearmon functionality
library(zoo)
FRED_infl <- FRED_infl %>% 
  mutate("yearmon" = as.yearmon(Date))

# adjust budgets for inflation
movies_scs_base_infl <- movies_scs_base %>% 
  mutate("Rel_yearmon" = as.yearmon(Rel_date_TH)) %>% 
  left_join(y = FRED_infl, Infl_index, by = c("Rel_yearmon" = "yearmon")) %>% 
  mutate("Infl_prod_budget" = as.numeric(Prod_budget * (100 / Infl_index)),
         "Infl_Dom_BO_FRED" = as.numeric(Dom_BO * (100 / Infl_index)),
         "Infl_total_BO" = as.numeric(Total_BO * (100 / Infl_index)))

movies_scs_base_ave <- movies_scs_base %>% 
  mutate("Rel_yearmon" = as.yearmon(Rel_date_TH)) %>% 
  left_join(y = FRED_infl, Infl_index, by = c("Rel_yearmon" = "yearmon")) %>% 
  mutate("Infl_prod_budget" = as.numeric(Prod_budget * (100 / Infl_index))) %>% 
  group_by("year" = year(Rel_date_TH)) %>% 
  summarize("ave" = mean(Infl_prod_budget), "count" = n())
