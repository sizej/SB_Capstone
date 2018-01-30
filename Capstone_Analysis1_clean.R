
# get an aggregate data set together for plotting

# create a month by month summary of movies data
annual_summary <- movies_original %>%
  group_by("HRMONTH" = month(Rel_date_TH)) %>% 
  summarise("total_bo" = sum(Total_BO), "count" = n(), "budg_tot" = sum(as.numeric(Prod_budget))) %>% 
  mutate("BO_index" = total_bo / sum(total_bo), "films_rel_index" = count / sum(count), "budg_index" = budg_tot / sum(budg_tot)) %>% 
  select(HRMONTH, BO_index, films_rel_index, budg_index) %>% 
  gather(key = "measure", value = "value", BO_index, films_rel_index, budg_index)

# create a month by month summary of ATUS data
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

ATUS_summ <- Cons_time_spent %>% 
  mutate("cons_tv_index" = tv_mean / tv_total, "cons_film_index" = film_mean / film_total) %>% 
  select(HRMONTH, cons_tv_index, cons_film_index) %>% 
  gather(key = "measure", value ="value", cons_tv_index, cons_film_index)

# bind the rows from ATUS_summ to annual_summary
annual_summary <- annual_summary %>% 
  bind_rows(y = ATUS_summ)

# plot annual summary
plt_an <- ggplot(annual_summary, aes(x = factor(HRMONTH), y = value, shape = measure, colour = measure,
                                     group = measure)) + scale_x_discrete(name = "Month")
plt_an + geom_point(size = 4, alpha = 0.8)

# data set for success rate by month
movies_scs <- movies_original %>%  
  mutate("perf_ratio" = Total_BO / Prod_budget) %>% 
  mutate("Is_comm_success" = ifelse(perf_ratio >= 4, "CS", "Not_CS")) %>% 
  select(Rel_date_TH, Is_comm_success) %>% 
  group_by("month" = month(Rel_date_TH), Is_comm_success) %>% 
  summarise("mcount" = n()) %>%
  spread(key = Is_comm_success, value = mcount) %>%
  mutate("success_rate" = CS / (CS + Not_CS))

ATUS_summ2 <- Cons_time_spent %>% 
  select(HRMONTH, film_mean)

an_movies <- movies_original %>%
  group_by("HRMONTH" = month(Rel_date_TH)) %>% 
  summarise("total_bo" = sum(Total_BO), "count" = n(), "budg_tot" = sum(as.numeric(Prod_budget))) %>% 
  select(HRMONTH, total_bo, count, budg_tot) %>% 
  left_join(y = select(movies_scs, month, success_rate), by = c("HRMONTH" = "month")) %>% 
  left_join(y = ATUS_summ2, by = c("HRMONTH" = "HRMONTH"))

# scale an_movies for analysis
an_movies_scaled <- an_movies %>% 
  select(total_bo, count, film_mean, success_rate) %>% 
  scale() %>% 
  as.data.frame()

# bring back month for charting
HRMONTH <- as.data.frame(an_movies$HRMONTH)
an_movies_scaled <- bind_cols(HRMONTH, an_movies_scaled)


# create new annual summary for plotting
an_movies_scaled <- an_movies_scaled %>% 
  gather(key = "measure", value = "value", total_bo, count, film_mean, success_rate)

colnames(an_movies_scaled) <- c("month", "measure", "value")

an_movies_not_tidy <- an_movies_scaled %>% 
  spread(key = measure, value = value)

lm_an <- lm(success_rate ~ total_bo + count + film_mean + month, data = an_movies_not_tidy)

# create plot for scaled annual summary
scld_plt <- ggplot(an_movies_scaled, aes(x = factor(month), y = value, group = measure, fill = measure,
                                         color = measure))
scld_plt + geom_point(size = 2, alpha = 0.8) + geom_line()


###########################################
# Yearmon Analysis
###########################################

# bring in zoo package for yearmon functionality
library(zoo)

# get consumer time spent raw data
Cons_time_spent_raw <- Surv_resp %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303" | TRCODEP == '120403') %>% 
  left_join(y = select(Cur_pop, TUCASEID, HRMONTH, HRYEAR4), by = 'TUCASEID') %>% 
  distinct()

# add yearmon to every entry and clean up column names/entries
CTS_YM <- Cons_time_spent_raw %>% 
  mutate("yearmon" = as.yearmon(paste(HRYEAR4, HRMONTH, sep = '-')))
colnames(CTS_YM) <- c('ID', "Act_dur", "Act_code", "Month", "Year", "yearmon")
CTS_YM$Act_code <- ifelse(CTS_YM$Act_code == '120303', "TV",
                          ifelse(CTS_YM$Act_code == '120403', "Movies_TH", ""))

# summarize for plotting
CTS_YM <- CTS_YM %>%
  select(yearmon, Act_code, Act_dur) %>% 
  group_by(yearmon, Act_code) %>% 
  summarize("Act_dur_mean" = mean(Act_dur)) %>% 
  spread(key = Act_code, value = Act_dur_mean)

# create success rate by yearmon for joining with other yearmon summary number
movies_CS_YM <- movies_scs_base_infl %>%
  mutate("Is_CS" = ifelse(Is_comm_success == 1, "CS", "Not_CS")) %>% 
  filter(Dom_BO > 10000, year(Rel_date_TH) > 2001) %>%  
  group_by(Rel_yearmon, Is_CS) %>% 
  summarise("mcount" = n()) %>% 
  spread(key = Is_CS, value = mcount) %>%
  mutate("success_rate" = CS / (CS + Not_CS))

movies_summ_YM <- movies_scs_base_infl %>% 
  filter(Dom_BO > 10000, year(Rel_date_TH) > 2001) %>% 
  select(Rel_yearmon, Infl_prod_budget, Infl_Dom_BO_FRED) %>% 
  group_by(Rel_yearmon) %>% 
  summarise("Prod_budg_mean" = mean(Infl_prod_budget), "Dom_BO_mean" = mean(Infl_Dom_BO_FRED))

# join yearmon movies summary and yearmon consumer summary
# consumer  data only available from 200209 to 201610
YM_All_1 <- CTS_YM %>% 
  filter(!is.na(Movies_TH)) %>% 
  left_join(y = movies_summ_YM, by = c("yearmon" = "Rel_yearmon")) %>% 
  left_join(y = select(movies_CS_YM, Rel_yearmon, success_rate), by = c("yearmon" = "Rel_yearmon")) %>% 
  ungroup()

# scale/normalize the data
YM_All_scaled <- YM_All_1 %>% 
  select(-yearmon) %>% 
  scale()

# strip out yearmon to be added back for charting
YM_yearmon <- YM_All_1 %>% 
  select(yearmon)

# add back yearmon to the scaled data
YM_All_scaled <- as.data.frame(YM_All_scaled)
YM_All_scaled_ind <- bind_cols(YM_yearmon, YM_All_scaled)

# convert to tidy for charting purposes
YM_All_tidy <- YM_All_scaled_ind %>% 
  gather(key = "measure", value = "value", Movies_TH, TV, Prod_budg_mean,Dom_BO_mean,success_rate)

ggplot(YM_All_tidy, aes(x = yearmon, y = value, group = measure, colour = measure)) + 
  geom_line() + 
  scale_x_discrete(name = "Month_Year", breaks = NULL)

# create correlation matrix for variables
YM_All_cor <- YM_All_scaled_ind %>% 
  select(-yearmon)
cor(YM_All_cor, use = "complete.obs")

# create data set for linear regression of movies for which all data (budget, consumer survey, month of release, and films released
# in the same month) is present
mcount_YM <- movies_scs_base_infl %>% 
  select(Rel_yearmon) %>% 
  group_by(Rel_yearmon) %>% 
  summarize("YM_count" = n()) %>% 
  arrange(Rel_yearmon)

movies_lm <- movies_scs_base_infl %>% 
  filter(between(as.Date(Rel_date_TH), as.Date("2002-09-01"), as.Date("2016-11-01"))) %>% 
  select(Title, Infl_prod_budget, Rel_month, Rel_yearmon, Is_comm_success) %>% 
  left_join(y = select(YM_All_1, Movies_TH, yearmon), by = c("Rel_yearmon" = "yearmon")) %>% 
  left_join(y = select(mcount_YM, YM_count, Rel_yearmon), by = c("Rel_yearmon" = "Rel_yearmon")) %>% 
  select(Title, Infl_prod_budget, Rel_month, Movies_TH, YM_count, Is_comm_success)

# scale numerical variables
movies_lm_scaled <- movies_lm %>%
  select(Infl_prod_budget, Movies_TH, YM_count) %>% 
  scale() %>% 
  as.data.frame()

# create dummy variables for logistic regression
library(dummies)
movies_lm$Rel_month <- factor(movies_lm$Rel_month)

movies_lm <- as.data.frame(movies_lm)

movies_lm_dum <- dummy.data.frame(data = movies_lm, names = "Rel_month")

movies_lm_dum <- movies_lm_dum %>% 
  bind_cols(movies_lm_scaled) %>% 
  select(-Title, -Infl_prod_budget, -Movies_TH, -YM_count)

# create logistic regression
logR_movies <- glm(Is_comm_success ~ ., data = movies_lm_dum, family = "binomial")

##################################
# doing the log regression with Is_rel_prime instead:
##################################

movies_lm2 <- movies_scs_base_infl %>% 
  filter(between(as.Date(Rel_date_TH), as.Date("2002-09-01"), as.Date("2016-11-01"))) %>% 
  select(Title, Infl_prod_budget, Rel_month, Rel_yearmon, Is_comm_success) %>%
  mutate("Is_rel_prime" = ifelse(Rel_month %in% c(5,6,7,11,12),1,0)) %>% 
  left_join(y = select(YM_All_1, Movies_TH, yearmon), by = c("Rel_yearmon" = "yearmon")) %>% 
  left_join(y = select(mcount_YM, YM_count, Rel_yearmon), by = c("Rel_yearmon" = "Rel_yearmon")) %>% 
  select(Title, Infl_prod_budget, Is_rel_prime, Movies_TH, YM_count, Is_comm_success)

movies_lm2_md <- movies_lm2 %>%
  select(Is_rel_prime, Is_comm_success) %>% 
  bind_cols(movies_lm_scaled)

logR_movies2 <- glm(Is_comm_success ~ ., data = movies_lm2_md, family = "binomial")
