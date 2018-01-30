# Logistic regression for entire data set
# First step is to make sure the data set contains everything I would want to include
# namely, that it includes consumer

CTS_YM <- Cons_time_spent_raw %>% 
  mutate("yearmon" = as.yearmon(paste(HRYEAR4, HRMONTH, sep = '-')))

colnames(CTS_YM) <- c('ID', "Act_dur", "Act_code", "Month", "Year", "yearmon")

CTS_YM$Act_code <- ifelse(CTS_YM$Act_code == '120303', "TV",
                          ifelse(CTS_YM$Act_code == '120403', "Movies_TH", ""))

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

CTS_YM <- CTS_YM %>%
  select(yearmon, Act_code, Act_dur) %>% 
  group_by(yearmon, Act_code) %>% 
  summarize("Act_dur_mean" = mean(Act_dur)) %>% 
  spread(key = Act_code, value = Act_dur_mean)

# consumer  data only available from 200209 to 201610
YM_All_1 <- CTS_YM %>% 
  filter(!is.na(Movies_TH)) %>% 
  left_join(y = movies_summ_YM, by = c("yearmon" = "Rel_yearmon")) %>% 
  left_join(y = select(movies_CS_YM, Rel_yearmon, success_rate), by = c("yearmon" = "Rel_yearmon")) %>% 
  ungroup()

#scale/normalize the data
YM_All_scaled <- YM_All_1 %>% 
  select(-yearmon) %>% 
  scale()

# strip out yearmon to be added back for charting
YM_yearmon <- YM_All_1 %>% 
  select(yearmon)

# add back yearmon to the scaled data
YM_All_scaled <- as.data.frame(YM_All_scaled)
YM_All_scaled_ind <- bind_cols(YM_yearmon, YM_All_scaled)

#convert to tidy for charting purposes
YM_All_tidy <- YM_All_scaled_ind %>% 
  gather(key = "measure", value = "value", Movies_TH, TV, Prod_budg_mean,Dom_BO_mean,success_rate)

ggplot(YM_All_tidy, aes(x = yearmon, y = value, group = measure, colour = measure)) + 
  geom_line() + 
  scale_x_discrete(name = "Month_Year", breaks = ch_br)

YM_Cons_tidy <- YM_All_scaled_ind %>% 
  gather(key = "measure", value = "value", Movies_TH, TV, success_rate)

ggplot(YM_Cons_tidy, aes(x = factor(as.Date.yearmon(yearmon)), y = value, group = measure, colour = measure)) + geom_point(size = 1) +
  geom_line() + scale_x_discrete(name = "Month_year")

lin_mod_all <- lm(success_rate ~ Movies_TH + TV + Prod_budg_mean + Dom_BO_mean, data = YM_All_1)

lin_mod_cons <- lm(success_rate ~ Movies_TH + TV, data = YM_All_1)

# look at the last three years?  Five years?