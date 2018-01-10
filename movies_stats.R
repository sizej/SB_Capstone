
# tidy summary data_frame for plotting monthly summary
annual_summary <- movies_original %>%
  group_by("month" = month(Rel_date_TH)) %>% 
  summarise("total" = sum(Total_BO), "count" = n(), "budg_tot" = sum(as.numeric(Prod_budget))) %>% 
  mutate("total_index" = total / sum(total), "count_index" = count / sum(count), "budg_index" = budg_tot / sum(budg_tot)) %>% 
  select(month, total_index, count_index, budg_index) %>% 
  gather(key = "measure", value = "value", total_index, count_index, budg_index)

# monthly summary of percent of films, box office, and budget
ggplot(annual_summary, aes(x = factor(month), y = value, shape = measure, colour = measure)) + geom_point(size = 6)

# same data but with lines instead of scatter
ggplot(annual_summary, aes(x = factor(month), y = value, group = measure, colour = measure)) + geom_smooth(method = "loess", se = FALSE)

# what would be considered a commercial success = 4 times the budget (just a guess)
movies_scs_base <- movies_original %>%  
  mutate("perf_ratio" = Total_BO / Prod_budget) %>% 
  mutate("Is_comm_success" = ifelse(perf_ratio >= 4, 1, 0)) 

movies_scs <- movies_original %>%  
  mutate("perf_ratio" = Total_BO / Prod_budget) %>% 
  mutate("Is_comm_success" = ifelse(perf_ratio >= 4, 1, 0)) %>% 
  select(Rel_date_TH, Is_comm_success) %>% 
  group_by("month" = month(Rel_date_TH), Is_comm_success)%>% 
  summarise("count" = n())

# bar chart of commercial successes by month
ggplot(movies_scs, aes(x = factor(month), y = count, group = Is_comm_success, fill = factor(Is_comm_success))) + geom_bar(position = "dodge", stat = "identity")

# data set for success rate by month
movies_scs_test <- movies_original %>%  
  mutate("perf_ratio" = Total_BO / Prod_budget) %>% 
  mutate("Is_comm_success" = ifelse(perf_ratio >= 4, "CS", "Not_CS")) %>% 
  select(Rel_date_TH, Is_comm_success) %>% 
  group_by("month" = month(Rel_date_TH), Is_comm_success) %>% 
  summarise("mcount" = n()) %>%
  spread(key = Is_comm_success, value = mcount) %>%
  mutate("success_rate" = CS / (CS + Not_CS))

# plot success rate by month
ggplot(movies_scs_test, aes(x = factor(month), y = success_rate)) + geom_bar(stat = "identity")



