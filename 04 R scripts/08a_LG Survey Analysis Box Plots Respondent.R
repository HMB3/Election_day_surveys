############################################ STAFF SURVEY BOX PLOTS ###################################


## Purpose ----
## The purpose of this code is to take the aggregated indexes of survey respondents - respondent/Venue/VenueName - 
## and plot the distribution of the indexes across topics and demographics

## To do ----
## Add ANOVA for index differences


## Two kinds of relationships::
## Discrete relationships   (boxplots, etc.)
## Continuous relationships (scatterplots)


## Consider how best to aggregate the data. By Venue, ReturningOffice, business unit, etc?
## In SG19, Respondent and Venue are not balanced - Not every respondent answers every question,
## Not every Venue is staffed by all roles
## Not all staff in all Venues are asked all questions, etc.
## - Respondent (n = 13617)
## - Venue      (n = 2265)
## - ReturningOffice   (n = 93)
nrow(SurveyData_numeric_questions)
length(unique(SurveyData_numeric_questions$VenueName))
length(unique(SurveyData_numeric_questions$ReturningOffice))


## Also consider how to present the data ::
## Can we present the analyses for all ReturningOffices? 
## This would be like Jerry's staffing dashboard Report for each ReturningOffice, 
## effectively each has it's own dashboard.





## 1). OVERALL BOXPLOTS  ========================================================================


## Boxplot of all respondent-level indexes for each topic
bp_resposes_all <- respondent_index_ro_join %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  factor_boxplots(., 
                  x_var       = "Category", 
                  y_var       = "Index", 
                  group_var   = "Category",
                  leg_pos     = 'none',
                  y_lab       = 'Respondent Index', 
                  x_lab       = '',
                  v_just      = 5,
                  mar         = 1.5,
                  
                  pallette    = brewer.pal(n = 10, name = "Paired"),
                  box_size    = 1.2, 
                  y_lim       = c(0.4, 1.0), 
                  lab_angle   = 45,
                  lab_size    = 18,
                  border_size = 3)


## Then run an anova for differences between response-level Topic Indexes
anova_responses_all <- respondent_index_ro_join %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  aov(Index ~ Category,  data = .)

hist(anova_responses_all$residuals)
plot(anova_responses_all, which = 2)

resp_resid_plot <- data_frame(
  fitted  = predict(anova_responses_all),
  residual = residuals(anova_responses_all)) %>%
  
  # and then plot points and a smoothed line
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))


responses_tukey_all <- TukeyHSD(anova_responses_all)





## Boxplot of all venue-level indexes for each topic
bp_venues_all <- respondent_index_venue_average %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  factor_boxplots(., 
                  x_var       = "Category", 
                  y_var       = "Index", 
                  group_var   = "Category",
                  leg_pos     = 'none',
                  y_lab       = 'Venue Index', 
                  x_lab       = '',
                  v_just      = 5,
                  mar         = 1.5,
                  
                  pallette    = brewer.pal(n = 10, name = "Paired"),
                  box_size    = 1.2, 
                  y_lim       = c(0.4, 1.0), 
                  lab_angle   = 45,
                  lab_size    = 18,
                  border_size = 3)


## Then run an anova for differences between Venue-level Topic Indexes
anova_venues_all <- respondent_index_venue_average %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  aov(Index ~ Category,  data = .)

hist(anova_venues_all$residuals)
plot(anova_venues_all, which = 2)


ven_resid_plot <- data_frame(
  fitted  = predict(anova_venues_all),
  residual = residuals(anova_venues_all)) %>%
  
  # and then plot points and a smoothed line
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))


venues_tukey_all <- TukeyHSD(anova_venues_all)





## Boxplot of all RO-level indexes for each topic
bp_returing_all <- respondent_index_ro_average %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  factor_boxplots(., 
                  x_var       = "Category", 
                  y_var       = "Index", 
                  group_var   = "Category",
                  leg_pos     = 'none',
                  y_lab       = 'RO Index', 
                  x_lab       = '',
                  v_just      = 5,
                  mar         = 1.5,
                  
                  pallette    = brewer.pal(n = 10, name = "Paired"),
                  box_size    = 1.2, 
                  y_lim       = c(0.4, 1.0), 
                  lab_angle   = 45,
                  lab_size    = 18,
                  border_size = 3)


## Then run an anova for differences between Venue-level Topic Indexes
anova_returing_all <- respondent_index_ro_average %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  aov(Index ~ Category,  data = .)

hist(anova_returing_all$residuals)
plot(anova_returing_all, which = 2)


RO_resid_plot <- data_frame(
  fitted  = predict(anova_returing_all),
  residual = residuals(anova_returing_all)) %>%
  
  # and then plot points and a smoothed line
  ggplot(aes(fitted, residual)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5))


returing_tukey_all   <- TukeyHSD(anova_returing_all)
returing_tukey_tab   <- TukeyHSD(anova_returing_all) %>% .[[1]]
returing_tukey_table <- returing_tukey_tab %>% as_tibble()
returing_tukey_table$compare = rownames(returing_tukey_tab)
returing_tukey_table <- dplyr::select(returing_tukey_table, compare, everything())




## 2). ROLE BOXPLOTS  ========================================================================


## Boxplots of Respondent indexes for each topic * role combination
respondent_role_boxplots <- boxplot_list(plot_list   = index_plot_columns,
                                         survey_data = respondent_index_ro_join,
                                         columns     = index_plot_columns,
                                         agg_level   = "Respondent",
                                         
                                         x_var       = "Category", 
                                         y_var       = "Index", 
                                         group_var   = "Role",
                                         leg_pos     = 'right',
                                         y_lab       = '', 
                                         x_lab       = '',
                                         pallette    = "Dark2",
                                         v_just      = 5,
                                         mar         = 1.5,
                                         
                                         box_size    = 1.2,
                                         y_lim       = c(0.4, 1.0),
                                         lab_angle   = 45,
                                         lab_size    = 18,
                                         border_size = 3)


## Need to return both elements
## index like this :: respondent_role_anovas[[1]][1]
respondent_role_anovas <- anova_list(plot_list   = index_plot_columns,
                                     survey_data = respondent_index_ro_join,
                                     columns     = index_plot_columns,
                                     group_var   = "Role")



## Boxplots of Venue indexes for each topic * role combination
venue_role_boxplots <- boxplot_list(plot_list   = index_plot_columns,
                                    survey_data = respondent_index_venue_average,
                                    columns     = index_plot_columns,
                                    agg_level   = "Venue",
                                    
                                    x_var       = "Category", 
                                    y_var       = "Index", 
                                    group_var   = "Role",
                                    leg_pos     = 'right',
                                    y_lab       = '', 
                                    x_lab       = '',
                                    pallette    = "Dark2",
                                    v_just      = 5,
                                    mar         = 1.5,
                                    
                                    box_size    = 1.2,
                                    y_lim       = c(0.4, 1.0),
                                    lab_angle   = 45,
                                    lab_size    = 18,
                                    border_size = 3)



## Boxplots of Venue indexes for each topic * role combination
returing_role_boxplots <- boxplot_list(plot_list   = index_plot_columns,
                                       survey_data = respondent_index_ro_average,
                                       columns     = index_plot_columns,
                                       agg_level   = "RO",
                                       
                                       x_var       = "Category", 
                                       y_var       = "Index", 
                                       group_var   = "Role",
                                       leg_pos     = 'right',
                                       y_lab       = '', 
                                       x_lab       = '',
                                       pallette    = "Dark2",
                                       v_just      = 1,
                                       mar         = 1.2,
                                       
                                       box_size    = 1.2,
                                       y_lim       = c(0.4, 1.0),
                                       lab_angle   = 45,
                                       lab_size    = 18,
                                       border_size = 3)





## 3). DEMOGRAPHY BOXPLOTS  ========================================================================



## Boxplots of indexes for each topic by demographic
respondent_demo_boxplots <- boxplot_group_list(plot_list   = index_plot_columns,
                                               survey_data = respondent_index_ro_join,
                                               columns     = index_plot_columns,
                                               agg_level   = "Respondent",
                                               
                                               x_var       = "Category", 
                                               group_var   = "Category",
                                               group_vars  = c("Gender", "Languages", "Disability", "Indigenous"),
                                               y_var       = "Index", 
                                               leg_pos     = 'none',
                                               y_lab       = 'Respondent Index',
                                               x_lab       = '',
                                               pallette    = brewer.pal(n = 9, name = "Paired"),
                                               v_just      = 4,
                                               mar         = 1.2,
                                               
                                               box_size    = 1.2,
                                               y_lim       = c(0.4, 1.0),
                                               lab_angle   = 45,
                                               lab_size    = 18,
                                               border_size = 3,
                                               levels      = c("Female",         "Male", "Other",
                                                               "Multilingual",   "English Only",   
                                                               "No Disability",  "Disability",
                                                               "Indigenous",     "Non-Indigenous"))


## Need to return both elements
## index like this :: respondent_demo_anovas[[1]][2]
respondent_demo_anovas <- anova_group_list(plot_list   = index_plot_columns,
                                           survey_data = respondent_index_ro_join,
                                           columns     = index_plot_columns,
                                           
                                           group_var   = "Category",
                                           group_vars  = c("Gender", "Languages", "Disability", "Indigenous"),
                                           levels      = c("Female",         "Male", "Other",
                                                           "Multilingual",   "English Only",   
                                                           "No Disability",  "Disability",
                                                           "Indigenous",     "Non-Indigenous"))


## Boxplots of indexes for each topic by remoteness category
respondent_remoteness_boxplots <- boxplot_group_list(plot_list   = index_plot_columns,
                                                     survey_data = respondent_index_ro_join,
                                                     columns     = index_plot_columns,
                                                     agg_level   = "Respondent",
                                                     
                                                     x_var       = "Category", 
                                                     y_var       = "Index", 
                                                     v_just      = 1,
                                                     mar         = 1.2,
                                                     
                                                     group_vars  = "Remoteness",
                                                     group_var   = "Category",
                                                     leg_pos     = 'none',
                                                     y_lab       = '',
                                                     x_lab       = '',
                                                     pallette    = brewer.pal(n = 3, name = "Paired"),
                                                     
                                                     box_size    = 1.2,
                                                     y_lim       = c(0.4, 1.0),
                                                     lab_angle   = 45,
                                                     lab_size    = 18,
                                                     border_size = 3,
                                                     levels      = c("Major Cities", "Regional", "Remote"))


## Those plots would need the context data joined to the Venue and/or RO

# venue_remoteness_boxplots <- boxplot_group_list(plot_list   = index_plot_columns,
#                                                 survey_data = respondent_index_venue_average,
#                                                 columns     = index_plot_columns,
#                                                 agg_level   = "Venue",
#                                                 
#                                                 x_var       = "Category", 
#                                                 y_var       = "Index", 
#                                                 
#                                                 group_vars  = "Remoteness",
#                                                 group_var   = "Category",
#                                                 leg_pos     = 'none',
#                                                 y_lab       = '',
#                                                 x_lab       = '',
#                                                 pallette    = brewer.pal(n = 3, name = "Paired"),
#                                                 
#                                                 box_size    = 1.2,
#                                                 y_lim       = c(0.4, 1.0),
#                                                 lab_angle   = 45,
#                                                 lab_size    = 18,
#                                                 border_size = 3,
#                                                 levels      = c("Major Cities", "Regional", "Remote"))



# returning_remoteness_boxplots <- boxplot_group_list(plot_list   = index_plot_columns,
#                                                     survey_data = respondent_index_ro_average,
#                                                     columns     = index_plot_columns,
#                                                     agg_level   = "Venue",
#                                                     
#                                                     x_var       = "Category", 
#                                                     y_var       = "Index", 
#                                                     
#                                                     group_vars  = "Remoteness",
#                                                     group_var   = "Category",
#                                                     leg_pos     = 'none',
#                                                     y_lab       = '',
#                                                     x_lab       = '',
#                                                     pallette    = brewer.pal(n = 3, name = "Paired"),
#                                                     
#                                                     box_size    = 1.2,
#                                                     y_lim       = c(0.4, 1.0),
#                                                     lab_angle   = 45,
#                                                     lab_size    = 18,
#                                                     border_size = 3,
#                                                     levels      = c("Major Cities", "Regional", "Remote"))





#################################################### TBC ###########################################################