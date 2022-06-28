#################################### ---- CREATE STAFF DEMOGRAPHY PLOTS ---- ################################################



## This code creates plots of the staff demography
## We could also test changes in demography between elections using a chi-squared tests, etc.


## Join the EMs to the survey data - what is the overlap?
# setdiff(LGA_column_venues$LGAreaCode, Survey_data_complete_LG_roles_demography$AREACODE)
# length(intersect(Survey_data_complete_LG_roles_demography$AREACODE, LGA_column_venues$LGAreaCode))

Survey_data_complete_LG_roles_demography_ROs <- Survey_data_complete_LG_roles_demography %>% 
  
  full_join(., LGA_column_venues,  by = c("VenueName", "AREACODE" = "LGAreaCode")) %>% 
  
  ## NA IDs have crept in
  filter(!is.na(`Respondent ID`))


## Get the duplicate rows
Survey_data_complete_LG_dupes       <- Survey_data_complete_LG_roles_demography_ROs[
  duplicated(Survey_data_complete_LG_roles_demography_ROs), ]


## The data still aren't distinct
Survey_data_complete_LG_RO_distinct <- Survey_data_complete_LG_roles_demography_ROs[
  !duplicated(Survey_data_complete_LG_roles_demography_ROs), ] %>% filter(!is.na(`Respondent ID`))
sum(is.na(Survey_data_complete_LG_RO_distinct$`Respondent ID`))


## Stop if the survey data is bigger than Respondents that completed the survey (i.e. a bad join)
stopifnot(nrow(Survey_data_complete_LG_RO_distinct) == complete_responses)





## 1). DEMOGRAPHY TABLES ==========================================================================


## Age distribution table -----
Age_distribution <- CrossTab(Survey_data_complete_LG_RO_distinct %>%
                               
                               filter(Gender %in% c('Male', 
                                                    'Female', 
                                                    'Prefer not to say',
                                                    'Other',
                                                    'Role')), 'Gender', 'Age', TRUE) %>% 
  
  ## Calucate the %
  mutate(Percentage = formattable::percent(Percentage)) %>% as.data.frame() 



Gender_distribution_complete <- 
  
  table(Survey_data_complete_LG_RO_distinct$Gender) %>% 
  as.data.frame() %>%  
  as_tibble() %>% 
  rename(Gender = Var1,
         Count  = Freq) %>% 
  
  mutate(Percent = (Count/sum(Count)) * 100)


Gender_distribution_incomplete <- 
  
  table(Survey_data$N1, useNA = "ifany") %>% 
  as.data.frame() %>%  
  as_tibble() %>% 
  rename(Gender = Var1,
         Count  = Freq) %>% 
  
  mutate(Percent = (Count/sum(Count)) * 100)





## Age * Gender * Role
Age_gend_role <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by Gender, Age and Role
  dplyr::select(Gender, Age, Role) %>%
  group_by(Role, Age, Gender) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Role, Age, Gender) %>% 
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame() 



## Age * Gender
Age_gender <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by Age and Gender
  dplyr::select(Age, Gender) %>%
  group_by(Age, Gender)      %>% 
  na.omit()                  %>%
  
  tally(name = 'Count') %>% 
  arrange(Age, Gender)  %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame() 



## Age * Gender
Role_age <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by Age and Role
  dplyr::select(Age, Role) %>%
  group_by(Role, Age)      %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Role, Age)    %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame() 





## Role * Demographic tables ----


## Role * Language
Role_language <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by
  dplyr::select(Role, Languages) %>%
  group_by(Role, Languages) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Role, Languages) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Role * Birthplace
# Role_birthplace <- Survey_data_complete_LG_RO_distinct %>% 
#   
#   ## Group by
#   dplyr::select(Role, Birthplace) %>%
#   group_by(Role, Birthplace) %>% 
#   na.omit() %>%
#   
#   tally(name = 'Count') %>% 
#   arrange(Role, Birthplace) %>% 
#   
#   ## Calucate the %
#   mutate(Percentage = Count/sum(Count),
#          Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Role * Disability
Role_disability <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by
  dplyr::select(Role, Disability) %>%
  group_by(Role, Disability) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Role, Disability) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Role * Disability
Role_Indigenous <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by
  dplyr::select(Role, Indigenous) %>%
  group_by(Role, Indigenous) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Role, Indigenous) %>% filter(Role != 'EMSO') %>% 
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()





## Age * Demographic tables ----


## Age * Birthplace
Age_language <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by Age and language
  dplyr::select(Age, Languages) %>%
  group_by(Age, Languages) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Age, Languages) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Age * Birthplace
# Age_birthplace <- Survey_data_complete_LG_RO_distinct %>% 
#   
#   ## Group by
#   dplyr::select(Age, Birthplace) %>%
#   group_by(Age, Birthplace) %>% 
#   na.omit() %>%
#   
#   tally(name = 'Count') %>% 
#   arrange(Age, Birthplace) %>% filter(Age != 'EMSO') %>% 
#   
#   ## Calucate the %
#   mutate(Percentage = Count/sum(Count),
#          Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Age * Disability
Age_disability <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by
  dplyr::select(Age, Disability) %>%
  group_by(Age, Disability) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Age, Disability) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Age * Disability
Age_Indigenous <- Survey_data_complete_LG_RO_distinct %>% 
  
  ## Group by
  dplyr::select(Age, Indigenous) %>%
  group_by(Age, Indigenous) %>% 
  na.omit() %>%
  
  tally(name = 'Count') %>% 
  arrange(Age, Indigenous) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()




## Create Q119 graph - this would have to be a collection of background columns
Background <- GridChoiceTable(Survey_data_complete_LG_RO_distinct, 
                              c('Languages', 'Disability', 'Indigenous'), TRUE) %>%
  
  mutate(Percent = formattable::percent(Percent))  %>%
  rename(., `Response`   = 'Answer')  %>%
  rename(., `Percentage` = 'Percent') %>%
  select(Response, Count, Percentage) %>%
  na.omit() %>% as.data.frame()





## 2). DEMOGRAPHY GRAPHS ==========================================================================


## Hi-chart of age * Gender
age_gender_graph = highchart() %>% 
  
  ## Data
  hc_add_series(Age_distribution, "column", hcaes(x = Age, y = Count, group = Gender)) %>%
  
  ## Options for each type of series
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%"
    ),
    column = list(
      colorByPoint = FALSE
    )
  ) %>%
  
  ## Axis
  hc_yAxis(
    title = list(text = "Number of respondens"),
    labels = list(format = "{value}")
  ) %>% 
  
  hc_xAxis(categories = unique(Age_distribution$Age)) %>%
  
  ## Titles and credits
  hc_title(
    text = "Respondent Age distribution"
  ) %>%
  
  hc_subtitle(text = "number of respondents in each age category") %>% 
  hc_credits(
    enabled = TRUE, text = "Source: LG21 staffing survey",
    href = "#",
    style = list(fontSize = "12px")
  )


## Hi-chart of age * Gender
gender_age_role_facet   <- horizontal_bar_two_factor_facet(df        = Age_gend_role,
                                                           title     = 'Gender-Age Distribution',
                                                           
                                                           caption   = paste0(complete_responses, 
                                                                              ' Responses'),
                                                           
                                                           xvar      = 'Age',
                                                           yvar      = 'Count',
                                                           group_var = 'Gender',
                                                           facet_var = 'Role',
                                                           colours   =  c('Female' = "#00BFC4", 
                                                                          'Male'   = "#F8766D",
                                                                          
                                                                          'Not Provided' = "#CC77FF", 
                                                                          'Other'        = "#7CAE00"),
                                                           
                                                           tsize     = 40,
                                                           strip_size = 25,
                                                           capt_size = 30,
                                                           xsize     = 20,
                                                           ysize     = 30,
                                                           xtitle    = 30,
                                                           ytitle    = 30,
                                                           
                                                           ycol      = 'black',
                                                           lab_size  = 20,
                                                           leg_pos   = 'bottom',
                                                           axis_size = 1,
                                                           h_just    = 0,
                                                           plot_mar  = 1.5,
                                                           lab_angle = 45,
                                                           leg_size  = 40,
                                                           
                                                           ylab       = 'Number of respondents',
                                                           xlab       = '',
                                                           wrap_scale = "free_y")



##  
age_role_facet   <- single_bar_order_factor_facet(df        = Role_age, 
                                                  Response  = 'Age',
                                                  title     = 'Age Distribution for each Role',
                                                  
                                                  caption   = paste0(complete_responses, 
                                                                     ' Responses'),
                                                  
                                                  col_palette = "Set2",
                                                  
                                                  tsize      = 40,
                                                  strip_size = 25,
                                                  capt_size  = 20,
                                                  xsize      = 20,
                                                  ysize      = 20,
                                                  ycol       = 'black',
                                                  lab_size   = 8,
                                                  mar        = 1.5,
                                                  
                                                  ymin       = 0, 
                                                  axis_multiplier = 0.30,
                                                  ylab       = 'Percentage (%)',
                                                  xlab       = '',
                                                  facet_var  = 'Role',
                                                  wrap_scale = "fixed")



##  
age_gender_facet   <- single_bar_order_factor_facet(df       = Age_gend_role, 
                                                    Response = 'Gender',
                                                    title    = '',
                                                    
                                                    caption   = paste0(complete_responses, 
                                                                       ' Responses'),
                                                    
                                                    col_palette = "Set2",
                                                    
                                                    tsize      = 40,
                                                    strip_size = 25,
                                                    capt_size  = 20,
                                                    xsize      = 20,
                                                    ysize      = 20,
                                                    ycol       = 'black',
                                                    lab_size   = 8,
                                                    mar        = 1.5,
                                                    
                                                    ymin       = 0, 
                                                    axis_multiplier = 0.30,
                                                    ylab       = 'Percentage (%)',
                                                    xlab       = '',
                                                    facet_var  = 'Age',
                                                    wrap_scale = "fixed")






## Create plot
background_graph <-         single_barchart_order_y(df       = Background,
                                                    title    = str_wrap('Survey Demographics', 40),
                                                    
                                                    caption   = paste0(complete_responses, 
                                                                       ' Responses'),
                                                    tsize     = 40,
                                                    capt_size = 20,
                                                    xsize     = 20,
                                                    ysize     = 20,
                                                    ycol      = 'black',
                                                    lab_size  = 8,
                                                    mar       = 1.5,
                                                    
                                                    ymin        = 0, 
                                                    axis_multiplier = 0.1,
                                                    ylab        = 'Percentage (%)',
                                                    xlab        = '',
                                                    color_n     = nrow(Background))



## Background facet plot
# Background_role_facet <- single_bar_order_factor_facet(df       = Backg_role,
#                                                        Response = 'Background',
#                                                        title    = '',
#                                                        caption   = paste0(complete_responses, ' Responses'),
#                                                        col_palette = "Set2",
# 
#                                                        tsize      = 40,
#                                                        strip_size = 25,
#                                                        capt_size  = 20,
#                                                        xsize      = 20,
#                                                        ysize      = 20,
#                                                        ycol       = 'black',
#                                                        lab_size   = 8,
#                                                        mar        = 1.5,
# 
#                                                        ymin      = 0,
#                                                        axis_multiplier = 0.30,
#                                                        ylab      = 'Percentage (%)',
#                                                        xlab      = '',
#                                                        facet_var = 'Role',
#                                                        wrap_scale = "fixed")



## Gender facet plot
# Background_gender_facet <- single_bar_order_factor_facet(df       = Gend_backg,
#                                                          Response = 'Background',
#                                                          title    = '',
#                                                          caption   = paste0(complete_responses, ' Responses')
#                                                          col_palette = "Set2",
#                                                          
#                                                          tsize      = 40,
#                                                          strip_size = 25,
#                                                          capt_size  = 20,
#                                                          xsize      = 20,
#                                                          ysize      = 20,
#                                                          ycol       = 'black',
#                                                          lab_size   = 8,
#                                                          mar        = 1.5,
#                                                          
#                                                          ymin       = 0,
#                                                          axis_multiplier = 0.30,
#                                                          ylab       = 'Percentage (%)',
#                                                          xlab       = '',
#                                                          facet_var  = 'Gender',
#                                                          wrap_scale = "fixed")


##  
# Background_age_facet   <- single_bar_order_factor_facet(df       = Backg_age,
#                                                         Response = 'Background',
#                                                         title    = '',
#                                                         caption   = paste0(complete_responses, ' Responses')
#                                                         col_palette = "Set2",
#                                                         
#                                                         tsize      = 40,
#                                                         strip_size = 25,
#                                                         capt_size  = 20,
#                                                         xsize      = 20,
#                                                         ysize      = 20,
#                                                         ycol       = 'black',
#                                                         lab_size   = 8,
#                                                         mar        = 1.5,
#                                                         
#                                                         ymin       = 0,
#                                                         axis_multiplier = 0.30,
#                                                         ylab       = 'Percentage (%)',
#                                                         xlab       = '',
#                                                         facet_var  = 'Age',
#                                                         wrap_scale = "fixed")



## Role Language facet plot
Role_language_facet <- single_bar_order_factor_facet(df       = Role_language, 
                                                     Response = 'Languages',
                                                     title    = 'Languages Spoken by Election Role',
                                                     caption     = paste0(complete_responses, ' Responses'),
                                                     col_palette = "Set2",
                                                     
                                                     tsize      = 40,
                                                     strip_size = 25,
                                                     capt_size  = 20,
                                                     xsize      = 20,
                                                     ysize      = 20,
                                                     ycol       = 'black',
                                                     lab_size   = 8,
                                                     mar        = 1.5,
                                                     
                                                     ymin      = 0, 
                                                     axis_multiplier = 0.30,
                                                     ylab      = 'Percentage (%)',
                                                     xlab      = '',
                                                     facet_var = 'Role',
                                                     wrap_scale = "fixed")



## Role Disability facet
Role_disability_facet <- single_bar_order_factor_facet(df       = Role_disability, 
                                                       Response = 'Disability',
                                                       title    = 'Disability Status by Election Role',
                                                       caption     = paste0(complete_responses, ' Responses'),
                                                       col_palette = "Set2",
                                                       
                                                       tsize      = 40,
                                                       strip_size = 25,
                                                       capt_size  = 20,
                                                       xsize      = 20,
                                                       ysize      = 20,
                                                       ycol       = 'black',
                                                       lab_size   = 8,
                                                       mar        = 1.5,
                                                       
                                                       ymin       = 0, 
                                                       axis_multiplier = 0.30,
                                                       ylab       = 'Percentage (%)',
                                                       xlab       = '',
                                                       facet_var  = 'Role',
                                                       wrap_scale = "fixed")



## Role Indigenous facet
Role_Indigenous_facet <- single_bar_order_factor_facet(df       = Role_Indigenous, 
                                                       Response = 'Indigenous',
                                                       title    = 'Indigenous Status by Election Role',
                                                       caption     = paste0(complete_responses, ' Responses'),
                                                       col_palette = "Set2",
                                                       
                                                       tsize      = 40,
                                                       strip_size = 25,
                                                       capt_size  = 20,
                                                       xsize      = 20,
                                                       ysize      = 20,
                                                       ycol       = 'black',
                                                       lab_size   = 8,
                                                       mar        = 1.5,
                                                       
                                                       ymin       = 0, 
                                                       axis_multiplier = 0.30,
                                                       ylab       = 'Percentage (%)',
                                                       xlab       = '',
                                                       facet_var  = 'Role',
                                                       wrap_scale = "fixed")





## Create Q120 graph
# Q120_plot <- SingleChoiceTable(Survey_data_complete_LG_RO_distinct, 'Role_comments', TRUE) %>%
#   mutate(Percentage = percent_num_form(Percentage))  %>%
#   rename(., `Response`   = 'Role_comments') %>%
#   filter(Response != '3DVCMDVCM') %>%
#   na.omit()


## Create plot
# Q120_graph <-  single_barchart_order_y(df    = Q120_plot,
#                                        title = str_wrap('Number of comments by Role', 40),
#                                        caption     = paste0(complete_responses, ' Responses'),
#                                        col_palette = "Set2",
#                                        
#                                        tsize     = 40,
#                                        capt_size = 20,
#                                        xsize     = 20,
#                                        ysize     = 20,
#                                        ycol      = 'black',
#                                        lab_size  = 8,
#                                        
#                                        ymin      = 0,
#                                        axis_multiplier = 0.2,
#                                        ylab      = 'Percentage (%)',
#                                        xlab      = '')





## 3). SATISFACTION * DEMOGRAPHY TABLES ====================================================================


## Create tables of satisfaction across the whole survey


## Satisfaction * Gender tables ---- 
gender_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    message("Getting most and least satisfactory responses for ", QU)
    gender_table <- table(Survey_data_complete_LG_RO_distinct$Gender,
                          Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>%
      
      ## Tabulate satisfaction by gender
      rename(Demographic  = Var1,
             Satisfaction = Var2,
             Count        = Freq) %>%
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',      'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied',   'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',      'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(gender_table)
    
  }) %>%
  
  ## Finally, bind all the tables together
  bind_rows() %>%
  group_by(Demographic, Satisfaction) %>%
  summarise(Count = sum(Count)) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()





## Satisfaction * Age tables ---- 
age_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    ## QU = ord5_questions[1]
    message("Getting most and least satisfactory responses for ", QU)
    age_table <- table(Survey_data_complete_LG_RO_distinct$Age, 
                       Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>%
      
      ## Tabulate satisfaction by gender
      rename(Demographic  = Var1,
             Satisfaction = Var2,
             Count        = Freq) %>%
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',      'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied',   'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',      'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(age_table)
    
  }) %>%
  
  ## Finally, bind all the tables together
  bind_rows() %>%
  group_by(Demographic, Satisfaction) %>%
  summarise(Count = sum(Count)) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()





## Satisfaction * Indigenous tables ---- 
Indigenous_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    ## QU = SUR_satis_questions[1]
    message("Getting most and least satisfactory responses for ", QU)
    Indigenous_table <- table(Survey_data_complete_LG_RO_distinct$Indigenous, 
                              Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>%
      
      ## Tabulate satisfaction by gender
      rename(Demographic  = Var1,
             Satisfaction = Var2,
             Count        = Freq) %>%
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',      'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied',   'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',      'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(Indigenous_table)
    
  }) %>%
  
  ## Finally, bind all the tables together
  bind_rows() %>%
  group_by(Demographic, Satisfaction) %>%
  
  ## Filter out the categories we don't want
  summarise(Count = sum(Count)) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()





## Satisfaction * Indigenous tables ---- 
disab_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    ## QU = ord5_questions[1]
    message("Getting most and least satisfactory responses for ", QU)
    disab_table <- table(Survey_data_complete_LG_RO_distinct$Disability, 
                         Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>%
      
      ## Tabulate satisfaction by gender
      rename(Demographic  = Var1,
             Satisfaction = Var2,
             Count        = Freq) %>%
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',      'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied',   'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',      'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(disab_table)
    
  }) %>%
  
  ## Finally, bind all the tables together
  bind_rows() %>%
  group_by(Demographic, Satisfaction) %>%
  summarise(Count = sum(Count)) %>%
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()



## Combine the table
demography_satisfaction_table <- bind_rows(gender_satisfaction_table,
                                           age_satisfaction_table,
                                           Indigenous_satisfaction_table,
                                           disab_satisfaction_table)





## 4). LOCATION SATISFACTION TABLES ====================================================================


## Satisfaction * District tables ---- 
RO_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    ## QU = ord5_questions[1]
    message("Getting most and least satisfactory responses for ", QU)
    satisfaction_table <- table(Survey_data_complete_LG_RO_distinct$ReturningOffice, 
                                Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>% 
      
      ## Tabulate satisfaction by gender
      rename(ReturningOffice = Var1,
             Satisfaction    = Var2,
             Count           = Freq) %>% 
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',      'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied',   'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',      'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(satisfaction_table)
    
  }) %>% 
  
  ## Finally, bind all the tables together
  bind_rows() %>% 
  group_by(ReturningOffice, Satisfaction) %>%
  summarise(Count = sum(Count)) %>% 
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()


## Create a table of overall satisfaction per RO
## This can be joined to the Survey data
RO_satisfaction_table_wider <- RO_satisfaction_table %>%
  
  pivot_wider(names_from  = Satisfaction,
              values_from = Percentage) %>% 
  
  group_by(ReturningOffice) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(ReturningOffice, Satisfied, Dissatisfied, Neutral) %>% 
  rename(RO_Satisfied    = Satisfied,
         RO_Dissatisfied = Dissatisfied,
         RO_Neutral      = Neutral)





## Satisfaction * Venue tables ---- 
Venue_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    ## QU = ord5_questions[1]
    message("Getting most and least satisfactory responses for ", QU)
    satisfaction_table <- table(Survey_data_complete_LG_RO_distinct$VenueName, 
                                Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>% 
      
      ## Tabulate satisfaction by gender
      rename(VenueName    = Var1,
             Satisfaction = Var2,
             Count        = Freq) %>% 
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',    'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied', 'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',    'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(satisfaction_table)
    
  }) %>% 
  
  ## Finally, bind all the tables together
  bind_rows() %>% 
  group_by(VenueName, Satisfaction) %>%
  summarise(Count = sum(Count)) %>% 
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()



## Create a table of overall satisfaction per Venue
## This can be joined to the Survey data
Venue_satisfaction_table_wider <- Venue_satisfaction_table %>%
  
  pivot_wider(names_from  = Satisfaction,
              values_from = Percentage) %>% 
  
  group_by(VenueName) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(VenueName, Satisfied, Dissatisfied, Neutral) %>% 
  rename(Venue_Satisfied    = Satisfied,
         Venue_Dissatisfied = Dissatisfied,
         Venue_Neutral      = Neutral)





## Satisfaction * Respondent tables ---- 
respondent_satisfaction_table <- SUR_satis_questions %>%
  
  ## Pipe the list into lapply
  lapply(function(QU) {
    
    ## QU = SUR_satis_questions[1]
    message("Getting most and least satisfactory responses for ", QU)
    satisfaction_table <- table(Survey_data_complete_LG_RO_distinct$`Respondent ID`, 
                                Survey_data_complete_LG_RO_distinct[[QU]]) %>% as.data.frame() %>% 
      
      ## Tabulate satisfaction by gender
      rename(`Respondent ID` = Var1,
             Satisfaction    = Var2,
             Count           = Freq) %>% 
      
      ## Group satisfaction together
      mutate(Satisfaction = gsub('Very satisfied',    'Satisfied',    Satisfaction)) %>%
      mutate(Satisfaction = gsub('Very dissatisfied', 'Dissatisfied', Satisfaction)) %>%
      mutate(Satisfaction = gsub('Not applicable',    'Neutral',      Satisfaction))
    
    ## Return the dataframe
    return(satisfaction_table)
    
  }) %>% 
  
  ## Finally, bind all the tables together
  bind_rows() %>% 
  group_by(`Respondent ID`, Satisfaction) %>%
  summarise(Count = sum(Count)) %>% 
  
  ## Calucate the %
  mutate(Percentage = Count/sum(Count),
         Percentage = formattable::percent(Percentage)) %>% as.data.frame()



## Create a table of overall satisfaction per Venue
## This can be joined to the Survey data
respondent_satis_tab_wider <- respondent_satisfaction_table %>%
  
  pivot_wider(names_from  = Satisfaction,
              values_from = Percentage) %>% 
  
  group_by(`Respondent ID`) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(`Respondent ID`, Satisfied, Dissatisfied, Neutral) %>% 
  rename(Respondent_Satisfied    = Satisfied,
         Respondent_Dissatisfied = Dissatisfied,
         Respondent_Neutral      = Neutral) 





## 5). SATISFACTION * DEMOGRAPHY GRAPHS ====================================================================


## Gender Satisfaction plot ----
Gender_satis_facet <- single_bar_order_factor_facet(df          = gender_satisfaction_table %>% 
                                                      filter(Demographic   == 'Male' |
                                                               Demographic == 'Female'), 
                                                    
                                                    Response    = 'Satisfaction',
                                                    title       = 'Satisfaction by Gender',
                                                    caption     = paste0(complete_responses, ' Responses'),
                                                    col_palette = "Set2",
                                                    
                                                    tsize      = 40,
                                                    strip_size = 25,
                                                    capt_size  = 20,
                                                    xsize      = 20,
                                                    ysize      = 20,
                                                    ycol       = 'black',
                                                    lab_size   = 8,
                                                    mar        = 1.6,
                                                    
                                                    ymin       = 0, 
                                                    axis_multiplier = 0.30,
                                                    ylab       = 'Percentage (%)',
                                                    xlab       = '',
                                                    facet_var  = 'Demographic',
                                                    wrap_scale = "fixed")





## Age Satisfaction plot ----
Age_satis_facet <- single_bar_order_factor_facet(df          = age_satisfaction_table, 
                                                 Response    = 'Satisfaction',
                                                 title       = 'Satisfaction by Age',
                                                 caption     = paste0(complete_responses, ' Responses'),
                                                 col_palette = "Set2",
                                                 
                                                 tsize      = 40,
                                                 strip_size = 20,
                                                 capt_size  = 20,
                                                 xsize      = 10,
                                                 ysize      = 10,
                                                 ycol       = 'black',
                                                 lab_size   = 4,
                                                 mar        = 1.5,
                                                 
                                                 ymin       = 0, 
                                                 axis_multiplier = 0.30,
                                                 ylab       = 'Percentage (%)',
                                                 xlab       = '',
                                                 facet_var  = 'Demographic',
                                                 wrap_scale = "fixed")





## Indigenous Satisfaction plot ----
Indigenous_satis_facet <- single_bar_order_factor_facet(df          = Indigenous_satisfaction_table,
                                                        Response    = 'Satisfaction',
                                                        title       = 'Satisfaction by Indigenous Status',
                                                        caption     = paste0(complete_responses, ' Responses'),
                                                        col_palette = "Set2",
                                                        
                                                        tsize      = 40,
                                                        strip_size = 25,
                                                        capt_size  = 20,
                                                        xsize      = 20,
                                                        ysize      = 20,
                                                        ycol       = 'black',
                                                        lab_size   = 8,
                                                        mar        = 1.5,
                                                        
                                                        ymin       = 0, 
                                                        axis_multiplier = 0.30,
                                                        ylab       = 'Percentage (%)',
                                                        xlab       = '',
                                                        facet_var  = 'Demographic',
                                                        wrap_scale = "fixed")




## Disability Satisfaction plot ----
Disab_satis_facet <- single_bar_order_factor_facet(df          = disab_satisfaction_table, 
                                                   Response    = 'Satisfaction',
                                                   title       = 'Satisfaction by Disability Status',
                                                   caption     = paste0(complete_responses, ' Responses'),
                                                   col_palette = "Set2",
                                                   
                                                   tsize      = 40,
                                                   strip_size = 25,
                                                   capt_size  = 20,
                                                   xsize      = 20,
                                                   ysize      = 20,
                                                   ycol       = 'black',
                                                   lab_size   = 8,
                                                   mar        = 1.5,
                                                   
                                                   ymin       = 0, 
                                                   axis_multiplier = 0.30,
                                                   ylab       = 'Percentage (%)',
                                                   xlab       = '',
                                                   facet_var  = 'Demographic',
                                                   wrap_scale = "fixed")






## 6). DEMOGRAPHY WORKSHEET ==========================================================================


## Create a workbook to store the demography analyses
# LG21_Demography_workbook <- createWorkbook()
# 
# 
# ## Add worksheet to the spread sheet
# demo_list = c('Age_distribution',
#               'Gender_distribution_complete',
#               'Age_gend_role',
#               'Background',
#               'demography_satisfaction_table',
#               'RO_satisfaction_table',
#               'RO_satisfaction_table_wider',
#               'Venue_satisfaction_table',
#               'Venue_satisfaction_table_wider')
# 
# 
# LG21_Demography_workbook <- createWorkbook()


# for(file in demo_list) {
#   
#   ## Get required columns.
#   File_to_Write <- get(file)
#   
#   ## Add worksheet to the spread sheet
#   message('writing ', file,  ' to verbatim spreadsheet')
#   addWorksheet(LG21_Demography_workbook, file)
#   
#   ## Write the data to the corresponding worksheet
#   writeDataTable(wb       = LG21_Demography_workbook, 
#                  sheet    = file,
#                  x        = get(file),
#                  startCol = 1, 
#                  startRow = 1, 
#                  rowNames = FALSE,
#                  tableStyle = "TableStyleMedium2")
#   
# }


## Save the whole workbook
# setwd(survey_root)
# saveWorkbook(LG21_Demography_workbook,
#              paste0(survey_tabular_output, 'LG21_Demography_workbook.xlsx'),
#              overwrite = TRUE)





#################################################### TBC ###########################################################