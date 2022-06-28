#################################### ----- CREATE STAFF SURVEY PLOTS ---- ################################################



## This code creates the tables needed for the Staff Survey Report, and aggregats the tables into a master 'table of 
## tables'. We are just showing the satisfaction tables, not the yes/no tables. For Yes/No, the story is told by the graphs :]





## 1). CREATE LISTS OF SATISFACTION TABLES TYPE ====================================================================


##
all_survey_cols <- names(Survey_data_complete_LG_roles_demography)


## List of ORD5 questions without sub-labels 
ord3_questions <- LUT %>% 
  filter(Response_type == 'Ord3') %>% 
  arrange(QuestionNumber) %>% 
  .$QuestionNumber %>% as.character()

ord3_questions_sub <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% ord3_questions]


## List of ORD5 questions without sub-labels 
ord5_likertise_nosub <- LUT %>% 
  filter(NumColumns == 1 & Response_type == 'Ord5') %>% 
  arrange(QuestionNumber) %>% 
  .$QuestionNumber %>% as.character() 



## List of ORD5 questions with sub-labels 
## We need to combine the main questions with the sub-questions
ord5_likertise_sub <- LUT %>% 
  filter(NumColumns > 1 & Response_type == 'Ord5') %>%
  arrange(QuestionNumber) %>% 
  
  ## This creates a list of the main questions
  ## We then bind on the sub-questions, using the question pre-fix
  .$QuestionNumber %>% as.character() 


## Create a List of ORD3 tables - still haven't run that completely
ord3_question_summary_table <- singlechoice_percent(singlechoice_list = ord3_questions_sub, 
                                                    survey_data       = Survey_data_complete_LG_roles, 
                                                    calc_percent      = TRUE,
                                                    all_questions     = Survey_questions)



## List of ORD5 tables without sub-lables 
ord5_simple_tables <- crosstab_percent(crosstab_list = ord5_likertise_nosub, 
                                       survey_data   = Survey_data_complete_LG_roles,
                                       calc_percent  = TRUE,
                                       table_suffix  = '_table_data')


## List of ORD5 tables with sub-lables
## Come bac to G10_table_data 
ord5_complex_tables <- GridChoiceTable_tables_sub(likertise_list  = ord5_likertise_sub,    
                                                  survey_data     = Survey_data_complete_LG_roles, 
                                                  sublabel_list   = subquestion_labels,
                                                  role_list       = role_list,
                                                  table_suffix    = '_table_data')


## These lists of tables are indexed like this ::
ord5_complex_tables[['F7_table_data']]





## 2). CREATE MASTER TABLE OF SATISFACTION SCORES ==================================================================


## Create a table of contents by joining the LUT to the count list
table_of_contents <- LUT %>%
  
  ## 
  select(Topic, `Page Number`, QuestionNumber, QuestionText) %>% 
  left_join(main_question_counts, ., by = "QuestionNumber") %>%
  rename(`n = ` = BaseSize) %>% 
  
  rename(Question = `Page Number`) %>%
  filter(Question > 0) %>% 
  select(Topic, Question, QuestionText, `n = `) %>% 
  na.omit()


table_of_question_number <- LUT %>%
  
  ## 
  select(Topic, `Page Number`, QuestionNumber, QuestionText) %>% 
  left_join(main_question_counts, ., by = "QuestionNumber") %>%
  rename(`n = ` = BaseSize) %>% 
  
  rename(Question = `Page Number`) %>%
  filter(Question > 0) %>% 
  select(Topic, Question, QuestionNumber, QuestionText, `n = `) %>% 
  na.omit()


## First, unlist the tables from above. This is circular, but needed to run things all at once and return
## everything to the global environment. The names should make them identifiable
list2env(ord5_simple_tables,  globalenv())
list2env(ord5_complex_tables, globalenv())



## Also create a list of all the correctly formatted satisfaction tables 
## From this, we can find the highest and lowest responses
table_list <- mget(ls(name = .GlobalEnv, pattern = '_table_data')) %>% names()


## Create table of satisfaction tables ---- 
Satisfaction_tables <- table_list %>%
  
  ## Pipe the list into lapply
  ## tab = table_list[7]
  lapply(function(tab) {
    
    ## tab = table_list[9]
    message("Getting most and least satisfactory responses")
    satisf_table = get(tab) %>% as.data.frame() 
    
    if (nrow(satisf_table) > 0) {
      
      ## Create question label
      QU    = tab
      QU    = gsub('_table_data', '', QU)
      
      ## Rename columns
      colnames(satisf_table)[1]   <- "SubQuestionText"
      satisf_table$QuestionNumber <- QU
      
      satisf_table <- satisf_table %>% 
        
        select(QuestionNumber, `SubQuestionText`, 
               everything())
      
    } else {
      message('Skip table for ', tab, ' no data')
      cat(tab)
    }
    
    ## Return the dataframe
  }) %>% 
  
  ## Finally, bind all the tables together
  bind_rows() %>% select(QuestionNumber, `SubQuestionText`, 
                         everything()) %>%
  
  ## Do the NA's mean the columns are unequal in length?
  replace_na(list(x = 0, y = "unknown")) 


## Now replace NA with 0 to make the tables complete
Satisfaction_tables[is.na(Satisfaction_tables)] <- 0





## Now join the TOC to the satisfaction questions on :
Master_satisfaction_table <- left_join(Satisfaction_tables,
                                       table_of_question_number, 
                                       by = 'QuestionNumber') %>% 
  select(Topic, 
         Question,
         QuestionText,
         SubQuestionText, 
         `Very satisfied`, 
         Satisfied, 
         Neutral, 
         Dissatisfied,  
         `Very dissatisfied`, Count) %>% rename(`n = ` = Count)



# If this is something the stakeholders will see as well, I'd add in a total satisfaction score, which summed 
# Very Satisfied and satisfied. (And same for Dissatisfied). I'd also get rid of the question numbers from the 
# labels - I don't think the question numbers will mean much for stakeholders, and just make it a bit more wordy 
# than we need.

## Now add overall satisfaction score
Master_satisfaction_table_combined <- Master_satisfaction_table %>%
  
  ## Add Satisfaction and disatisfaction together
  mutate(Satisfied    = `Very satisfied` + Satisfied,
         Dissatisfied = Dissatisfied     + `Very dissatisfied`,
         Total        = Satisfied        + Dissatisfied + Neutral) %>% 
  
  ## Arguably exclude the other scores
  select(Topic, Question, QuestionText, SubQuestionText, 
         Satisfied, Neutral, Dissatisfied, Total, `n = `)


## Are there any duplicated rows in the master satisfaction table?
table(duplicated(Master_satisfaction_table_combined))




## 3). CREATE KPI's FROM SATISFACTION SCORES ==================================================================


## We need a column in the LUT for whether or not each question will be in the index, and if it will contribute to cv vcv xc vv c
## the KPIs. These could be the same thing?


## Pending that, we can just average the dissatisfaction and satisfaction scores for each business unit, as a blunt
## KPI.


## Create a table of mean statisfaction for each
mean_satisfaction_table <- Master_satisfaction_table_combined %>% 
  
  ## Just get the columns we need
  select(Topic, Dissatisfied, Neutral, Satisfied) %>% 
  group_by(Topic) %>% 
  
  ## Calculate the mean satisfaction scores
  summarise(Dissatisfied = mean(Dissatisfied),
            Neutral      = mean(Neutral),
            Satisfied    = mean(Satisfied)) %>% 
  
  ## Pivot longer
  pivot_longer(., cols   = c('Dissatisfied',
                             'Neutral',
                             'Satisfied'),
               names_to  = 'Satisfaction',
               values_to = 'Percent') %>% 
  
  mutate(Topic           = factor(Topic, levels = rev(unique(Topic))),
         
         ## Change the percent to it will display properly
         Percent = round((as.numeric(Percent) * 100),  digits = 1))



## Create a theme for the hi-chart
my_theme <- hc_theme(
  chart = list(
    backgroundColor = "transparent", 
    style = list(
      fontFamily = "Helvetica")))
cols <-  c('#ff7f50', '#d3d3d3', '#50C878')


## Overall satisfaction graph ----
Overall_satisfaction_graph <- hchart(mean_satisfaction_table, type = 'bar', 
                                     hcaes(y = Percent, group = Satisfaction, x = Topic)) %>%
  
  hc_title(
    text = "",
    margin = 20,
    align = "left",
    style = list(color = "#000000", useHTML = TRUE)) %>% 
  
  hc_yAxis(
    min   = 0, max = 100,
    title = list(text = "Percent"),
    fontWeight = "bold",
    # gridLineWidth = 10,
    # gridLineDashStyle = "shortdash",
    title = list(
      text = "Proportion",    # The actual title text
      align = "high",         # Documentation says options are: low, middle or high
      margin = 10,            # Number of pixels between the title and the axis line
      style = list(
        fontWeight = "bold",   # Bold
        fontSize = '2.5em',    # 1.4 x tthe size of the default text
        color = "#7cb5ec"      # Hex code for the default blue
      ))) %>% 
  
  hc_plotOptions(series = list(stacking = "normal")) %>% 
  hc_add_theme(my_theme) %>% 
  hc_colors(cols)






#################################################### TBC ###########################################################