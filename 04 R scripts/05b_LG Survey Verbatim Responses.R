#################################### ----- SUBSET SURVEY RESPONSESS ---- ################################################



## This code subsets the survey data into just the responses for each Question




## 1). CREATE OVERALL QUESTION LISTS ==============================================================================


## Create list of satisfaction questions
## This includes sub-questions, used for transformations, 
## And also used for looping table creation - and can be used for creating 'covariance' matrix
satisfaction_format <- LUT %>% 
  filter(Response_type == 'Ord5'
         #& Index == 'Yes'
  ) %>% 
  .$QuestionNumber %>% as.character() %>% sort()


yes_no_format <- LUT %>% 
  filter(Response_type == 'Ord3' 
         #& Index == 'Yes'
  ) %>% 
  .$QuestionNumber %>% as.character() %>% sort()


## Index main questions
Index_main_questions <- LUT %>% 
  filter(!is.na(Index)) %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


## Index main questions
Index_questions_LUT <- LUT %>% 
  filter(!is.na(Index))


Index_main_ord_5_questions <- LUT %>% 
  
  filter(!is.na(Index) & Response_type == "Ord5") %>% 
  .$QuestionNumber    %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


Index_main_ord_3_questions <- LUT %>%
  
  filter(!is.na(Index) & Response_type == "Ord3") %>%  
  .$QuestionNumber    %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Index lists
Counting_indexes <- LUT %>% filter(Index == 'Counting') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Support_indexes <- LUT %>% filter(Index == 'General Support') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


IS_indexes <- LUT %>% filter(Index == 'IS') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Logistics_indexes <- LUT %>% filter(Index == 'Logistics') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Operations_indexes <- LUT %>% filter(Index == 'Operations') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Recruitment_indexes <- LUT %>% filter(Index == 'Recruitment') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Staffing_indexes <- LUT %>% filter(Index == 'Staffing') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Training_indexes <- LUT %>% filter(Index == 'Training') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


Venues_indexes <- LUT %>% filter(Index == 'Venues') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


WHS_indexes <- LUT %>% filter(Index == 'WHS') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()


## Select postive and negative questions
Index_main_ord_3_yes_neg_questions <- Index_main_ord_3_questions[c(9:11)] 
Index_main_ord_3_yes_pos_questions <- Index_main_ord_3_questions[c(1:8)]


## str_subset does not work reliably...try gsub instead
LUT_index_questions <- c(satisfaction_format, yes_no_format)
SUR_satis_questions <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% satisfaction_format]
SUR_index_satisf    <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Index_main_ord_5_questions]
SUR_index_yes_neg   <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Index_main_ord_3_yes_neg_questions]
SUR_index_yes_pos   <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Index_main_ord_3_yes_pos_questions]


## Also here we need to add if each question is part of the index



## When creating an index, consider the distribution of question types. 
## Should we be aiming for a more balanced number of questions?
## Create a table to plot
business.plot <- table(table_of_contents$Topic) %>% 
  as.data.frame() %>% 
  rename(., Topic = Var1, Question_count = Freq) %>% 
  arrange(-Question_count)


## Business Topics
business_Topics  <- unique(table_of_contents$Topic)
business_Indexes <- unique(LUT$Index) %>% na.omit() %>% c()


## Consider the distribution of questions - the aggregation scheme will affec the results
## This graph could be put on the main page.
## We need to add some descriotive stuff to the main page - response rate, etc 


## 
ggplot(business.plot , aes(x = reorder(Topic, -Question_count), Question_count)) + 
  geom_bar(stat ="identity", fill = "Blue") +
  theme(axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  
  ## And title
  ggtitle(paste0(''))


## Add to the verbatims ----
Survey_data_complete_LG_roles_RO <- Survey_data_complete_LG_roles_demography %>% 
  
  left_join(., RO_venues_distinct, by = "VenueName") 





## 2). CREATE LISTS OF VERBATIM QUESTIONS ====================================================================


## To do ----

## Check question 103 and 66
## Put question topic in header
## Use RO office + venue, then put question text in the header too


# Function  - expand verbatim question list to include related question numbers
queston_id_list <- function(data, verbatim_list) {
  
  if(length(verbatim_list) > 0) {
    verbatim_list <- str_subset(names(data), str_c(verbatim_list, collapse = "|"))
  }
  return(verbatim_list)
}


Support_verbatims     <- queston_id_list(Survey_data_complete_LG_roles_RO, Support_verbatims)
IT_verbatims          <- queston_id_list(Survey_data_complete_LG_roles_RO, IT_verbatims)
Resource_verbatims    <- queston_id_list(Survey_data_complete_LG_roles_RO, Resource_verbatims)
Recruitment_verbatims <- queston_id_list(Survey_data_complete_LG_roles_RO, Recruitment_verbatims)
Office_verbatims      <- queston_id_list(Survey_data_complete_LG_roles_RO, Office_verbatims)
Training_verbatims    <- queston_id_list(Survey_data_complete_LG_roles_RO, Training_verbatims)
Venue_verbatim        <- queston_id_list(Survey_data_complete_LG_roles_RO, Venue_verbatims)
Counting_verbatims    <- queston_id_list(Survey_data_complete_LG_roles_RO, Counting_verbatims)
Operations_verbatims  <- queston_id_list(Survey_data_complete_LG_roles_RO, Operations_verbatims)
WHS_verbatims         <- queston_id_list(Survey_data_complete_LG_roles_RO, WHS_verbatims)
Conclusion_verbatims  <- queston_id_list(Survey_data_complete_LG_roles_RO, Conclusion_verbatims)
Overall_verbatims     <- queston_id_list(Survey_data_complete_LG_roles_RO, Overall_verbatims)



## Create subsets of questions for each business Topic
## Try creating a subset of the questions by satisfaction, plus a textual analysis of verbatims
## Also, we'd need the Demography here too? 
SURV_Recruitment_questions <- queston_id_list(Survey_data_complete_LG_roles_RO, Recruitment_questions)
SURV_Training_questions    <- queston_id_list(Survey_data_complete_LG_roles_RO, Training_questions)
SURV_Resource_questions    <- queston_id_list(Survey_data_complete_LG_roles_RO, Resource_questions)
SURV_Venue_questions       <- queston_id_list(Survey_data_complete_LG_roles_RO, Venue_questions)
SURV_Office_questions      <- queston_id_list(Survey_data_complete_LG_roles_RO, Office_questions)
SURV_Operations_questions  <- queston_id_list(Survey_data_complete_LG_roles_RO, Operations_questions)
SURV_Counting_questions    <- queston_id_list(Survey_data_complete_LG_roles_RO, Counting_questions)
SURV_IT_questions          <- queston_id_list(Survey_data_complete_LG_roles_RO, IT_questions)
SURV_WHS_questions         <- queston_id_list(Survey_data_complete_LG_roles_RO, WHS_questions)
SUR_Conclusion_questions   <- queston_id_list(Survey_data_complete_LG_roles_RO, Conclusion_questions)
SURV_Support_questions     <- queston_id_list(Survey_data_complete_LG_roles_RO, Support_questions)


## Create subsets of questions for each business Index
## Try creating a subset of the questions by satisfaction, plus a textual analysis of verbatims
## This subsetting code below seems more accurate than the str_detect
SURV_Index_Recruitment_questions <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Recruitment_indexes]
SURV_Index_Training_questions    <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Training_indexes]
SURV_Index_Staffing_questions    <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Staffing_indexes]
SURV_Index_Logistics_questions   <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Logistics_indexes]
SURV_Index_Venues_questions      <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Venues_indexes]
SURV_Index_Operations_questions  <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Operations_indexes]
SURV_Index_Counting_questions    <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% Counting_indexes]
SURV_Index_IS_questions          <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% IS_indexes]
SURV_Index_WHS_questions         <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% WHS_indexes]



## List of words that need to be removed
keywords_to_remove <- c("No", "None", "Nope", "Nil", "no", "none",
                        "Na","N/A","NA") 


## Also subset the survey data into just the questions for each unit
## The data needs to be formatted so they can see it, but also so that it's useful


## Recruitment Verbatims ----
Survey_Recruitment               <- Survey_data_complete_LG_roles_RO %>% select(SURV_Recruitment_questions)
Survey_Recruitment_satis         <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Recruitment_questions))
Survey_Recruitment_verbatim      <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Recruitment_verbatims) %>% .[rowSums( is.na(.) ) <=2, ] 


## Remove the "No" responses from the verbatims
Survey_Recruitment_verbatim_no <- Survey_data_complete_LG_roles_RO %>% as_tibble() %>%
  select(ReturningOffice, VenueName, Role, Recruitment_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Recruitment_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Recruitment_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Recruitment_verbatims)) %>%
  select(-(No_Count:Remove_Count))





## Harmonise names
Recruitment_verbatims_LUT <- LUT %>% 
  filter(Topic == 'Recruitment' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText) %>%
  mutate(Question = str_c("Q",Question),
         Topic    = 'Recruitment')


## Name the verbatims
names(Survey_Recruitment_verbatim)[3:length(names(Survey_Recruitment_verbatim))] <- as.matrix(Recruitment_verbatims_LUT[1][1])
names(Survey_Recruitment_verbatim_no) <- names(Survey_Recruitment_verbatim_no) %>%
  
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Recruitment_Question_text <- paste0(c('', '', '', Recruitment_verbatims_LUT$QuestionText))
names(Survey_Recruitment_verbatim_no) <- paste(names(Survey_Recruitment_verbatim_no), Recruitment_Question_text, sep = ": ")





## Training Verbatims -----
Survey_Training             <- Survey_data_complete_LG_roles_RO %>% select(SURV_Training_questions)
Survey_Training_satis       <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Training_questions))
Survey_Training_verbatim    <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Training_verbatims) %>% .[rowSums( is.na(.)) <=3, ]

Survey_Training_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Training_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Training_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Training_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Training_verbatims)) %>%
  select(-(No_Count:Remove_Count))



## Harmonise names
Training_verbatims_LUT <- LUT %>% 
  filter(Topic == 'Training' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText) %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "Training")


## Rename
names(Survey_Training_verbatim)[2:length(names(Survey_Training_verbatim))] <- as.matrix(Training_verbatims_LUT [1][1])
names(Survey_Training_verbatim_no) <- names(Survey_Training_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Training_Question_text <- paste0(c('', '', '', Training_verbatims_LUT$QuestionText))
names(Survey_Training_verbatim_no) <- paste(names(Survey_Training_verbatim_no), Training_Question_text, sep = ": ")


## Resource Verbatims ----
Survey_Resource             <- Survey_data_complete_LG_roles_RO %>% select(SURV_Resource_questions)
Survey_Resource_satis       <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Resource_questions))
Survey_Resource_verbatim    <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Resource_verbatims) %>% .[rowSums( is.na(.) ) <=6, ]


Survey_Resource_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Resource_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Resource_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Resource_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Resource_verbatims)) %>%
  select(-(No_Count:Remove_Count))


## Harmonise names
Resource_verbatims_LUT <- LUT %>% 
  filter(Topic == 'Resources' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "Resources")


## Rename
names(Survey_Resource_verbatim)[3:length(names(Survey_Resource_verbatim))] <- as.matrix(Resource_verbatims_LUT [1][1])
names(Survey_Resource_verbatim_no) <- names(Survey_Resource_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Resource_Question_text <- paste0(c('', '', '', Resource_verbatims_LUT$QuestionText))
names(Survey_Resource_verbatim_no) <- paste(names(Survey_Resource_verbatim_no), Resource_Question_text, sep = ": ")


## Which Venues ran out of labels :
# Venue_labels <- Survey_data_complete_LG_roles_RO %>% select(`Respondent ID`, Role, d7_5)
# Venue_labels <- Venue_labels[!is.na(Venue_labels$d7_5), ]
# write_csv(Venue_labels, './05 Tabular output/Verbatim_responses/LG21_Q52_d7_verbatims.csv')


## Venue Verbatims ----
Survey_Venue                <- Survey_data_complete_LG_roles_RO %>% select(SURV_Venue_questions)
Survey_Venue_satis          <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Venue_questions))
Survey_Venue_verbatim       <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Venue_verbatims) %>% 
  mutate(NA_per_row = apply(., 1, function(x) sum(is.na(x)))) %>% filter(NA_per_row <= 3) %>% select(-NA_per_row)

Survey_Venue_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Venue_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Venue_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Venue_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Venue_verbatims)) %>%
  select(-(No_Count:Remove_Count))



## Harmonise names
Venue_verbatims_LUT <- LUT %>% 
  filter(Topic == 'Venues' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "Venues")


## Rename
names(Survey_Venue_verbatim)[2:length(names(Survey_Venue_verbatim))] <- as.matrix(Venue_verbatims_LUT [1][1])
names(Survey_Venue_verbatim_no) <- names(Survey_Venue_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Venue_Question_text <- paste0(c('', '', '', Venue_verbatims_LUT$QuestionText))
names(Survey_Venue_verbatim_no) <- paste(names(Survey_Venue_verbatim_no), Venue_Question_text, sep = ": ")





## Office Verbatims ----
Survey_Office               <- Survey_data_complete_LG_roles_RO %>% select(SURV_Office_questions)
Survey_Office_satis         <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Office_questions))
Survey_Office_verbatim      <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Office_verbatims) %>% .[rowSums( is.na(.) ) <=6, ]

Survey_Office_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Office_verbatims) %>%
  rowwise %>%
  mutate(No_Count = length(intersect(c_across(Office_verbatims),keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Office_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Office_verbatims)) %>%
  select(-(No_Count:Remove_Count))


## harmonise
Office_verbatims_LUT <- LUT %>% 
  filter(Topic == 'EM/RO Office operations' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "EM/RO Office operations")


## Rename
names(Survey_Office_verbatim)[3:length(names(Survey_Office_verbatim))] <- as.matrix(Office_verbatims_LUT [1][1])
names(Survey_Office_verbatim_no) <- names(Survey_Office_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Office_Question_text <- paste0(c('', '', '', Office_verbatims_LUT$QuestionText))
names(Survey_Office_verbatim_no) <- paste(names(Survey_Office_verbatim_no), Office_Question_text, sep = ": ")





## Operations Verbatims ----
Survey_Operations           <- Survey_data_complete_LG_roles_RO %>% select(SURV_Operations_questions)
Survey_Operations_satis     <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Operations_questions))
Survey_Operations_verbatim  <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Operations_verbatims) %>% .[rowSums( is.na(.) ) <=10, ]

Survey_Operations_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Operations_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Operations_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Operations_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Operations_verbatims)) %>%
  select(-(No_Count:Remove_Count))


## Harmonise names
Operations_verbatims_LUT <- LUT %>% 
  filter(Topic == 'Voting Operations' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "Voting Operation")


## Rename
names(Survey_Operations_verbatim)[3:length(names(Survey_Operations_verbatim))] <- as.matrix(Operations_verbatims_LUT [1][1])
names(Survey_Operations_verbatim_no) <- names(Survey_Operations_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Operations_Question_text <- paste0(c('', '', '', Operations_verbatims_LUT$QuestionText))
names(Survey_Operations_verbatim_no) <- paste(names(Survey_Operations_verbatim_no), Operations_Question_text, sep = ": ")





## Counting Verbatims ----
Survey_Counting             <- Survey_data_complete_LG_roles_RO %>% select(SURV_Counting_questions)
Survey_Counting_satis       <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Counting_questions))
Survey_Counting_verbatim    <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Counting_verbatims) %>% .[rowSums( is.na(.) ) <=6, ]


# CODE ERROR OCCURRING HERE - Counting_verbatims contains all columns!

Survey_Counting_verbatim_no <- Survey_data_complete_LG_roles_RO %>%
  select(ReturningOffice, VenueName, Role, Counting_verbatims) %>%
  rowwise %>%
  mutate(No_Count = length(intersect(c_across(Counting_verbatims),keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Counting_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Counting_verbatims)) %>%
  select(-(No_Count:Remove_Count))

## Harmonise names
Counting_verbatims_LUT <- LUT %>%
  filter(Topic == 'Vote Counting' & Response_type == "Free Text") %>%
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>%
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)



Counting_Question_text <- paste0(c('', '', '', Counting_verbatims_LUT$QuestionText))
names(Survey_Counting_verbatim_no) <- paste(names(Survey_Counting_verbatim_no), Counting_Question_text, sep = ": ")





## IT Verbatims ----
Survey_IT                   <- Survey_data_complete_LG_roles_RO %>% select(SURV_IT_questions)
Survey_IT_satis             <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_IT_questions))
Survey_IT_verbatim          <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, IT_verbatims) %>% .[rowSums( is.na(.) ) <=2, ]

Survey_IT_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, IT_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(IT_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(IT_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(IT_verbatims)) %>%
  select(-(No_Count:Remove_Count))

## Harmonise names
IT_verbatims_LUT <- LUT %>% 
  filter(Topic == 'IT and Tech Issues' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "IS")


## Rename 
names(Survey_IT_verbatim)[3:length(names(Survey_IT_verbatim))] <- as.matrix(IT_verbatims_LUT[1][1])
names(Survey_IT_verbatim_no) <- names(Survey_IT_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


IT_Question_text <- paste0(c('', '', '', IT_verbatims_LUT$QuestionText))
names(Survey_IT_verbatim_no) <- paste(names(Survey_IT_verbatim_no), IT_Question_text, sep = ": ")





## WHS Verbatims ----
Survey_WHS                  <- Survey_data_complete_LG_roles_RO %>% select(SURV_WHS_questions)
Survey_WHS_satis            <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_WHS_questions))
Survey_WHS_verbatim         <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, WHS_verbatims) %>% .[rowSums( is.na(.) ) <=5, ] 

Survey_WHS_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, WHS_verbatims) %>%
  rowwise %>%
  mutate(No_Count = length(intersect(c_across(WHS_verbatims),keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(WHS_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(WHS_verbatims)) %>%
  select(-(No_Count:Remove_Count))


## Harmonise
WHS_verbatims_LUT <- LUT %>% 
  filter(Topic == 'WHS' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText) %>%
  
  mutate(Question = str_c("Q",Question),
         Topic    = "WHS")


## Rename
names(Survey_WHS_verbatim)[3:length(names(Survey_WHS_verbatim))] <- as.matrix(WHS_verbatims_LUT [1][1])
names(Survey_WHS_verbatim_no) <- names(Survey_WHS_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


WHS_Question_text <- paste0(c('', '', '', WHS_verbatims_LUT$QuestionText))
names(Survey_WHS_verbatim_no) <- paste(names(Survey_WHS_verbatim_no), WHS_Question_text, sep = ": ")





## Support Verbatims ----
Survey_Support              <- Survey_data_complete_LG_roles_RO %>% select(SURV_Support_questions)
Survey_Support_satis        <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SURV_Support_questions))
Survey_Support_verbatim     <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Support_verbatims) %>% .[rowSums( is.na(.)) <=2, ]

Survey_Support_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Support_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Support_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Support_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count)) %>%
  filter(Remove_Count < length(Support_verbatims)) %>%
  select(-(No_Count:Remove_Count))

## Harmonise
Support_verbatims_LUT <- LUT %>% 
  filter(Topic == 'General Support' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText) %>%
  
  mutate(Question = str_c("Q",Question),
         Topic    = "General Support")


## Rename
names(Survey_Support_verbatim)[3:length(names(Survey_Support_verbatim))] <- as.matrix(Support_verbatims_LUT [1][1])
names(Survey_Support_verbatim_no) <- names(Survey_Support_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value) %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Support_Question_text <- paste0(c('', '', '', Support_verbatims_LUT$QuestionText))
names(Survey_Support_verbatim_no) <- paste(names(Survey_Support_verbatim_no), Support_Question_text, sep = ": ")





## Conclusion Verbatims ----
Survey_Conclusion              <- Survey_data_complete_LG_roles_RO %>% select(SUR_Conclusion_questions)
Survey_Conclusion_satis        <- Survey_data_complete_LG_roles_RO %>% select(intersect(SUR_satis_questions, SUR_Conclusion_questions))
Survey_Conclusion_verbatim     <- Survey_data_complete_LG_roles_RO %>% select(ReturningOffice, VenueName, Role, Conclusion_verbatims) %>% .[rowSums( is.na(.)) <=2, ]


Survey_Conclusion_verbatim_no <- Survey_data_complete_LG_roles_RO %>% 
  select(ReturningOffice, VenueName, Role, Conclusion_verbatims) %>%
  rowwise %>%
  mutate(No_Count = sum((c_across(Conclusion_verbatims) %in% keywords_to_remove))) %>%
  mutate(NA_Count = sum(is.na(c_across(Conclusion_verbatims)))) %>%
  mutate(Remove_Count = sum(No_Count+NA_Count))     %>%
  filter(Remove_Count < length(Conclusion_verbatims)) %>%
  select(-(No_Count:Remove_Count))

## Harmonise
Conclusion_verbatims_LUT <- LUT %>% 
  filter(Topic == 'Conclusion' & Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText) %>% left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  rename(Question = `Page Number`)     %>% select(Question, QuestionText)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "Conclusion")


## Rename
names(Survey_Conclusion_verbatim)[3:length(names(Survey_Conclusion_verbatim))] <- as.matrix(Conclusion_verbatims_LUT [1][1])
names(Survey_Conclusion_verbatim_no) <- names(Survey_Conclusion_verbatim_no) %>%
  as_tibble() %>% 
  rename(QuestionNumber = value)     %>%
  left_join(table_of_question_number %>%
              select(Question, QuestionNumber), by="QuestionNumber") %>%
  mutate(Question = ifelse(is.na(Question),QuestionNumber,Question)) %>%
  mutate(Question = ifelse(str_detect(Question,"\\d"),str_c("Q",Question),Question)) %>%
  select(Question) %>%
  pull()


Conclusion_Question_text <- paste0(c('', '', '', Conclusion_verbatims_LUT$QuestionText))
names(Survey_Conclusion_verbatim_no) <- paste(names(Survey_Conclusion_verbatim_no), Conclusion_Question_text, sep = ": ")





## Venue Combo Verbatims ----
## Create a master table of the verbatims for just the questions relating to Venues
venues_review_questions <- c('E5',
                             'E5a',
                             'E6',
                             'E6a',
                             'G5',
                             'G5a',
                             'G15a',
                             'G17',
                             'G18',
                             'd7',
                             'D7a')

SURV_verbatim_venues_review_questions <- all_survey_cols[gsub("_.*","", all_survey_cols) %in% venues_review_questions]

## Venue Verbatims ----
Survey_Venues_review_verbatim <- Survey_data_complete_LG_roles_RO %>%
  dplyr::select(ReturningOffice, VenueName, Role, 'Venue Complexity', SURV_verbatim_venues_review_questions) %>%
  
  ## NA_per_row condition needs to be the No. of questions for that topic, minus 1
  mutate(NA_per_row = apply(., 1, function(x) sum(is.na(x)))) %>% filter(NA_per_row <= 34) %>% select(-NA_per_row) 



## Harmonise names
Venues_review_verbatims_LUT <- Survey_questions %>% 
  
  filter(QuestionNumber %in% venues_review_questions) %>% 
  select(QuestionNumber, QuestionNumber_sub, QuestionText, QuestionText_sub) %>% 
  left_join(LUT_Page_QU, by = "QuestionNumber") %>% 
  
  rename(Question = `Page Number`)     %>% select(Question, QuestionText, QuestionText_sub, QuestionNumber_sub)  %>%
  mutate(Question = str_c("Q",Question),
         Topic    = "Venues")


## Rename
names(Survey_Venues_review_verbatim)[5:length(names(Survey_Venues_review_verbatim))] <- as.matrix(Venues_review_verbatims_LUT[4][1])


## Four blanks are for the context
Venues_Review_Question_text <- paste0(c('', '', '', '', paste0(Venues_review_verbatims_LUT$QuestionText, ' ', 
                                      Venues_review_verbatims_LUT$QuestionText_sub)))

names(Survey_Venues_review_verbatim) <- paste(names(Survey_Venues_review_verbatim), 
                                                 Venues_Review_Question_text, sep = ": ")


## Save a CSV, column names are too long
# write_csv(Survey_Venues_review_verbatim, paste0(survey_tabular_output, 'LG21_Staff_Survey_Verbatims_Venues_Review.csv'))





## SAVE VERBATIMS AS EXCEL DATA  ==========================================================


## Collect all the verbatims together into one table?


## This info needs to be on the RO dashboard.
## Combine the LUTs into a single table
# LUT_verbatims <- bind_rows(Recruitment_verbatims_LUT,
#                            Training_verbatims_LUT,
#                            Resource_verbatims_LUT,
#                            Training_verbatims_LUT,
#                            Venue_verbatims_LUT,
#                            Venues_review_verbatims_LUT,
#                            Office_verbatims_LUT,
#                            Operations_verbatims_LUT,
#                            # Counting_verbatims_LUT,
#                            IT_verbatims_LUT,
#                            WHS_verbatims_LUT,
#                            Support_verbatims_LUT,
#                            Conclusion_verbatims_LUT) %>% arrange(Question)


## Now find out which rows are in the endpoint, but not the webportal
# topic_list <- c('LUT_verbatims',
#                 'Survey_Recruitment_verbatim_no', 
#                 
#                 'Survey_Training_verbatim_no',
#                 'Survey_Resource_verbatim_no',
#                 
#                 'Survey_Venue_verbatim_no',
#                 'Survey_Venues_review_verbatim',
#                 'Survey_Office_verbatim_no',
#                 
#                 'Survey_Operations_verbatim_no',
#                 'Survey_IT_verbatim_no',
#                 
#                 'Survey_WHS_verbatim_no',
#                 'Survey_Support_verbatim_no')


## Add data to workbook
# Verbatim_workbook <- createWorkbook()
# 
# 
# ## file = topic_list[1]
# for(file in topic_list) {
#   
#   ## Get required columns.
#   File_to_Write <- get(file)
#   
#   ## Add worksheet to the spread sheet
#   message('writing ', file,  ' to verbatim spreadsheet')
#   addWorksheet(Verbatim_workbook, file)
#   
#   ## Write the data to the corresponding worksheet
#   writeDataTable(wb       = Verbatim_workbook, 
#                  sheet    = file,
#                  x        = get(file),
#                  startCol = 1, 
#                  startRow = 1, 
#                  rowNames = FALSE,
#                  tableStyle = "TableStyleMedium2")
#   
# }


# if(write_verbatims_spreadsheet) {
#   
#   ## Save the whole workbook
#   saveWorkbook(Verbatim_workbook, 
#                paste0(survey_tabular_output, 'LG21_Staff_Survey_Verbatims_spreadsheet.xlsx'),
#                overwrite = TRUE)
#   
# }




#################################################### TBC ###########################################################