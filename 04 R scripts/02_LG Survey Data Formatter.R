################################# ---- FORMAT STAFF SURVEY DATA TABLE ---- ################################


## Purpose ----
## This code pulls data from Survey Monkey. It formats the venue names of the staff survey data, and melts 
## the table for subsequent analysis. The staff survey is the results of a survey monkey, submitted by people 
## that worked at the polling stations.




## To-do ----
## Make sure the monkey import works for LG21, tweaks needed





## 2). CREATE SURVEY QUESTIONS & SUB-QUESTIONS ==================================================================


## Look for exceptions : these are things Tom can change in the Survey Monkey code


## We need a column in the LUT for whether or not each question will be in the index, and if it will contribute to
## the KPIs. These could be the same thing?


## Load LUT ----
LUT <- read_excel('./02 Source data/LGE21_LUT.xlsx',
                  sheet    = 'LUT') %>% 
  
  ## Make sure there's no white space, etc.
  mutate(QuestionText = trimws(QuestionText))


## Remove white space in column names
names(LUT) <- gsub(x = names(LUT), pattern = "\\.", replacement = " ")


## Question_translate
LUT_Page_QU <- LUT %>% select(`Page Number`, QuestionNumber)


## LUT of just the index questions
LUT_index <- LUT %>% filter(!is.na(Index)) %>% 
  rename(`Question Number` = `Page Number`,
         `Question label` = `QuestionNumber`) %>% 
  select(Topic, Index, `Question Number`, `Question label`, QuestionText, `Count of Roles`, Response_type) 


## Load Survey Monkey data ----
## Survey is considered complete is they answered the question towards the end :: 'Would you work again?'
Survey_data  <- read_csv('./02 Source data/LGE21 Local Elections Staff Survey 20221001.csv', 
                         locale = locale(encoding = "windows-1252"),
                         guess_max = 10000,
                         col_names = FALSE)

Survey_staff <- read_csv('./04 R scripts/Email List/Working Data/Total Staff Info.csv', 
                         locale = locale(encoding = "windows-1252"),
                         col_names = TRUE)


## Turn <NA> values into true NA values
Survey_data[Survey_data == "<NA>"] = NA





## Need to update the 'please eleborate question names' - question text must be unique
Update      <- which(Survey_data[1,]=="Please elaborate")
UpdateNames <- names(Survey_data[,Update])


## Create a list of questions and sub-questions to link with the LUT
#First 9 columns are junk survey monkey variables. 
Survey_questions <- Survey_data[1:2, 10:ncol(Survey_data)] %>% 
  
  ## Transpose top two rows of the Survey
  {as_tibble(t(.))} %>% 
  rename("QuestionText"     = `V1`, 
         "QuestionText_sub" = `V2`) %>% 
  
  ## Create all the columns needed for the LUT
  mutate(QuestionText = na.locf(QuestionText)) %>% 
  mutate(QuestionText = trimws(QuestionText)) %>%
  
  ## Join on the LUT
  left_join(., LUT %>% select(Topic, QuestionNumber, QuestionText, NumColumns, Response_type),
            by = c("QuestionText")) %>% 
  
  ## Fill the question down 
  group_by(QuestionText) %>% 
  mutate(QuestionNumber_sub = ifelse(NumColumns > 1, 
                                     paste0(QuestionNumber, '_', 1:n()), QuestionNumber)) %>% 
  
  ## Re-order the columns
  select(Topic, QuestionNumber, QuestionText, QuestionNumber_sub, QuestionText_sub, NumColumns, Response_type) %>%
  
  ## Eliminate the white space
  mutate(QuestionNumber     = trimws(QuestionNumber),
         QuestionNumber_sub = trimws(QuestionNumber_sub)) %>% 
  
  ## Remove any duplicates in the sub question column, not sure why these are coming in...
  .[!duplicated(.$QuestionNumber_sub),]



## Remove more weird characters here
# Survey_questions$QuestionText_sub <- gsub(Survey_questions$QuestionText_sub, pattern = "????",      replacement = "")
# Survey_questions$QuestionText_sub <- gsub(Survey_questions$QuestionText_sub, pattern = "?",         replacement = "")
# Survey_questions$QuestionText_sub <- gsub(Survey_questions$QuestionText_sub, pattern = "NSWEC????", replacement = "")


## Remove Other (please specify)
Survey_questions$QuestionText_sub <- gsub("\\s*\\([^\\)]+\\)","", as.character(Survey_questions$QuestionText_sub))
Survey_questions$QuestionText_sub <- gsub("Any other suggestions", "Other", Survey_questions$QuestionText_sub)
Survey_questions$QuestionText_sub <- gsub("Any other material",    "Other", Survey_questions$QuestionText_sub)


## Split the questions up into labels, and sub-labels...
 Survey_sub_questions <- Survey_questions %>% filter(NumColumns > 1)  %>% na.omit()
Survey_main_questions <- Survey_questions %>% filter(NumColumns == 1) %>% na.omit() %>% 
  mutate(label = paste0(QuestionNumber, ' - ', QuestionText))


## Check the match between the LUT and the Survey
## Save this table out to show Tom
length(unique(Survey_questions$QuestionText));length(unique(LUT$QuestionText))
write.csv(Survey_questions %>% select(Topic, QuestionNumber,QuestionText,NumColumns) %>% distinct(), 
          './02 Source data/Test_data/LG_survey_question_labels.csv', row.names = FALSE)





## Create dataframe list for the plot sub-labels ----


## Create the list of questions :: 
## Those that need sub-questions/labels
multicol_questions <- Survey_sub_questions %>% filter(NumColumns > 1) %>% 
  .$QuestionNumber %>% unique() %>% gsub(., pattern = "\\ ", replacement = "")


## Those that need sub-questions/labels
singlecol_questions <- Survey_questions %>% filter(NumColumns == 1) %>% 
  .$QuestionNumber %>% gsub(., pattern = "\\ ", replacement = "")


## This becomes the list of sub-labels that we index using the table and graph function
main_questions_labels         <- Survey_main_questions$label 
names(main_questions_labels)  <- Survey_main_questions$QuestionNumber
names(main_questions_labels)  <- paste0(names(main_questions_labels), '_label')


## Use a function to create the sub-labels
## Don't return these lists to the environment
## Index them using the question name
subquestion_labels <- sub_questions_labels(survey_data   = Survey_sub_questions,
                                           question_list = multicol_questions,
                                           table_suffix  = '_sub_labels')




# Add column names to survey data and decipher staff logins ---------------



# Update column names in the survey data
# First 9 columns get names from first row.
# Then use names from LUT
names(Survey_data)[1:9]  <- as.matrix(Survey_data[1:9][1, ]) ## Add others in here
names(Survey_data)[10:ncol(Survey_data)] <- Survey_questions$QuestionNumber_sub
Survey_data <- Survey_data[3:nrow(Survey_data),]

## Decipher the survey ID's to give staff login ID
Survey_data$LoginID <- decipher_LG21(Survey_data$`Staff ID`)





## 3). CREATE QUESTION LISTS FOR EACH Topic ==================================================================


## These lists of questions are used to analyse the survery results, and create the index values.
## Now we want to just use the "index" column (used to group the questions by meaning for analysis), 
## not the "topic" column, which was used to group the questions for the electors themselves.


## Demography questions 
Demography_questions <- LUT %>% 
  filter(Topic == 'Demographics') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## General Support questions 
Support_questions <- LUT %>% 
  filter(Topic == 'General Support') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Support_verbatims <- LUT %>% 
  filter(Topic == 'General Support' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## IT questions 
IT_questions <- LUT %>% 
  filter(Topic == 'IT and Tech Issues') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

IT_verbatims <- LUT %>% 
  filter(Topic == 'IT and Tech Issues' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Resource questions 
Resource_questions <- LUT %>% 
  filter(Topic == 'Resources') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Resource_verbatims <- LUT %>% 
  filter(Topic == 'Resources' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Recruitment questions 
Recruitment_questions <- LUT %>% 
  filter(Topic == 'Recruitment') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Recruitment_verbatims <- LUT %>% 
  filter(Topic == 'Recruitment' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Office questions 
Office_questions <- LUT %>% 
  filter(Topic == 'EM/RO Office operations') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Office_verbatims <- LUT %>% 
  filter(Topic == 'EM/RO Office operations' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Training questions 
Training_questions <- LUT %>% 
  filter(Topic == 'Training') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()

Training_verbatims <- LUT %>% 
  filter(Topic == 'Training' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Venue questions 
Venue_questions <- LUT %>% 
  filter(Topic == 'Venues') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Venue_verbatims <- LUT %>% 
  filter(Topic == 'Venues' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Counting questions 
Counting_questions <- LUT %>% 
  filter(Topic == 'Vote Counting') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


Counting_verbatims <- LUT %>% 
  filter(Topic == 'Vote Counting' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()



## Operations questions 
Operations_questions <- LUT %>% 
  filter(Topic == 'Voting Operations') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Operations_verbatims <- LUT %>% 
  filter(Topic == 'Voting Operations' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()



## WHS questions 
WHS_questions <- LUT %>% 
  filter(Topic == 'WHS') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

WHS_verbatims <- LUT %>% 
  filter(Topic == 'WHS' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()


## Verbatims 
Overall_verbatims <- LUT %>% 
  filter(Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Overall_verbatims_LUT <- LUT %>% 
  filter(Response_type == "Free Text") %>% 
  select(QuestionNumber, QuestionText)


## Conclusion questions 
Conclusion_questions <- LUT %>% 
  filter(Topic == 'Conclusion') %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique() %>% sort()

Conclusion_verbatims <- LUT %>% 
  filter(Topic == 'Conclusion' & Response_type == "Free Text") %>% 
  .$QuestionNumber %>% as.character() %>% 
  gsub("\\_.*","", .) %>% unique()    %>% sort()




