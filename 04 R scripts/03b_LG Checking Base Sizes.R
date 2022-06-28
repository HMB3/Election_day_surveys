################################# ---- CHECK SURVEY DATA BASES SIZES ---- ################################


## Purpose ----
## This code finds the base size for each question, and determines if the correct number of respondents have 
## answered that question staff survey is the results of a survey monkey, submitted by people that worked at 
## the polling stations EG one polling place thought they ran out of ballot papers, but they didn't. 




# Base sizes --------------------------------------------------------------



# Aims

#find the base size for each question, and determine if the correct number of respondents have answered that question

#this can then be used to ensure routing etc. is working.

#Will do this by calculating the number of respondents for each question, and comparing that to the amount of expected responses.

#This will be based off the data in the LUT - if it can't be done programatically, it will be left for a human to check


#Calculating actual base sizes------------------


BaseCheck <- Survey_data

#limit only to those who have a response at N4 - that means they finished.

Update <- which(is.na(BaseCheck$N4))

#Total base count will be everyone who finished.

TotalBase <- length(BaseCheck$N4) - length(Update)

if (TotalBase!=length(BaseCheck$N4)){
  BaseCheck<-BaseCheck[-Update,]
}


## Create an LUT of all the questions and their counts
BaseCount <- names(BaseCheck) %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    message("Creating count for ", x)
    
    # Create count and role objects separately
    count  <- BaseCheck %>% filter(!is.na(!!sym(x))) %>% nrow() 
    
  }) %>% c()

#make names consistent. Turn into data frame for neatness.
names(BaseCount) <- names(BaseCheck)

BaseCount <- as.data.frame(BaseCount)
rownames(BaseCount)[1]<-"BaseSize"



#Update Survey_questions to serve as the new LUT. Want to include response type, role list, and table type

NewLUT <- Survey_questions %>%
  
  left_join(LUT %>% select(QuestionText,
                           EM, 
                           SOAPP, 
                           SOA, 
                           OA,
                           VCM,
                           DVCM,
                           EO,
                           Table_type),
            
            by = c("QuestionText"))




#calculate expected base size, assuming no dependencies------------------------
#first question from NewLUT - what SHOULD base size be?
#begin with assumption that every question should be answered by everyone of that role.
#role is established with rows RO:EO of NewLUT

#Role is the correct variable

#remake BaseCheck as a factor to allow for summary
BaseCheck$Role <- as.factor(BaseCheck$Role)
RoleCount <- summary(BaseCheck$Role)

#role count provides the numbers of each role who have answered the survey

#Make a new variable in NewLUT with expected base size (assuming all members answered every question for that role)

NewLUT$ExpectedBase <- 0

#If a role isn't indicated in columns RO:EO of NewLUT, a respondent should not have seen it.
#We can easily check to see if the response is above the upper limit.
#If a role is ticked, add the number of responses from RoleCount to ExpectedBase

#!!!WARNING - TERMINOLOGY MAY CHANGE AND NEED TO BE UPDATED

#LUT uses state terminology. But RoleCount has already been changed to local terminology.



#EMs

if ("RO" %in% names(RoleCount)) {
Update <- which(NewLUT$RO=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["RO"]]

#SOAPPs


Update <- which(NewLUT$SOAPP=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["SOAPP"]]

#SOAs
Update <- which(NewLUT$SOA=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["SOA"]]

#OAs
Update <- which(NewLUT$OA=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["OA"]]

}



#VCMs
Update <- which(NewLUT$VCM=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["VCM"]]

#DVCMs
Update <- which(NewLUT$DVCM=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["DVCM"]]

#Election Officials
Update <- which(NewLUT$EO=="Y")
NewLUT$ExpectedBase[Update]<-NewLUT$ExpectedBase[Update]+RoleCount[["EO"]]




#Check Custom Variables----------------------
#first set of results in BaseCount are the custom variables. Treat them separately from the LUT.

#Exact number of custom variables may change. Use NewLUT to find the first question number in the LUT. Last Custom variable will be the value before that.

Update <- which(colnames(BaseCount)==NewLUT$QuestionNumber[1]) -1


#Should have exactly the total number of respondents  for all values (assuming the data is good)

Custom <- BaseCount[1:Update]
Custom[2,] <- TotalBase
rownames(Custom)[2] <- "ExpectedBase"

#Create a 'base correct' row. Assume it's false.

Custom[3,] <- FALSE
rownames(Custom)[3] <- "BaseCorrect"

#Find variables where ExpectedBase and BaseCount are the same. 

Update <- which(Custom["BaseSize",]==Custom["ExpectedBase",])

#mark those as 'True'

Custom["BaseCorrect",Update] <- TRUE

#Custom variables that need to be checked will be those which are marked as false.

IncorrectCustom <- Custom[,Custom["BaseCorrect",]==FALSE]


#Check Base sizes, no dependancies.-------------------

#Other base sizes will vary depending on roles

#!!!!WARNING - this is based on length of custom variables - if Custom isn't outputting correctly, there will be an issue here.

Update <- length(Custom) + 1

BaseCount <- BaseCount[Update:ncol(BaseCount)]

BaseCount <- rbind(BaseCount,NewLUT$ExpectedBase)
rownames(BaseCount)[2] <- "ExpectedBase"

#Create a 'base correct' row. Assume it's false.

BaseCount[3,] <- FALSE
rownames(BaseCount)[3] <- "BaseCorrect"




#dealing with variables with dependencies---------------

#If base size matches, that's good. But there are reasons why things won't match. The below code works through those expections.

#expecting there to be two types of exceptions that are easy to deal with - satisfaction follow ups and multi-choice responses

#a third type will be harder, as it will vary - following up on 'Did you do this' type questions.May leave this for manual checking.

#first, only keep variables which are not matching

BaseCount <- BaseCount[, BaseCount["BaseCorrect",] == FALSE]



#Dealing with multiple choice questons---------------------------

#multiple choice variables are marked in NewLUT
Update <- which(NewLUT$Table_type=="MultiChoiceTable")
Update <- NewLUT$QuestionNumber_sub[Update]


#create new table which contains just the multichoice responses.
MultiBase <- BaseCheck[, Update]

#base size is defined as those who have ANY response in the row for that question.

#need to match all questions that belong together. This is already done in NewLUT. 

#prepare new dataframe for matching

MultiBaseNames <- as.data.frame(names(MultiBase))
names(MultiBaseNames)[1] <- "QuestionNumber_sub"


#match to the Question Numbers

MultiBaseNames <- left_join(MultiBaseNames,
                          NewLUT,
                          by="QuestionNumber_sub") 


MultiBaseNames <- MultiBaseNames %>% 
  select(QuestionNumber_sub,QuestionNumber)

#We can find the unique question numbers and number of columns via 

MultiBaseInfo <- dplyr::count(MultiBaseNames, QuestionNumber)




#need to loop through each row in the above to find the true base size.

Loop <- 1:length(MultiBaseInfo$QuestionNumber)

for (i in Loop){
  #Create list which has all column names for ONE question
  Update <- MultiBaseNames$QuestionNumber_sub[MultiBaseNames$QuestionNumber==MultiBaseInfo$QuestionNumber[i]]
  
  #use list to create a table with all columns
  UpdateTable <- MultiBase[,Update]
  
  #find the number of not NAs in each row
  UpdateTable$Count <- rowSums(!is.na(UpdateTable))
  
  #find count of non-zero rows
  MultiBaseCount <- length(UpdateTable$Count[UpdateTable$Count!=0])
  
  #feed this back into BaseCount by giving it the names of the columns (saved in Update)
  
  BaseCount["BaseSize",Update] <- MultiBaseCount
}

#remove unneeded data structures.
rm(MultiBase, MultiBaseInfo, MultiBaseCount, MultiBaseNames, UpdateTable, i, Loop)





## Dealing with Ord5 follow ups -------------------------

#a large proportion of those remaining with unexpected base sizes will be follow ups to satisfaction scales.

#these will fufill the following criteria:

#1) will be marked as 'Free Text' in NewLUT$Response_type
#2) Will have an 'a' in the column name.
#3) be proceeded by a question or questions with the same number in the column name that are Ord5. 

#i.e. D15a will be marked as 'Free Text', and D15 will be marked as Ord5.


#problem the first: Identify those variables.

#find Free Text with an a in question name.
SatFollowUp <- NewLUT[NewLUT$Response_type=="Free Text",]

Update <- which(str_detect(SatFollowUp$QuestionNumber,"a"))
SatFollowUp <- SatFollowUp[Update,]

#check to see if previous question is an Ord5.

#create list of questions without an a.
PrevCheck <- as_tibble(str_sub(SatFollowUp$QuestionNumber, end=-2))

names(PrevCheck)[1] <- "QuestionNumber"

#Join it to LUT, remove irrelevant questions.
PrevCheck <- left_join(PrevCheck,LUT, by="QuestionNumber")

PrevCheck <- PrevCheck %>%
  select(QuestionNumber,Response_type)

#Find Ord5 questions with a follow up.

PrevCheck <- PrevCheck %>%
  filter(Response_type == "Ord5") %>%
  na.omit()



#create list of questions with follow ups. Show both the initial question number and the follow up question.

PrevCheck$FollowUpQNumber <- str_c(PrevCheck$QuestionNumber, "a")

#will loop through all of PrevCheck.
#Not everyone who answers the sat question will be followed up.
#Ord5 questions will be followed up if AT LEAST one variable was marked as 'dissatisfied' or 'very dissatisfied'

Loop <- 1:length(PrevCheck$QuestionNumber)

for (i in Loop) {
  #find the satisfaction questions
  Update <- NewLUT$QuestionNumber_sub[NewLUT$QuestionNumber==PrevCheck$QuestionNumber[i]]
  
  UpdateTable <- as_tibble(BaseCheck[,Update])
  
  #find all values which have been marked as 'dissatsified' or 'very dissatisfied'. 
  
  UpdateTable <- apply(UpdateTable,2,function(x) str_detect(x,"issatisfied"))
  
  #count how many rows have at least one true
  UpdateCount <- apply(UpdateTable,1,any)
  
  #count number of rows with at least one true
  FollowUpBase <- sum(UpdateCount, na.rm=TRUE)
  
  #insert FollowUpBase into appropriate point in BaseCount
  if (!is.null(BaseCount["ExpectedBase", PrevCheck$FollowUpQNumber[i]])) {
    BaseCount["ExpectedBase", PrevCheck$FollowUpQNumber[i]] <- FollowUpBase
  }
}

#this breaks of F3a, as there are multiple F3a follow ups. 
rm(PrevCheck, FollowUpBase, i, Loop, SatFollowUp, UpdateCount)





## Repeat same process for ord3 variables-------------

#a large proportion of those remaining with unexpected base sizes will be follow ups to yes no maybe questions.

#these will fufill the following criteria:

#1) will be marked as 'Free Text' in NewLUT$Response_type
#2) Will have an 'a' in the column name.
#3) be proceeded by a question or questions with the same number in the column name that are Ord3. 

#i.e. D15a will be marked as 'Free Text', and D15 will be marked as Ord3.


#problem the first: Identify those variables.

#find Free Text with an a in question name.
SatFollowUp <- NewLUT[NewLUT$Response_type=="Free Text",]

Update <- which(str_detect(SatFollowUp$QuestionNumber,"a"))
SatFollowUp <- SatFollowUp[Update,]

#check to see if previous question is an Ord5.

#create list of questions without an a.
PrevCheck <- as_tibble(str_sub(SatFollowUp$QuestionNumber, end=-2))

names(PrevCheck)[1] <- "QuestionNumber"

#Join it to LUT, remove irrelevant questions.
PrevCheck <- left_join(PrevCheck,LUT, by="QuestionNumber")

PrevCheck <- PrevCheck %>%
  dplyr::select(QuestionNumber,Response_type)

#Find Ord5 questions with a follow up.

PrevCheck <- PrevCheck %>%
  filter(Response_type=="Ord3") %>%
  na.omit()



#create list of questions with follow ups. Show both the initial question number and the follow up question.

PrevCheck$FollowUpQNumber <- str_c(PrevCheck$QuestionNumber, "a")

#will loop through all of PrevCheck.
#Not everyone who answers the binary question will be followed up.
#Ord3 questions are usually followed up if there is a 'no' response.


Loop <- 1:length(PrevCheck$QuestionNumber)

for (i in Loop) {
  #find the satisfaction questions
  Update <- NewLUT$QuestionNumber_sub[NewLUT$QuestionNumber==PrevCheck$QuestionNumber[i]]
  
  #remove NAs if neccesary
  Update <- Update[!is.na(Update)]
  
  
  UpdateTable <- as_tibble(BaseCheck[,Update])
  
  #find all values which have been marked as 'No'. 
  
  UpdateTable <- apply(UpdateTable,2,function(x) str_detect(x,"No"))
  
  #count how many rows have at least one true
  UpdateCount <- apply(UpdateTable,1,any)
  
  #count number of rows with at least one true
  FollowUpBase <- sum(UpdateCount, na.rm=TRUE)
  
  #insert FollowUpBase into appropriate point in BaseCount
  if (!is.null(BaseCount["ExpectedBase",PrevCheck$FollowUpQNumber[i]])) {
    BaseCount["ExpectedBase",PrevCheck$FollowUpQNumber[i]] <- FollowUpBase
  }
  
  
}

#this breaks of F3a, as there are multiple F3a follow ups. 
rm(PrevCheck,FollowUpBase,i,Loop, SatFollowUp, UpdateCount)





#outputting the mis-based variables------------------

#Find variables where ExpectedBase and BaseCount are correct. 

BaseCount <- BaseCount%>%
  t() %>%
  as.data.frame() 

Update <-  which(BaseCount[,"BaseSize"]==BaseCount[,"ExpectedBase"])


#mark those as 'True'
BaseCount[Update,"BaseCorrect"] <- TRUE

# setwd('./03 Working data')
# #print mismatched to a csv file for manual inspection
# 
# if(write_base_count_check) {
#   
#   write.csv(BaseCount, 
#             './TotalBaseCount.csv', row.names = TRUE)
#   
#   write.csv(BaseCount[-Update,], 
#             './MismatchedBaseCount.csv', row.names = TRUE)
#   
#   #write checking data as well.
#   write.csv(Survey_data_complete_LG_roles, './Base Checking Data.csv', row.names=TRUE, na = "")
#   
#   write.csv(NewLUT,"./NewLUT for Base Check.csv")
#   
# }





## Create a list of counts for each question, using the base sizes
BaseCount$QuestionNumber <- rownames(BaseCount)
base_size_list           <- BaseCount %>% as_tibble %>% .[,c(4,1)] %>% .$BaseSize %>% as.list() 
names(base_size_list)    <- BaseCount$QuestionNumber
names(base_size_list)    <- paste0(names(base_size_list), "_Count")



## Create a table of question counts, to be used later in the Table of contents
main_question_counts <- BaseCount %>% 
  select(QuestionNumber, BaseSize) %>%
  
  ## Aggegrate all the split questions
  mutate(QuestionNumber   = gsub("\\_.*","",    QuestionNumber)) %>% 
  
  ## Summarise by main question
  group_by(QuestionNumber) %>% 
  summarise(`BaseSize` = mean(`BaseSize`, na.rm = TRUE)) %>% 
  mutate(BaseSize      = round(BaseSize, 0))


## Rename question counts
main_question_count_lists        <- main_question_counts$`BaseSize`
names(main_question_count_lists) <- main_question_counts$QuestionNumber
names(main_question_count_lists) <- paste0(names(main_question_count_lists), "_Count")






# Write to file -----------------------------------------------------------


# Note that this script is riddled with other non-switchable file outputs

## Save the survey data to send reminder emails
if(write_reminder_table){
  
  write.csv(Survey_data, 
            './04 R scripts/Email List/Working Data/Survey_Data_2012_2021.csv', row.names = FALSE)
}










