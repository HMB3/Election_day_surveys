#################################### STAFF SURVEY CONVERT RESPONSES #######################################


## Purpose ----
## The purpose of this code is to take the dataframe of staff survey responses and 
## create a numeric scale for questions selected for the index (e.g. satisfaction ratings)
## Then create tables with the numeric indexes for each business unit.


## The index gives different information from satisfaction.
## The index analysis warps the data, and breaks it down by topic.
## The satisfaction analysis doesn't do that, so they provide different info 


## To do ----


## Remove the RO responses from the Survey entirely.
## Then the RO workload is not related to the RO responses, it's just for the staff who 
## worked at venues that relate to that RO. EG agggreate venue level responses up to
## RO, then relate workload to the number of venues that were over-projected for that RO.
## This is essentially and ordinal variable of the continous proportion at the venue level
## (i.e. % over-projected for each venue).


## When creating an index, consider the distribution of question types. 
## Should we be aiming for a more balanced number of questions?
## Equal number of Index questions in each Topic
business.plot <- table(table_of_contents$Topic) %>% 
  as.data.frame() %>% 
  dplyr::rename(., Topic = Var1, Question_count = Freq) %>% 
  arrange(-Question_count)


## Consider the distribution of questions - the aggregation scheme will affec the results
ggplot(business.plot , aes(x = reorder(Topic, -Question_count), Question_count)) + 
  geom_bar(stat = "identity", fill = "Blue") +
  theme(axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  
  ## And title
  ggtitle(paste0(''))





## This analysis assumes that analysing the indexes within business units is a valid approach


## This data frame assumes we have tables of satisfaction aggregated to Venue and RO level.
## Curently, those tables are not working 100% as they need to properly aggregated
venue_workload_satisfaction <- venue_workload %>%
  
  left_join(Venue_satisfaction_table_wider, by = c("VenueName")) %>% 
  
  ## Aggregate the Remoteness categories
  mutate(Remoteness = gsub('Inner Regional Australia',   'Regional',     Remoteness),
         Remoteness = gsub('Outer Regional Australia',   'Regional',     Remoteness),
         Remoteness = gsub('Major Cities of Australia',  'Major Cities', Remoteness),
         Remoteness = gsub('Very Remote Australia',      'Remote',       Remoteness),
         Remoteness = gsub('Remote Australia',           'Remote',       Remoteness)) %>% 
  
  ## Fill in the extra RO office data and locations that went missing earlier
  ## Ultimately, come back and fix that up with cleaner joins earlier in the process 
  inner_join(., select(LGA_column_venues, -LGAreaCode), 
             by = c("ReturningOffice", "VenueName")) %>% 
  inner_join(., venue_location_LGA_columns, 
             by = c("ReturningOffice", "VenueName", "Latitude", "Longitude")) %>% 
  
  ## Remove the Duplicate Venues in Away LGAs
  .[!duplicated(.$`VenueName`), ]


## This table is missing ABS context data for Cootamundara Gundagai, and Dubbo
RO_workload_satisfaction <- RO_workload %>%
  
  left_join(RO_satisfaction_table_wider,    by = c("ReturningOffice")) %>% 
  
  ## Aggregate the Remoteness categories
  mutate(Remoteness = gsub('Inner Regional Australia',   'Regional',     Remoteness),
         Remoteness = gsub('Outer Regional Australia',   'Regional',     Remoteness),
         Remoteness = gsub('Major Cities of Australia',  'Major Cities', Remoteness),
         Remoteness = gsub('Very Remote Australia',      'Remote',       Remoteness),
         Remoteness = gsub('Remote Australia',           'Remote',       Remoteness)) 


## Intersect the Venues and EMs in these data frames
length(intersect(venue_workload_satisfaction$VenueName, 
                 Survey_data_complete_LG_RO_distinct$VenueName))


## EMs are being dropped out somewhere in the vote calc stage...
length(intersect(RO_workload_satisfaction$VenueName, 
                 Survey_data_complete_LG_RO_distinct$ReturningOffice))





## 1). CONVERT SURVEY RESPONSES TO NUMERICS ====================================================


## In the new survey, the time change step will be easier.
## However, we still need to convert the data to 0-1 for all questions chosen to be in the index
## The effect on the final responses is worth considering.
## We need to create a separate RMD file which documents what we did


## The column names needed for the response-level index
demography_cols <- c('Languages',
                     'Disability',
                     'Indigenous',
                     'Age',
                     'Gender')


## List of unique respondents
complete_respondents <- unique(Survey_data_complete_LG_RO_distinct$`Respondent ID`)


## Convert satisfaction survey responses to numeric ---- 
SurveyData_numeric_satis <- Survey_data_complete_LG_RO_distinct %>%
  
  ## Remove the EMs at this point - their responses cannot be used effectively
  ## In either a thematic or spatial analysis of responses
  filter(Role != "RO") %>% 
  
  ## Just get the index satisfaction columns
  dplyr::select(`Respondent ID`, 
                ReturningOffice, 
                VenueName, 
                Role, 
                Status, 
                demography_cols, 
                SUR_index_satisf) %>% 
  
  mutate_at(
    vars(SUR_index_satisf),
    funs(case_when(
      
      ## For time questions, use the six-figure summary
      ## min - 1st quartile       = -1/+1
      ## 1st quartile - 3rd quart =   0
      ## 3rd quart - max          = -1/+1
      
      ## Replace all satisfactory values
      ## Dissatisfied is always bad
      . == 'Very satisfied'     ~ 5,
      . == 'Satisfied'          ~ 4,
      . == 'Neutral'            ~ 3,
      . == 'Dissatisfied'       ~ 2,
      . == 'Very dissatisfied'  ~ 1))) 



SurveyData_numeric_yes_neg <- Survey_data_complete_LG_RO_distinct %>%
  
  ## Just get the index yes/no columns where yes is a negative
  dplyr::select(SUR_index_yes_neg,
                `Respondent ID`, 
                ReturningOffice, 
                VenueName) %>%
  
  mutate_at(
    
    vars(SUR_index_yes_neg),
    funs(case_when(
      
      ## Check these values makes sense for every
      ## This assumes yes is always negative, 
      ## and no is always postive
      . == 'Yes'        ~  1,
      . == 'Uncertain'  ~  2,
      . == 'No'         ~  3)))


SurveyData_numeric_yes_pos <- Survey_data_complete_LG_RO_distinct %>%
  
  ## Just get the index yes/no columns where yes is a negative
  dplyr::select(SUR_index_yes_pos,
                `Respondent ID`, 
                ReturningOffice, 
                VenueName) %>%
  mutate_at(
    vars(SUR_index_yes_pos),
    funs(case_when(
      
      ## Check these values makes sense for every
      ## This assumes yes is always negative, 
      ## and no is always postive
      . == 'Yes'        ~  3,
      . == 'Uncertain'  ~  2,
      . == 'No'         ~  1)))


## Bind the numeric data back together ---
SurveyData_numeric_questions <- SurveyData_numeric_satis %>% 
  
  ## Data should be exactly in order
  left_join(SurveyData_numeric_yes_neg, by = c("Respondent ID", "ReturningOffice", "VenueName")) %>% 
  left_join(SurveyData_numeric_yes_pos, by = c("Respondent ID", "ReturningOffice", "VenueName"))


## Now join on the LGA context for LG21, so we can aggregate up to the RO level
## Not all IDs are unique - consider what this means?


## Get the duplicate rows
SurveyData_numeric_join_dupes <- SurveyData_numeric_questions[
  duplicated(SurveyData_numeric_questions), ]


## Logical check - only unique IDs used in the analysis?
# nrow(SurveyData_numeric_questions);complete_responses
# stopifnot(nrow(SurveyData_numeric_questions) == complete_responses)





## 2). CREATE INDEX COLUMN LISTS ====================================================================


##
response_index_cols <- c(
  
  ## Respondent/Venue
  "Respondent ID",
  "Role",
  "ReturningOffice",            
  "VenueName",               
  "Latitude",            
  "Longitude",
  
  ## Context
  "Gender",
  "Age",
  "Languages",
  "Disability",
  "Indigenous",
  
  ## Update these 
  "StaffingCategory",    
  "Remoteness",          
  "High_SEIFA_prop",         
  "Low_SEIFA_prop",
  
  ## Voting - Update these
  "Total_Votes",    
  "Ordinary_Projected",   
  "Venue_Voting_Load",
  
  ## Index Values - don't necessarily match topics
  "Counting",         
  "IS",
  "Logistics",
  "Operations",
  "Recruitment",
  "Staffing",
  "Training", 
  "Venues",
  "WHS")


## The column names needed for the venue-level index
venue_index_cols <- c(
  
  ## Respondent/Venue
  "ReturningOffice",
  "LGAreaCode",
  "VenueName",
  "LocationTypeCode",
  "Latitude",            
  "Longitude",
  
  ## Context of the area
  "StaffingCategory",    
  "Remoteness",          
  "CALD_prop",           
  "Indigenous_prop",           
  "High_SEIFA_prop",         
  "Low_SEIFA_prop",
  
  ## Voting - Update these
  "Total_Votes",    
  "Ordinary_Projected",   
  "Informality",       
  "Venue_Voting_Load",
  "Venue_Satisfied",
  "Venue_Dissatisfied",
  "Venue_Neutral",
  
  
  ## Index Values - don't necessarily match topics
  "Counting",         
  "IS",
  "Logistics",
  "Operations",
  "Recruitment",
  "Staffing",
  "Training", 
  "Venues",
  "WHS")


## The column names needed for the district-level index
RO_index_cols <- c(
  
  ## Respondent/Venue
  "ReturningOffice", 
  "LGAreaCode",
  "StaffingCategory",    
  "Remoteness",
  "CALD_prop",
  "Indigenous_prop",           
  "High_SEIFA_prop",          
  "Low_SEIFA_prop",
  
  ## Voting
  "Total_Votes",
  "Informality",
  "Venues_over_proj",
  "Respondent_Satisfied",
  "Respondent_Satisfied",
  "RO_Neutral",
  
  ## Index Values - don't necessarily match topics
  "Counting",         
  "IS",
  "Logistics",
  "Operations",
  "Recruitment",
  "Staffing",
  "Training", 
  "Venues",
  "WHS")


## Just the numeric index colums for data slices
index_plot_columns <- c("Counting",           
                        "IS",                 
                        "Logistics",         
                        "Operations",         
                        "Recruitment",        
                        "Staffing",           
                        "Training",           
                        "Venues",             
                        "WHS") %>% sort()


## Create list of business units to create  indexes for
index_list  <- mget(ls(name = .GlobalEnv, pattern = 'SURV_Index_')) 
index_names <- mget(ls(name = .GlobalEnv, pattern = 'SURV_Index_')) %>% names()





## 3). CREATE RESPONSE, VENUE AND DISTRICT INDEXES ========================================================

## The functions below calculate the survey indexes for different levels of aggreation.
## EG Numeric indexes on each topic for all respondents, venues and RO Offices.


## Which level makes sense? Respondent level is the best, because it captures the variability
## of the responses from individaul people, not some arbitrary unit of aggregation (e.g, Venue, RO, LGA, District,
## etc.). This is related to the MAUP.


## Calculate the response-level survey indexes ----
## Note that only the EMs which we don't have details for answered the "General Suport" index,
## So we we can't really use that data.


## All other aggregations to use this data frame 
## Don't Remove the NA Venues or the NA responses, do this after
respondent_index <- respondent_indexes(survey_indexes    = index_names,
                                       survey_numeric    = SurveyData_numeric_questions,
                                       questions_LUT     = Survey_questions, 
                                       election_data     = venue_workload_satisfaction,
                                       demography_cols   = demography_cols,
                                       index_cols        = response_index_cols)


## Fill in the extra RO office data and locations that went missing earlier
## This should just be a mistmatch between the ABS 2016 LGA names, and NSWEC LGA names
## EG Dubbo Region RO Office and Cootamundra-Gundagai Region RO Office Don't match
respondent_index_ro_join <- respondent_index %>% 
  
  ## Fill in the extra RO office data and locations that went missing earlier
  ## Ultimately, come back and fix that up with cleaner joins earlier in the process
  inner_join(., LGA_column_venues, 
             by = c("ReturningOffice", "VenueName")) %>% 
  
  inner_join(., venue_location_LGA_columns, 
             by = c("ReturningOffice", "VenueName", "Latitude", "Longitude")) %>% 
  
  inner_join(., respondent_satis_tab_wider, by = "Respondent ID") %>% 
  
  .[!duplicated(.$`Respondent ID`), ] %>% 
  
  dplyr::select('Respondent ID',    Role,                 ReturningOffice,   LGAreaCode,           VenueName,             Latitude,               
                Longitude,          Gender,               Age,               Languages,            Disability,             
                Indigenous,         StaffingCategory,     Remoteness,        High_SEIFA_prop,      Low_SEIFA_prop,         
                Total_Votes,        Ordinary_Projected,   Venue_Voting_Load, Respondent_Satisfied, Respondent_Dissatisfied,
                Respondent_Neutral, everything())


## Now get the



## Resp. level, averaged to venue
respondent_index_venue_average <- respondent_index_ro_join %>% 
  
  ## Note we are averaging the satisfaction and index scores for all respondents in each Venue.
  ## This is attempting to capture the respondent-level variability
  group_by(VenueName) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  dplyr::select(VenueName, 
                "Respondent_Satisfied",    
                "Respondent_Dissatisfied", 
                "Respondent_Neutral",
                index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(dplyr::select(venue_workload_satisfaction, 
                           -Venue_Satisfied, 
                           -Venue_Dissatisfied, 
                           -Venue_Neutral), ., by = "VenueName") %>% 
  
  ## Re-order columns
  dplyr::select(ReturningOffice,    LGAreaCode,           VenueName,    LocationTypeCode,  Latitude,               
                Longitude,          StaffingCategory,     Remoteness,   High_SEIFA_prop,   Low_SEIFA_prop,         
                Total_Votes,        Ordinary_Projected,   Informality,  Venue_Voting_Load, Respondent_Satisfied, 
                
                Respondent_Dissatisfied,
                Respondent_Neutral, everything())



## Resp. level averaged to RO
respondent_index_ro_average <- respondent_index_ro_join %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  dplyr::select(ReturningOffice, 
                "Respondent_Satisfied",    
                "Respondent_Dissatisfied", 
                "Respondent_Neutral",
                index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(dplyr::select(RO_workload_satisfaction, 
                           -RO_Satisfied, 
                           -RO_Dissatisfied, 
                           -RO_Neutral), ., by = "ReturningOffice")



## Just Respondent satisfaction, but including
resp_satis_tab_wider_cont <- respondent_satis_tab_wider %>% 
  
  left_join(., dplyr::select(respondent_index_ro_join,
                             'Respondent ID',  Role,                 ReturningOffice,   LGAreaCode,           VenueName,    Latitude,               
                             Longitude,        Gender,               Age,               Languages,            Disability,             
                             Indigenous,       StaffingCategory,     Remoteness,        High_SEIFA_prop,      Low_SEIFA_prop),
  by = 'Respondent ID')




#################################################### TBC ###########################################################