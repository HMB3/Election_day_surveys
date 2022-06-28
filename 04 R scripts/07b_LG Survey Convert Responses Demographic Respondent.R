#################################### STAFF SURVEY CONVERT RESPONSES #######################################


## Purpose ----
## The purpose of this code is to take the dataframe of staff survey responses and 
## create a numeric scale for questions selected for the index (e.g. satisfaction ratings)
## Then create tables with the numeric indexes for each business unit





## 4). CREATE ROLE INDEXES ====================================================================


## Combine indexes for each Role and topic - Note that some Roles didn't respond to the questions
## Calculate Venue indexes for each Role ----


## Calculate the RO-level Role indexes
## unique(SurveyData_numeric_questions$Role)
respondent_EO_index <- respondent_indexes(survey_indexes  = index_names,
                                          survey_numeric  = SurveyData_numeric_questions %>% filter(Role == "EO"),
                                          election_data   = venue_workload_satisfaction,
                                          max_score       = 5, 
                                          demography_cols = demography_cols,
                                          index_cols      = response_index_cols)


respondent_OA_index <- respondent_indexes(survey_indexes  = index_names, 
                                          survey_numeric  = SurveyData_numeric_questions %>% filter(Role == "OA"),
                                          demography_cols = demography_cols,
                                          election_data   = venue_workload_satisfaction,
                                          index_cols      = response_index_cols)


respondent_DPM_index <- respondent_indexes(survey_indexes  = index_names, 
                                           survey_numeric  = SurveyData_numeric_questions %>% filter(Role == "DPPM"),
                                           demography_cols = demography_cols,
                                           election_data   = venue_workload_satisfaction,
                                           index_cols      = response_index_cols)


respondent_PPM_index <- respondent_indexes(survey_indexes   = index_names, 
                                           survey_numeric   = SurveyData_numeric_questions %>% filter(Role == "PPM"),
                                           demography_cols  = demography_cols,
                                           election_data    = venue_workload_satisfaction,
                                           index_cols       = response_index_cols)


respondent_SOA_index <- respondent_indexes(survey_indexes  = index_names, 
                                           survey_numeric  = SurveyData_numeric_questions %>% filter(Role == "SOA"),
                                           demography_cols = demography_cols,
                                           election_data   = venue_workload_satisfaction,
                                           index_cols      = response_index_cols)


respondent_SOAP_index <- respondent_indexes(survey_indexes  = index_names, 
                                            survey_numeric  = SurveyData_numeric_questions %>% filter(Role == "SOAPP"),
                                            demography_cols = demography_cols,
                                            election_data   = venue_workload_satisfaction,
                                            index_cols      = response_index_cols)


gc()


## Aggregate Role Index tables ----


## This needs to change for Respondent level?
## Can only combine respondents at the topic level 


## Counting RO-level Indexes for each Role
respondent_ROLES_Counting_index <- respondent_EO_index %>% 
  
  select(`Respondent ID`, Counting) %>%
  rename(EO_Counting = Counting)    %>% 
  
  left_join(., select(respondent_OA_index,   "Respondent ID", Counting), by = "Respondent ID")  %>% 
  rename(OA_Counting = Counting)    %>% 
  
  left_join(., select(respondent_SOA_index,  "Respondent ID", Counting), by = "Respondent ID") %>% 
  rename(SOA_Counting = Counting)   %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Counting), by = 'VenueName') %>% 
  rename(SOAP_Counting = Counting)  %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Counting), by = 'VenueName') %>%  
  rename(DPM_Counting = Counting)   %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Counting), by = 'VenueName') %>%  
  rename(PPM_Counting = Counting)


gc()


## IS RO-level Indexes for each Role
respondent_ROLES_IS_index <- respondent_EO_index %>% 
  
  select(VenueName, IS) %>%
  rename(EO_IS = IS)   %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, IS), by = 'VenueName') %>% 
  rename(OA_IS = IS)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, IS), by = 'VenueName') %>% 
  rename(SOA_IS = IS)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, IS), by = 'VenueName') %>% 
  rename(SOAP_IS = IS) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, IS), by = 'VenueName') %>%  
  rename(DPM_IS = IS)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, IS), by = 'VenueName') %>%  
  rename(PPM_IS = IS)


gc()


## Logistics RO-level Indexes for each Role
respondent_ROLES_Logistics_index <- respondent_EO_index %>% 
  
  select(VenueName, Logistics)  %>%
  rename(EO_Logistics = Logistics)    %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Logistics), by = 'VenueName') %>% 
  rename(OA_Logistics = Logistics)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Logistics), by = 'VenueName') %>% 
  rename(SOA_Logistics = Logistics)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Logistics), by = 'VenueName') %>% 
  rename(SOAP_Logistics = Logistics) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Logistics), by = 'VenueName') %>%  
  rename(DPM_Logistics = Logistics)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Logistics), by = 'VenueName') %>%  
  rename(PPM_Logistics = Logistics)


gc()


## Operations RO-level Indexes for each Role
respondent_ROLES_Operations_index <- respondent_EO_index %>% 
  
  select(VenueName, Operations)  %>%
  rename(EO_Operations = Operations)   %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Operations), by = 'VenueName') %>% 
  rename(OA_Operations = Operations)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Operations), by = 'VenueName') %>% 
  rename(SOA_Operations = Operations)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Operations), by = 'VenueName') %>% 
  rename(SOAP_Operations = Operations) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Operations), by = 'VenueName') %>%  
  rename(DPM_Operations = Operations)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Operations), by = 'VenueName') %>%  
  rename(PPM_Operations = Operations)


gc()


## Recruitment RO-level Indexes for each Role
respondent_ROLES_Recruitment_index <- respondent_EO_index %>% 
  
  select(VenueName, Recruitment) %>%
  rename(EO_Recruitment = Recruitment) %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Recruitment), by = 'VenueName') %>% 
  rename(OA_Recruitment = Recruitment)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Recruitment), by = 'VenueName') %>% 
  rename(SOA_Recruitment = Recruitment)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Recruitment), by = 'VenueName') %>% 
  rename(SOAP_Recruitment = Recruitment) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Recruitment), by = 'VenueName') %>%  
  rename(DPM_Recruitment = Recruitment)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Recruitment), by = 'VenueName') %>%  
  rename(PPM_Recruitment = Recruitment) 


gc()


## Resource RO-level Indexes for each Role
respondent_ROLES_Staffing_index <- respondent_EO_index %>% 
  
  select(VenueName, Staffing) %>%
  rename(EO_Staffing = Staffing)    %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Staffing), by = 'VenueName') %>% 
  rename(OA_Staffing = Staffing)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Staffing), by = 'VenueName') %>% 
  rename(SOA_Staffing = Staffing)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Staffing), by = 'VenueName') %>% 
  rename(SOAP_Staffing = Staffing) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Staffing), by = 'VenueName') %>%  
  rename(DPM_Staffing = Staffing)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Staffing), by = 'VenueName') %>%  
  rename(PPM_Staffing = Staffing)

gc()


## Support RO-level Indexes for each Role
respondent_ROLES_Support_index <- respondent_EO_index %>% 
  
  select(VenueName, Support) %>%
  rename(EO_Support = Support)     %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Support), by = 'VenueName') %>% 
  rename(OA_Support = Support)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Support), by = 'VenueName') %>% 
  rename(SOA_Support = Support)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Support), by = 'VenueName') %>% 
  rename(SOAP_Support = Support) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Support), by = 'VenueName') %>%  
  rename(DPM_Support = Support)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Support), by = 'VenueName') %>%  
  rename(PPM_Support = Support) 


gc()


## Training RO-level Indexes for each Role
respondent_ROLES_Training_index <- respondent_EO_index %>% 
  
  select(VenueName, Training) %>%
  rename(EO_Training = Training)    %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Training), by = 'VenueName') %>% 
  rename(OA_Training = Training)    %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Training), by = 'VenueName') %>% 
  rename(SOA_Training = Training)   %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Training), by = 'VenueName') %>% 
  rename(SOAP_Training = Training)  %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Training), by = 'VenueName') %>%  
  rename(DPM_Training = Training)   %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Training), by = 'VenueName') %>%  
  rename(PPM_Training = Training)

gc()

## Venues RO-level Indexes for each Role
respondent_ROLES_Venues_index <- respondent_EO_index %>% 
  
  select(VenueName, Venues) %>%
  rename(EO_Venues = Venues)      %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, Venues), by = 'VenueName') %>% 
  rename(OA_Venues = Venues)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, Venues), by = 'VenueName') %>% 
  rename(SOA_Venues = Venues)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, Venues), by = 'VenueName') %>% 
  rename(SOAP_Venues = Venues) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, Venues), by = 'VenueName') %>%  
  rename(DPM_Venues = Venues)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, Venues), by = 'VenueName') %>%  
  rename(PPM_Venues = Venues)


gc()


## Venues RO-level Indexes for each Role
respondent_ROLES_WHS_index <- respondent_EO_index %>% 
  
  select(VenueName, WHS) %>%
  rename(EO_WHS = WHS)    %>% 
  
  left_join(., select(respondent_OA_index,   VenueName, WHS), by = 'VenueName') %>% 
  rename(OA_WHS = WHS)   %>% 
  
  left_join(., select(respondent_SOA_index,  VenueName, WHS), by = 'VenueName') %>% 
  rename(SOA_WHS = WHS)  %>% 
  
  left_join(., select(respondent_SOAP_index, VenueName, WHS), by = 'VenueName') %>% 
  rename(SOAP_WHS = WHS) %>% 
  
  left_join(., select(respondent_DPM_index,  VenueName, WHS), by = 'VenueName') %>%  
  rename(DPM_WHS = WHS)  %>%
  
  left_join(., select(respondent_PPM_index,  VenueName, WHS), by = 'VenueName') %>%  
  rename(PPM_WHS = WHS)

gc()





## 5). CREATE DEMOGRAPHIC INDEXES ====================================================================


## Calculate Venue indexes for each Demographic ----


## Calculate the district-level Role indexes
respondent_male_index        <- respondent_indexes(survey_indexes  = index_names, 
                                                   
                                                   survey_numeric  = SurveyData_numeric_questions %>% 
                                                     filter(Gender == 'Male'),
                                                   
                                                   demography_cols  = demography_cols,
                                                   election_data   = venue_workload_satisfaction,
                                                   index_cols      = response_index_cols)

gc()


respondent_female_index      <- respondent_indexes(survey_indexes  = index_names,
                                                   
                                                   survey_numeric  = SurveyData_numeric_questions %>% 
                                                     filter(Gender == 'Female'),
                                                   
                                                   demography_cols  = demography_cols,
                                                   election_data   = venue_workload_satisfaction,
                                                   index_cols      = response_index_cols)

gc()



respondent_multi_index        <- respondent_indexes(survey_indexes  = index_names, 
                                                    
                                                    survey_numeric  = SurveyData_numeric_questions %>% 
                                                      filter(Languages == 'Multilingual'),
                                                    
                                                    demography_cols  = demography_cols,
                                                    election_data   = venue_workload_satisfaction,
                                                    index_cols      = response_index_cols)

gc()


respondent_english_index    <- respondent_indexes(survey_indexes  = index_names, 
                                                  
                                                  survey_numeric  = SurveyData_numeric_questions %>% 
                                                    filter(Languages == 'English Only'),
                                                  
                                                  demography_cols  = demography_cols,
                                                  election_data   = venue_workload_satisfaction,
                                                  index_cols      = response_index_cols)

gc()


respondent_disability_index  <- respondent_indexes(survey_indexes  = index_names, 
                                                   
                                                   survey_numeric  = SurveyData_numeric_questions %>% 
                                                     filter(Disability == 'Disability'),
                                                   
                                                   demography_cols  = demography_cols,
                                                   election_data   = venue_workload_satisfaction,
                                                   index_cols      = response_index_cols)

gc()


respondent_no_disability_index       <- respondent_indexes(survey_indexes  = index_names, 
                                                           
                                                           survey_numeric  = SurveyData_numeric_questions %>% 
                                                             filter(Disability == 'No Disability'),
                                                           
                                                           demography_cols  = demography_cols,
                                                           election_data   = venue_workload_satisfaction,
                                                           index_cols      = response_index_cols)

gc()


respondent_indigenous_index       <- respondent_indexes(survey_indexes  = index_names, 
                                                        
                                                        survey_numeric  = SurveyData_numeric_questions %>% 
                                                          filter(Indigenous == 'Indigenous'),
                                                        
                                                        demography_cols  = demography_cols,
                                                        election_data   = venue_workload_satisfaction,
                                                        index_cols      = response_index_cols)

gc()


respondent_non_indigenous_index   <- respondent_indexes(survey_indexes  = index_names, 
                                                        
                                                        survey_numeric  = SurveyData_numeric_questions %>% 
                                                          filter(Indigenous == 'Non-Indigenous'),
                                                        
                                                        demography_cols  = demography_cols,
                                                        election_data   = venue_workload_satisfaction,
                                                        index_cols      = response_index_cols)


gc()





## 6). AGGREGATE RO DEMOGRAPHIC INDEXES ====================================================================


## Aggregate Demographic Index tables ----


## Combine indexes for each Role and topic - this doesn't work for all topics in SG19
## Counting Index by Demographics
respondent_demography_Counting_index <- respondent_male_index %>% 
  
  select(VenueName, Counting) %>%
  rename(Male_Counting = Counting) %>% 
  
  left_join(., select(respondent_female_index,         VenueName, Counting), by = 'VenueName') %>% 
  rename(Female_Counting = Counting) %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, Counting), by = 'VenueName') %>% 
  rename(Multi_Counting = Counting) %>%
  
  left_join(., select(respondent_english_index,        VenueName, Counting), by = 'VenueName') %>% 
  rename(English_Counting = Counting) %>%
  
  left_join(., select(respondent_disability_index,     VenueName, Counting), by = 'VenueName') %>% 
  rename(Disability_Counting = Counting) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Counting), by = 'VenueName') %>% 
  rename(No_disability_Counting = Counting) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Counting), by = 'VenueName') %>% 
  rename(Indigenous_Counting = Counting) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Counting), by = 'VenueName') %>% 
  rename(Non_Indigenous_Counting = Counting) 

gc()


## IS Index by Demographics
respondent_demography_IS_index <- respondent_male_index %>% 
  
  select(VenueName, IS)    %>%
  rename(Male_IS = IS)     %>% 
  
  left_join(., select(respondent_female_index,         VenueName, IS), by = 'VenueName') %>% 
  rename(Female_IS = IS)   %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, IS), by = 'VenueName') %>% 
  rename(Multi_IS = IS)     %>%
  
  left_join(., select(respondent_english_index,        VenueName, IS), by = 'VenueName') %>% 
  rename(English_IS = IS)    %>%
  
  left_join(., select(respondent_disability_index,     VenueName, IS), by = 'VenueName') %>% 
  rename(Disability_IS = IS) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, IS), by = 'VenueName') %>% 
  rename(No_disability_IS = IS)     %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, IS), by = 'VenueName') %>% 
  rename(Indigenous_IS = IS)     %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, IS), by = 'VenueName') %>% 
  rename(Non_Indigenous_IS = IS) 


gc()


## Operations Index by Demographics
respondent_demography_Operations_index <- respondent_male_index %>% 
  
  select(VenueName, Operations) %>%
  rename(Male_Operations = Operations) %>% 
  
  left_join(., select(respondent_female_index,        VenueName, Operations),  by = 'VenueName') %>% 
  rename(Female_Operations = Operations) %>% 
  
  left_join(., select(respondent_multi_index,         VenueName, Operations),  by = 'VenueName') %>% 
  rename(Multi_Operations = Operations) %>%
  
  left_join(., select(respondent_english_index,       VenueName, Operations),  by = 'VenueName') %>% 
  rename(English_Operations = Operations) %>%
  
  left_join(., select(respondent_disability_index,    VenueName, Operations),  by = 'VenueName') %>% 
  rename(Disability_Operations = Operations) %>%
  
  left_join(., select(respondent_no_disability_index, VenueName, Operations),  by = 'VenueName') %>% 
  rename(No_disability_Operations = Operations) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Operations), by = 'VenueName') %>% 
  rename(Indigenous_Operations = Operations) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Operations), by = 'VenueName') %>% 
  rename(Non_Indigenous_Operations = Operations) 


gc()


## Recruitment Index by Demographics
respondent_demography_Recruitment_index <- respondent_male_index %>% 
  
  select(VenueName, Recruitment) %>%
  rename(Male_Recruitment = Recruitment) %>% 
  
  left_join(., select(respondent_female_index,         VenueName, Recruitment), by = 'VenueName') %>% 
  rename(Female_Recruitment = Recruitment) %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, Recruitment), by = 'VenueName') %>% 
  rename(Multi_Recruitment = Recruitment) %>%
  
  left_join(., select(respondent_english_index,        VenueName, Recruitment), by = 'VenueName') %>% 
  rename(English_Recruitment = Recruitment) %>%
  
  left_join(., select(respondent_disability_index,     VenueName, Recruitment), by = 'VenueName') %>% 
  rename(Disability_Recruitment = Recruitment) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Recruitment), by = 'VenueName') %>% 
  rename(No_disability_Recruitment = Recruitment) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Recruitment), by = 'VenueName') %>% 
  rename(Indigenous_Recruitment = Recruitment) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Recruitment), by = 'VenueName') %>% 
  rename(Non_Indigenous_Recruitment = Recruitment) 


gc()


## Staffing Index by Demographics
respondent_demography_Staffing_index <- respondent_male_index %>% 
  
  select(VenueName, Staffing) %>%
  rename(Male_Staffing = Staffing) %>% 
  
  left_join(., select(respondent_female_index,         VenueName, Staffing), by = 'VenueName') %>% 
  rename(Female_Staffing = Staffing) %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, Staffing), by = 'VenueName') %>% 
  rename(Multi_Staffing = Staffing) %>%
  
  left_join(., select(respondent_english_index,        VenueName, Staffing), by = 'VenueName') %>% 
  rename(English_Staffing = Staffing) %>%
  
  left_join(., select(respondent_disability_index,     VenueName, Staffing), by = 'VenueName') %>% 
  rename(Disability_Staffing = Staffing) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Staffing), by = 'VenueName') %>% 
  rename(No_disability_Staffing = Staffing) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Staffing), by = 'VenueName') %>% 
  rename(Indigenous_Staffing = Staffing) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Staffing), by = 'VenueName') %>% 
  rename(Non_Indigenous_Staffing = Staffing)


gc()


## Resource Index by Demographics
respondent_demography_Support_index <- respondent_male_index %>% 
  
  select(VenueName, Support) %>%
  rename(Male_Support = Support) %>% 
  
  left_join(., select(respondent_female_index,        VenueName, Support),   by = 'VenueName') %>% 
  rename(Female_Support = Support) %>% 
  
  left_join(., select(respondent_multi_index,         VenueName, Support),   by = 'VenueName') %>% 
  rename(Multi_Support = Support) %>%
  
  left_join(., select(respondent_english_index,       VenueName, Support),   by = 'VenueName') %>% 
  rename(English_Support = Support) %>%
  
  left_join(., select(respondent_disability_index,    VenueName, Support),   by = 'VenueName') %>% 
  rename(Disability_Support = Support) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Support),  by = 'VenueName') %>% 
  rename(No_disability_Support = Support) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Support),  by = 'VenueName') %>% 
  rename(Indigenous_Support = Support) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Support),  by = 'VenueName') %>% 
  rename(Non_Indigenous_Support = Support) 

gc()


## Resource Index by Demographics
respondent_demography_Training_index <- respondent_male_index %>% 
  
  select(VenueName, Training) %>%
  rename(Male_Training = Training) %>% 
  
  left_join(., select(respondent_female_index,         VenueName, Training), by = 'VenueName') %>% 
  rename(Female_Training = Training) %>% 
  
  left_join(., select(respondent_english_index,        VenueName, Training), by = 'VenueName') %>% 
  rename(English_Training = Training) %>%
  
  left_join(., select(respondent_disability_index,     VenueName, Training), by = 'VenueName') %>% 
  rename(Disability_Training = Training) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Training), by = 'VenueName') %>% 
  rename(No_disability_Training = Training) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Training), by = 'VenueName') %>% 
  rename(Indigenous_Training = Training) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Training), by = 'VenueName') %>% 
  rename(Non_Indigenous_Training = Training) 

gc()


## Venue Index by Demographics
respondent_demography_Venues_index <- respondent_male_index %>% 
  
  select(VenueName, Venues) %>%
  rename(Male_Venues = Venues)%>% 
  
  left_join(., select(respondent_female_index,         VenueName, Venues), by = 'VenueName') %>% 
  rename(Female_Venues = Venues) %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, Venues), by = 'VenueName') %>% 
  rename(Multi_Venues = Venues) %>%
  
  left_join(., select(respondent_english_index,        VenueName, Venues), by = 'VenueName') %>% 
  rename(English_Venues = Venues) %>%
  
  left_join(., select(respondent_disability_index,     VenueName, Venues), by = 'VenueName') %>% 
  rename(Disability_Venues = Venues) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Venues), by = 'VenueName') %>% 
  rename(No_disability_Venues = Venues) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Venues), by = 'VenueName') %>% 
  rename(Indigenous_Venues = Venues) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Venues), by = 'VenueName') %>% 
  rename(Non_Indigenous_Venues = Venues) 


gc()


## Venue Index by Demographics
respondent_demography_Logistics_index <- respondent_male_index %>% 
  
  select(VenueName, Logistics) %>%
  rename(Male_Logistics = Logistics)%>% 
  
  left_join(., select(respondent_female_index,         VenueName, Logistics), by = 'VenueName') %>% 
  rename(Female_Logistics = Logistics) %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, Logistics), by = 'VenueName') %>% 
  rename(Multi_Logistics = Logistics) %>%
  
  left_join(., select(respondent_english_index,        VenueName, Logistics), by = 'VenueName') %>% 
  rename(English_Logistics = Logistics) %>%
  
  left_join(., select(respondent_disability_index,     VenueName, Logistics), by = 'VenueName') %>% 
  rename(Disability_Logistics = Logistics) %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, Logistics), by = 'VenueName') %>% 
  rename(No_disability_Logistics = Logistics) %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, Logistics), by = 'VenueName') %>% 
  rename(Indigenous_Logistics = Logistics) %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, Logistics), by = 'VenueName') %>% 
  rename(Non_Indigenous_Logistics = Logistics) 


gc()


## Training Index by Demographics
respondent_demography_WHS_index <- respondent_male_index %>% 
  
  select(VenueName, WHS) %>%
  rename(Male_WHS = WHS)       %>% 
  
  left_join(., select(respondent_female_index,         VenueName, WHS), by = 'VenueName') %>% 
  rename(Female_WHS = WHS)         %>% 
  
  left_join(., select(respondent_multi_index,          VenueName, WHS), by = 'VenueName') %>% 
  rename(Multi_WHS = WHS)          %>%
  
  left_join(., select(respondent_english_index,        VenueName, WHS), by = 'VenueName') %>% 
  rename(English_WHS = WHS)      %>%
  
  left_join(., select(respondent_disability_index,     VenueName, WHS), by = 'VenueName') %>% 
  rename(Disability_WHS = WHS)     %>%
  
  left_join(., select(respondent_no_disability_index,  VenueName, WHS), by = 'VenueName') %>% 
  rename(No_disability_WHS = WHS)  %>% 
  
  left_join(., select(respondent_indigenous_index,     VenueName, WHS), by = 'VenueName') %>% 
  rename(Indigenous_WHS = WHS)     %>%
  
  left_join(., select(respondent_non_indigenous_index, VenueName, WHS), by = 'VenueName') %>% 
  rename(Non_Indigenous_WHS = WHS)


gc()


#################################################### TBC ###########################################################