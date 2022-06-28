


# Staff survey demographics -----------------------------------------------




# Initialisation ----------------------------------------------------------


# Create vector
staff_loginid_unique <- unique(Survey_data$LoginID)




# EMA source data queries -------------------------------------------------

setwd(database_connections)
source("EMA.R")
source("EMS.R")
source("PRCC.R")



## Staff positions
staff_positions <- sqlExecute(EMA_connection,
                              
                              "SELECT
                                stafftypecode,
                                areacode,
                                locationid,
                                positionid,
                                loginid,
                                positiontypecode
                              
                              FROM STAFFING.ES_STAFFPOSITIONS
                              WHERE POSITIONSTATUS = 'EMPLOYED'
                              AND ELECTIONEVENTID = ?",
                              
                              data             = event_group_ID,
                              fetch            = TRUE,
                              stringsAsFactors = FALSE)


## Staff details
staff_details <- sqlExecute(EMA_connection,
                            
                            "SELECT 
                              loginid,
                              dateofbirth,
                              postcode,
                              haselectoralexperience
                            FROM STAFFING.ES_STAFF
                             WHERE STAFFSTATUS = 'ACTIVE'
                             AND ELECTIONEVENTID = ?",
                            
                            data             = event_group_ID,
                            fetch            = TRUE,
                            stringsAsFactors = FALSE)


## Demographics

staff_demographics <- sqlExecute(EMA_connection,
                                 
                                 "SELECT LOGINID,
                                     TYPECODE,
                                     VALUEENTERED
                                 FROM STAFFING.ES_STAFFCOMPLIANCE
                                 WHERE ELECTIONEVENTID = ?
                                    AND typecode IN (
                                         'SecondLanguage1'
                                         ,'SecondLanguage2'
                                         ,'SecondLanguage3'
                                         ,'HasAboriginalTSIBackground'
                                         ,'COM032_ReqAdjToWorkEnv'
                                         ,'COM032_Disability'
                                         )
                                    AND VALUEENTERED IS NOT NULL",
                                 data             = event_group_ID,
                                 fetch            = TRUE,
                                 stringsAsFactors = FALSE)




# EMS source queries ------------------------------------------------------




## Replace this query with 'VenueName last exported to EMA'
venue_locations <- sqlExecute(EMS_connection,
                              
                              "SELECT [EventID]
                            ,case when evl.[VenueNameLastExportedToEMA] IS NOT NULL
                            THEN evl.[VenueNameLastExportedToEMA]
                            ELSE evl.[VenueName] END AS VenueName
                            ,[LocationTypeCode]
                            ,[LocationStatusCode]
                            ,loc.StreetAddressID
                            ,addr.Latitude
                            ,addr.Longitude
                            ,addr.AddressName
                            ,addr.AddressLine1
                            ,addr.AddressLine2
                            ,addr.LocalityName
                            ,addr.PostCode
                            FROM EMS2009.[Events].[EMS_EventLocation] evl
                            left join EMS2009.[Resources].[EMS_Location] loc on loc.VenueName = evl.VenueName
                            left join EMS2009.[Resources].[EMS_Address] addr on loc.StreetAddressID = addr.AddressID
                            where [EventID] like ?
                            AND evl.LocationStatusCode NOT IN ('Away Cancelled',
                                                               'Cancellation Notified',
                                                               'Cancelled',
                                                               'Deferred',
                                                               'Hire Agreement Rejected',
                                                               'Initial',
                                                               'Managed by 3rd party',
                                                               'Print Cancelled letter',
                                                               'Print Deferred letter',
                                                               'Processed',
                                                               'Unacceptable',
                                                               'Unavailable',
                                                               'Unavailable Indefinitely',
                                                               'Uncontested',
                                                               'Visit Not Required')",
                              
                              data = paste0(event_group_ID, '%'),
                              TRUE,
                              stringsAsFactors = FALSE) %>%
  as_tibble()





# PRCC source queries ------------------------------------------------------


## Final votes
PRCC_final_votes <- sqlQuery(PRCC_connection,
                             
                             "SELECT  AW.AREA_CODE
                                ,ea.[ELECTION_NAME]
                                        ,PP.POLLING_PLACE_NAME AS VENUE_VOTE_TYPE
                                        ,INF.INFORMAL AS INFORMAL_VOTES
                                        ,PP.TOTAL_FORMAL_VOTES AS FORMAL_VOTES
                                        ,pp.[POLLING_PLACE_TYPE]
                                        FROM [POLLINGPLACE] PP
                                            INNER JOIN [AREAWARD] AW
                                                ON PP.AREAWARD_ID = AW.AREAWARD_ID
                                    LEFT JOIN [dbo].[ELECTIONAREA] ea on pp.[ELECTION_AREA_ID] = ea.[ELECTION_AREA_ID]
                                            LEFT JOIN (SELECT PP.POLLINGPLACE_ID, COUNT(1) INFORMAL
                                                        FROM [POLLINGPLACE] PP
                                                            LEFT JOIN [BATCH] BAT
                                                                ON pp.POLLINGPLACE_ID = BAT.POLLINGPLACE_ID
                                                            LEFT JOIN [BALLOT_PAPER_R1] BPR1
                                                                ON BAT.BATCHID = BPR1.BATCHID 
                                                        WHERE BPR1.TYPE = 40
                                                        GROUP BY pp.POLLINGPLACE_ID) INF 
                                                ON PP.POLLINGPLACE_ID = INF.POLLINGPLACE_ID
                                        WHERE PP.POLLINGPLACE_ID > 0
                                        ORDER BY AW.AREA_CODE, PP.SORT_ORDER, PP.POLLING_PLACE_NAME",
                             
                             stringsAsFactors = FALSE) %>%
  
  mutate(INFORMAL_VOTES = ifelse(is.na(INFORMAL_VOTES), 0, INFORMAL_VOTES),
         FORMAL_VOTES   = ifelse(is.na(FORMAL_VOTES),   0, FORMAL_VOTES),
         TOTAL_VOTES    = INFORMAL_VOTES + FORMAL_VOTES) %>%
  rename(WardAreaCode   = AREA_CODE) %>%
  as_tibble()





## Councillor vote formality by contest area and venue -----------
Councillor_formality_contest_venue <- PRCC_final_votes %>%
  
  #get councillors only
  filter(str_detect(ELECTION_NAME, "Councillor")) %>%
  
  group_by(WardAreaCode, VENUE_VOTE_TYPE) %>%
  summarise(INFORMAL_VOTES = sum(INFORMAL_VOTES),
            FORMAL_VOTES   = sum(FORMAL_VOTES),
            TOTAL_VOTES    = sum(TOTAL_VOTES),
            `FORMALITY_RATE_(%)`   = round((FORMAL_VOTES/TOTAL_VOTES)  *100, digits = 2),
            `INFORMALITY_RATE_(%)` = round((INFORMAL_VOTES/TOTAL_VOTES)*100, digits = 2)) %>%
  ungroup() %>%
  rename(VenueName = VENUE_VOTE_TYPE)


odbcCloseAll()



# Implement hierarchy of roles and de-dupe staffing data ------------------



# Only one survey link was sent out per staff member

# A hierarchy was used to choose role. Preference given to rarer roles. Generated dynamically from full list.

Preference <- staff_positions %>%
  select(POSITIONTYPECODE) %>% 
  group_by(POSITIONTYPECODE) %>%
  mutate(Count = n()) %>%
  distinct() %>%
  arrange(Count) %>%
  filter(!is.na(POSITIONTYPECODE)) %>%
  ungroup() %>%
  mutate(Preference = rank(Count, ties.method = c("first"))) %>%
  select(-Count)


## 
Ranked_Respondents <- staff_positions %>%
  distinct() %>% 
  left_join(Preference
            ,by="POSITIONTYPECODE") %>%
  group_by(LOGINID) %>%
  filter(Preference == min(Preference)) %>%
  select(-Preference)


## There are still situations where the same email has multiple staff IDs. In that case, just take the first one.
Final_Respondents <- Ranked_Respondents %>%
  group_by(LOGINID)   %>%
  mutate(Count = n()) %>%
  filter(Count == 1)  %>%
  select(-Count)      %>%
  ungroup()



# Data validation
staffing_respondent_check <- identical(length(unique(Final_Respondents$LOGINID)), nrow(Final_Respondents))

if(!staffing_respondent_check) {
  
  stop("The final data from staffing is not unique!")
  
}




# Process staff demographics ----------------------------------------------



staff_demographics_wide <- staff_demographics %>%
  
  mutate(TYPECODE = ifelse(grepl('Language', TYPECODE), 'LanguageCount', TYPECODE)
         ,VALUEENTERED = ifelse(grepl('english', VALUEENTERED, ignore.case = TRUE), NA, VALUEENTERED)
         ,VALUEENTERED = ifelse(!is.na(VALUEENTERED) & TYPECODE == 'LanguageCount', 1, VALUEENTERED)
  ) %>%
  
  {
    bind_rows(filter(., TYPECODE != 'LanguageCount')
              
              ,filter(., TYPECODE == 'LanguageCount') %>%
                group_by(LOGINID, TYPECODE) %>%
                summarise(VALUEENTERED = sum(as.numeric(VALUEENTERED))) %>%
                mutate(VALUEENTERED = as.character(VALUEENTERED))
    )
  } %>%
  
  pivot_wider(names_from = "TYPECODE"
              ,values_from = "VALUEENTERED")






# Join demographics to results --------------------------------------------


## 
Survey_data_context <- Survey_data %>%
  
  left_join(Final_Respondents %>%
              mutate(StreetAddressID = as.numeric(gsub('[^0-9]','', LOCATIONID)))
            ,by = c("LoginID" = "LOGINID")) %>%
  
  left_join(staff_details
            ,by = c("LoginID" = "LOGINID")) %>%
  
  left_join(staff_demographics_wide
            ,by = c("LoginID" = "LOGINID")) %>%
  
  left_join(venue_locations %>%
              select(VenueName, StreetAddressID) %>%
              #get rid of away venues
              distinct()
            ,by = c("StreetAddressID")) %>%
  
  as_tibble() %>% 
  
  ## Replace out of data locationID numbers
  mutate(VenueName = case_when(StreetAddressID == 10853 ~ 'Gunnedah Region RO Office',
                               StreetAddressID == 10790 ~ 'Cobar Region RO Office',
                               
                               StreetAddressID == 10785 ~ 'Orange Region RO Office',
                               StreetAddressID == 10506 ~ 'Sutherland RO Office',
                               StreetAddressID == 10855 ~ 'Wollongong RO Office',
                               
                               TRUE ~ VenueName))


# View(Survey_data_context %>% filter(AREACODE == "Gunnedah"))


## Check the difference
length(setdiff(Survey_data$LoginID, Final_Respondents$LOGINID))
length(setdiff(Survey_data$LoginID, staff_details$LOGINID))
length(setdiff(Survey_data$LoginID, staff_demographics_wide$LOGINID))


## Take the distinct of the above join
# Survey_data_context_distinct <- Survey_data_context[
#   !duplicated(Survey_data_context), ]




# Calculate response metrics ----------------------------------------------



## Filter for complete survey responses
Survey_data_complete <- Survey_data_context %>%
  mutate(Status = ifelse(is.na(N4),'incomplete','complete')) %>%
  filter(!is.na(`Respondent ID`)) %>%
  filter(Status == 'complete')


## What is the survey response rate?
complete_responses  <- nrow(Survey_data %>%
                              mutate(Status = ifelse(is.na(N4),'incomplete','complete')) %>% 
                              mutate(Status = ifelse(is.na(N4),'incomplete','complete')) %>%
                              filter(!is.na(`Respondent ID`)) %>%
                              filter(Status == 'complete'))

all_staff           <- nrow(Survey_staff)
all_responses       <- nrow(Survey_data)
all_resp_rate       <- round((all_responses/all_staff)*100)
complete_resp_rate  <- round((complete_responses/all_staff)*100)


## Rename original role column from survey data
## Update role names for SG terminology
Survey_data_complete_LG_roles <- Survey_data_complete %>%
  
  rename(Role_survey = Role) %>% 
  
  mutate (Role = case_when(
    
    str_detect(POSITIONTYPECODE,  "DPPM")  ~ "DPPM",
    str_detect(POSITIONTYPECODE,  "PPM")   ~ "PPM",
    POSITIONTYPECODE ==           "SOAPP"  ~ "SOAPP",
    str_detect(POSITIONTYPECODE,  "SOA")   ~ "SOA",
    str_detect(POSITIONTYPECODE,  "DVIO")  ~ "EO",
    str_detect(POSITIONTYPECODE,  "EO")    ~ "EO",
    str_detect(POSITIONTYPECODE,  "EM")    ~ "EM",
    POSITIONTYPECODE ==           "CSPP"   ~ "EO",
    # POSITIONTYPECODE ==           "EM"     ~ "RO",
    TRUE ~ POSITIONTYPECODE)) %>%
  
  ## Replace the EMs with ROs
  # mutate(Role = gsub("EM", "RO", Role)) %>% 
  
  # Add RO from survey role question and clean up missing info from staffing
  mutate(Role = ifelse(Role_survey == 'RO', "RO", Role)
         ,DemographicsSource = ifelse(is.na(Role), "Not found in staffing", "Found in staffing")
         ,Role = ifelse(is.na(Role), Role_survey, Role)
         
         ,Role = case_when(
           str_detect(Role, "DVCM") ~ "DPPM" 
           ,str_detect(Role, "VCM") ~ "PPM"
           ,TRUE ~ Role)) %>% 
  
  mutate(Role = gsub("EM", "RO", Role)) 


## Counts of staff sent the survey by role

staff_counts_role <- Survey_staff %>%
  group_by(H1) %>%
  summarise(Count = n()) %>%
  rename(Role = H1)


Role_complete_responses <- Survey_data_complete_LG_roles %>%
  group_by(Role) %>%
  summarise(Count = n())




## Percent of complete survey responses by role
response_rate_role <- staff_counts_role %>%
  
  full_join(Role_complete_responses, by = c("Role")) %>%
  rename(Staff = Count.x
         ,Responses = Count.y) %>%
  mutate(Staff = ifelse(Role == "RO", 70, Staff)
         ,ResponseRate = round((Responses/Staff) * 100)) %>% arrange(-ResponseRate)


# Store table of reponses that do not have demographic data
## These people could be those without venues
missing_staff_demographics <- Survey_data_complete_LG_roles[Survey_data_complete_LG_roles$DemographicsSource == 
                                                              "Not found in staffing", 485:504]
missing_staff_login <- unique(missing_staff_demographics$LoginID)



# Generate list of roles for each question --------------------------------



role_list <- names(Survey_data_complete_LG_roles) %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    message("Creating role list for ", x)
    role  <- Survey_data_complete_LG_roles %>% filter(!is.na(!!sym(x)) & !is.na(Role)) %>%
      
      ## Remove the first two rows from the Role column
      .$Role %>% as.character %>% unique() 
    
  }) %>% c()


names(role_list) <- names(Survey_data_complete_LG_roles)
names(role_list) <- paste0(names(role_list), "_Roles")




## Create a list of labels
label_list        <- LUT$QuestionText 
names(label_list) <- LUT$QuestionNumber


## gsub from the list of labels
label_list <- gsub("\\s*\\([^\\)]+\\)","", label_list)





## Convert demography column names
Survey_data_complete_LG_roles_demography <- Survey_data_complete_LG_roles %>%
  
  ## Change the names for display
  rename(Languages  = LanguageCount,
         Disability = COM032_Disability,
         Indigenous = HasAboriginalTSIBackground,
         Age        = N2,
         Gender     = N1) %>%
  
  ## Change Values - NA means 'NO' for each.
  ## Any value for langue (1, 2 or 3) means they are multi-lingual
  mutate(Languages  = ifelse(is.na(Languages), 'English Only', 'Multilingual'),
         
         Indigenous = gsub('Y', 'Indigenous',     Indigenous),
         Indigenous = gsub('N', 'Non-Indigenous', Indigenous),
         Indigenous = replace_na(Indigenous, 'Non-Indigenous'),
         
         Disability = gsub('Y', 'Disability',    Disability),
         Disability = gsub('N', 'No Disability', Disability),
         Disability = replace_na(Disability, 'No Disability')) 


## Get the distinct Rows here - duplicates are emerging in the process.
Survey_data_complete_LG_roles_demography_distinct <- Survey_data_complete_LG_roles_demography[
  !duplicated(Survey_data_complete_LG_roles_demography), ]
sum(is.na(Survey_data_complete_LG_roles_demography_distinct$`Respondent ID`))


## Stop if the survey data is bigger than Respondents that completed the survey (i.e. a bad join)
stopifnot(nrow(Survey_data_complete_LG_roles_demography_distinct) == complete_responses)





#################################################### TBC ###########################################################