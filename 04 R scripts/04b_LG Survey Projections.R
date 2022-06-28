################################# QUANTIFY STAFF WORKLOAD ##############################################


## Aim ----
## The purpose of this code is quantify staff workload, using the difference between Projections and turnout,


## For SG19, the hypothesised predictor of elector satisfaction repsonses was workload. The proxy for workload was 
## the difference in Projections vs turnout. However, this is only one measure of workload....





## 1). IMPORT PROJECTION DATA ==============================================================================



## RE staffing level of each venue Bron Says ----
## There's positions in EMS, ESA also has who was employed and then attendance timesheets says who actually turned up
## You just need to be aware not all positions are issuing positions. So to calculate who's issuing votes, you really 
## need to look at staff in EMS


## Import basedata
# Basedata_councils       <- read_csv(paste0(base_data_working_folder, '/Base_data_councils.csv'))
# Basedata_wards          <- read_csv(paste0(base_data_working_folder, '/Base_data_wards.csv'))
# Basedata_venues_council <- read_csv(paste0(base_data_working_folder, '/Base_data_venues_by_council.csv'))
# Basedata_unique_venues  <- read_csv(paste0(base_data_working_folder, '/Base_data_unique_venues.csv'))


## Get the key base data LGA columns
LGA_columns_areas <- Basedata_councils %>% 
  
  select(LGAreaCode, ReturningOffice, StaffingCategory) %>% 
  distinct() %>% na.omit()


## Get the key base data Venue columns
LGA_column_venues <- Basedata_unique_venues %>% 
  
  select(ReturningOffice, VenueName) %>% 
  left_join(LGA_columns_areas) %>% dplyr::select(ReturningOffice, LGAreaCode, VenueName)


## Get the key EMS Location columns
venue_location_columns <- venue_locations %>% 
  
  select(VenueName, Latitude, Longitude) %>% 
  distinct()


venue_location_LGA_columns <- venue_locations %>% 
  
  select(VenueName, Latitude, Longitude) %>% 
  left_join(select(LGA_column_venues, ReturningOffice, VenueName)) %>% distinct()


RO_venues_distinct <- LGA_column_venues %>% select(ReturningOffice, VenueName) %>% distinct()


## Get the key base data ward columns
ward_columns <- Basedata_wards %>% 
  
  select(ElectoralAreaName, 
         LGAreaCode,             
         WardAreaCode, 
         WardName,
         ReturningOffice) %>% 
  distinct() %>% na.omit()





## 2). MANIPULATE PROJECTION DATA ========================================================================


## This section will need to change quite a bit - where will the data come from?
## The 2019 data only matches to the RO level. LG21 data needs to match to the Venue level, just to be thorough.
## The data wrangling below is a workaround to try and get the 2019 post election data to server this purpose,
## quantifying workload. Since we can only tie 2019 survey responses to the Disctict, we can't really do this.... 



## Create table of votes at each Venue
Councillor_formality_contest_venue_LGA <- ward_columns %>% left_join(Councillor_formality_contest_venue,
                                                                     by = "WardAreaCode") %>%
  filter(VenueName != "NAMAV" &
           VenueName != "Postal" &
           VenueName != "iVote"  &
           VenueName != "Enrolment") %>% 
  dplyr::select(-"FORMALITY_RATE_(%)", -"INFORMAL_VOTES") %>%
  
  rename(Total_Votes = TOTAL_VOTES,
         Informality = "INFORMALITY_RATE_(%)") %>% 
  
  ## Sumarise to LGA level
  group_by(ElectoralAreaName, LGAreaCode, VenueName) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  mutate(Informality = (1- (FORMAL_VOTES/Total_Votes))*100)


## Projections can just use base data, for both Venue and RO level. 
## Start with Venue and aggregated up to the Venue
## level, then work from there.
venue_projections <- Basedata_venues_council %>% 
  
  ## Remove the cols we don't need
  dplyr::select(ReturningOffice, LGAreaCode, LocationTypeCode, VenueName, Ordinary_Projected) %>% 
  filter(LocationTypeCode != "Declared Institution")


## These LGAs don't match up between the survey Venues, and the ABS - check they make sense
length(intersect(venue_projections$LGAreaCode, LGA_CALD_ABS_CONTEXT$LGA))
setdiff(venue_projections$LGAreaCode, LGA_CALD_ABS_CONTEXT$LGA)
setdiff(LGA_CALD_ABS_CONTEXT$LGA,   venue_projections$LGAreaCode)


## Join LGAreaCode to venue_locations
venue_locations_LGA <- left_join(venue_locations, select(Basedata_councils, EventID, LGAreaCode)) 


## Total votes will need to be aggregated up to the council level
## Venues overall load, apply to responses.


## Join the Venue Votes and the projections
venue_votes_projections <- left_join(venue_projections, 
                                     Councillor_formality_contest_venue_LGA,
                                     
                                     by = c("LGAreaCode", "VenueName")) %>%
  
  ## Removing EMs that didn't take votes, and Venues that didn't have councillor votes
  # filter(Ordinary_Projected > 0) %>% 
  filter(!is.na(Total_Votes)) %>% 
  
  ## Join on the lat/longs
  left_join(dplyr::select(venue_locations_LGA, LGAreaCode, VenueName, Latitude, Longitude, LocalityName, PostCode), 
            
            by = c("VenueName", "LGAreaCode")) %>% 
  
  ## The NA rows here are real, Venue's without Vote turnout?
  dplyr::select("ReturningOffice",      
                "LGAreaCode",           
                "LocationTypeCode",
                "VenueName",
                "Latitude",             
                "Longitude",            
                "LocalityName",         
                "PostCode",
                "Ordinary_Projected",      
                "Total_Votes",          
                "Informality")



## Check the venue intersects - are all the EMs there?
ROs_in_survey <- length(intersect(unique(Basedata_councils %>% 
                                           filter(ElectionStatus == "Election" & 
                                                    !is.na(ReturningOffice)) %>% 
                                           .$ReturningOffice), 
                                  unique(Survey_data_complete_LG_roles_demography_distinct$VenueName)))

## Stop if
stopifnot(nrow(ROs_in_survey) == number_of_ROs)


##
Missing_ROs <- setdiff(unique(Basedata_councils %>% 
                                filter(ElectionStatus == "Election") 
                              %>% .$ReturningOffice), 
                       unique(Survey_data_complete_LG_roles_demography_distinct$VenueName))


## Stop if
stopifnot(nrow(Missing_ROs) == 0)


## Check the Staff with no venues all have an explanation
missing_venues_not_ROs <- Survey_data_complete_LG_roles_demography_distinct %>% 
  filter(is.na(VenueName)) %>% 
  filter(!(LoginID %in% missing_staff_demographics$LoginID)) %>% 
  filter(Role != "RO")


## Stop if 
stopifnot(nrow(missing_venues_not_ROs) == 0)


## Some people just didn't fill
missing_venues <- setdiff(unique(venue_votes_projections$VenueName),
                          unique(Survey_data_complete_LG_roles_demography_distinct$VenueName))


## This relationship is wrong - the Venue votes need to aggregated up to the council
## Currently, venues are not getting assifgned properly
plot(venue_votes_projections$Ordinary_Projected,
     venue_votes_projections$Total_Votes, col = 'red')





## 3). CALCULATE VOTING WORKLOADS ================================================================================


## The code below combines voter turnout with projections, to try and calculate workload at the Venue level

## The ABS SAI CALD data is missing Dubbo, Cootamundar Gundagai and Nambucca 

## Projected workload for each Venue, within the RO
venue_workload <- venue_votes_projections %>% left_join(LGA_CALD_ABS_CONTEXT, 
                                                        by = c("LGAreaCode" = "LGA")) %>%
  
  left_join(LGA_columns_areas, by = c("ReturningOffice", "LGAreaCode")) %>%
  
  ## Create measures of the Venue and RO level workload
  ## This should be re-done as % overprojected
  mutate(Venue_Voting_Load = ifelse(`Total_Votes` > 0, 
                                    round((Total_Votes / Ordinary_Projected), digits = 2), 0) %>% 
           formattable::percent(.)) %>% 
  
  ## Get the required columns
  dplyr::select(ReturningOffice,      
                LGAreaCode, 
                StaffingCategory,
                names(LGA_CALD_ABS_CONTEXT)[-1][-4],
                LocationTypeCode,
                VenueName, 
                Latitude, 
                Longitude, 
                Total_Votes, 
                Ordinary_Projected, 
                Informality,     
                Venue_Voting_Load) 


## Logical test of basedata numbers....the Venue workload table has the same unique venues as basedata?
## Different venues are hopefully Murray River, etc. Filter out Inelligible venues from base data
identical(length(unique(venue_workload$VenueName)), nrow(Basedata_unique_venues))
setdiff(Basedata_unique_venues$VenueName, unique(venue_workload$VenueName))


## Get the RO classifications - CALD is from staffing, but we can quantify from the ABS too.
## Some of these are NA, when they really shouldn't be.
SED_categories <- venue_workload %>% 
  dplyr::select(ReturningOffice, StaffingCategory, Remoteness, CALD) %>% 
  distinct()


## For the RO workload, the different variables needs to be aggregated up separately


## Calculate mean formality for each RO.
## Formality is incomplete, potentially because of join problems
RO_formality <- venue_votes_projections %>% 
  
  ## Just group by RO and summarise the categorial variables
  group_by(ReturningOffice)   %>%
  summarise(Informality = mean(Informality))


## Sum the votes for each RO
RO_Votes <- venue_votes_projections %>% 
  
  ## Just group by RO and summarise the categorial variables
  group_by(ReturningOffice) %>%
  summarise_at(c("Ordinary_Projected", "Total_Votes"), sum, na.rm = TRUE)



## For each RO, how many venues where over-projected?
## Which EMs had to deal with more venues that were over-projected?
RO_over_projected <- venue_workload %>% 
  
  ## Just group by RO and summarise the categorial variables
  group_by(ReturningOffice) %>%
  summarise(across(any_of(c("Ordinary_Projected", "Total_Votes")), sum, na.rm = TRUE),
            Venues_over_proj = length(Venue_Voting_Load[Venue_Voting_Load > 1])) 


## Projected workload for each RO.
## This has columns that can we aggregate (e.g. votes and projections), and columns we can't aggregate (lat/lot, etc.)

## RO workload is not the same
## How many venues are over-projected.
## EMs not compareable to Venues...

##
RO_workload <- RO_over_projected %>% 
  
  ## Just group by RO and summarise the categorial variables
  left_join(RO_formality, by = "ReturningOffice") %>%
  
  ## Join context data on after calculating for the RO level
  left_join(LGA_columns_areas, by = c("ReturningOffice")) %>%
  distinct(ReturningOffice, .keep_all = TRUE) %>% 
  left_join(LGA_CALD_ABS_CONTEXT, 
            by = c("LGAreaCode" = "LGA")) %>%
  
  ## Get the required columns
  dplyr::select(ReturningOffice,      
                LGAreaCode, 
                StaffingCategory,
                names(LGA_CALD_ABS_CONTEXT)[-1][-2][-3],
                Total_Votes, 
                Ordinary_Projected, 
                Informality,     
                Venues_over_proj) 


## So they always over-project at the RO level, but this masks venue-level under-projections...
## Negative values are under-projections, positive values are over projections
summary(RO_workload$Total_Votes)
summary(RO_workload$Venues_over_proj)


## Data summaries like the ones below are similar to the PED ----


## Join venue-level to respondent level data
## Looks like all the EMs had more voters than projected?  
RO_mean <- RO_workload %>%
  group_by(Remoteness) %>%
  dplyr::summarize(Mean_load = mean(Venues_over_proj, na.rm = TRUE))

venue_mean <- venue_workload %>%
  group_by(Remoteness) %>%
  dplyr::summarize(Mean_load = mean(Venue_Voting_Load, na.rm = TRUE))


## Create a histogram of the distribution of venue voting workloads
venue.votes.plot <- ggplot(venue_workload, aes(x    = Venue_Voting_Load, 
                                               fill = Remoteness, color = Remoteness)) +
  
  ## 
  geom_histogram(position = "identity",  alpha = 0.3) +
  geom_vline(data         = venue_mean, aes(xintercept = Mean_load, color = Remoteness),
             linetype     = "dashed") + theme(legend.position = "bottom")


## Create a histogram of the distribution of RO workloads 
dist.votes.plot <- ggplot(RO_workload, aes(x    = Venues_over_proj, 
                                           fill = Remoteness, color = Remoteness)) +
  
  geom_histogram(position = "identity", alpha = 0.3) +
  geom_vline(data         = RO_mean, aes(xintercept = Mean_load, color = Remoteness),
             linetype     = "dashed") + ggtitle(paste0('')) + 
  theme(axis.title.y      = element_blank())

venue.votes.plot;dist.votes.plot




#################################################### TBC ###########################################################