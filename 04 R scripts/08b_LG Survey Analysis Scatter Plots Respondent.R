################################### STAFF SURVEY VENUE COVARIANCE PLOTS ###################################


## Purpose ----
## The purpose of this code is to take the aggregated indexes of survey Responses - Response/Venue/District - 
## and plot relationships between the indexes and other variables (e.g. Workload, Demographics, etc)


## To do ----
## Clean out all the plots that are not needed...
## Aggregate ABS categories into 3 - Metro, Regional, Remote
## Just spell out what they are in the code.


## Two kinds of relationships::
## Discrete relationships   (boxplots, etc.)
## Continuous relationships (scatterplots)


## Things to explore....
## Does workload affect index values?
## Does worload vary with demographics?
## Is there a geographic pattern in Responses (Satisfaction/Dissatisfaction)?


## Consider how best to aggregate the data. By Venue, Venue, business unit, etc?
## In SG19, Respondent level and Venue level are not balanced - Not every respondent answers every question,
## Not every Venue is staffed by all roles
## Not all staff in all Venues are asked all questions, etc.
## - Respondent (n = 13617)
## - Venue      (n = 2265)
## - Venue   (n = 93)
nrow(SurveyData_numeric_questions)
length(unique(SurveyData_numeric_questions$VenueName))
length(unique(SurveyData_numeric_questions$ReturningOffice))


## Also consider how to best present the data ::
## Can we present the analyses for all Venues? 
## This would be like Jerry's staffing dashboard Report for each Venue, 
## effectively each has it's own dashboard





## 1). CONTEXT SCATTERPLOTS ==================================================================


## What are these scatterplots trying to tell us?


## The differences between SG19 code and LG21 are "Venue" satisfaction, and Venue CALD/remotentess


## Plot voting correlations : venue-level indexes plus ABS context data ----
respondent_seifa <- dplyr::select(respondent_index_ro_join,
                                  Venue_Voting_Load, 
                                  Respondent_Satisfied,
                                  Respondent_Dissatisfied,
                                  High_SEIFA_prop, 
                                  Low_SEIFA_prop, 
                                  Remoteness) %>%
  
  mutate(Venue_Voting_Load       = as.numeric(Venue_Voting_Load),
         Respondent_Satisfied    = as.numeric(Respondent_Satisfied),
         Respondent_Dissatisfied = as.numeric(Respondent_Dissatisfied),
         High_SEIFA_prop         = as.numeric(High_SEIFA_prop),
         Low_SEIFA_prop          = as.numeric(Low_SEIFA_prop)) %>% 
  
  rename(`Venue Work`  = Venue_Voting_Load,
         `Satisfied`   = Respondent_Satisfied,
         `UnSatisfied` = Respondent_Dissatisfied,
         
         High_SES        = High_SEIFA_prop,
         Low_SES         = Low_SEIFA_prop)


## What Venues/LGAs are missing ABS context data?
summary(respondent_seifa)



## Plot voting correlations : venue-level WHS index plus ABS context data ----
respondent_seifa_Recruit <- dplyr::select(respondent_index_ro_join,
                                          Recruitment,
                                          Venue_Voting_Load, 
                                          Respondent_Satisfied,
                                          Respondent_Dissatisfied,
                                          High_SEIFA_prop, 
                                          Low_SEIFA_prop, 
                                          Remoteness) %>%
  
  mutate(Venue_Voting_Load       = as.numeric(Venue_Voting_Load),
         Respondent_Satisfied    = as.numeric(Respondent_Satisfied),
         Respondent_Dissatisfied = as.numeric(Respondent_Dissatisfied),
         High_SEIFA_prop         = as.numeric(High_SEIFA_prop),
         Low_SEIFA_prop          = as.numeric(Low_SEIFA_prop)) %>% 
  
  rename(`Venue Work`  = Venue_Voting_Load,
         `Satisfied`   = Respondent_Satisfied,
         `UnSatisfied` = Respondent_Dissatisfied,
         
         High_SES        = High_SEIFA_prop,
         Low_SES         = Low_SEIFA_prop)


## Create table of venue indexes (just the index questions) and overall satisfaction (all questions)
respondent_satisf_index <- dplyr::select(respondent_index_ro_join,
                                         Respondent_Satisfied,
                                         Counting,
                                         IS,
                                         Logistics,
                                         Operations,
                                         Recruitment,
                                         Staffing,               
                                         Training,        
                                         Logistics,             
                                         Training,            
                                         Venues,               
                                         WHS) 


## What Venues/LGAs are missing ABS context data?
summary(respondent_seifa_Recruit);summary(respondent_satisf_index)


## Create table of venue indexes (just the index questions) and overall dissatisfaction (all questions)
respondent_unsatisf_index <- dplyr::select(respondent_index_ro_join,
                                           Respondent_Dissatisfied,
                                           Counting,
                                           IS,
                                           Logistics,
                                           Operations,
                                           Recruitment,
                                           Staffing,               
                                           Training,        
                                           Logistics,             
                                           Training,            
                                           Venues,               
                                           WHS) 


## Describe what each panel is indication to us?
respondent_work_ses_remotess_plot <- scatter_matrix_grouped_histo(scat_df     = respondent_seifa, 
                                                                  cols        = 1:5, 
                                                                  scat_col    = 'Remoteness',
                                                                  alpha       = 0.5, 
                                                                  alignPer    = 0.8,
                                                                  upper_size  = 2, 
                                                                  lower_size  = 1, 
                                                                  axis_size   = 10,
                                                                  labelSize   = 15,
                                                                  title_size  = 15, 
                                                                  leg_size    = 10, 
                                                                  legend_pos  = 'bottom',
                                                                  title       = 'LG21 Projected Venue workload vs. SES')


venue_work_ses_remotess_recruitment_plot <- scatter_matrix_grouped_histo(scat_df     = respondent_seifa_Recruit,
                                                                         cols        = 1:6,
                                                                         scat_col    = 'Remoteness',
                                                                         alpha       = 0.5, 
                                                                         alignPer    = 0.8,
                                                                         upper_size  = 2, 
                                                                         lower_size  = 1, 
                                                                         axis_size   = 10,
                                                                         labelSize   = 15,
                                                                         title_size  = 15, 
                                                                         leg_size    = 10, 
                                                                         legend_pos  = 'bottom',
                                                                         title       = 'LG21 Projected Venue workload vs. SES')



## Also plot relationship between Venue indexes, and Venue satisfaction
## These should be similar, but contribute different information
respond_satisfaction_vs_indexes   <- psych::pairs.panels(respondent_satisf_index,
                                                         method   = "pearson", # correlation method
                                                         hist.col = "#00AFBB",
                                                         density  = TRUE,      # show density plots
                                                         ellipses = FALSE,
                                                         cex = 4,
                                                         # cex.cor = 2,
                                                         cex.labels = 1.5,
                                                         lwd = 2,
                                                         col = "blue")


respond_dissatisfaction_vs_indexes <-pairs.panels(respondent_unsatisf_index,
                                                  method   = "pearson", # correlation method
                                                  hist.col = "#00AFBB",
                                                  density  = TRUE,      # show density plots
                                                  ellipses = FALSE,
                                                  cex = 4,
                                                  # cex.cor = 2,
                                                  cex.labels = 1.5,
                                                  lwd = 2,
                                                  col = "blue")







## 2). CREATE SCATTER DATAFRAMES ==================================================================


## We need different dataframes for each purpose.
## So we need different sets of data frames with different columns for different views.


## Create a dataframe for each data 'slice' or 'view'


## Create a list of the business units, that will be used to aggregate the survey respondent indexes
## Not all plots wil have all these units....this is ok for the straight plots, but not for the
## Demographic ones.
index_plot_columns <- c('Counting',
                        'IS',
                        'Logistics',
                        'Operations',
                        'Recruitment',
                        'Staffing',               
                        'Training',        
                        'Logistics',             
                        'Training',            
                        'Venues',               
                        'WHS')


## Create dataframe for all responses
respondent_index_load_plot_df = dplyr::select(respondent_index_ro_join, 
                                              -ReturningOffice, 
                                              -Total_Votes, 
                                              -Low_SEIFA_prop,
                                              -High_SEIFA_prop) %>%
  
  ## Select columns
  ## Remove the columns that contain ONLY NA values
  dplyr::select(index_plot_columns, Venue_Voting_Load) %>%
  .[colSums(!is.na(.)) > 0]


## Create dataframe for all respondents
respondent_index_context_plot_df = dplyr::select(respondent_index_ro_join, 
                                                 -ReturningOffice, 
                                                 -Total_Votes, 
                                                 -Low_SEIFA_prop,
                                                 -High_SEIFA_prop) %>%
  
  ## Select columns
  ## Remove the columns that contain ONLY NA values
  dplyr::select(index_plot_columns, Venue_Voting_Load, Remoteness, StaffingCategory) %>%
  .[colSums(!is.na(.)) > 0]


## Create DF for venue-level respondents
# venue_index_load_plot_df = dplyr::select(respondent_index_ro_join, 
#                                          -VenueName) %>%
#   
#   dplyr::select(index_plot_columns, 
#                 Venue_Voting_Load, 
#                 Remoteness
#   ) %>%
#   .[colSums(!is.na(.)) > 0]


## Create DF for venue-level respondents
# venue_index_context_plot_df = dplyr::select(venue_index,
#                                             -VenueName) %>%
#   
#   dplyr::select(index_plot_columns,
#                 Venue_Voting_Load,
#                 Remoteness,
#                 StaffingCategory,
#                 Indigenous_prop) %>%
#   
#   .[colSums(!is.na(.)) > 0]


## Subset the respondent index df by Role :: this is easier to plot
respondent_EO_index   <- respondent_index_ro_join %>% filter(Role == "EO")
respondent_OA_index   <- respondent_index_ro_join %>% filter(Role == "OA")
respondent_DPM_index  <- respondent_index_ro_join %>% filter(Role == "DPPM")
respondent_PPM_index  <- respondent_index_ro_join %>% filter(Role == "PPM")
respondent_SOA_index  <- respondent_index_ro_join %>% filter(Role == "SOA")
respondent_SOAP_index <- respondent_index_ro_join %>% filter(Role == "SOAPP")


## Create a list of dataframes that just show voting loads  
respondent_plot_overall  <- c("respondent_index_ro_join",
                              "respondent_OA_index", 
                              "respondent_EO_index", 
                              "respondent_SOA_index", 
                              "respondent_SOAP_index", 
                              "respondent_PPM_index", 
                              "respondent_DPM_index")


## Create a list for the scatterplots
respondent_load_scatterplots <- respondent_plot_overall %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    ## x = venue_plot_overall[1]
    message("Creating scatterplot dfs for ", x)
    df      <- get(x) 
    
    ## Create table
    scatter.df <- df %>% 
      
      dplyr::select(VenueName, index_plot_columns, Venue_Voting_Load) %>%
      .[colSums(!is.na(.)) > 0] 
    
    ## Return the dataframe
    return(scatter.df)
    
  }) %>% c()





## Create a list of dataframes that show voting loads including context 
respondent_context_scatterplots <- respondent_plot_overall %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    ## x = venue_plot_overall[4]
    message("Creating scatterplot dfs ", x)
    df      <- get(x) 
    
    ## Create table
    scatter.df <- df %>% 
      
      dplyr::select(VenueName, index_plot_columns, Venue_Voting_Load, Remoteness, 
                    StaffingCategory, High_SEIFA_prop, Low_SEIFA_prop) %>%
      
      ## Do we want to do this? Will cut out incomplete data
      .[colSums(!is.na(.)) > 0] %>%
      completeFun(., "Remoteness")
    
    ## Return the dataframe
    return(scatter.df)
    
  }) %>% c()



## Rename and dump to the environment - not sure if we still want to do this...
names(respondent_load_scatterplots)    <- c("respondent_load_plot_df_numeric",
                                            "respondent_OA_load_plot_df", 
                                            "respondent_EO_load_plot_df", 
                                            "respondent_SOA_load_plot_df", 
                                            "respondent_SOAP_load_plot_df", 
                                            "respondent_PPM_load_plot_df", 
                                            "respondent_DPM_load_plot_df")


names(respondent_context_scatterplots) <- c("respondent_context_plot_df",
                                            "respondent_OA_context_plot_df", 
                                            "respondent_EO_context_plot_df", 
                                            "respondent_SOA_context_plot_df", 
                                            "respondent_SOAP_context_plot_df", 
                                            "respondent_PPM_context_plot_df", 
                                            "respondent_DPM_context_plot_df")


list2env(respondent_load_scatterplots,    globalenv())
list2env(respondent_context_scatterplots, globalenv())


## Then create a table for just the Indexes of each Topic
## These should be at the Venue and VenueName level


## Create a list of the dataframes with district scatterplots, excluding the repondent and venue level
respondent_index_load_names <- mget(ls(name = .GlobalEnv, pattern = 'load_plot_df',    mode == "symbol")) %>% 
  names() %>% sort()
respondent_context_names    <- mget(ls(name = .GlobalEnv, pattern = 'context_plot_df', mode == "symbol")) %>% 
  names() %>% sort() 






## 3). INDEX VS. WORKLOAD SCATTERPLOTS ==================================================================


## The purpose of this code is to examine how the indexes to other variables that might 
## drive satisfaction, to explore what might explain satisfaction before using stats 
## to formally analyse any relationships


## How are the indexes related to workload across Venues? ----
## Workload for each Venue, and aggregated up to Venue : as per early Voting


# Although some indices are correlated with each other, 
# (e.g. the voting and Traininging indices, the recruting and staffing indices),
# none of the indices are related to the workload for each Venue?


## As Tom notes, only a small number of people work at each Venue, so might not get much 
## Data/insight out of that analsysis.


## Consier the aggregation :: if we want to analyse at the Venue level, because this level
## should drive outcomes, do we need to pick questions for the index which are answered by
## people in ALL venues? Or can we just exclude NA columns from every data frame?
## I.e. think about what happens to the NA values in the Index matrices...


## Create a list of Venue index scatter plots, without using a grouping variable (e.g. not using regional, etc.)
## Response Indexes vs. Venue Voting load ----  
scatterplot_respondent_indexes_workload <- 
  
  scatter_keyvar_ungroup_list(scatplot_list      = names(respondent_load_scatterplots)[-1], 
                              
                              scat_var           = "Venue_Voting_Load",
                              index_plot_columns = index_plot_columns,
                              x_label            = 'Venue Workload (Votes / Projections)',
                              
                              axis_size   = 10, 
                              axis_title  = 25, 
                              point_col   = "blue",
                              point_size  = 3,
                              labelSize   = 25,
                              title_size  = 30, 
                              leg_size    = 20, 
                              legend_pos  = 'none',
                              
                              ## Names
                              scatplot_names = names(respondent_load_scatterplots)[-1])





## Response Indexes vs. High SES ----
scatterplot_respondent_indexes_context_HSES <- 
  
  scatter_keyvar_ungroup_list(scatplot_list  = names(respondent_context_scatterplots), 
                              
                              scat_var    = "High_SEIFA_prop",
                              index_plot_columns = index_plot_columns,
                              x_label            = ' High SES (no. SA1)',
                              
                              axis_size   = 10, 
                              axis_title  = 25, 
                              point_col   = "blue",
                              point_size  = 3,
                              labelSize   = 25,
                              title_size  = 30, 
                              leg_size    = 20, 
                              legend_pos  = 'none',
                              
                              ## Names
                              scatplot_names = names(respondent_context_scatterplots))





## Response Indexes vs. Low SES ----
scatterplot_respondent_indexes_context_LSES <- 
  
  scatter_keyvar_ungroup_list(scatplot_list  = names(respondent_context_scatterplots), 
                              
                              scat_var    = "Low_SEIFA_prop",
                              index_plot_columns = index_plot_columns,
                              x_label            = ' Low SES (no. SA1)',
                              
                              axis_size   = 10, 
                              axis_title  = 25, 
                              point_col   = "blue",
                              point_size  = 3,
                              labelSize   = 25,
                              title_size  = 30, 
                              leg_size    = 20, 
                              legend_pos  = 'none',
                              
                              ## Names
                              scatplot_names = names(respondent_context_scatterplots))




## Index vs. workload for under and over-projected venues ----
response_under_projected_v_work <- 
  
  scatter_matrix_keyvar(scat_df <- respondent_index_load_plot_df %>%
                          
                          filter(Venue_Voting_Load < 1) %>% melt(., "Venue_Voting_Load"),
                        
                        ## This will chop off the last column
                        scat_var    = "Venue_Voting_Load",
                        axis_size   = 15,
                        axis_title  = 20,
                        point_col   = "blue",
                        point_size  = 1.5,
                        labelSize   = 20,
                        title_size  = 25,
                        leg_size    = 20,
                        legend_pos  = 'none',
                        ylab        = 'Index Value',
                        xlab        = 'Venue Workload (Votes/Projections)',
                        title       = 'LG21 Response-level Indexes')


response_over_projected_v_work <-
  
  scatter_matrix_keyvar(scat_df <- respondent_index_load_plot_df %>%
                          
                          filter(Venue_Voting_Load > 1) %>% melt(., "Venue_Voting_Load"),
                        
                        ## This will chop off the last column
                        scat_var    = "Venue_Voting_Load",
                        axis_size   = 15,
                        axis_title  = 20,
                        point_col   = "blue",
                        point_size  = 1.5,
                        labelSize   = 20,
                        title_size  = 25,
                        leg_size    = 20,
                        legend_pos  = 'none',
                        ylab        = 'Index Value',
                        xlab        = 'Venue Workload (Votes/Projections)',
                        title       = 'LG21 Response-level Indexes')




## Venue-level Index correlations
respondent_index_correlations <- 
  
  scatter_matrix_grouped_histo(scat_df <- respondent_index_load_plot_df %>%
                                 filter(Venue_Voting_Load < 1) %>%
                                 dplyr::select(-Venue_Voting_Load),
                               
                               ## This will chop off the last column
                               cols        = 1:(ncol(scat_df)-1),
                               scat_col    = 'Remoteness',
                               
                               alpha       = 0.5,
                               alignPer    = 0.8,
                               upper_size  = 8,
                               lower_size  = 3,
                               axis_size   = 18,
                               labelSize   = 25,
                               title_size  = 30,
                               leg_size    = 30,
                               legend_pos  = 'bottom',
                               title       = '')





## 4). INDEX VS. INDEX SCATTERPLOTS ==================================================================


## These plots are about exploring continuous relationships, but including categroical variables
## Just the index correlations among each other + Remoteness
respondent_index_correlations_remote <- 
  
  scatter_matrix_grouped_histo(scat_df <- respondent_context_plot_df %>% 
                                 dplyr::select(-Venue_Voting_Load, -VenueName),
                               
                               ## Chop off the columns we don't want to see
                               cols        = 1:(ncol(scat_df)-6),
                               scat_col    = 'Remoteness',
                               
                               alpha       = 0.5, 
                               alignPer    = 0.8,
                               upper_size  = 5, 
                               lower_size  = 2.5, 
                               axis_size   = 20, 
                               labelSize   = 25,
                               title_size  = 30, 
                               leg_size    = 30, 
                               legend_pos  = 'bottom',
                               title       = '')





#################################################### TBC ###########################################################