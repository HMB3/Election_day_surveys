################################### STAFF SURVEY RO COVARIANCE PLOTS ###################################


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
length(unique(SurveyData_numeric_questions$ReturningOffice))
length(unique(SurveyData_numeric_questions$ReturningOffice))


## Also consider how to best present the data ::
## Can we present the analyses for all Venues? 
## This would be like Jerry's staffing dashboard Report for each Venue, 
## effectively each has it's own dashboard





## 1). CONTEXT SCATTERPLOTS ==================================================================


## What are these scatterplots trying to tell us?


## The differences between SG19 code and LG21 are "Venue" satisfaction, and Venue CALD/remotentess


## Plot voting correlations : venue-level indexes plus ABS context data ----
returning_seifa <- dplyr::select(respondent_index_ro_average,
                                 Venues_over_proj, 
                                 Respondent_Satisfied,
                                 Respondent_Dissatisfied,
                                 Informality, 
                                 High_SEIFA_prop, 
                                 Low_SEIFA_prop, 
                                 Remoteness) %>%
  
  mutate(Informality      = as.numeric(Informality),
         Venues_over_proj   = as.numeric(Venues_over_proj),
         
         Respondent_Satisfied     = as.numeric(Respondent_Satisfied),
         Respondent_Dissatisfied  = as.numeric(Respondent_Dissatisfied)) %>% 
  
  rename(`Venue Work`     = Venues_over_proj,
         `Satisfied`      = Respondent_Satisfied,
         `UnSatisfied`    = Respondent_Dissatisfied,
         
         High_SES         = High_SEIFA_prop,
         Low_SES          = Low_SEIFA_prop,
         Informal         = Informality)


## What Venues/LGAs are missing ABS context data?
summary(returning_seifa)



## Plot voting correlations : venue-level WHS index plus ABS context data ----
returning_seifa_WHS <- dplyr::select(respondent_index_ro_average,
                                     WHS,
                                     Respondent_Satisfied,
                                     Respondent_Dissatisfied,
                                     Venues_over_proj, 
                                     Informality, 
                                     High_SEIFA_prop, 
                                     Low_SEIFA_prop, 
                                     Remoteness) %>%
  
  mutate(Informality        = as.numeric(Informality),
         Venues_over_proj   = as.numeric(Venues_over_proj),
         
         Respondent_Satisfied     = as.numeric(Respondent_Satisfied),
         Respondent_Dissatisfied  = as.numeric(Respondent_Dissatisfied)) %>% 
  
  rename(`Venue Work`     = Venues_over_proj,
         `Satisfied`      = Respondent_Satisfied,
         `UnSatisfied`    = Respondent_Dissatisfied,
         
         High_SES         = High_SEIFA_prop,
         Low_SES          = Low_SEIFA_prop,
         Informal         = Informality)


## Create table of venue indexes (just the index questions) and overall satisfaction (all questions)
returning_satisf_index <- dplyr::select(respondent_index_ro_average,
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
summary(returning_seifa_WHS);summary(returning_satisf_index)


## Create table of venue indexes (just the index questions) and overall dissatisfaction (all questions)
returning_unsatisf_index <- dplyr::select(respondent_index_ro_average,
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
returning_work_ses_remotess_plot <- 
  
  scatter_matrix_grouped_histo(scat_df     = returning_seifa, 
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


returning_work_ses_remotess_WHS_plot <- 
  
  scatter_matrix_grouped_histo(scat_df     = returning_seifa_WHS %>% dplyr::select(-Informal),
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


## Describe what each panel is indicating to us?
returning_work_ses_CALD_plot <- 
  
  scatter_matrix_grouped_histo(scat_df     = returning_seifa, 
                               cols        = 1:6, 
                               scat_col    = 'CALD',
                               alpha       = 0.5, 
                               alignPer    = 0.8,
                               upper_size  = 3.2, 
                               lower_size  = 1, 
                               axis_size   = 10,
                               labelSize   = 15,
                               title_size  = 15, 
                               leg_size    = 10, 
                               legend_pos  = 'bottom',
                               title       = 'LG21 Projected Venue workload vs. SES')


## Also plot relationship between Venue indexes, and Venue satisfaction
## These should be similar, but each contributes different information
returning_satisfaction_vs_indexes   <- psych::pairs.panels(returning_satisf_index,
                                                           method   = "pearson", # correlation method
                                                           hist.col = "#00AFBB",
                                                           density  = TRUE,      # show density plots
                                                           ellipses = FALSE,
                                                           cex = 4,
                                                           # cex.cor = 2,
                                                           cex.labels = 1.5,
                                                           lwd = 2,
                                                           col = "blue")


returning_dissatisfaction_vs_indexes <- pairs.panels(returning_unsatisf_index,
                                                     method   = "pearson", # correlation method
                                                     hist.col = "#00AFBB",
                                                     density  = TRUE,      # show density plots
                                                     ellipses = FALSE,
                                                     cex = 4,
                                                     # cex.cor = 2,
                                                     cex.labels = 1.5,
                                                     lwd = 2,
                                                     col = "blue")





## 2). INDEX VS. WORKLOAD SCATTERPLOTS ==================================================================


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
## people in ALL EMs? Or can we just exclude NA columns from every data frame?
## I.e. think about what happens to the NA values in the Index matrices...



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


## Create DF for venue-level respondents
returning_index_load_plot_df = dplyr::select(respondent_index_ro_average, 
                                             -ReturningOffice) %>%
  
  dplyr::select(index_plot_columns, 
                Venues_over_proj, 
                Remoteness) %>%
  .[colSums(!is.na(.)) > 0]


## Create DF for venue-level respondents
returning_index_context_plot_df = dplyr::select(respondent_index_ro_average,
                                                -ReturningOffice) %>%
  
  dplyr::select(index_plot_columns,
                Venues_over_proj,
                Remoteness,
                StaffingCategory,
                Indigenous_prop) %>%
  
  .[colSums(!is.na(.)) > 0]





## Subset the venue index df by Role :: this is easier to plot
returning_EO_index   <- respondent_index_ro_join %>% filter(Role == "EO") %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(ReturningOffice, index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(RO_workload_satisfaction, ., by = "ReturningOffice")


returning_OA_index   <- respondent_index_ro_join %>% filter(Role == "OA") %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(ReturningOffice, index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(RO_workload_satisfaction, ., by = "ReturningOffice")


returning_DPM_index  <- respondent_index_ro_join %>% filter(Role == "DPPM") %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(ReturningOffice, index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(RO_workload_satisfaction, ., by = "ReturningOffice")


returning_PPM_index  <- respondent_index_ro_join %>% filter(Role == "PPM") %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(ReturningOffice, index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(RO_workload_satisfaction, ., by = "ReturningOffice")


returning_SOA_index  <- respondent_index_ro_join %>% filter(Role == "SOA") %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(ReturningOffice, index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(RO_workload_satisfaction, ., by = "ReturningOffice")


returning_SOAP_index <- respondent_index_ro_join %>% filter(Role == "SOAPP") %>% 
  
  group_by(ReturningOffice) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(ReturningOffice, index_plot_columns) %>% 
  
  ## Join on the context venue columns
  right_join(RO_workload_satisfaction, ., by = "ReturningOffice")





## Create a list of dataframes that just show voting loads  
returning_plot_overall <- c("respondent_index_ro_average",
                            "returning_OA_index", 
                            "returning_EO_index", 
                            "returning_SOA_index", 
                            "returning_SOAP_index", 
                            "returning_PPM_index", 
                            "returning_DPM_index")




## Create a list for the scatterplots
returning_load_scatterplots <- returning_plot_overall %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    ## x = returning_plot_overall[1]
    message("Creating scatterplot dfs for ", x)
    df      <- get(x) 
    
    ## Create table
    scatter.df <- df %>% 
      
      dplyr::select(ReturningOffice, index_plot_columns, Venues_over_proj) %>%
      .[colSums(!is.na(.)) > 0] 
    
    ## Return the dataframe
    return(scatter.df)
    
  }) %>% c()





## Create a list of dataframes that show voting loads including context 
returning_context_scatterplots <- returning_plot_overall %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    ## x = returning_plot_overall[4]
    message("Creating scatterplot dfs ", x)
    df      <- get(x) 
    
    ## Create table
    scatter.df <- df %>% 
      
      dplyr::select(ReturningOffice, index_plot_columns, Venues_over_proj, Remoteness, 
                    StaffingCategory, CALD_prop, High_SEIFA_prop, Low_SEIFA_prop, Informality) %>%
      
      ## Do we want to do this? Will cut out incomplete data
      .[colSums(!is.na(.)) > 0] %>%
      completeFun(., "Remoteness")
    
    ## Return the dataframe
    return(scatter.df)
    
  }) %>% c()



## Rename and dump to the environment - not sure if we still want to do this...
names(returning_load_scatterplots)    <- c("returning_load_plot_df_numeric",
                                           "returning_OA_load_plot_df", 
                                           "returning_EO_load_plot_df", 
                                           "returning_SOA_load_plot_df", 
                                           "returning_SOAP_load_plot_df", 
                                           "returning_PPM_load_plot_df", 
                                           "returning_DPM_load_plot_df")


names(returning_context_scatterplots) <- c("returning_context_plot_df",
                                           "returning_OA_context_plot_df", 
                                           "returning_EO_context_plot_df", 
                                           "returning_SOA_context_plot_df", 
                                           "returning_SOAP_context_plot_df", 
                                           "returning_PPM_context_plot_df", 
                                           "returning_DPM_context_plot_df")


list2env(returning_load_scatterplots,    globalenv())
list2env(returning_context_scatterplots, globalenv())


## Then create a table for just the Indexes of each Topic
## These should be at the Venue and VenueName level


## Create a list of the dataframes with district scatterplots, excluding the repondent and venue level
returning_index_load_names <- mget(ls(name = .GlobalEnv, pattern = 'load_plot_df',    mode == "symbol")) %>% 
  names() %>% sort()
returning_context_names    <- mget(ls(name = .GlobalEnv, pattern = 'context_plot_df', mode == "symbol")) %>% 
  names() %>% sort() 





## 3). INDEX VS. WORKLOAD SCATTERPLOTS ==================================================================


## Create a list of Venue index scatter plots, without using a grouping variable (e.g. not using regional, etc.)
## Response Indexes vs. Venue Voting load ----  
scatterplot_returning_indexes_workload <- 
  
  scatter_keyvar_ungroup_list(scatplot_list      = names(returning_load_scatterplots), 
                              
                              scat_var           = "Venues_over_proj",
                              index_plot_columns = index_plot_columns,
                              x_label            = 'RO Workload (Votes / Projections)',
                              
                              axis_size   = 10, 
                              axis_title  = 25, 
                              point_col   = "blue",
                              point_size  = 3,
                              labelSize   = 25,
                              title_size  = 30, 
                              leg_size    = 20, 
                              legend_pos  = 'none',
                              
                              ## Names
                              scatplot_names = names(returning_load_scatterplots))


## Response Indexes vs. High SES ----
scatterplot_returning_indexes_context_HSES <- 
  
  scatter_keyvar_ungroup_list(scatplot_list  = names(returning_context_scatterplots), 
                              
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
                              scatplot_names = names(returning_context_scatterplots))


## Response Indexes vs. Low SES ----
scatterplot_returning_indexes_context_LSES <- 
  scatter_keyvar_ungroup_list(scatplot_list  = names(returning_context_scatterplots), 
                              
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
                              scatplot_names = names(returning_context_scatterplots))





## Response Indexes vs. % CALD ----
scatterplot_returning_indexes_context_CALD <- 
  
  scatter_keyvar_ungroup_list(scatplot_list  = names(returning_context_scatterplots), 
                              
                              scat_var    = "CALD_prop",
                              index_plot_columns = index_plot_columns,
                              x_label            = ' % CALD',
                              
                              axis_size   = 10, 
                              axis_title  = 25, 
                              point_col   = "blue",
                              point_size  = 3,
                              labelSize   = 25,
                              title_size  = 30, 
                              leg_size    = 20, 
                              legend_pos  = 'none',
                              
                              ## Names
                              scatplot_names = names(returning_context_scatterplots))





## Index vs. workload for EMs which were under-projected
response_under_projected_v_work <- 
  
  scatter_matrix_keyvar(scat_df <- returning_index_load_plot_df %>% select(-Remoteness) %>% 
                          
                          filter(Venues_over_proj < 3) %>% melt(., "Venues_over_proj") %>% 
                          filter(variable != 'ReturningOffice') %>% mutate(value = as.numeric(value)),
                        
                        ## This will chop off the last column
                        scat_var    = "Venues_over_proj",
                        axis_size   = 15,
                        axis_title  = 20,
                        point_col   = "blue",
                        point_size  = 1.5,
                        labelSize   = 20,
                        title_size  = 25,
                        leg_size    = 20,
                        legend_pos  = 'none',
                        ylab        = 'Index Value',
                        xlab        = 'No. Over-projected Venues',
                        title       = 'LG21 Response-level Indexes')


response_over_projected_v_work <-

  scatter_matrix_keyvar(scat_df <- returning_index_load_plot_df %>% select(-Remoteness) %>% 

                          filter(Venues_over_proj > 3) %>% melt(., "Venues_over_proj") %>%
                          filter(variable != 'ReturningOffice') %>% mutate(value = as.numeric(value)),

                        ## This will chop off the last column
                        scat_var    = "Venues_over_proj",
                        axis_size   = 15,
                        axis_title  = 20,
                        point_col   = "blue",
                        point_size  = 1.5,
                        labelSize   = 20,
                        title_size  = 25,
                        leg_size    = 20,
                        legend_pos  = 'none',
                        ylab        = 'Index Value',
                        xlab        = 'No. Over-projected Venues',
                        title       = 'LG21 Response-level Indexes')


## These plots are about exploring continuous relationships, but including categroical variables
## Just the index correlations among each other + Remoteness
returning_index_correlations_remote <- 
  
  scatter_matrix_grouped_histo(scat_df <- returning_context_plot_df %>% 
                                 dplyr::select(-Venues_over_proj, -ReturningOffice),
                               
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