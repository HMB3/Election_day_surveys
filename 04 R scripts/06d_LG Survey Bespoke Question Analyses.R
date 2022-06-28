#################################### ---- CREATE BEPSOKE PLOTS ---- ################################################



## Aim : This code creates additional break downs of individual survery questions for particular topics, 
## That were requested by individual SMEs.


## First create a count off staff in each Role - Dylan is interested in this
Survey_complete_role_counts <- table(Survey_data_complete_LG_roles$Role)




## 1). STAFFING BESPOKE QUESTIONS ==========================================================================


## Dylan and Tanya would like to know for EOs :


# Of the election officials who responded to the survey:
#   
# . Q1  :XX per cent stated the NSW Electoral Commission conducted the election fairly and impartially Q1
# 
# . Q8  :XX per cent reported they were overall satisfied with the recruitment process run by the EC
# 
# . Q20 :XX per cent stated they were satisfied with the training they received.



## EO Role analysis
EO_count             <- table(Survey_data_complete_LG_roles$Role)[['EO']]
EO_role_list         <- rep('EO', length(role_list))
EO_count_list        <- rep(EO_count, length(role_list))

names(EO_role_list)  <- names(role_list)
names(EO_count_list) <- names(base_size_list)


plots_yesno_EO <- yesno_plots(plot_list        = yesno_barchart_questions,
                              survey_data      = Survey_data_complete_LG_roles %>% filter(Role == 'EO'),
                              survey_questions = Survey_questions,
                              role_list        = EO_role_list,
                              
                              count_list       = BaseCount %>% 
                                mutate(BaseSize = EO_count),
                              
                              calc_percent     = TRUE,  
                              
                              plot_levels  = c("Uncertain",
                                               "No",
                                               "Yes"),
                              
                              plot_colours = c('No'         = 'Coral2', 
                                               'Yes'        = 'green4',
                                               "Uncertain"  = 'dodgerblue1'),
                              
                              ## Set the plot parameters
                              mar       = 1,
                              tsize     = 35,
                              capt_size = 25,
                              xsize     = 25,
                              ysize     = 25,
                              ycol      = 'black',
                              lab_size  = 8,
                              ylab      = '\nPercent (%)\n',
                              xlab      = '')


## What are the numbers?
plots_yesno_EO[["A1_graph"]]
plots_yesno_EO[["N4_graph"]]




## 
ord5_plots_singlegrid_EO <- 
  
  zero_centred_plots_single_gridchoice(plot_list        = ord5_singlegrid, 
                                       survey_data      = Survey_data_complete_LG_roles %>% filter(Role == 'EO'),
                                       survey_questions = Survey_questions,
                                       role_list        = EO_role_list,
                                       count_list       = EO_count_list,
                                       
                                       neutral          = 'Neutral',
                                       negative         = 'Dissatisfied|Very dissatisfied',
                                       graph_scale      =  c('Very satisfied',
                                                             'Satisfied',
                                                             'Neutral',
                                                             'Dissatisfied',
                                                             'Very dissatisfied'),
                                       
                                       scale_cols       = c('Neutral'           = 'grey',
                                                            'Satisfied'         = 'lightblue',
                                                            'Very satisfied'    = 'skyblue3',
                                                            'Dissatisfied'      = 'darkorange1',
                                                            'Very dissatisfied' = 'brown1'),
                                       
                                       leg_order        = c('Very dissatisfied',
                                                            'Dissatisfied',
                                                            'Neutral',
                                                            'Satisfied',
                                                            'Very satisfied'),
                                       
                                       tsize      = 40,
                                       lab_size   = 12,
                                       leg_size   = 30,
                                       ysize      = 30, 
                                       xsize      = 30,
                                       capt_size  = 30,
                                       width      = 0.5, 
                                       ymin       = -0.5,
                                       ymax       = 1.1,
                                       high_just  = -0.5,
                                       low_just   = 1.5)




## 
ord5_plots_singlegrid_EO[['B1_graph']]
ord5_plots_singlegrid_EO[['C1_graph']]





## Dylan and Tanya would like to know for polling place managers :


# Of the polling place managers and deputy polling place managers who responded to the survey:
#   
# . Q1  :XX per cent stated the NSW Electoral Commission conducted the election fairly and impartially Q1
# 
# . Q8  :XX per cent reported they were overall satisfied with the recruitment process run by the EC
# 
# . Q20 :XX per cent stated they were satisfied with the training they received.

# . Q2 XX per cent stated they were satisfied with the Covid safety measures put in place for this election


## EO Role analysis
PPM_count             <- table(Survey_data_complete_LG_roles$Role)[['PPM']]
PPM_role_list         <- rep('PPM', length(role_list))
PPM_count_list        <- rep(PPM_count, length(role_list))

names(PPM_role_list)  <- names(role_list)
names(PPM_count_list) <- names(base_size_list)



plots_yesno_PPM <- yesno_plots(plot_list        = yesno_barchart_questions,
                               survey_data      = Survey_data_complete_LG_roles %>% filter(Role == 'PPM'),
                               survey_questions = Survey_questions,
                               role_list        = PPM_role_list,
                               
                               count_list       = BaseCount %>% 
                                 mutate(BaseSize = PPM_count),
                               
                               calc_percent     = TRUE,  
                               
                               plot_levels  = c("Uncertain",
                                                "No",
                                                "Yes"),
                               
                               plot_colours = c('No'         = 'Coral2', 
                                                'Yes'        = 'green4',
                                                "Uncertain"  = 'dodgerblue1'),
                               
                               ## Set the plot parameters
                               mar       = 1,
                               tsize     = 35,
                               capt_size = 25,
                               xsize     = 25,
                               ysize     = 25,
                               ycol      = 'black',
                               lab_size  = 8,
                               ylab      = '\nPercent (%)\n',
                               xlab      = '')


## What are the numbers?
plots_yesno_PPM[["A1_graph"]]
plots_yesno_PPM[["A2_graph"]]
plots_yesno_PPM[["N4_graph"]]





## 
ord5_plots_singlegrid_PPM <- 
  
  zero_centred_plots_single_gridchoice(plot_list        = ord5_singlegrid, 
                                       survey_data      = Survey_data_complete_LG_roles %>% filter(Role == 'PPM'),
                                       survey_questions = Survey_questions,
                                       role_list        = PPM_role_list,
                                       count_list       = PPM_count_list,
                                       
                                       neutral          = 'Neutral',
                                       negative         = 'Dissatisfied|Very dissatisfied',
                                       graph_scale      =  c('Very satisfied',
                                                             'Satisfied',
                                                             'Neutral',
                                                             'Dissatisfied',
                                                             'Very dissatisfied'),
                                       
                                       scale_cols       = c('Neutral'           = 'grey',
                                                            'Satisfied'         = 'lightblue',
                                                            'Very satisfied'    = 'skyblue3',
                                                            'Dissatisfied'      = 'darkorange1',
                                                            'Very dissatisfied' = 'brown1'),
                                       
                                       leg_order        = c('Very dissatisfied',
                                                            'Dissatisfied',
                                                            'Neutral',
                                                            'Satisfied',
                                                            'Very satisfied'),
                                       
                                       tsize      = 40,
                                       lab_size   = 12,
                                       leg_size   = 30,
                                       ysize      = 30, 
                                       xsize      = 30,
                                       capt_size  = 30,
                                       width      = 0.5, 
                                       ymin       = -0.5,
                                       ymax       = 1.1,
                                       high_just  = -0.5,
                                       low_just   = 1.5)




## 
ord5_plots_singlegrid_PPM[['B1_graph']]
ord5_plots_singlegrid_PPM[['C1_graph']]




## Q164 : What % of all staff who finished counting before 10.30pm? 
Question_164 <- time_questions[2]

## If we do this instead - we won't pollute the global environment as much
Label = Survey_questions %>%
  filter(QuestionNumber_sub == Question_164) %>% 
  dplyr::select(QuestionText) %>% 
  distinct() %>% .[1, ] %>% .[[1]] %>% 
  str_wrap(., 50)

Role = Question_164 %>%
  as.character() %>%
  paste0(., '_Roles') %>%
  role_list[[.]] %>% paste0(., sep = '')

Count = BaseCount %>%
  filter(QuestionNumber == Question_164) %>% 
  .$BaseSize

## Create a caption with Roles and counts
Role_caption <- paste(c(Role, Count, ' Responses'), collapse = " ") %>% str_wrap(50)

# All time histogram questions relate to election night
# Data needs to be cleaned and filtered accordingly
election_day_close_voting <- as.POSIXct(paste(election_day, "06:00 PM")
                                        ,format = "%Y-%m-%d %I:%M %p"
                                        ,optional = TRUE)

# Graph data
time_histo <- Survey_data_complete_LG_roles %>% dplyr::select(Question_164) %>%
  
  mutate(
    # Convert time character to POSISct
    Time  = strptime(!!sym(Question_164), "%I:%M %p")
    # Add correct dates to time values
    ,Time  = case_when(
      # If time is between midnight and 6am, make it election sunday
      Time >= strptime("12:00 AM", "%I:%M %p") & 
        Time < strptime("06:00 AM", "%I:%M %p") ~ as.POSIXct(paste(election_sunday, !!sym(Question_164))
                                                             ,format   = "%Y-%m-%d %I:%M %p"
                                                             ,optional = TRUE)
      # All other times, election day
      ,TRUE ~ as.POSIXct(paste(election_day, !!sym(Question_164))
                         ,format = "%Y-%m-%d %I:%M %p"
                         ,optional = TRUE))
  ) %>% na.omit() %>% filter(Time >= election_day_close_voting)


## The % of staff who finished before 10.30pm on election night
time_histo_before_1030pm <- time_histo %>% filter(Time < "2021-12-04 22:30:00")
WHS_finish_prop     <- nrow(time_histo_before_1030pm)/nrow(Survey_data_complete_LG_roles) * 100
WHS_finish_prop_ans <- nrow(time_histo_before_1030pm)/nrow(time_histo) * 100


# Calculate median of post 6pm finishing times
median_fin_var   <- time_histo %>%
  filter(Time >= election_day_close_voting) %>%
  .$Time %>%
  median(na.rm = TRUE)

average_fin_time <- time_histo %>%
  filter(Time >= election_day_close_voting) %>%
  .$Time %>%
  mean(na.rm = TRUE)



## What is the average finishing time please? Looks like around 8:15pm
time_histograms_median <- histogram_time_plots(plot_list        = time_questions[2],
                                               survey_data      = Survey_data_complete_LG_roles,
                                               survey_questions = Survey_questions,
                                               role_list        = role_list,
                                               count_list       = BaseCount,
                                               
                                               ## Set the plot parameters
                                               tsize     = 25,
                                               capt_size = 20,
                                               xsize     = 20,
                                               ysize     = 20,
                                               lab_size  = 8,
                                               ylab      = '\nFrequency\n',
                                               xlab      = 'Time (24hr)',
                                               bin       = 30)



## Q168 Return of material staff will not work past 12.30am. Q168 Are you able to provide 
## the % of SOA staff who finished before 12.30am?
# Graph data
time_histo_SOA <- Survey_data_complete_LG_roles %>% filter(Role == 'SOA') %>% dplyr::select(Question_164) %>%
  
  mutate(
    # Convert time character to POSISct
    Time  = strptime(!!sym(Question_164), "%I:%M %p")
    # Add correct dates to time values
    ,Time  = case_when(
      # If time is between midnight and 6am, make it election sunday
      Time >= strptime("12:00 AM", "%I:%M %p") & 
        Time < strptime("06:00 AM", "%I:%M %p") ~ as.POSIXct(paste(election_sunday, !!sym(Question_164))
                                                             ,format   = "%Y-%m-%d %I:%M %p"
                                                             ,optional = TRUE)
      # All other times, election day
      ,TRUE ~ as.POSIXct(paste(election_day, !!sym(Question_164))
                         ,format = "%Y-%m-%d %I:%M %p"
                         ,optional = TRUE))
  ) %>% na.omit() %>% filter(Time >= election_day_close_voting)


## The % of staff who finished before 10.30pm on election night
time_histo_SOA_before_12am <- time_histo_SOA %>% filter(Time < "2021-12-05 00:00:00")
SOA_finish_prop            <- nrow(time_histo_SOA_before_12am)/Survey_complete_role_counts[['SOA']] * 100
SOA_finish_prop_answ       <- nrow(time_histo_SOA_before_12am)/nrow(time_histo_SOA) * 100





#################################################### TBC ###########################################################