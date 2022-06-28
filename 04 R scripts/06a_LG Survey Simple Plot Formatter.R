#################################### ---- CREATE STAFF SURVEY PLOTS ---- ################################################



## This code formats all the simple survey question figures using lists of question types, looping through each type


## To-do ----
# - Check all the factor and time questions. Can they be functionalised too?
# - Problem is for tables and figs with sub-components





## 1). CREATE LISTS OF EACH QUESTION TYPE FOR PLOT CREATION ========================================================


## In Tom's new system, the graph and table types are homogenous, which makes things much easier...
## But we need to check this is the case!

## Yes/No lists ----
yesno_barchart_questions <- LUT %>% 
  dplyr::filter(Graph_type == "single_barchart_order_y_small"
                & NumColumns == 1 ) %>% 
  .$QuestionNumber %>% as.character() %>% .[!is.na(.)]


yesno_barchart_questions_sub <- LUT %>% 
  dplyr::filter(Graph_type == "single_barchart_order_y_small"
                & NumColumns > 1 ) %>% 
  .$QuestionNumber %>% as.character() %>% .[!is.na(.)]


## Barchart lists
order_barchart_questions <- LUT %>% 
  dplyr::filter(Graph_type == "single_barchart_order_y"
                & NumColumns > 1) %>%
  .$QuestionNumber %>% as.character() %>% .[!is.na(.)]


factor_barchart_questions <- LUT %>% 
  dplyr::filter(Graph_type == "single_bar_order_factor"
                & NumColumns == 1) %>%
  .$QuestionNumber %>% as.character() %>% .[!is.na(.)] 


histo_questions <- LUT %>% 
  dplyr::filter(Graph_type == "Boxplot" & NumColumns == 1) %>% 
  .$QuestionNumber %>% as.character() %>% .[!is.na(.)] %>% sort()


## Boxplot lists
time_questions <- LUT %>% 
  dplyr::filter(Graph_type == "histogram_time" & NumColumns == 1) %>% 
  .$QuestionNumber %>% as.character() %>% .[!is.na(.)] %>% sort()


histo_sub_questions <- LUT %>% 
  dplyr::filter(Graph_type == "Boxplot" & NumColumns > 1) %>% 
  left_join(., Survey_questions, 
            by = c("Topic", "QuestionNumber", "QuestionText", "NumColumns")) %>% 
  .$QuestionNumber_sub %>% as.character() %>% .[!is.na(.)]


# BaseCount <- tibble::rownames_to_column(BaseCount, "QuestionNumber")




## 2). HISTOGRAMs ==========================================================================


## Loop over all the histograms without sub-labels
plot_histograms <- histogram_plots(plot_list        = histo_questions,
                                   survey_data      = Survey_data_complete_LG_roles,
                                   survey_questions = Survey_questions %>% as_tibble(),
                                   role_list        = role_list,
                                   count_list       = BaseCount,
                                   
                                   ## Set the plot parameters
                                   tsize     = 25,
                                   capt_size = 20,
                                   xsize     = 20,
                                   ysize     = 20,
                                   lab_size  = 8,
                                   ylab      = '\nFrequency\n',
                                   xlab      = 'Estimate',
                                   bin       = 2)





## Loop over all the histograms without sub-labels
## The x-scale here is wrong, but not sure how to fix it...
time_histograms <- histogram_time_plots(plot_list        = time_questions,
                                        survey_data      = Survey_data_complete_LG_roles,
                                        survey_questions = Survey_questions,
                                        role_list        = role_list,
                                        count_list       = BaseCount,
                                        election_day     = election_day, 
                                        election_sunday  = election_sunday, 
                                        
                                        ## Set the plot parameters
                                        tsize     = 25,
                                        capt_size = 20,
                                        xsize     = 20,
                                        ysize     = 20,
                                        lab_size  = 8,
                                        ylab      = '\nFrequency\n',
                                        xlab      = 'Time (24hr)',
                                        bin       = 30)





## Loop over all the boxplots with sub-labels
plot_sub_histograms <- histogram_sub_plots(plot_list        = histo_sub_questions,
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
                                           xlab      = 'Estimate',
                                           bin       = 10, 
                                           max_est   = 5000)





## 3). YES/NO BARCHARTS ==========================================================================


## These plots are just % yes/no/maybe. Not that useful, but we can apply it to something else :]


## Loop over all the yes/no tables and create a plot for each
## Filter out the not/applicable
plots_yesno <- yesno_plots(plot_list        = yesno_barchart_questions,
                           survey_data      = Survey_data_complete_LG_roles,
                           survey_questions = Survey_questions,
                           role_list        = role_list,
                           count_list       = BaseCount,
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





## 4). Y-ORDERED BARCHARTS ==========================================================================


## These plots don't sum to 100, check the table creation
plots_y_ordered <- barchart_order_y_plots(plot_list        = order_barchart_questions,
                                          survey_data      = Survey_data_complete_LG_roles,
                                          survey_questions = Survey_questions,
                                          role_list        = role_list,
                                          count_list       = BaseCount,
                                          calc_percent     = TRUE,
                                          
                                          ## Set the plot parameters
                                          tsize     = 35,
                                          capt_size = 25,
                                          xsize     = 25,
                                          ysize     = 25,
                                          ycol      = 'black',
                                          lab_size  = 8,
                                          ymin      = 0,
                                          mar       = 1,
                                          axis_multiplier = .2,
                                          ylab        = '\nPercent (%)\n',
                                          xlab        = '',
                                          col_palette = "Set2")





## 5). FACTOR-ORDERED BARCHARTS ==========================================================================


## This bit can be turned into a function, that creates the table needed and uses the
## level as an argument (e.g. 10 minutes, etc.). That would be a list of 'levels'
## However, the levels are different between questions, so we need to see what the new data looks like


## Question E3 Plot ----
## factor_barchart_questions
if(nrow(Survey_data_complete_LG_roles %>%
        dplyr::filter(!is.na(E3))) > 0) {
  
  E3_plot <- SingleChoiceTable(Survey_data_complete_LG_roles %>%
                                 dplyr::filter(!is.na(E3)), "E3", TRUE) %>% 
    rename(., `Response`  = E3) %>%
    mutate(Percentage = percent_num_form(Percentage),
           Percentage = round(Percentage, 0)) %>%
    filter(Response %in% as.character(unique(Survey_data_complete_LG_roles %>% .$E3))) %>% 
    na.omit() 
  E3_plot$Response <- factor(E3_plot$Response, levels = rev(c("Yes, this location was ideal",
                                                              "Yes, this location was satisfactory",
                                                              "Yes, although there were significant issues with this location",
                                                              "No, not under any circumstances"))) 
  
  
  E3_graph <- single_bar_order_factor(df      = E3_plot, 
                                      title   = str_wrap(main_questions_labels[['E3_label']], width = 40) %>% 
                                        gsub(".* - ","", .), 
                                      caption = paste0(role_list[['E3_Roles']], 
                                                       ' ',  base_size_list[['E3_Count']], ' Responses'),
                                      col_palette = "Set2",
                                      
                                      tsize     = 35,
                                      capt_size = 25,
                                      xsize     = 25,
                                      ysize     = 25,
                                      ycol      = 'black',
                                      lab_size  = 8,
                                      
                                      ymin      = 0, 
                                      axis_multiplier = 0.2,
                                      ylab  = '\nPercent (%)\n',
                                      xlab  = '')
  
}



## Question M3c Plot ----
if(nrow(Survey_data_complete_LG_roles %>%
        dplyr::filter(!is.na(M3c))) > 0) {
  
  M3c_plot <- SingleChoiceTable(Survey_data_complete_LG_roles %>%
                                  filter(!is.na("M3c")), "M3c", TRUE) %>%
    rename(., `Response`  = M3c) %>%
    mutate(Percentage = percent_num_form(Percentage),
           Percentage = round(Percentage, 0)) %>%
    filter(Response %in% as.character(unique(Survey_data_complete_LG_roles %>% .$M3c))) %>%
    na.omit()
  M3c_plot$Response <- factor(M3c_plot$Response, levels = rev(c("Less than two minutes",
                                                                "Two to five minutes",
                                                                "More than five minutes",
                                                                "Not sure")))
  
  
  M3c_graph <- single_bar_order_factor(df      = M3c_plot,
                                       title   = str_wrap(main_questions_labels[['M3c_label']], width = 40) %>% 
                                         gsub(".* - ","", .),
                                       caption = paste0(role_list[['M3c_Roles']],
                                                        ' ',  base_size_list[['M3c_Count']], ' Responses'),
                                       col_palette = "Set2",
                                       
                                       tsize     = 35,
                                       capt_size = 25,
                                       xsize     = 25,
                                       ysize     = 25,
                                       ycol      = 'black',
                                       lab_size  = 8,
                                       
                                       ymin      = 0,
                                       axis_multiplier = 0.2,
                                       ylab  = '\nPercent (%)\n',
                                       xlab  = '')
  
}


## Create M4b plot ----
if(nrow(Survey_data_complete_LG_roles %>%
        dplyr::filter(!is.na(M4b))) > 0) {
  
  
  M4b_plot <- SingleChoiceTable(Survey_data_complete_LG_roles %>%
                                  filter(!is.na("M4b")), "M4b", TRUE) %>% 
    rename(., `Response`  = M4b) %>%
    mutate(Percentage = percent_num_form(Percentage),
           Percentage = round(Percentage, 0)) %>%
    filter(Response %in% as.character(unique(Survey_data_complete_LG_roles %>% .$M4b))) %>%
    na.omit() 
  M4b_plot$Response <- factor(M4b_plot$Response, 
                              levels = rev(c("Less than two minutes",
                                             "Two to five minutes",
                                             "More than five minutes"))) 
  
  
  M4b_graph <- single_bar_order_factor(df      = M4b_plot, 
                                       title   = str_wrap(main_questions_labels[['M4b_label']], width = 40) %>% 
                                         gsub(".* - ","", .), 
                                       caption = paste0(role_list[['M4b_Roles']], 
                                                        ' ',  base_size_list[['M4b_Count']], ' Responses'),
                                       col_palette = "Set2",
                                       
                                       tsize     = 35,
                                       capt_size = 25,
                                       xsize     = 25,
                                       ysize     = 25,
                                       ycol      = 'black',
                                       lab_size  = 8,
                                       
                                       ymin      = 0, 
                                       axis_multiplier = 0.2,
                                       ylab  = '\nPercent (%)\n',
                                       xlab  = '')
  
}



## Create G14 plot ----
if(nrow(Survey_data_complete_LG_roles %>%
        dplyr::filter(!is.na(G14))) > 0) {
  
  G14_plot <- SingleChoiceTable(Survey_data_complete_LG_roles %>%
                                  filter(!is.na("G14")), "G14", TRUE) %>% 
    rename(., `Response`  = G14) %>%
    mutate(Percentage = percent_num_form(Percentage),
           Percentage = round(Percentage, 0)) %>%
    filter(Response %in% as.character(unique(Survey_data_complete_LG_roles %>% .$G14))) %>%
    na.omit() 
  G14_plot$Response <- factor(G14_plot$Response, 
                              levels = rev(c("8am-9am",
                                             "9am-10am",
                                             "10am-11am",
                                             "11am-12pm",
                                             "12pm-1pm",
                                             "1pm-2pm",
                                             "2pm-3pm",
                                             "3pm-4pm",
                                             "4pm-5pm",
                                             "5pm-6pm"))) 
  
  
  G14_graph <- single_bar_order_factor(df      = G14_plot, 
                                       title   = str_wrap(main_questions_labels[['G14_label']], width = 40) %>% 
                                         gsub(".* - ","", .), 
                                       caption = paste0(role_list[['G14_Roles']], 
                                                        ' ',  base_size_list[['G14_Count']], ' Responses'),
                                       col_palette = "Set2",
                                       
                                       tsize     = 35,
                                       capt_size = 25,
                                       xsize     = 25,
                                       ysize     = 25,
                                       ycol      = 'black',
                                       lab_size  = 8,
                                       
                                       ymin      = 0, 
                                       axis_multiplier = 0.2,
                                       ylab  = '\nPercent (%)\n',
                                       xlab  = '')
  
  
}





## 6). DEMOGRAPHY BARCHARTS ==========================================================================


## Create plot of Gender * Age -----
## demography_questions
Age_distribution <- CrossTab(Survey_data_complete_LG_roles %>% 
                               filter(N1 %in% c('Male', 'Female')), 'N1', 'N2', TRUE) %>% 
  filter(N1 %in% as.character(unique(Survey_data_complete_LG_roles %>% .$N1))) %>% 
  na.omit()
Age_distribution


Age_gender_graph = highchart() %>% 
  
  ## Data
  hc_add_series(Age_distribution, "column",
                pointWidth = 20,
                hcaes(x = N2, y = Count, group = N1)) %>%
  
  ## Options for each type of series
  hc_plotOptions(
    series = list(
      showInLegend = FALSE,
      pointFormat = "{point.y}%"
    ),
    column = list(
      colorByPoint = FALSE
    )
  ) %>%
  
  ## Axis
  hc_yAxis(
    title  = list(text   = "Number of respondens"),
    labels = list(format = "{value}")
  ) %>% 
  
  hc_xAxis(categories = unique(Age_distribution$N2)) %>%
  
  ## Titles and credits
  hc_title(
    text = "Respondent Age distribution"
  ) %>%
  
  hc_subtitle(text = "number of respondents in each age category") %>% 
  hc_credits(
    enabled = TRUE, text = "Source: LG21 Election Staff Survey",
    href = "#",
    style = list(fontSize = "12px")
  )





## Plot N3 graph ----
N3_plot <- SingleChoiceTable(Survey_data_complete_LG_roles %>%
                               filter(!is.na(N3)),'N3', TRUE) %>%
  
  rename(., `Response`  = N3) %>%
  select(-Count) %>%
  na.omit() %>% 
  filter(Response %in% 
           as.character(unique(Survey_data_complete_LG_roles %>% .$N3)))



## Create plot
N3_graph <- single_barchart_order_y(df              = N3_plot,
                                    title           = str_wrap(main_questions_labels[['N3_label']], 40) %>% 
                                      gsub(".* - ","", .), 
                                    caption         = paste0(role_list[['N3_Roles']], 
                                                             ' ',  base_size_list[['N3_Count']], ' Responses'),
                                    # col_palette     = "Set2",
                                    color_n        = nrow(N3_plot),
                                    
                                    tsize           = 40,
                                    capt_size       = 25,
                                    xsize           = 25,
                                    ysize           = 25,
                                    ycol            = 'black',
                                    lab_size        = 8,
                                    mar             = 1.5,
                                    
                                    ymin            = 0,
                                    axis_multiplier = 0.2,
                                    ylab            = 'Percentage (%)',
                                    xlab            = '')





#################################################### TBC ###########################################################