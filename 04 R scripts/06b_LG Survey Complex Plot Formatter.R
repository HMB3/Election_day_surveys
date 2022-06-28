#################################### ---- CREATE STAFF SURVEY PLOTS ---- ############################################


## This code creates complex plots - the five-point satisfaction graphs, etc.



## 1). CREATE LISTS OF EACH QUESTION TYPE FOR PLOT CREATION ========================================================



## List of single-plot satisfaction questions
ord5_singlegrid <- LUT %>% 
  filter(Graph_type   == 'zero_centred_barchart_single' 
         & Response_type == 'Ord5') %>% 
  arrange(QuestionNumber) %>% 
  .$QuestionNumber %>% as.character() %>% unique() %>% sort()


## List of plot questions without sub-labels
ord5_nosub <- LUT %>% 
  filter(Graph_type   == 'zero_centred_barchart'
         & NumColumns == 1 & Response_type == 'Ord5') %>% 
  arrange(QuestionNumber) %>% 
  .$QuestionNumber %>% as.character() %>% unique() %>% sort()



## List of plot questions with sub-labels
## We need to combine the main questions with the sub-questions
ord3_sub <- LUT %>% 
  filter(Graph_type   == 'zero_centred_barchart'
         & NumColumns > 1 & Response_type == 'Ord3') %>%
  arrange(QuestionNumber) %>%
  .$QuestionNumber %>% as.character() %>% unique() %>% sort()


ord5_sub <- LUT %>% 
  filter(Graph_type   == 'zero_centred_barchart'
         & NumColumns > 1 & Response_type == 'Ord5') %>%
  arrange(QuestionNumber) %>% 
  .$QuestionNumber %>% as.character() %>% unique() %>% sort()





## 2). YES/NO ZERO-CENTRED GRAPHS ==========================================================================


## ORD3 Plots with sub-labels
ord3_plots_sub <- zero_centred_plots_sub(plot_list        = ord3_sub, 
                                         survey_data      = Survey_data_complete_LG_roles,
                                         survey_questions = Survey_questions,
                                         sublabel_list    = subquestion_labels,
                                         role_list        = role_list,
                                         count_list       = base_size_list,
                                         
                                         neutral          = "Uncertain",
                                         negative         = 'No',
                                         
                                         graph_scale      =  c('Yes',
                                                               "Uncertain",
                                                               'No'),
                                         
                                         scale_cols       = c('No'        = 'darkorange1', 
                                                              'Yes'       = 'skyblue3',
                                                              "Uncertain" = 'grey'),
                                         
                                         leg_order        = c('No', 
                                                              "Uncertain", 
                                                              'Yes'),
                                         
                                         tsize      = 35,
                                         lab_size   = 8,
                                         leg_size   = 25,
                                         capt_size  = 20,
                                         ysize      = 25, 
                                         xsize      = 25,
                                         width      = 0.5, 
                                         ymin       = -1.2,
                                         ymax       = 1.2,
                                         high_just  = 0,
                                         low_just   = 1.5)





## 2). SATISFACTION ZERO-CENTRED GRAPHS ==========================================================================


## Single ORD5 Plots singlechoice 
ord5_plots_singlegrid <- zero_centred_plots_single_gridchoice(plot_list        = ord5_singlegrid, 
                                                              survey_data      = Survey_data_complete_LG_roles,
                                                              survey_questions = Survey_questions,
                                                              role_list        = role_list,
                                                              count_list       = base_size_list,
                                                              
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





## ORD5 Plots without sub-lables 
ord5_plots <- zero_centred_plots(plot_list        = ord5_nosub, 
                                 survey_data      = Survey_data_complete_LG_roles,
                                 survey_questions = Survey_questions,
                                 role_list        = role_list,
                                 count_list       = base_size_list,
                                 
                                 neutral        = 'Neutral',
                                 negative       = 'Dissatisfied|Very dissatisfied',
                                 graph_scale    =  c('Very satisfied',
                                                     'Satisfied',
                                                     'Neutral',
                                                     'Dissatisfied',
                                                     'Very dissatisfied'),
                                 
                                 scale_cols     = c('Neutral'           = 'grey',
                                                    'Satisfied'         = 'lightblue',
                                                    'Very satisfied'    = 'skyblue3',
                                                    'Dissatisfied'      = 'darkorange1',
                                                    'Very dissatisfied' = 'brown1'),
                                 
                                 leg_order      = c('Very dissatisfied',
                                                    'Dissatisfied',
                                                    'Neutral',
                                                    'Satisfied',
                                                    'Very satisfied'),
                                 
                                 tsize      = 35,
                                 lab_size   = 8,
                                 leg_size   = 25,
                                 ysize      = 25, 
                                 xsize      = 25,
                                 capt_size  = 20,
                                 width      = 0.5, 
                                 ymin       = -1.1,
                                 ymax       = 1.1,
                                 high_just  = 0,
                                 low_just   = 1.5)




## ORD5 Plots with sub-lables 
ord5_plots_sub <- zero_centred_plots_sub(plot_list        = ord5_sub, 
                                         survey_data      = Survey_data_complete_LG_roles,
                                         survey_questions = Survey_questions,
                                         role_list        = role_list,
                                         count_list       = base_size_list,
                                         sublabel_list    = subquestion_labels,
                                         
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
                                         ymin       = -1,
                                         ymax       = 1.2,
                                         high_just  = -0.5,
                                         low_just   = 1.5)






#################################################### TBC ###########################################################