############################################################################################
############################ ----  PLOTTING FUNCTIONS ---- #################################
############################################################################################





## BARCHART COMBO PLOTS ====================================================================


##
# plot_list        = yesno_barchart_questions
# survey_data      = Survey_data
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = BaseCount
# calc_percent     = TRUE
# 
# plot_levels  = c("Uncertain",
#                  "No",
#                  "Yes")
# 
# plot_colours = c('No'         = 'Coral2',
#                  'Yes'        = 'green4',
#                  "Uncertain"  = 'dodgerblue1')
# 
# ## Set the plot parameters
# tsize     = 35
# capt_size = 25
# xsize     = 25
# ysize     = 25
# ycol      = 'black'
# lab_size  = 8
# ylab      = '\nPercent (%)\n'
# xlab      = ''

## Create a list of yes/no plots----
yesno_plots <- function(plot_list, survey_data, survey_questions, mar,
                        role_list, count_list,
                        
                        calc_percent,
                        plot_levels, plot_colours,
                        tsize, capt_size, xsize, 
                        ysize, ycol, lab_size, xlab, ylab) {
  
  ## Pipe the list into Lapply
  yesno_graphs <- plot_list %>%
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      
      ## Check the dimensions of the data
      test_df <- survey_data %>% dplyr::filter(!is.na(!!sym(Question))) %>% nrow()
      if (test_df > 0) {
        
        ## Question = yesno_barchart_questions[1]
        message("Creating yes/no plot for ", Question)
        
        ## If we do this instead - we won't pollute the global environment as much
        Label = survey_questions %>%
          dplyr::filter(QuestionNumber_sub == Question) %>% 
          dplyr::select(QuestionText) %>% 
          distinct() %>% .[1, ] %>% .[[1]] %>% 
          str_wrap(., 50)
        
        Role = Question %>%
          as.character() %>%
          paste0(., '_Roles') %>%
          role_list[[.]] %>% paste0(., sep = ' ')
        
        Count = count_list %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          .$BaseSize
        
        ## Create a caption with Roles and counts
        Role_caption <- paste(c(Role, Count, ' Responses'), collapse = " ") %>% str_wrap(50)
        
        ## Create the table of percentages for each response
        yes_table <- SingleChoiceTable(survey_data %>% 
                                         dplyr::filter(!is.na(!!sym(Question))), 
                                       Question, calc_percent) %>%
          
          ## Get rid of the weirdos
          dplyr::filter(!!sym(Question) %in% plot_levels) %>% 
          
          ## Create a number, rather than a %
          mutate(Percentage = formattable::percent(Percentage, digits = 0)) %>%  
          rename(., Response = Question) %>%
          na.omit() %>%
          arrange(-Percentage) %>%
          
          ## Re-order the factor
          mutate(Response = factor(Response, levels = plot_levels))
        
        ## Now pipe the table into the function
        single_barchart_order_y_small(df    = yes_table, 
                                      title = Label, 
                                      
                                      ## This creates the question number
                                      caption   = Role_caption,
                                      
                                      tsize     = tsize,
                                      capt_size = capt_size,
                                      xsize     = xsize,
                                      ysize     = ysize,
                                      ycol      = ycol,
                                      lab_size  = lab_size,
                                      mar       = mar,
                                      
                                      ymin = 0, 
                                      ymax = max(yes_table$Percentage) + 
                                        max(yes_table$Percentage*0.2),
                                      ylab  = ylab,
                                      xlab  = xlab,
                                      
                                      colours = plot_colours)
        
      } else {
        message('Skip graph for ', Question)
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Rename the list items
  names(yesno_graphs) <- plot_list
  names(yesno_graphs) <- paste0(names(yesno_graphs), "_graph")
  return(yesno_graphs)
  
}


# plot_list        = histo_questions
# survey_data      = Survey_data_complete_staff
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = BaseCount
# 
# ## Set the plot parameters
# tsize     = 25
# capt_size = 20
# xsize     = 20
# ysize     = 20
# lab_size  = 8
# ylab      = '\nFrequency\n'
# xlab      = 'Estimate'
# bin       = 5


## Create a list of histogram plots----
histogram_plots <- function(plot_list, survey_data, survey_questions, 
                            role_list, count_list,
                            
                            ## This creates the question number
                            tsize, capt_size, xsize, ysize, lab_size,
                            bin,   ylab, xlab, median_var) {
  
  ## Pipe the list into Lapply
  histograms <- plot_list %>%
    
    ## Pipe the list into lapply
    ## Question <- plot_list[1]
    lapply(function(Question) {
      
      ## Check the dimensions of the data
      ## histogram(as.numeric(levels(Survey_data$C11))[Survey_data$C11])
      test_df <- survey_data %>% dplyr::filter(!is.na(!!sym(Question))) %>% nrow()
      if (test_df > 5) {
        
        ## Question = histo_questions[1]
        message("Creating histograms for ", Question)
        
        ## If we do this instead - we won't pollute the global environment as much
        Label = survey_questions %>%
          dplyr::filter(QuestionNumber_sub == Question) %>% 
          dplyr::select(QuestionText) %>% 
          distinct() %>% .[1, ] %>% .[[1]] %>% 
          str_wrap(., 50)
        
        Role = Question %>%
          as.character() %>%
          paste0(., '_Roles') %>%
          role_list[[.]] %>% paste0(., sep = '')
        
        Count = count_list %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          .$BaseSize
        
        ## Create a caption with Roles and counts
        Role_caption <- paste(c(Role, Count, ' Responses'), collapse = " ") %>% str_wrap(50)
        
        ## Create the table of percentages for each response
        histo <- survey_data %>% select(!!sym(Question)) %>% 
          dplyr::filter(!is.na(!!sym(Question))) %>% .[,1] %>% 
          lapply(., as.numeric) %>% as.data.frame()
        
        median_var   <- median(histo[[1]], na.rm = TRUE)
        
        ## Count the frequency of each value (e.g. 20 minutes) and make that the y-max
        freq_max     <- table(histo) %>% as.data.frame() %>% 
          arrange(-Freq) %>% select(Freq) %>% .[1, ]
        
        ## Now pipe the table into the function
        plot_histogram(df       = histo, 
                       title    = Label,
                       subtitle = '',
                       
                       ## This creates the question number
                       caption   = Role_caption,
                       tsize     = tsize,
                       capt_size = capt_size,
                       xsize     = xsize,
                       ysize     = ysize,
                       lab_size  = lab_size,
                       bin       = bin,
                       ymax      = freq_max,
                       ymin      = 0,
                       
                       xvar      = Question,
                       col_var   = Question,
                       med       = median_var,
                       ylab      = ylab,
                       xlab      = xlab)
        
      } else {
        message('Skip graph for ', Question)
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Rename the list items
  names(histograms) <- plot_list
  names(histograms) <- paste0(names(histograms), "_graph")
  return(histograms)
  
}



# plot_list        = time_questions[2]
# survey_data      = Survey_data_complete_LG_roles
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = BaseCount
# 
# ## Set the plot parameters
# tsize     = 25
# capt_size = 20
# xsize     = 20
# ysize     = 20
# lab_size  = 8
# ylab      = '\nFrequency\n'
# xlab      = 'Time (24hr)'
# bin       = 30





## Create a list of yes/no plots----
histogram_time_plots <- function(plot_list, survey_data, survey_questions, 
                                 role_list, count_list, election_day, election_sunday,
                                 
                                 ## This creates the question number
                                 tsize, capt_size, xsize, ysize, lab_size,
                                 bin,   ylab, xlab, median_var) {
  
  ## Pipe the list into Lapply
  histograms <- plot_list %>%
    
    ## Pipe the list into lapply
    ## Question <- plot_list[1]
    lapply(function(Question) {
      
      ## Check the dimensions of the data
      test_df <- survey_data %>% dplyr::filter(!is.na(!!sym(Question))) %>% nrow()
      if (test_df > 5) {
        
        ## Question = time_questions[1]
        message("Creating histograms for ", Question)
        
        ## If we do this instead - we won't pollute the global environment as much
        Label = survey_questions %>%
          dplyr::filter(QuestionNumber_sub == Question) %>% 
          dplyr::select(QuestionText) %>% 
          distinct() %>% .[1, ] %>% .[[1]] %>% 
          str_wrap(., 50)
        
        Role = Question %>%
          as.character() %>%
          paste0(., '_Roles') %>%
          role_list[[.]] %>% paste0(., sep = '')
        
        Count = count_list %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          .$BaseSize
        
        ## Create a caption with Roles and counts
        Role_caption <- paste(c(Role, Count, ' Responses'), collapse = " ") %>% str_wrap(50)
        
        # All time histogram questions relate to election night
        # Data needs to be cleaned and filtered accordingly
        election_day_close_voting <- as.POSIXct(paste(election_day, "06:00 PM")
                                                ,format = "%Y-%m-%d %I:%M %p"
                                                ,optional = TRUE)
        
        # Graph data
        time_histo <- survey_data %>% dplyr::select(Question) %>% 
          mutate(
            
            # Convert time character to POSISct
            Time  = strptime(!!sym(Question), "%I:%M %p")
            # Add correct dates to time values
            ,Time  = case_when(
              
              # If time is between midnight and 6am, make it election sunday
              Time >= strptime("12:00 AM", "%I:%M %p") & 
                Time < strptime("06:00 AM", "%I:%M %p") ~ 
                as.POSIXct(paste(election_sunday, !!sym(Question))
                           ,format = "%Y-%m-%d %I:%M %p"
                           ,optional = TRUE)
              
              # All other times, election day
              ,TRUE ~ as.POSIXct(paste(election_day, !!sym(Question))
                                 ,format = "%Y-%m-%d %I:%M %p"
                                 ,optional = TRUE))
          ) %>% 
          
          # Remove nulls
          na.omit() %>%
          
          # Remove values prior to close of voting
          dplyr::filter(Time >= election_day_close_voting)
        
        # Calculate median of post 6pm finishing times
        median_var   <- time_histo %>%
          dplyr::filter(Time >= election_day_close_voting) %>%
          .$Time %>%
          median(na.rm = TRUE)
        
        ## Count the frequency of each value (e.g. 20 minutes) and make that the y-max
        freq_max <- table(time_histo$Time) %>% as.data.frame() %>% 
          arrange(-Freq) %>% select(Freq) %>% .[1, ]
        
        ## Now pipe the table into the function
        plot_time_histogram(df       = time_histo,
                            title    = Label,
                            subtitle = '',
                            
                            ## This creates the question number
                            caption   = Role_caption,
                            tsize     = tsize,
                            capt_size = capt_size,
                            xsize     = xsize,
                            ysize     = ysize,
                            lab_size  = lab_size,
                            bin       = bin,
                            
                            xvar      = 'Time',
                            col_var   = 'grey',
                            med       = median_var,
                            median_pos = freq_max,
                            ylab      = ylab,
                            xlab      = xlab)
        
      } else {
        message('Skip graph for ', Question)
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Rename the list items
  names(histograms) <- plot_list
  names(histograms) <- paste0(names(histograms), "_graph")
  return(histograms)
  
}




# plot_list        = histo_sub_questions
# survey_data      = Survey_data_complete
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = BaseCount
# 
# ## Set the plot parameters
# tsize     = 25
# capt_size = 20
# xsize     = 20
# ysize     = 20
# lab_size  = 8
# ylab      = '\nFrequency\n'
# xlab      = 'Estimate'
# bin       = 8


# Create a list of yes/no plots----
histogram_sub_plots <- function(plot_list, survey_data, 
                                survey_questions, role_list, count_list,
                                
                                ## This creates the question number
                                tsize, capt_size, xsize, ysize, lab_size,
                                bin,   ylab, xlab, max_est) {
  
  ## Pipe the list into Lapply
  histograms <- plot_list %>%
    
    ## Pipe the list into lapply
    ## Question = plot_list[1]
    lapply(function(Question) {
      
      ## Check the dimensions of the data
      test_df <- survey_data %>% dplyr::filter(!is.na(!!sym(Question))) %>% nrow()
      if (test_df > 5) {
        
        ## Question = histo_sub_questions[1]
        message("Creating histograms for ", Question)
        
        ## Create all the label objects
        Label = survey_questions %>%
          dplyr::filter(QuestionNumber_sub == Question) %>% 
          dplyr::select(QuestionText) %>% 
          distinct() %>% .[1, ] %>% .[[1]] %>% 
          str_wrap(., 50)
        
        Sublabels = survey_questions %>%
          dplyr::filter(QuestionNumber_sub == Question) %>% 
          .$QuestionText_sub
        
        Role = Question %>%
          as.character() %>%
          paste0(., '_Roles') %>%
          role_list[[.]] %>% paste0(., sep = '')
        
        Count = count_list %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          .$BaseSize
        
        ## Create a caption with Roles and counts
        Role_caption <- paste(c(Role, Count, ': Responses'), collapse = " ") %>% str_wrap(50)
        
        
        histo <- survey_data %>% select(!!sym(Question)) %>% 
          dplyr::filter(!is.na(!!sym(Question))) %>% .[,1] %>% 
          lapply(., as.numeric) %>% as.data.frame() %>% 
          filter_at(1, all_vars(. > 0)) %>% filter_at(1, all_vars(. < max_est))
        
        median_var   <- median(histo[[1]], na.rm = TRUE)
        
        ## Count the frequency of each value (e.g. 20 minutes) and make that the y-max
        freq_max     <- table(histo) %>% as.data.frame() %>% 
          arrange(-Freq) %>% select(Freq) %>% .[1, ]
        
        ## Now pipe the table into the function
        plot_histogram(df       = histo, 
                       title    = Label, 
                       subtitle = Sublabels,
                       
                       ## This creates the question number
                       caption   = Role_caption,
                       tsize     = tsize,
                       capt_size = capt_size,
                       xsize     = xsize,
                       ysize     = ysize,
                       lab_size  = lab_size,
                       bin       = bin,
                       ymax      = freq_max,
                       ymin      = 0,
                       
                       xvar      = Question,
                       col_var   = Question,
                       med       = median_var,
                       ylab      = ylab,
                       xlab      = xlab)
        
      } else {
        message('Skip graph for ', Question)
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Rename the list items
  names(histograms) <- plot_list
  names(histograms) <- paste0(names(histograms), "_graph")
  return(histograms)
  
}




# plot_list        = order_barchart_questions
# survey_data      = Survey_data_complete
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = BaseCount
# calc_percent     = TRUE

## Set the plot parameters
# tsize     = 35
# capt_size = 25
# xsize     = 25
# ysize     = 25
# ycol      = 'black'
# lab_size  = 8
# ymin      = 0
# axis_multiplier = .2
# ylab        = '\nPercent (%)\n'
# xlab        = ''
# col_palette = "Set2"



## Create a list of y-ordered plots----
barchart_order_y_plots <- function(plot_list, survey_data, mar,
                                   survey_questions, role_list, count_list,
                                   calc_percent,
                                   col_palette,
                                   tsize, capt_size, xsize,
                                   ysize, ycol, lab_size, ymin,
                                   axis_multiplier, ylab, xlab) {
  
  ## Pipe the list into Lapply
  barchart_graphs <- plot_list %>%
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      
      ## Only run plots if the question has been answered
      
      ## Question = plot_list[1]
      message("Creating barchart for ", Question)
      QU_1    <- paste0(Question, '_2')
      test_df <- survey_data %>% dplyr::filter(!is.na(!!sym(QU_1))) %>% nrow()
      
      if (test_df > 0) {
        
        ## Question = histo_sub_questions[1]
        message("Creating histograms for ", Question)
        
        ## Create all the label objects
        Label = survey_questions %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          dplyr::select(QuestionText) %>% 
          distinct() %>% .[1, ] %>% .[[1]] %>% 
          str_wrap(., 50)
        
        Sublabels = survey_questions %>%
          dplyr::filter(QuestionNumber == Question) %>%
          ungroup() %>% dplyr::select(QuestionNumber_sub, QuestionText_sub, -QuestionText) %>% 
          as.data.frame()
        
        Section_labels <- survey_questions %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          .$QuestionNumber_sub %>% c()
        
        Role = Question %>%
          as.character() %>%
          paste0(., '_1_Roles') %>%
          role_list[[.]] %>% paste0(., sep = '')
        
        Count = count_list %>%
          dplyr::filter(QuestionNumber == paste0(Question, '_1')) %>% 
          .$BaseSize
        
        ## Create a caption with Roles and counts
        Role_caption <- paste(c(Role, Count, ' Responses'), collapse = " ") %>% str_wrap(50)
        
        ## Create the table of percentages for each response
        ## These percentages don't sum to 100, so the table might need to change
        barchart_table <- MultiChoiceTable(survey_data, Section_labels, calc_percent) %>%
          rename(QuestionNumber_sub = Cname) %>% 
          
          left_join(Sublabels, by = "QuestionNumber_sub") %>%
          dplyr::select(QuestionText_sub, everything(), -QuestionNumber_sub) %>%
          mutate(Percentage = formattable::percent(Percentage, digits = 0)) %>%
          rename(., `Response`  = QuestionText_sub) %>%
          mutate(Response = str_wrap(Response, 40)) %>% 
          arrange(-Count)
        
        ## Color pallette
        graph_cols <- nrow(barchart_table) 
        
        ## Now pipe the table into the graph function
        single_barchart_order_y(df    = barchart_table,
                                title = Label,
                                
                                ## This creates the question number
                                caption   = Role_caption,
                                tsize     = tsize,
                                capt_size = capt_size,
                                xsize     = xsize,
                                ysize     = ysize,
                                ycol      = ycol,
                                lab_size  = lab_size,
                                
                                ymin        = ymin,
                                mar         = mar,
                                axis_multiplier = axis_multiplier,
                                ylab        = ylab,
                                xlab        = xlab,
                                color_n     = graph_cols)
        
        
      } else {
        message('Skip graph for ', Question, ' no responses for this question')
        cat(Question)
      }
      
    }) %>% c()
  
  ## Rename the list items so we can index them
  names(barchart_graphs) <- plot_list
  names(barchart_graphs) <- paste0(names(barchart_graphs), "_graph")
  return(barchart_graphs)
  
}





# plot_list   = ord5_nosub
# survey_data      = Survey_data_complete
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = base_size_list
# 
# neutral        = 'Neutral'
# negative       = 'Dissatisfied|Very dissatisfied'
# graph_scale    =  c('Very satisfied',
#                     'Satisfied',
#                     'Neutral',
#                     'Dissatisfied',
#                     'Very dissatisfied')
# 
# scale_cols     = c('Neutral'             = 'grey',
#                    'Satisfied'        = 'lightblue',
#                    'Very satisfied'   = 'skyblue3',
#                    'Dissatisfied'      = 'darkorange1',
#                    'Very dissatisfied' = 'brown1')
# 
# leg_order      = c('Very dissatisfied',
#                    'Dissatisfied',
#                    'Neutral',
#                    'Satisfied',
#                    'Very satisfied')
# 
# tsize      = 35
# lab_size   = 8
# leg_size   = 25
# ysize      = 25
# xsize      = 25
# capt_size  = 20
# width      = 0.5
# neg_min    = -1




## ZERO-CENTRED COMBO PLOTS ==========================================================


## Create a list of satisfactory plots without sub-lablels ----
## Add a 'label list' argumnet to this
zero_centred_plots <- function(plot_list, 
                               survey_data,
                               survey_questions,  
                               role_list, 
                               count_list,
                               neutral, 
                               negative, 
                               graph_scale, 
                               scale_cols,  
                               leg_order,
                               tsize,
                               capt_size,
                               lab_size,
                               leg_size,
                               ysize, 
                               xsize,
                               width, 
                               ymin,
                               ymax,
                               high_just,
                               low_just) {
  
  ## Pipe the list into Lapply
  ## Question = plot_list[1]
  satisfactory_graphs <- plot_list %>%  ## List is a variable
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      message("Creating satisfaction graphs for ", Question)
      
      ## Chec if there's data - all NA rows means no one answered the question
      test_df <- survey_data %>% dplyr::filter(!is.na(!!sym(Question))) %>% nrow()
      if (test_df > 4) { 
        
        ## Create subsetting variables using lists
        Role_Question = Question %>%
          as.character() %>%
          paste0(., '_Roles') %>%
          role_list[[.]]
        
        ## Create a label for the plot
        Label = survey_questions %>%
          dplyr::filter(QuestionNumber == Question) %>% 
          dplyr::select(QuestionText) %>% 
          distinct() %>% .[1, ] %>% .[[1]] %>% 
          #paste0(Question, ' - ', .) %>% 
          str_wrap(., 50)
        
        Count = Question %>%
          as.character() %>%
          paste0(., '_Count') %>%
          count_list[[.]]
        
        ## Create a caption with Roles and counts
        Count_caption <- paste(c(Count, ' Responses'), collapse = "")
        
        ## Create dat object
        ## Need to filter out the 
        # DataSet = survey_data
        # QID = Question
        # Group = NULL
        # Percent = FALSE
        dat <- CrossTab(survey_data, Question, 'Role', TRUE) %>% ## table is a variable
          dplyr::select(-Count) %>% na.omit() %>% 
          dplyr::filter(Percentage > 0)
        
        ## These objects are temporary
        QID  <- Question
        Cnum <- which(colnames(dat) == QID)
        
        ## Dataset wrangling for negatives and sides - not re-used
        chart_data <- bind_rows(
          dat %>% 
            
            dplyr::filter(dat[Cnum] == neutral) %>%
            mutate(LowNeutral = -Percentage/2,
                   HighNeutral = Percentage/2) %>% 
            dplyr::select(-Percentage) %>%
            
            gather(!!sym(QID), Percentage, -Role, -!!sym(QID)) %>%
            mutate(!!sym(QID) := neutral),
          
          ## Update this
          dat %>% 
            dplyr::filter(dat[Cnum] != neutral) %>%
            mutate(Percentage = ifelse(grepl(negative, !!sym(QID)), -Percentage, Percentage)) 
          
        ) %>%
          mutate(!!sym(QID) := factor(!!sym(QID), levels = graph_scale)) %>%
          rename(., `Response`  = !!sym(QID)) %>% 
          na.omit()
        
        ## Create the postive and negative bars
        highs <- chart_data %>% dplyr::filter(Percentage > 0 | Percentage == 0) %>% na.omit()
        lows  <- chart_data %>% dplyr::filter(Percentage < 0) %>% na.omit()
        
        ## Re-order the lows, as they are negative in order
        lows$Response <- forcats::fct_rev(lows$Response)
        
        ## Create labels for high and low
        high_label <- highs %>% dplyr::filter(Response != neutral) %>% group_by(Role) %>%
          summarise(Percentage = formattable::percent(sum(Percentage), digits = 0))
        
        low_label <- lows %>% dplyr::filter(Response != neutral) %>% group_by(Role) %>%
          summarise(Percentage = formattable::percent(sum(Percentage), digits = 0))
        
        ## Create graph using another function
        print(paste0('Creating graph for table ', QID))
        satisfac_plot = zero_centred_barchart(highs      = highs,
                                              lows       = lows,
                                              high_label = high_label,
                                              low_label  = low_label,
                                              
                                              scale_cols = scale_cols,
                                              leg_order  = leg_order,
                                              
                                              title      = Label,
                                              caption    = Count_caption,
                                              capt_size  = capt_size,
                                              tsize      = tsize,
                                              lab_size   = lab_size,
                                              leg_size   = leg_size,
                                              ysize      = ysize, 
                                              xsize      = xsize,
                                              width      = width, 
                                              
                                              ymin       = ymin,
                                              ymax       = ymax,
                                              high_just  = high_just,
                                              low_just   = low_just)
        
      } else {
        message('Skip graph for ', Question, ' insufficient responses for this question')
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Assign names to list in the same order the graphs were created
  names(satisfactory_graphs) <- plot_list
  names(satisfactory_graphs) <- paste0(names(satisfactory_graphs), "_graph")
  return(satisfactory_graphs)
  
}



# plot_list        = ord5_sub
# survey_data      = Survey_data_complete
# survey_questions = Survey_questions
# role_list        = role_list
# count_list       = base_size_list
# sublabel_list    = subquestion_labels
# 
# neutral        = 'Neutral'
# negative       = 'Dissatisfied|Very dissatisfied'
# graph_scale    =  c('Very satisfied',
#                     'Satisfied',
#                     'Neutral',
#                     'Dissatisfied',
#                     'Very dissatisfied')
# 
# scale_cols     = c('Neutral'             = 'grey',
#                    'Satisfied'        = 'lightblue',
#                    'Very satisfied'   = 'skyblue3',
#                    'Dissatisfied'      = 'darkorange1',
#                    'Very dissatisfied' = 'brown1')
# 
# leg_order      = c('Very dissatisfied',
#                    'Dissatisfied',
#                    'Neutral',
#                    'Satisfied',
#                    'Very satisfied')
# 
# tsize      = 35
# lab_size   = 8
# leg_size   = 25
# ysize      = 25
# xsize      = 25
# capt_size  = 20
# width      = 0.5
# neg_min    = -1
# ymin       = -1
# ymax       = 1+1*0.5
# high_just  = 1.5
# low_just   = 1.5


# plot_list        = ord3_sub[14]
# survey_data      = Survey_data_complete
# survey_questions = Survey_questions
# sublabel_list    = subquestion_labels
# role_list        = role_list
# count_list       = base_size_list
# 
# neutral          = "Uncertain"
# negative         = 'No'
# 
# graph_scale      =  c('Yes',
#                       "Uncertain",
#                       'No')
# 
# scale_cols       = c('No'        = 'darkorange1',
#                      'Yes'       = 'skyblue3',
#                      "Uncertain" = 'grey')
# 
# leg_order        = c('No',
#                      "Uncertain",
#                      'Yes')
# 
# tsize      = 35
# lab_size   = 8
# leg_size   = 25
# capt_size  = 20
# ysize      = 25
# xsize      = 25
# width      = 0.5
# ymin       = -1.2
# ymax       = 1.2
# high_just  = 0
# low_just   = 1.5



## Create a list of satisfactory plots without sub-lablels ----
## Add a 'label list' argumnet to this
zero_centred_plots_sub <- function(plot_list,    
                                   survey_data, 
                                   survey_questions, 
                                   sublabel_list,
                                   role_list, 
                                   count_list,
                                   neutral, 
                                   negative, 
                                   graph_scale, 
                                   scale_cols,  
                                   leg_order,
                                   tsize,
                                   lab_size,
                                   leg_size,
                                   capt_size,
                                   ysize, 
                                   xsize,
                                   width, 
                                   ymin,
                                   ymax,
                                   high_just,
                                   low_just) {
  
  ## Pipe the list into Lapply
  satisfactory_graphs <- plot_list %>%  ## List is a variable
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      
      ## Question = plot_list[1]
      message("Creating satisfaction graphs for ", Question)
      
      ## Create subsetting variables using lists...
      Label = survey_questions %>%
        dplyr::filter(QuestionNumber == Question) %>% 
        dplyr::select(QuestionText) %>% 
        distinct() %>% .[1, ] %>% .[[1]] %>% 
        str_wrap(., 50)
      
      Sublabels = Question %>%
        paste0(., '_sub_labels') %>%
        sublabel_list[[.]] %>%
        mutate(Sublabels = str_wrap(Sublabels, 40))
      
      QU   <- Sublabels$Cname %>% c()
      
      Role_QU = Question %>%
        as.character() %>%
        paste0(., '_1_Roles') %>%
        role_list[[.]] %>% paste(.,  collapse = " ")
      
      Roles = Question %>%
        as.character() %>%
        paste0(., '_1_Roles') %>%
        role_list[[.]]
      
      Count = Question %>%
        as.character() %>%
        paste0(., '_1_Count') %>%
        count_list[[.]]
      
      ## Create a caption with Roles and counts
      Role_caption <- paste(c(Roles, Count, ' Responses'), collapse = " ") %>% str_wrap(50)
      
      ## test_df if there's data...
      test_df <- survey_data %>% mutate(Role = as.character(Role))     %>% 
        dplyr::select(one_of(QU), Role) %>% dplyr::filter(Role %in% c(Roles)) %>% 
        .[,colSums(is.na(.)) < nrow(.)]
      
      QU <- intersect(names(test_df), QU)
      
      if (nrow(test_df) > 0) {
        
        ## Create a grid choice table and join on the sub-labels
        dat <- survey_data %>% mutate(Role = as.character(Role)) %>% 
          dplyr::select(one_of(QU), Role) %>% dplyr::filter(Role %in% c(Roles)) %>%
          GridChoiceTable(., QU, TRUE) %>% 
          
          ## Table is a variable
          na.omit() %>% dplyr::filter(Percent > 0) %>% 
          left_join(Sublabels) %>%
          dplyr::select(-Count, -Cname) %>%
          rename(., Percentage        = Percent) %>% 
          rename(., !!sym(Question)  := Answer) %>% 
          rename(., Role              = Sublabels) 
        
        ## These objects are temporary
        QID  <- Question
        Cnum <- which(colnames(dat) == QID)
        
        ## Dataset wrangling for negatives and sides - not re-used
        chart_data <- bind_rows(
          dat %>% 
            
            dplyr::filter(dat[Cnum] == neutral) %>%
            mutate(LowNeutral  = -Percentage/2,
                   HighNeutral = Percentage/2) %>% dplyr::select(-Percentage) %>%
            
            gather(!!sym(QID), Percentage, -Role, -!!sym(QID)) %>%
            mutate(!!sym(QID) := neutral),
          
          ## Update this
          dat %>% 
            dplyr::filter(dat[Cnum] != neutral) %>%
            mutate(Percentage = ifelse(grepl(negative, !!sym(QID)), -Percentage, Percentage)) 
          
        ) %>%
          mutate(!!sym(QID) := factor(!!sym(QID), levels = graph_scale)) %>%
          rename(., `Response` = !!sym(QID)) %>% 
          na.omit()
        
        ## Create the postive and negative bars
        highs <- chart_data %>% dplyr::filter(Percentage > 0 | Percentage == 0) %>% na.omit()
        lows  <- chart_data %>% dplyr::filter(Percentage < 0) %>% na.omit()
        
        ## Re-order the lows, as they are negative in order
        lows$Response <- forcats::fct_rev(lows$Response)
        
        ## Create labels for high and low
        high_label <- highs %>% dplyr::filter(Response != neutral) %>% group_by(Role) %>%
          summarise(Percentage = formattable::percent(sum(Percentage), digits = 0))
        
        low_label <- lows %>% dplyr::filter(Response != neutral) %>% group_by(Role) %>%
          summarise(Percentage = formattable::percent(sum(Percentage), digits = 0))
        
        ## Create graph using another function
        print(paste0('Creating graph for table ', QID))
        satisfac_plot = zero_centred_barchart(highs      = highs,
                                              lows       = lows,
                                              high_label = high_label,
                                              low_label  = low_label,
                                              
                                              scale_cols = scale_cols,
                                              leg_order  = leg_order,
                                              
                                              title      = Label,
                                              caption    = Role_caption,
                                              tsize      = tsize,
                                              lab_size   = lab_size,
                                              leg_size   = leg_size,
                                              capt_size  = capt_size,
                                              ysize      = ysize, 
                                              xsize      = xsize,
                                              width      = width, 
                                              
                                              ymin       = ymin,
                                              ymax       = ymax,
                                              high_just  = high_just,
                                              low_just   = low_just)
        
      } else {
        message('Skip graph for ', Question, ' no responses for this question')
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Assign names to list in the same order the graphs were created
  names(satisfactory_graphs) <- plot_list
  names(satisfactory_graphs) <- paste0(names(satisfactory_graphs), "_graph")
  return(satisfactory_graphs)
  
}




# 
# plot_list        = ord5_singlegrid
# survey_data      = Survey_data_complete_LG_roles %>% dplyr::filter(Role == 'EO')
# survey_questions = Survey_questions
# role_list        = EO_role_list
# 
# count_list       = BaseCount %>% 
#   mutate(BaseSize = EO_count)
# 
# neutral          = 'Neutral'
# negative         = 'Dissatisfied|Very Dissatisfied'
# graph_scale      =  c('Very Satisfied',
#                       'Satisfied',
#                       'Neutral',
#                       'Dissatisfied',
#                       'Very Dissatisfied')
# 
# scale_cols       = c('Neutral'           = 'grey',
#                      'Satisfied'         = 'lightblue',
#                      'Very Satisfied'    = 'skyblue3',
#                      'Dissatisfied'      = 'darkorange1',
#                      'Very Dissatisfied' = 'brown1')
# 
# leg_order        = c('Very Dissatisfied',
#                      'Dissatisfied',
#                      'Neutral',
#                      'Satisfied',
#                      'Very Satisfied')
# 
# tsize      = 35
# lab_size   = 8
# leg_size   = 25
# ysize      = 25
# xsize      = 25
# capt_size  = 20
# width      = 0.5
# ymin       = -1
# ymax       = 1+1*0.1
# high_just  = -0.5
# low_just   = 1.5



## Create a list of single satisfactory plots ----
## Add a 'label list' argumnet to this
zero_centred_plots_single_gridchoice <- function(plot_list, 
                                                 survey_data, 
                                                 survey_questions, 
                                                 role_list, 
                                                 count_list,
                                                 neutral, 
                                                 negative, 
                                                 graph_scale, 
                                                 scale_cols, 
                                                 leg_order,
                                                 tsize,
                                                 lab_size,
                                                 leg_size,
                                                 capt_size,
                                                 ysize,
                                                 xsize,
                                                 width,
                                                 ymin,
                                                 ymax,
                                                 high_just,
                                                 low_just) {
  
  ## Pipe the list into Lapply
  satisfactory_graphs <- plot_list %>%  ## List is a variable
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      message("Creating satisfaction graphs for ", Question)
      
      ## Create subsetting variables using lists
      ## Question = ord5_singlegrid[1]
      Role_QU = Question %>%
        as.character() %>%
        paste0(., '_Roles') %>%
        role_list[[.]] %>%
        paste(.,  collapse = " ")
      
      Roles = Question %>%
        as.character() %>%
        paste0(., '_Roles') %>%
        role_list[[.]]
      
      ## Create a label for the plot
      Label = survey_questions %>%
        dplyr::filter(QuestionNumber == Question) %>% 
        dplyr::select(QuestionText) %>% 
        distinct() %>% .[1, ] %>% .[[1]] %>% 
        str_wrap(., 50)
      
      Count = Question %>%
        as.character() %>%
        paste0(., '_Count') %>%
        count_list[[.]]
      
      ## Create a caption with Roles and counts
      Role_caption  <- paste(c(Role_QU, ':', Count, ' Responses'), collapse = " ") %>% str_wrap(50)
      test_df       <- survey_data %>% 
        dplyr::select(starts_with(Question, ignore.case = FALSE)) %>% 
        dplyr::filter(!is.na(.[,1])) 
      
      if (nrow(test_df) > 0) {
        
        ## Create dat object
        dat <- GridChoiceTable(survey_data %>% 
                                 dplyr::filter(Role %in% Roles), Question, TRUE) %>%
          
          ## Table is a variable
          na.omit() %>% dplyr::filter(Percent > 0) %>%
          dplyr::select(-Count, -Cname) %>%
          rename(., Percentage        = Percent) %>%
          rename(., !!sym(Question)  := Answer) %>%
          mutate(Role = paste(Role_QU, collapse = "|"))
        
        ## These objects are temporary
        QID  <- Question
        Cnum <- which(colnames(dat) == QID)
        
        ## Need to filter out the
        ## Dataset wrangling for negatives and sides - not re-used
        chart_data <- bind_rows(
          dat %>%
            
            dplyr::filter(dat[Cnum] == neutral) %>%
            mutate(LowNeutral  = -Percentage/2,
                   HighNeutral = Percentage/2) %>% 
            dplyr::select(-Percentage) %>%
            
            gather(!!sym(QID), Percentage, -Role, -!!sym(QID)) %>%
            mutate(!!sym(QID) := neutral),
          
          ## Update this
          dat %>%
            dplyr::filter(dat[Cnum] != neutral) %>%
            mutate(Percentage = ifelse(grepl(negative, !!sym(QID)), -Percentage, Percentage))
          
        ) %>%
          mutate(!!sym(QID) := factor(!!sym(QID), levels = graph_scale)) %>%
          rename(., `Response` = !!sym(QID)) %>%
          na.omit()
        
        ## Create the postive and negative bars
        highs <- chart_data %>% dplyr::filter(Percentage > 0 | Percentage == 0) %>% na.omit()
        lows  <- chart_data %>% dplyr::filter(Percentage < 0) %>% na.omit()
        
        ## Re-order the lows, as they are negative in order
        lows$Response <- forcats::fct_rev(lows$Response)
        
        ## Create labels for high and low
        high_label <- highs %>% dplyr::filter(Response != neutral) %>% group_by(Role) %>%
          summarise(Percentage = formattable::percent(sum(Percentage), digits = 0))
        
        low_label <- lows %>% dplyr::filter(Response != neutral) %>% group_by(Role) %>%
          summarise(Percentage = formattable::percent(sum(Percentage), digits = 0))
        
        ## Create graph using another function
        print(paste0('Creating graph for table ', QID))
        satisfac_plot = zero_centred_barchart_single(highs      = highs,
                                                     lows       = lows,
                                                     high_label = high_label,
                                                     low_label  = low_label,
                                                     
                                                     scale_cols = scale_cols,
                                                     leg_order  = leg_order,
                                                     
                                                     title      = Label,
                                                     caption      = Role_caption,
                                                     tsize      = tsize,
                                                     lab_size   = lab_size,
                                                     leg_size   = leg_size,
                                                     ysize      = ysize,
                                                     xsize      = xsize,
                                                     capt_size  = capt_size,
                                                     width      = width,
                                                     ymin       = ymin,
                                                     ymax       = ymax,
                                                     high_just  = high_just,
                                                     low_just   = low_just)
        
      } else {
        message('Skip graph for ', Question, ' no responses for this question')
        cat(Question)
      }
      
    }) %>% c()
  
  ## Assign names to list in the same order the graphs were created
  names(satisfactory_graphs) <- plot_list
  names(satisfactory_graphs) <- paste0(names(satisfactory_graphs), "_graph")
  return(satisfactory_graphs)
  
}





## BOXPLOT COMBO PLOTS ==============================================================================


## Create a list of overall boxplots for each group ----
boxplot_list <- function(plot_list, survey_data, columns, 
                         role_list, count_list, agg_level,
                         
                         ## This creates the question number
                         x_var, y_var, v_just, mar, pallette, group_var,
                         box_size,  x_lab,     y_lab, y_lim, leg_pos,
                         lab_size,  lab_angle, border_size) {
  
  ## Pipe the list into Lapply
  boxplot_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      y_lab = y_lab
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_var)
      bp_role  <- respondent_index %>% dplyr::select(one_of(cols)) %>%
        
        gather(key = "Category", value = "Index", -!!as.symbol(group_var)) %>%
        dplyr::filter(Index != 0) %>% 
        mutate(!!group_var := as.factor(!!as.symbol(group_var))) %>%
        mutate(Category = factor(Category, levels = index_plot_columns),
               Index    = as.numeric(Index)) %>% 
        completeFun(., 'Category') %>% na.omit() %>% 
        
        factor_boxplots(., 
                        x_var       = x_var, 
                        y_var       = y_var, 
                        group_var   = group_var,
                        leg_pos     = leg_pos,
                        y_lab       = y_lab, 
                        x_lab       = x_lab,
                        pallette    = pallette,
                        v_just      = v_just,
                        mar         = mar,
                        
                        box_size    = box_size, 
                        y_lim       = y_lim, 
                        lab_angle   = lab_angle,
                        lab_size    = lab_size,
                        border_size = border_size)
      
      
    }) %>% c() 
  
  ## Rename the list items
  names(boxplot_list) <- plot_list
  names(boxplot_list) <- paste0(names(boxplot_list), "_boxplot")
  return(boxplot_list)
  
}





## Create a list of grouped boxplots for each topic ----
boxplot_group_list <- function(plot_list, survey_data, agg_level,
                               columns, levels, v_just, mar,
                               
                               ## This creates the question number
                               x_var, y_var,         pallette, group_vars,
                               group_var,
                               box_size,  x_lab,     y_lab, y_lim, leg_pos,
                               lab_size,  lab_angle, border_size) {
  
  ## Pipe the list into Lapply
  boxplot_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      # x_lab = topic
      y_lab = y_lab
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_vars)
      bp_role  <- survey_data %>% dplyr::select(one_of(cols)) %>%
        
        pivot_longer(cols = group_vars, values_to = "Category") %>%
        rename(Index   = !!as.symbol(topic)) %>% 
        dplyr::filter(Index  != 0) %>% 
        mutate(Category = factor(Category, levels = unique(levels))) %>% 
        completeFun(., 'Category') %>% na.omit() %>% 
        
        factor_boxplots(., 
                        x_var       = x_var, 
                        y_var       = y_var, 
                        group_var   = group_var,
                        leg_pos     = leg_pos,
                        y_lab       = y_lab, 
                        x_lab       = x_lab,
                        pallette    = pallette,
                        v_just      = v_just,
                        mar         = mar,
                        
                        box_size    = box_size, 
                        y_lim       = y_lim, 
                        lab_angle   = lab_angle,
                        lab_size    = lab_size,
                        border_size = border_size)      
      
    }) %>% c() 
  
  ## Rename the list items
  names(boxplot_list) <- plot_list
  names(boxplot_list) <- paste0(names(boxplot_list), "_boxplot")
  return(boxplot_list)
  
}





## SCATTER COMBO PLOTS ==============================================================================


# scatplot_list      = names(respondent_load_scatterplots)[-1] 
# 
# scat_var           = "Venue_Voting_Load"
# index_plot_columns = index_plot_columns
# x_label            = 'Venue Workload (Votes / Projections)'
# 
# axis_size   = 10 
# axis_title  = 25 
# point_col   = "blue"
# point_size  = 3
# labelSize   = 25
# title_size  = 30 
# leg_size    = 20 
# legend_pos  = 'none'

## Names
# scatplot_names = names(respondent_load_scatterplots)[-1]



## Key variable scatterplot list witout grouping variable ---- 
scatter_keyvar_ungroup_list = function(scatplot_list, 
                                       scatplot_names, 
                                       group_var, 
                                       scat_var,
                                       x_label,
                                       index_plot_columns,
                                       axis_size, 
                                       axis_title, 
                                       point_col,
                                       point_size,
                                       labelSize,
                                       title_size, 
                                       leg_size, 
                                       legend_pos) {
  
  ## Pipe the list into lapply
  scatterplot_indexes <- scatplot_list %>%
    
    lapply(function(index) {
      
      #index = scatplot_list[1]
      # label      <- names(index)
      scatter_df <- get(index)
      # title      <- gsub("\\_index_.*", " Workload vs. Index", index) %>% firstup()
      level      <- gsub("\\_index_.*", "", index) %>% firstup()
      
      ## Create table
      message("Creating survey index scatterplots for ", names(index), ' ', nrow(scatter_df), ' rows')
      scatter_plot <- scatter_matrix_keyvar(scat_df <- scatter_df %>% select(one_of(scat_var, index_plot_columns)) %>% 
                                              melt(., scat_var),
                                            
                                            ## This will chop off the last column
                                            scat_var    = scat_var,
                                            axis_size   = axis_size, 
                                            axis_title  = axis_title, 
                                            point_col   = point_col,
                                            point_size  = point_size,
                                            labelSize   = labelSize,
                                            title_size  = title_size, 
                                            leg_size    = leg_size , 
                                            legend_pos  = legend_pos,
                                            ylab        = 'Index Value',
                                            # xlab        = paste0(level, x_label),
                                            xlab        =  x_label,
                                            title       = '')
      
    }) %>% c() 
  
  ## Rename the list items
  names(scatterplot_indexes) <- scatplot_names
  return(scatterplot_indexes)
  
}





## Scatterplot matrix list without grouping variable ---- 
scatter_matrix_ungroup_list = function(scatplot_list, scatplot_names, group_var) {
  
  ## Pipe the list into lapply
  scatterplot_indexes <- scatplot_list %>%
    
    lapply(function(index) {
      
      #index = plot_index_names[3]
      scatter_df <- get(index) %>% dplyr::select(-group_var)
      cols_nums  <- 1:ncol(scatter_df)
      
      ## Create table
      message("Creating survey index scatterplots for ", names(index), ' ', nrow(scatter_df), ' rows')
      scatter_plot <- scatter_matrix_colour(scat_df     = scatter_df,
                                            
                                            ## This will chop off the last column
                                            cols        = cols_nums,
                                            scat_col    = "turquoise4",
                                            
                                            alpha       = 0.5, 
                                            alignPer    = 0.8,
                                            upper_size  = 10, 
                                            lower_size  = 3, 
                                            axis_size   = 20, 
                                            title_size  = 30, 
                                            leg_size    = 30, 
                                            legend_pos  = 'none',
                                            title       = '') 
      
    }) %>% c() 
  
  ## Rename the list items
  names(scatterplot_indexes) <- scatplot_names
  return(scatterplot_indexes)
  
}





## Scatterplot matrix list with grouping variable ---- 
scatter_matrix_group_list = function(scatplot_list, scatplot_names) {
  
  ## Pipe the list into lapply
  scatterplot_indexes <- scatplot_list %>%
    
    lapply(function(index) {
      
      #index = plot_index_names[3]
      scatter_df <- get(index)
      cols_nums  <- 1:ncol(scatter_df)
      
      ## Create table
      message("Creating survey index scatterplots for ", names(index), ' ', nrow(scatter_df), ' rows')
      scatter_plot <- scatter_matrix_colour(scat_df     = scatter_df,
                                            
                                            ## This will chop off the last column
                                            cols        = cols_nums,
                                            scat_col    = "turquoise4",
                                            
                                            alpha       = 0.5, 
                                            alignPer    = 0.8,
                                            upper_size  = 10, 
                                            lower_size  = 3, 
                                            axis_size   = 20, 
                                            title_size  = 30, 
                                            leg_size    = 30, 
                                            legend_pos  = 'none',
                                            title       = '') 
      
    }) %>% c() 
  
  ## Rename the list items
  names(scatterplot_indexes) <- scatplot_names
  return(scatterplot_indexes)
  
}





## MAP COMBO PLOTS ==============================================================================


# topic_list = index_plot_columns
# bound_poly = NSW_LGA_boundaries
# point_data = venue_indexes_spatial %>% sf:::as_Spatial()
# jenk_no    = 5
# rev_leg    = TRUE
# leg_text   = ' Questions Index'
# 
# location_labels = paste(
# 
#   ## Create a list of variables people can see by hovering
#   "RO: ",               point_data$ReturningOffice,     "<br/>",
#   "Venue: ",            point_data$VenueName,           "<br/>",
#   "Votes: ",            point_data$Total_Votes,         "<br/>",
#   "Workload: ",         point_data$Venue_Voting_Load,   "<br/>",
#   "Remoteness: ",       point_data$Remoteness,          "<br/>",
#   "Informal rate: ",    point_data$Informality,         "<br/>",
#   "Satisfaction: ",     point_data$Respondent_Satisfied,    "<br/>",
#   "Disatisfaction: ",   point_data$Respondent_Dissatisfied, "<br/>",
#   sep = "") %>% lapply(htmltools::HTML)

# view_lon   = 151.0000486
# view_lat   = -33.8027653
# zoom_set   = 6
# fillOp     = 0.9
# fillcol    = "blue"
# fillwt     = 2
# textsz     = "13px"


## Create a list of leaflet point maps ----
leaflet_point_map_list <- function(topic_list, 
                                   bound_poly,
                                   point_data,
                                   jenk_no,
                                   rev_leg,
                                   leg_text,
                                   view_lon,
                                   view_lat, 
                                   zoom_set,
                                   fillOp,
                                   fillcol,
                                   fillwt,
                                   textsz) {
  
  ## Pipe the list into Lapply
  map_list <- topic_list %>%
    
    ## Pipe the list into lapply
    ## topic <- topic_list[1]
    lapply(function(topic) {
      
      ## Get the Jenks breaks
      jenks <- getJenksBreaks(point_data[[topic]], jenk_no, subset = NULL)
      bins_topic_index <- c(jenks[1], jenks[2], jenks[3], jenks[4], jenks[5]) %>% 
        unique()
      bins_topic_pal   <- colorBin(palette  = "YlOrBr", 
                                   domain   = bound_poly@data[[topic]], 
                                   na.color = "transparent", 
                                   bins     = bins_topic_index,
                                   reverse  = rev_leg)
      
      location_labels <- paste(
        
        ## Create a list of variables people can see by hovering
        "RO: ",               point_data$ReturningOffice,         "<br/>",
        "Venue: ",            point_data$VenueName,               "<br/>",
        "Votes: ",            point_data$Total_Votes,             "<br/>",
        "Workload: ",         point_data$Venue_Voting_Load,       "<br/>",
        "Remoteness: ",       point_data$Remoteness,              "<br/>",
        "Informal rate: ",    round(point_data$Informality, 3),   "<br/>",
        "Satisfaction: ",     point_data$Respondent_Satisfied,    "<br/>",
        "Disatisfaction: ",   point_data$Respondent_Dissatisfied, "<br/>",
        "Index : ",           round(point_data[[topic]], 3),      "<br/>",
        sep = "") %>% lapply(htmltools::HTML)
      
      ## Get the message
      message('creating leaflet map for ', topic, ' index')
      topic_choropleth <- leaflet(bound_poly) %>%
        
        addTiles()  %>% 
        setView(view_lon, view_lat, zoom = zoom_set) %>%
        
        leaflet::addPolygons(
          weight      = 2,
          fillOpacity = 0.05,
          
          highlight      = highlightOptions(
            color        = "#514d4d",
            fillOpacity  = 0.05,
            bringToFront = FALSE),
          popup = location_labels,
          
          labelOptions = labelOptions(
            style      = list("font-weight" = "normal", padding = "3px 8px"),
            textsize   = "15px",
            direction  = "auto"),
          group        = 'LGA') %>% 
        
        addCircleMarkers(data         = point_data,
                         lat          = point_data$Latitude, 
                         lng          = point_data$Longitude,
                         label        = location_labels, 
                         labelOptions = labelOptions(textsize = 15),
                         radius       = 1.5, ## could manipluate this
                         color        = ~ bins_topic_pal(point_data[[topic]]),
                         
                         
                         ## Group is used to 'facet' the layers
                         group = 'Venue Workloads') %>% 
        
        ## Add legend
        addLegend(pal      =  bins_topic_pal, 
                  values   =~ bound_poly@data[[topic]], 
                  opacity  =  fillOp, 
                  title    = paste0(topic, leg_text), 
                  position = "topright")    
      
    }) %>% c() 
  
  ## Rename the list items
  names(map_list) <- topic_list
  names(map_list) <- paste0(names(map_list), "_polygon_map")
  return(map_list)
  
}


# topic_list = index_plot_columns 
# bound_poly = NSWEC_RO_boundaries
# jenk_no    = 5
# rev_leg    = TRUE
# leg_text   = ' Questions Index'
# 
# returning_topic_labels = paste(
#   
#   ## Create a list of variables people can see by hovering
#   "ReturningOffice: ",  NSWEC_RO_boundaries@data$ReturningOffice,  "<br/>", 
#   "Enrolment: ",        NSWEC_RO_boundaries@data$Enrolment,        "<br/>",
#   "Remoteness: ",       NSWEC_RO_boundaries@data$Remoteness,       "<br/>",
#   "CALD: ",             NSWEC_RO_boundaries@data$CALD_prop,        "<br/>",
#   "ATSI: ",             NSWEC_RO_boundaries@data$Indigenous_prop,  "<br/>",
#   "High SEIFA: ",       NSWEC_RO_boundaries@data$High_SEIFA,       "<br/>",
#   "Low SEIFA: ",        NSWEC_RO_boundaries@data$Low_SEIFA,        "<br/>",
#   "Informal rate: ",    NSWEC_RO_boundaries@data$Informality,      "<br/>",
#   "Venues over prj: ",  NSWEC_RO_boundaries@data$Venues_over_proj, "<br/>",
#   "Satisfaction: ",     NSWEC_RO_boundaries@data$RO_Satisfied,     "<br/>",
#   "Disatisfaction: ",   NSWEC_RO_boundaries@data$RO_Dissatisfied,  "<br/>", 
#   sep = "") %>% lapply(htmltools::HTML)
# 
# view_lon   = 151.0000486
# view_lat   = -33.8027653 
# zoom_set   = 6
# fillOp     = 0.9
# fillcol    = "blue"
# fillwt     = 2
# textsz     = "13px"



## Create a list of leaflet polygon maps ----
leaflet_poly_map_list <- function(topic_list, 
                                  bound_poly,
                                  jenk_no,
                                  rev_leg,
                                  leg_text,
                                  returning_topic_labels,
                                  view_lon,
                                  view_lat, 
                                  zoom_set,
                                  fillOp,
                                  fillcol,
                                  fillwt,
                                  textsz) {
  
  ## Pipe the list into Lapply
  map_list <- topic_list %>%
    
    ## Pipe the list into lapply
    ## topic <- topic_list[3]
    lapply(function(topic) {
      
      ## Get the Jenks breaks
      jenks <- getJenksBreaks(bound_poly@data[[topic]], jenk_no, subset = NULL)
      bins_returning_topic_index <- c(jenks[1], jenks[2], jenks[3], jenks[4], jenks[5]) %>% 
        unique()
      bins_returning_topic_pal   <- colorBin(palette     = "YlOrBr", 
                                             domain   = bound_poly@data[[topic]], 
                                             na.color = "transparent", 
                                             bins     = bins_returning_topic_index,
                                             reverse  = rev_leg)
      ## Get the message
      message('creating leaflet map for ', topic, ' index')
      topic_choropleth <- leaflet(bound_poly) %>%
        
        addTiles()  %>% 
        setView(view_lon, view_lat, zoom = zoom_set) %>%
        
        leaflet::addPolygons( 
          
          fillColor    = ~bins_returning_topic_pal(bound_poly@data[[topic]]), 
          stroke       = TRUE, 
          fillOpacity  = fillOp, 
          color        = fillcol, 
          weight       = fillwt,
          label        = returning_topic_labels,
          labelOptions = labelOptions(
            
            style      = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize   = textsz, 
            direction  = "auto")) %>%
        
        addLegend(pal      =  bins_returning_topic_pal, 
                  values   =~ bound_poly@data[[topic]], 
                  opacity  =  fillOp, 
                  title    = paste0(topic, leg_text), 
                  position = "topright")    
      
    }) %>% c() 
  
  ## Rename the list items
  names(map_list) <- topic_list
  names(map_list) <- paste0(names(map_list), "_polygon_map")
  return(map_list)
  
}





## Create a list of leaflet polygon maps with indexes ----
leaflet_poly_map_list_index <- function(topic_list, 
                                        bound_poly,
                                        jenk_no,
                                        rev_leg,
                                        leg_text,
                                        view_lon,
                                        view_lat, 
                                        zoom_set,
                                        fillOp,
                                        fillcol,
                                        fillwt,
                                        textsz) {
  
  ## Pipe the list into Lapply
  map_list <- topic_list %>%
    
    ## Pipe the list into lapply
    ## topic <- topic_list[3]
    lapply(function(topic) {
      
      ## Get the Jenks breaks
      jenks <- getJenksBreaks(bound_poly@data[[topic]], jenk_no, subset = NULL)
      bins_returning_topic_index <- c(jenks[1], jenks[2], jenks[3], jenks[4], jenks[5]) %>% 
        unique()
      bins_returning_topic_pal   <- colorBin(palette     = "YlOrBr", 
                                             domain   = bound_poly@data[[topic]], 
                                             na.color = "transparent", 
                                             bins     = bins_returning_topic_index,
                                             reverse  = rev_leg)
      
      returning_topic_labels <- paste(
        
        ## Create a list of variables people can see by hovering
        "ReturningOffice: ",  bound_poly@data$ReturningOffice,  "<br/>", 
        "Enrolment: ",        bound_poly@data$Enrolment,        "<br/>",
        "Remoteness: ",       bound_poly@data$Remoteness,       "<br/>",
        "CALD: ",             bound_poly@data$CALD_prop,        "<br/>",
        "ATSI: ",             bound_poly@data$Indigenous_prop,  "<br/>",
        "High SES: ",         bound_poly@data$High_SEIFA_prop,  "<br/>",
        "Low SES: ",          bound_poly@data$Low_SEIFA_prop,           "<br/>",
        "Informal rate: ",    round(bound_poly@data$Informality, 3),    "<br/>",
        "Venues over prj: ",  bound_poly@data$Venues_over_proj,         "<br/>",
        "Satisfaction: ",     bound_poly@data$Respondent_Satisfied,     "<br/>",
        "Disatisfaction: ",   bound_poly@data$Respondent_Dissatisfied,  "<br/>", 
        "Index: ",            round(bound_poly@data[[topic]], 3),       "<br/>",
        sep = "") %>% lapply(htmltools::HTML)
      
      ## Get the message
      message('creating leaflet map for ', topic, ' index')
      topic_choropleth <- leaflet(bound_poly) %>%
        
        addTiles()  %>% 
        setView(view_lon, view_lat, zoom = zoom_set) %>%
        
        leaflet::addPolygons( 
          
          fillColor    = ~bins_returning_topic_pal(bound_poly@data[[topic]]), 
          stroke       = TRUE, 
          fillOpacity  = fillOp, 
          color        = fillcol, 
          weight       = fillwt,
          label        = returning_topic_labels,
          labelOptions = labelOptions(
            
            style      = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize   = textsz, 
            direction  = "auto")) %>%
        
        addLegend(pal      =  bins_returning_topic_pal, 
                  values   =~ bound_poly@data[[topic]], 
                  opacity  =  fillOp, 
                  title    = paste0(topic, leg_text), 
                  position = "topright")    
      
    }) %>% c() 
  
  ## Rename the list items
  names(map_list) <- topic_list
  names(map_list) <- paste0(names(map_list), "_polygon_map")
  return(map_list)
  
}






############################################################################################
############################################ ---TBC---- ####################################
############################################################################################