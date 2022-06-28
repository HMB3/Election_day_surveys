############################################################################################
################################# -----TABLE FUNCTIONS ---- ################################
############################################################################################


## GROUP TABULATE FUNCTIONS =================================================================


## Create a table for each sub_label ----
sub_questions_labels <- function(survey_data, 
                                 question_list, 
                                 table_suffix) {
  
  ## Pipe the list into Lapply
  sub_question_df_list <- question_list %>%
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      
      ## Question = question_list[1]
      message("Creating table of sub questions for ", Question)
      
      ## Create cross-tabulated table
      ## This assumes the data is in the survey Monkey format
      test_df <- survey_data %>% dplyr::filter(QuestionNumber == Question) %>% nrow()
      if (test_df > 0) {
        
        sub_quetion_df <- survey_data %>% dplyr::filter(QuestionNumber == Question) %>%
          
          ## Just get the columns we need, and rename
          rename(Cname     = QuestionNumber_sub,
                 Sublabels = QuestionText_sub) %>% 
          ungroup() %>% 
          dplyr::mutate(Cname = gsub(Cname, pattern = " _", replacement = "_")) %>% 
          dplyr::select(Cname, Sublabels)
        
      } else {
        message('Skip table for ', Question, ' no responses for this question')
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Rename the list items
  names(sub_question_df_list)  <- question_list
  names(sub_question_df_list)  <- paste0(names(sub_question_df_list), table_suffix)
  return(sub_question_df_list)
  
}


## It would seem that is.nan doesn't actually have a method for data frames, 
## unlike is.na. So, let's fix that!
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


# crosstab_list = ord5_likertise_nosub
# survey_data   = Survey_data_complete
# calc_percent  = TRUE
# table_suffix  = '_table_data'



## Create a list of cross-tabulated tables ----
crosstab_percent <- function(crosstab_list, 
                             survey_data, 
                             table_suffix,
                             calc_percent) {
  
  ## Pipe the list into Lapply
  crosstab_percent_list <- crosstab_list %>%
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## x = crosstab_list[1]
      message("Creating table for ", x)
      
      ## Create cross-tabulated table
      ## This assumes the data is in the survey Monkey format
      test_df <- survey_data %>% dplyr::filter(!is.na(!!as.symbol(x))) %>% nrow()
      if (test_df > 0) {
        
        #create a temporary data frame
        dat <- survey_data %>% 
          dplyr::filter(!is.na(!!as.symbol(x))) 
        
        cross_percent <- CrossTab(dat, x, 'Role', calc_percent) %>%
          dplyr::filter(Count    > 0) %>% 
          rename(Response = x) %>% 
          
          ## Now filter out the stuff we don't want
          dplyr::filter(Response %in% as.character(unique(survey_data %>% .[[sym(x)]]))) %>% 
          dplyr::filter(Role     %in% as.character(unique(survey_data %>% .$Role))) %>% 
          
          ## Group by Role and Sum
          group_by(Response,  Role) %>% summarise(Count = sum(Count),
                                                  Percentage = sum(Percentage)) %>% 
          
          ## This bit makes the % a formattable number
          dplyr::mutate_at(vars(-Count), funs(percent_formatter)) %>%
          dplyr::mutate(Percentage = formattable::percent(Percentage)) %>%
          na.omit()
        
        ## Count the Roles separately
        Role_count <- cross_percent %>% group_by(Role) %>% summarise(Count = sum(Count))
        
        ## Combine the % table with the Role count
        cross_percent_role_count <- cross_percent %>% 
          spread(Response, Percentage) %>% group_by(Role) %>% 
          summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
          select(-Count) %>% 
          left_join(., Role_count, by = "Role")
        
      } else {
        message('Skip Table for ', x, ' no responses for this question')
      }
      
    }) %>% c() 
  
  ## Rename the list items
  names(crosstab_percent_list) <- crosstab_list
  names(crosstab_percent_list) <- paste0(names(crosstab_percent_list), table_suffix)
  return(crosstab_percent_list)
  
}


# singlechoice_list = ord3_questions_sub 
# survey_data       = Survey_data_complete_LG_roles 
# calc_percent      = TRUE


## Create a list of singlechoice tables with percent ----
singlechoice_percent <- function(singlechoice_list, 
                                 survey_data, 
                                 calc_percent,
                                 all_questions) {
  
  ## Pipe the list into Lapply
  singlechoice_percent_df <- singlechoice_list %>%
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      
      ## Question = singlechoice_list[1]
      message("Creating table for ", Question)
      
      ## Create cross-tabulated table
      test_df <- survey_data %>% 
        dplyr::filter(!is.na(!!sym(Question))) %>% nrow()
      
      if(test_df > 0) {
        
        singlechoice_percent_tab <- SingleChoiceTable(survey_data %>% 
                                                        dplyr::filter(!is.na(!!sym(Question))), 
                                                      Question, calc_percent) %>%
          
          ## This bit makes the % a formattable number
          arrange(-Percentage) %>% 
          na.omit() %>% 
          
          ## Not everything can be liktertised
          ## Do this in the RMD?
          rename(Response = Question) %>% 
          dplyr::mutate(Question_number = Question) %>% 
          
          left_join(., dplyr::select(all_questions, 
                                     QuestionNumber_sub,
                                     QuestionText,
                                     QuestionText_sub), 
                                     by = c('Question_number' = 'QuestionNumber_sub')) %>% 
          dplyr::select(Question_number, QuestionText, QuestionText_sub, Response, Count, Percentage)
        
        
      } else {
        message('Skip Table for ', Question, ' no responses for this question')
        cat(Question)
      }
      
    }) %>% bind_rows() 
  
  ## Rename the list items
  return(singlechoice_percent_df)
  
}



# likertise_list  = ord5_likertise_sub    
# survey_data     = Survey_data_complete 
# sublabel_list   = subquestion_labels
# role_list       = role_list
# table_suffix    = '_table_data'


## Create a list of gridchoice tables with percent ----
GridChoiceTable_tables_sub <- function(likertise_list,    
                                       survey_data,
                                       sublabel_list, 
                                       role_list,
                                       table_suffix) {
  
  ## Pipe the list into Lapply
  satisfaction_tables <- likertise_list %>%  ## List is a variable
    
    ## Pipe the list into lapply
    lapply(function(Question) {
      
      ## Question = ord5_likertise_sub[1]
      message("Creating satisfaction tables for ", Question)
      
      Sublabels = Question %>%
        paste0(., '_sub_labels') %>%
        sublabel_list[[.]] %>% 
        dplyr::mutate(Sublabels = str_wrap(Sublabels, 40))
      
      QU   <- Sublabels$Cname %>% c()
      
      Role_QU = Question %>%
        as.character() %>%
        paste0(., '_1_Roles') %>%
        role_list[[.]]
      
      ## Create a grid choice table and join on the sub-labels
      test_df <- survey_data %>% dplyr::mutate(Role = as.character(Role)) %>% 
        dplyr::select(one_of(QU), Role) %>% dplyr::filter(Role %in% c(Role_QU)) %>%
        nrow()
      
      if (test_df > 0) {
        
        satisfaction_table <- GridChoiceTable(survey_data %>% 
                                                dplyr::filter(Role %in% Role_QU), 
                                              QID     = QU, 
                                              Percent = TRUE) %>% 
          na.omit() %>% 
          
          ## Join on the grid-choice sub-labels
          dplyr::filter(Percent > 0) %>% 
          left_join(Sublabels) %>%
          dplyr::select(-Cname) %>%
          rename(., Percentage        = Percent) %>% 
          rename(., !!sym(Question)  := Answer) %>% 
          rename(., Item              = Sublabels) %>% 
          
          ## Covert percentages to numeric
          dplyr::mutate_at(vars(-Count), funs(percent_formatter)) %>% 
          dplyr::mutate(Percentage = formattable::percent(Percentage)) 
        
        ## Count the Roles separately - the questions with < 3 responses are tallying properly
        Item_count <- satisfaction_table %>% group_by(Item) %>% summarise(Count = sum(Count))
        
        ## Combine the % table with the Role count
        satisfaction_table_count <- satisfaction_table %>% 
          
          ## Just in case some categories were not filled in, replace NA with 0
          rename(Response = !!sym(Question)) %>% 
          spread(Response, Percentage) %>% replace(is.na(.), 0)          %>% 
          group_by(Item) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
          
          ## Replace count with the real count
          select(-Count) %>% 
          left_join(., Item_count, by = "Item")
        
        
      } else {
        message('Skip Table for ', Question, ' no responses for this question')
        cat(Question)
      }
      
    }) %>% c() 
  
  ## Assign names to list in the same order the graphs were created
  names(satisfaction_tables) <- likertise_list
  names(satisfaction_tables) <- paste0(names(satisfaction_tables), table_suffix)
  return(satisfaction_tables)
  
}



## INDEX FUNCTIONS =======================================================================


# survey_indexes    = index_names
# survey_numeric    = SurveyData_numeric_questions
# questions_LUT     = Survey_questions
# election_data     = venue_workload_satisfaction
# demography_cols   = demography_cols
# index_cols        = response_index_cols


## Calcualate response-level index
respondent_indexes <- function(survey_indexes, 
                               survey_numeric,
                               questions_LUT,
                               demography_cols,
                               max_score,
                               election_data,
                               index_cols) {
  
  response_index_df <- survey_indexes %>%            
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## Get survey columns for business units
      ## Problems for Venues?
      ## x = survey_indexes[9]
      message("Creating response-level index for ", x)
      unit_cols = get(x)
      
      ## Create question
      rowindex <- x %>% 
        gsub('_subset', '', .,)
      
      ## Get names of the just the numeric columns
      numeric_cols <- survey_numeric    %>% 
        dplyr::select(., unit_cols)     %>% 
        dplyr::select_if(., is.numeric) %>% names()
      
      question_score_df <- questions_LUT %>%
        
        dplyr::filter(QuestionNumber_sub %in% numeric_cols) %>% 
        dplyr::mutate(score_total = case_when(Response_type == 'Ord5' ~ 5, 
                                       Response_type == 'Ord3' ~ 3,
                                       TRUE ~ NA_real_))
      question_score_total <- sum(question_score_df$score_total)
      
      ## Create matrix of questions and totals
      question_total_matrix <- question_score_df %>% ungroup() %>% select(QuestionNumber_sub, score_total) %>% 
        pivot_wider(names_from = QuestionNumber_sub, values_from = score_total) %>% 
        slice(rep(1, nrow(survey_numeric))) %>% as.matrix()
      
      question_response_matrix <- survey_numeric %>%                                   
        
        ## For each respondent, take the row sums of all questions of that index / no. of questions
        dplyr::select(numeric_cols) %>% as.matrix()
      
      ## Combine the two matrices
      question_total_matrix[is.na(question_response_matrix)] <- NA
      question_totals <- rowSums(question_total_matrix, na.rm = TRUE)
      
      ## Convert back to df, sum each row, and join to question response matrix for that index
      
      ## Create table of index values for each Respondent
      table_index <- survey_numeric %>%                                   
        
        ## For each respondent, take the row sums of all questions of that index / no. of questions
        dplyr::select(`Respondent ID`, VenueName, Role, Status, demography_cols, numeric_cols) %>%                    
        dplyr::mutate(!!rowindex :=  rowSums(dplyr::select(., numeric_cols), na.rm = TRUE)/question_totals) %>%   
        dplyr::select(`Respondent ID`, VenueName, Role, Status, demography_cols, rowindex) %>%                 
        as.data.frame() %>% dplyr::mutate(VenueName = as.factor(VenueName))
      
    }) %>%
    
    ## Finally, bind the context columns on to the index
    ## left_join is not working. Maybe because of the NA values 
    reduce(merge) %>% 
    left_join(., election_data, by = c("VenueName")) %>%
    distinct(., `Respondent ID`, .keep_all  = TRUE)  
  
  names(response_index_df) <- gsub(x = names(response_index_df), pattern = "SURV_Index_", replacement = "")
  names(response_index_df) <- gsub(x = names(response_index_df), pattern = "_questions",  replacement = "")
  
  ## Re-order columns
  response_index_df <- dplyr::select(response_index_df, index_cols) 
  
  ## Return a dataframe of the response-level indexes
  return(response_index_df)
  
}





# survey_indexes   = index_names
# survey_numeric   = SurveyData_numeric_questions
# questions_LUT    = Survey_questions
# group_level      = 'VenueName'
# election_data    = venue_workload_satisfaction
# index_cols       = venue_index_cols


## Response-level index
aggregate_index_sum <- function(survey_indexes, 
                                survey_numeric,
                                group_level,
                                questions_LUT,
                                election_data,
                                index_cols) {
  
  aggregate_index_df <- survey_indexes %>%  
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## x = survey_indexes[1]
      message("Creating Venue-level index for ", x)
      unit_cols = get(x)
      
      ## Create question
      rowindex <- x %>% 
        gsub('_subset', '', .,)
      
      ## Get names of the just the numeric columns
      numeric_cols <- survey_numeric    %>% 
        dplyr::select(., unit_cols)     %>% 
        dplyr::select_if(., is.numeric) %>% names()
      
      question_score_df <- questions_LUT %>%
        
        dplyr::filter(QuestionNumber_sub %in% numeric_cols) %>% 
        dplyr::mutate(score_total = case_when(Response_type == 'Ord5' ~ 5, 
                                       Response_type == 'Ord3' ~ 3,
                                       TRUE ~ NA_real_))
      question_score_total <- sum(question_score_df$score_total)
      
      table_sums <- survey_numeric  %>%
        group_by(!!sym(group_level)) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
      
      ## Create matrix of questions and totals
      question_total_matrix <- question_score_df %>% ungroup() %>% select(QuestionNumber_sub, score_total) %>% 
        pivot_wider(names_from = QuestionNumber_sub, values_from = score_total) %>% 
        slice(rep(1, nrow(table_sums))) %>% as.matrix()
      
      question_response_matrix <- survey_numeric %>% 
        
        group_by(!!sym(group_level)) %>% 
        
        dplyr::select(!!sym(group_level), numeric_cols) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>% as.matrix()
      
      ## Combine the two matrices
      question_total_matrix[is.na(question_response_matrix)] <- NA
      question_totals <- rowSums(question_total_matrix, na.rm = TRUE)
      
      ## Create table of index values for each Venue
      table_index <- survey_numeric  %>%
        group_by(!!sym(group_level)) %>% 
        
        dplyr::select(!!sym(group_level), numeric_cols) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)    %>%
        dplyr::mutate(!!rowindex := rowSums(dplyr::select(., numeric_cols), na.rm = TRUE)/question_score_total) %>% 
        dplyr::select(!!sym(group_level), rowindex)      %>% 
        dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0)) %>% 
        as.data.frame()
      
    }) %>%
    
    ## Finally, bind all the venue indices together
    reduce(merge) %>%
    
    left_join(., election_data, by = c(group_level))  %>% 
    distinct(., !!sym(group_level), .keep_all = TRUE) %>% 
    
    ## Filter out NA Venues
    dplyr::filter(!is.na(!!sym(group_level)))
  
  ## Rename indexes
  names(aggregate_index_df) <- gsub(x = names(aggregate_index_df), pattern = "SURV_Index_", replacement = "")
  names(aggregate_index_df) <- gsub(x = names(aggregate_index_df), pattern = "_questions",  replacement = "")
  
  ## Re-order columns
  aggregate_index_df <- aggregate_index_df %>% dplyr::select(index_cols) 
  
  ## Return a dataframe of the response-level indexes
  return(aggregate_index_df)
  
}




# survey_indexes   = index_names 
# survey_numeric   = SurveyData_numeric_all_questions %>% dplyr::filter(Role == "RO")
# group_level      = 'VenueName'
# election_data    = venue_workload_satisfaction
# index_cols       = venue_index_cols


## Response-level index
aggregate_indexes <- function(survey_indexes, 
                              survey_numeric,
                              group_level,
                              election_data,
                              index_cols) {
  
  aggregate_index_df <- survey_indexes %>%  
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## x = survey_indexes[10]
      message("Creating Venue-level index for ", x)
      unit_cols = get(x)
      
      ## Create question
      rowindex <- x %>% 
        gsub('_subset', '', .,)
      
      ## Get names of the numeric columns
      numeric_cols <- survey_numeric %>% 
        dplyr::select(., unit_cols)  %>% 
        select_if(., is.numeric) %>% names()
      
      ## Create table of index values for each Venue
      table_index <- survey_numeric  %>%
        group_by(!!sym(group_level)) %>% 
        
        dplyr::select(!!sym(group_level), numeric_cols) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)    %>%
        dplyr::mutate(!!rowindex := rowMeans(dplyr::select(., numeric_cols), na.rm = TRUE)) %>% 
        dplyr::select(!!sym(group_level), rowindex)      %>% 
        dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0)) %>% 
        as.data.frame()
      
    }) %>%
    
    ## Finally, bind all the venue indices together
    reduce(merge) %>%
    
    left_join(., election_data, by = c(group_level))  %>% 
    distinct(., !!sym(group_level), .keep_all = TRUE) %>% 
    
    ## Filter out NA Venues
    dplyr::filter(!is.na(!!sym(group_level)))
  
  ## Rename indexes
  names(aggregate_index_df) <- gsub(x = names(aggregate_index_df), pattern = "SURV_Index_", replacement = "")
  names(aggregate_index_df) <- gsub(x = names(aggregate_index_df), pattern = "_questions",  replacement = "")
  
  ## Re-order columns
  aggregate_index_df <- aggregate_index_df %>% dplyr::select(index_cols) 
  
  ## Return a dataframe of the response-level indexes
  return(aggregate_index_df)
  
}




## Response-level index
aggregate_role_indexes <- function(survey_indexes, 
                                   survey_numeric,
                                   agg_level,
                                   group_level,
                                   election_data,
                                   index_cols) {
  
  aggregate_index_df <- survey_indexes %>%  
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## x = survey_indexes[10]
      message("Creating Venue-level index for ", x)
      unit_cols = get(x)
      
      ## Create question
      rowindex <- x %>% 
        gsub('_subset', '', .,)
      
      ## Get names of the numeric columns
      numeric_cols <- survey_numeric %>% 
        dplyr::select(., unit_cols)  %>% 
        select_if(., is.numeric) %>% names()
      
      table_index <- survey_numeric  %>%
        group_by(!!sym(group_level)) %>% 
        
        dplyr::select(!!sym(group_level), numeric_cols) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)    %>%
        dplyr::mutate(!!rowindex := rowMeans(dplyr::select(., numeric_cols), na.rm = TRUE)) %>% 
        dplyr::select(!!sym(group_level), rowindex)      %>%
        dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0)) %>% 
        as.data.frame()
      
    }) %>%
    
    ## Finally, bind all the venue indices together
    reduce(merge)
  
  ## Rename indexes
  names(aggregate_index_df) <- gsub(x = names(aggregate_index_df), pattern = "SURV_Index_", replacement = "")
  names(aggregate_index_df) <- gsub(x = names(aggregate_index_df), pattern = "_questions",  replacement = "")
  
  ## Re-order columns
  # aggregate_index_df <- aggregate_index_df %>% dplyr::select(index_cols) 
  
  ## Return a dataframe of the response-level indexes
  return(aggregate_index_df)
  
}




## STATS TABLES ==============================================================================


# plot_list   = index_plot_columns
# survey_data = respondent_index_ro_join
# columns     = index_plot_columns
# agg_level   = "Respondent"
# group_var   = "Role"



## Create a list of overall boxplots for each group ----
anova_list <- function(plot_list, 
                       survey_data, 
                       columns, 
                       group_var) {
  
  ## Pipe the list into Lapply
  anova_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_var)
      
      ## Run an Anova of index vs Role for each topic
      anova_role  <- respondent_index %>% dplyr::select(one_of(cols)) %>%
        
        gather(key = "Category", value = "Index", -!!as.symbol(group_var)) %>%
        dplyr::filter(Index != 0) %>% 
        dplyr::mutate(!!group_var := as.factor(!!as.symbol(group_var))) %>%
        dplyr::mutate(Category = factor(Category, levels = index_plot_columns),
               Index    = as.numeric(Index)) %>% 
        completeFun(., 'Category') %>% na.omit() %>% 
        
        ## 
        aov(Index ~ Role,  data = .)
      
      resid_plot <- data_frame(
        fitted  = predict(anova_role),
        residual = residuals(anova_role)) %>%
        
        ## and then plot points and a smoothed line
        ggplot(aes(fitted, residual)) +
        geom_point() +
        geom_hline(yintercept = 0, col = "red")
      
      tukey_test  <- TukeyHSD(anova_role) %>% .[[1]]
      tukey_table <- tukey_test %>% as_tibble()
      tukey_table$compare = rownames(tukey_test)
      tukey_table <- dplyr::select(tukey_table, compare, everything())
      
      anova_results <- list(anova_role, resid_plot, tukey_table)
      names(anova_results) <- c(paste0(topic, '_anova_role'), 
                                paste0(topic, '_resid_plot'),
                                paste0(topic, '_tukey_test'))
      
      return(anova_results)
      
    })
  
  ## Rename the list items
  names(anova_list) <- plot_list
  names(anova_list) <- paste0(names(anova_list), "_anova")
  return(anova_list)
  
}



# plot_list   = index_plot_columns
# survey_data = respondent_index_ro_join
# columns     = index_plot_columns
# agg_level   = "Respondent"
# 
# x_var       = "Category"
# group_var   = "Category"
# group_vars  = c("Gender", "Languages", "Disability", "Indigenous")
# levels      = c("Female",         "Male", "Other",
#                 "Multilingual",   "English Only",   
#                 "No Disability",  "Disability",
#                 "Indigenous",     "Non-Indigenous")



## Create a list of grouped boxplots for each topic ----
anova_group_list <- function(plot_list, 
                             survey_data, 
                             columns, 
                             group_var,
                             group_vars,
                             levels) {
  
  ## Pipe the list into Lapply
  anova_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      ## Check the dimensions of the data
      message('running anova for demographics of', topic)
      cols     <- c(topic, group_vars)
      anova_demog <- survey_data %>% dplyr::select(one_of(cols)) %>%
        
        pivot_longer(cols = group_vars, values_to = "Category") %>%
        rename(Index   = !!as.symbol(topic)) %>% 
        dplyr::filter(Index  != 0) %>% 
        dplyr::mutate(Category = factor(Category, levels = unique(levels))) %>% 
        completeFun(., 'Category') %>% na.omit()  %>% 
        
        ## 
        aov(Index ~ Category,  data = .)
      
      resid_plot <- data_frame(
        fitted  = predict(anova_demog),
        residual = residuals(anova_demog)) %>%
        
        ## and then plot points and a smoothed line
        ggplot(aes(fitted, residual)) +
        geom_point() +
        geom_hline(yintercept = 0, col = "red")
      
      tukey_test  <- TukeyHSD(anova_demog) %>% .[[1]]
      tukey_table <- tukey_test %>% as_tibble()
      tukey_table$compare = rownames(tukey_test)
      tukey_table <- dplyr::select(tukey_table, compare, everything())
      
      anova_results <- list(anova_demog, resid_plot, tukey_table)
      names(anova_results) <- c(paste0(topic, '_anova_role'), 
                                paste0(topic, '_resid_plot'),
                                paste0(topic, '_tukey_test'))
      
      return(anova_results)
      
    })
  
  ## Rename the list items
  names(anova_list) <- plot_list
  names(anova_list) <- paste0(names(anova_list), "_anova")
  return(anova_list)
  
}





# analysis_data = NSWEC_RO_boundaries %>% sf:::as_Spatial()
# 
# var_list      = c("Informality",         
#                   "Venues_over_proj", 
#                   "RO_Satisfied",
#                   "RO_Dissatisfied", 
#                   
#                   "Counting",
#                   "IS",  
#                   "Logistics",
#                   "Operations",     
#                   "Recruitment",
#                   "Staffing",
#                   "Training",     
#                   "Venues",     
#                   "WHS")
# 
# moran_test     = "greater"
# randomisations = 50000
# seed           = 131



## Create a list of Moran's I output for each topic ----
morans_i_variable_tests <- function(var_list, 
                                    analysis_data,
                                    moran_test,
                                    randomisations, 
                                    seed) {
  
  ## Pipe the list into Lapply
  morans_list <- var_list %>%
    
    ## Pipe the list into lapply
    ## var <-  var_list[1]
    lapply(function(var) {
      
      ## Check the dimensions of the data
      message('running morans I for ', var)
      
      ## Step 1: Define neighboring polygons
      nb <- poly2nb(analysis_data, queen = TRUE)
      
      ## Step 2: Assign weights to the neighbors
      lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
      
      ## Step 3 Compute the (weighted) neighbor mean values
      var.lag <- lag.listw(lw, analysis_data[[var]] %>% as.numeric(), NAOK = TRUE)
      
      ## 
      morans_plot <- data_frame(
        lag  = var.lag,
        dat  = analysis_data[[var]] %>% as.numeric()) %>%
        
        ## and then plot points and a smoothed line
        ggplot(aes(dat, lag)) +
        geom_point() +
        geom_smooth(method = 'lm', col = "red") +
        labs(x = var, y = paste0(var, ' lag'))
      
      ## Run a Moran's I using MC simulations
      MC <- moran.mc(x             = analysis_data[[var]] %>% as.numeric(), 
                     listw         = lw, 
                     nsim          = randomisations, 
                     zero.policy   = TRUE,
                     alternative   = "greater", 
                     na.action     = na.omit)
      
      MC_table <- data_frame(
        Variable = var,
        Morans_I = MC[1][[1]],
        Pval     = MC[3][[1]])
      
      ## Plot randomisations
      set.seed(seed)
      analysis_data$rand1 <- sample(analysis_data[[var]] %>% as.numeric(), 
                                    length(analysis_data[[var]]), replace = FALSE)
      
      analysis_data$rand2 <- sample(analysis_data[[var]] %>% as.numeric(), 
                                    length(analysis_data[[var]]), replace = FALSE)
      
      analysis_data$rand3 <- sample(analysis_data[[var]] %>% as.numeric(), 
                                    length(analysis_data[[var]]), replace = FALSE)
      
      
      ## Plot the observed distribution of Informality against the random distributions
      var_rand_plot <- tm_shape(analysis_data) + 
        
        tm_fill(col = c(var, "rand1", "rand2", "rand3"),
                style = "quantile", n = 8, 
                palette = "Greens", legend.show = FALSE) +
        tm_facets(nrow = 1)
      
      morans_results <- list(morans_plot, MC, MC_table, var_rand_plot)
      names(morans_results) <- c(paste0(var, '_lag_plot'), 
                                 paste0(var, '_MC_simulation'),
                                 paste0(var, '_MC_table'),
                                 paste0(var, '_random_plots'))
      
      return(morans_results)
      
    })
  
  ## Rename the list items
  names(morans_list) <- var_list
  names(morans_list) <- paste0(names(morans_list), "_morans")
  return(morans_list)
  
}



# analysis_data = test_data
# formula_list  = c('Respondent_Satisfied ~ Venue_Voting_Load',
#                   'Respondent_Dissatisfied ~ Venue_Voting_Load',
#                   'Counting ~ Venue_Voting_Load',
#                   'IS ~ Venue_Voting_Load',
#                   'Counting ~ Venue_Voting_Load',
#                   'Logistics ~ Venue_Voting_Load',
#                   'Operations ~ Venue_Voting_Load',
#                   'Recruitment ~ Venue_Voting_Load',
#                   'Staffing ~ Venue_Voting_Load',
#                   'Training ~ Venue_Voting_Load',
#                   'Venues ~ Venue_Voting_Load',
#                   'WHS ~ Venue_Voting_Load')
# adapt          = TRUE
# clus        = cl



## Create a list of GWR analyses for each topic ----
gwr_formula_analyses <- function(formula_list, 
                                 analysis_data,
                                 adapt,
                                 clust) {
  
  ## Pipe the list into Lapply
  gwr_list <- formula_list %>%
    
    ## Pipe the list into lapply
    ## formula <-  formula_list[3]
    lapply(function(formula) {
      
      exp_var <- gsub( " .*$", "", formula)
      
      if(!all(is.na(analysis_data[[exp_var]]))) {
        
        ## Check the dimensions of the data
        message('running gwr for ', formula)
        formula_data <- analysis_data %>% drop_na(exp_var)
        
        ## Step 1: Define GWR bandwidth
        gwr_bandwidth <- gwr.sel(as.Formula(formula),
                                 
                                 ## Coordinates
                                 data   = formula_data, 
                                 coords = cbind(formula_data$Latitude, 
                                                formula_data$Longitude), 
                                 
                                 ## Adapative bandwidth
                                 adapt  = TRUE)
        
        ## Step 2: run GWR formula
        gwr_model <- gwr(as.Formula(formula),
                         
                         data   = formula_data, 
                         coords = cbind(formula_data$Latitude, 
                                        formula_data$Longitude), 
                         
                         adapt     = gwr_bandwidth, 
                         hatmatrix = TRUE, 
                         se.fit    = TRUE,
                         cl        = clust)
        
        ## create global R2
        globalR2 <- (1 - (gwr_model$results$rss / gwr_model$gTSS))
        
        # get spatial spatialpolygondataframe from regression results + 
        # convert it into sf object. The spatial object brings the regressions 
        # results within it's data component
        sp <- LG21_GWR_model$SDF
        sp$Formula <- formula
        sf <- st_as_sf(sp) 
        
        # map local R2
        gwr_R2_map <- ggplot() + geom_sf(data = sf, aes(fill = localR2)) +
          coord_sf() +
          ggtitle(paste0("GWR : ", formula)) +
          labs(subtitle = paste("Global R2:", round(globalR2, 2)))
        
        # map residuals gwr.e
        gwr_resid_map <- ggplot() + geom_sf(data = sf, aes(fill = gwr.e)) +
          coord_sf() +
          ggtitle(paste0("GWR : ", formula)) +
          labs(subtitle = paste("Residuals"))
        
        
        gwr_results <- list(gwr_model, globalR2, sp, gwr_R2_map, gwr_resid_map)
        names(gwr_results) <- c(paste0(formula, '_GWR'), 
                                paste0(formula, '_globalR2'),
                                paste0(formula, '_GWR_spdf'),
                                paste0(formula, '_GWR_R2_map'),
                                paste0(formula, '_GWR_resid_map'))
        
        return(gwr_results)
        
      } else {
        message('Skip GWR for ', formula, ' insufficient data for this question')
        cat(formula)
      }
      
    })
  
  ## Rename the list items
  names(gwr_list) <- formula_list
  names(gwr_list) <- paste0("gwr formula : ", names(gwr_list))
  return(gwr_list)
  
}




# analysis_data = test_data
# formula_list  = c('Respondent_Satisfied ~ Venue_Voting_Load',
#                   'Respondent_Dissatisfied ~ Venue_Voting_Load',
#                   'Counting ~ Venue_Voting_Load',
#                   'IS ~ Venue_Voting_Load',
#                   'Counting ~ Venue_Voting_Load',
#                   'Logistics ~ Venue_Voting_Load',
#                   'Operations ~ Venue_Voting_Load',
#                   'Recruitment ~ Venue_Voting_Load',
#                   'Staffing ~ Venue_Voting_Load',
#                   'Training ~ Venue_Voting_Load',
#                   'Venues ~ Venue_Voting_Load',
#                   'WHS ~ Venue_Voting_Load')
# adapt         = TRUE
# clus          = cl


## Create a list of GWR analyses for each topic ----
gam_formula_analyses <- function(formula_list, 
                                 analysis_data,
                                 adapt,
                                 clust) {
  
  ## Pipe the list into Lapply
  gam_list <- formula_list %>%
    
    ## Pipe the list into lapply
    ## formula <-  formula_list[3]
    lapply(function(formula) {
      
      exp_var <- gsub( " .*$", "", formula)
      
      if(!all(is.na(analysis_data[[exp_var]]))) {
        
        ## Check the dimensions of the data
        message('running gwr for ', formula)
        formula_data <- analysis_data %>% drop_na(exp_var)
        
        ## Step 1: Define GWR bandwidth
        gwr_bandwidth <- gwr.sel(as.Formula(formula),
                                 
                                 ## Coordinates
                                 data   = formula_data, 
                                 coords = cbind(formula_data$Latitude, 
                                                formula_data$Longitude), 
                                 
                                 ## Adapative bandwidth
                                 adapt  = TRUE)
        
        ## Step 2: run GWR formula
        gwr_model <- gwr(as.Formula(formula),
                         
                         data   = formula_data, 
                         coords = cbind(formula_data$Latitude, 
                                        formula_data$Longitude), 
                         
                         adapt     = gwr_bandwidth, 
                         hatmatrix = TRUE, 
                         se.fit    = TRUE,
                         cl        = clust)
        
        ## create global R2
        globalR2 <- (1 - (gwr_model$results$rss / gwr_model$gTSS))
        
        # get spatial spatialpolygondataframe from regression results + 
        # convert it into sf object. The spatial object brings the regressions 
        # results within it's data component
        sp <- LG21_GWR_model$SDF
        sp$Formula <- formula
        sf <- st_as_sf(sp) 
        
        # map local R2
        gwr_R2_map <- ggplot() + geom_sf(data = sf, aes(fill = localR2)) +
          coord_sf() +
          ggtitle(paste0("GWR : ", formula)) +
          labs(subtitle = paste("Global R2:", round(globalR2, 2)))
        
        # map residuals gwr.e
        gwr_resid_map <- ggplot() + geom_sf(data = sf, aes(fill = gwr.e)) +
          coord_sf() +
          ggtitle(paste0("GWR : ", formula)) +
          labs(subtitle = paste("Residuals"))
        
        
        gwr_results <- list(gwr_model, globalR2, sp, gwr_R2_map, gwr_resid_map)
        names(gwr_results) <- c(paste0(formula, '_GWR'), 
                                paste0(formula, '_globalR2'),
                                paste0(formula, '_GWR_spdf'),
                                paste0(formula, '_GWR_R2_map'),
                                paste0(formula, '_GWR_resid_map'))
        
        return(gwr_results)
        
      } else {
        message('Skip GWR for ', formula, ' insufficient data for this question')
        cat(formula)
      }
      
    })
  
  ## Rename the list items
  names(gwr_list) <- formula_list
  names(gwr_list) <- paste0("gwr formula : ", names(gwr_list))
  return(gwr_list)
  
}





############################################################################################
############################################ ---TBC---- ####################################
############################################################################################