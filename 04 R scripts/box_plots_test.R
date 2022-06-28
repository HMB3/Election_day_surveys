## We need four different aggregations of the respondent-level table

## 1). Group by :: Topic
## 2). Topic and Role
## 3). Topic and demographic
## 4). Topic and remoteness


## 1). Box plot of all respondents by topic (only one of these)
bp_resposes_all <- respondent_index %>% dplyr::select(index_plot_columns) %>%
  
  gather(key = "Category", value = "Index") %>%
  filter(Index != 0) %>% 
  mutate(Category = factor(Category, levels = index_plot_columns),
         Index    = as.numeric(Index)) %>% na.omit() %>% 
  
  factor_boxplots(., 
                  x_var       = "Category", 
                  y_var       = "Index", 
                  group_var   = "Category",
                  leg_pos     = 'none',
                  y_lab       = 'Numeric Index', 
                  x_lab       = '',
                  
                  pallette    = brewer.pal(n = 10, name = "Paired"),
                  box_size    = 1.2, 
                  y_lim       = c(0, 1.2), 
                  lab_angle   = 45,
                  lab_size    = 18,
                  border_size = 3)



plot_list        = index_plot_columns
survey_data      = respondent_index
columns          = index_plot_columns



x_var       = "Category" 
y_var       = "Index" 
group_vars  = c("Gender", "Languages", "Disability", "Indigenous")
group_var   = "Role"
leg_pos     = 'right'
y_lab       = '' 
x_lab       = ''
pallette    = "Dark2"

box_size    = 1.2
y_lim       = c(0, 1.2)
lab_angle   = 45
lab_size    = 18
border_size = 3


## Create a list of overall boxplots for each group ----
boxplot_list <- function(plot_list, survey_data, columns, 
                         role_list, count_list,
                         
                         ## This creates the question number
                         x_var, y_var,         pallette, group_var,
                         box_size,  x_lab,     y_lab, y_lim, leg_pos,
                         lab_size,  lab_angle, border_size) {
  
  ## Pipe the list into Lapply
  boxplot_list <- plot_list %>%
    
    ## Pipe the list into lapply
    ## topic <- plot_list[1]
    lapply(function(topic) {
      
      y_lab = paste0('Index')
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_var)
      bp_role  <- respondent_index %>% dplyr::select(one_of(cols)) %>%
        
        gather(key = "Category", value = "Index", -!!as.symbol(group_var)) %>%
        filter(Index != 0) %>% 
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




plot_list        = index_plot_columns
survey_data      = respondent_index
columns          = index_plot_columns

x_var       = "Category" 
# group_vars  = c("Gender", "Languages", "Disability", "Indigenous")
group_vars  = c("Remoteness")
group_var   = "Category"
y_var       = "Index" 
leg_pos     = 'right'
y_lab       = ''
x_lab       = ''
pallette    = brewer.pal(n = 9, name = "Paired")

box_size    = 1.2
y_lim       = c(0, 1.2)
lab_angle   = 45
lab_size    = 18
border_size = 3
levels      = c("Major Cities", "Regional", "Remote")
# levels      = c("Female",         "Male", "Other",
#                 "Multilingual",   "English Only",   
#                 "No Disability",  "Disability",
#                 "Indigenous",     "Non-Indigenous")




## Create a list of histogram plots----
boxplot_group_list <- function(plot_list, survey_data, 
                               columns, levels,
                               
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
      y_lab = paste0(topic, ' Index')
      
      ## Check the dimensions of the data
      message('creating index boxplots for ', topic)
      cols     <- c(topic, group_vars)
      bp_role  <- respondent_index %>% dplyr::select(one_of(cols)) %>%
        
        pivot_longer(cols = group_vars, values_to = "Category") %>%
        rename(Index   = !!as.symbol(topic)) %>% 
        filter(Index  != 0) %>% 
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





## 2). Box plots for each Topic * Role - one for each topic is needed
## We just need this code block repated, for each topic. We don't really need the tables either,
## they are intermediary. So we can create a function that does the whole thing.
## - 1 create box plot table, then plot the boxplot, but need 
respondent_role_boxplots <- boxplot_list(plot_list        = index_plot_columns,
                                         survey_data      = respondent_index,
                                         columns          = index_plot_columns,
                                         
                                         x_var       = "Category", 
                                         y_var       = "Index", 
                                         group_var   = "Role",
                                         leg_pos     = 'right',
                                         y_lab       = '', 
                                         x_lab       = '',
                                         pallette    = "Dark2",
                                         
                                         box_size    = 1.2,
                                         y_lim       = c(0, 1.2),
                                         lab_angle   = 45,
                                         lab_size    = 18,
                                         border_size = 3)




## 3). Box plots for each Topic * Role - one for each topic is needed
## We just need this code block repated, for each topic. We don't really need the tables either,
## they are intermediary. So we can create a function that does the whole thing.
## - 1 create box plot table, then plot the boxplot, but need 
respondent_demo_boxplots <- boxplot_group_list(plot_list   = index_plot_columns,
                                               survey_data = respondent_index,
                                               columns     = index_plot_columns,
                                               
                                               x_var       = "Category", 
                                               group_var   = "Category",
                                               group_vars  = c("Gender", "Languages", "Disability", "Indigenous"),
                                               y_var       = "Index", 
                                               leg_pos     = 'none',
                                               y_lab       = '',
                                               x_lab       = '',
                                               pallette    = brewer.pal(n = 9, name = "Paired"),
                                               
                                               box_size    = 1.2,
                                               y_lim       = c(0, 1.2),
                                               lab_angle   = 45,
                                               lab_size    = 18,
                                               border_size = 3,
                                               levels      = c("Female",         "Male", "Other",
                                                               "Multilingual",   "English Only",   
                                                               "No Disability",  "Disability",
                                                               "Indigenous",     "Non-Indigenous"))



respondent_remoteness_boxplots <- boxplot_group_list(plot_list   = index_plot_columns,
                                                     survey_data = respondent_index,
                                                     columns     = index_plot_columns,
                                                     
                                                     x_var       = "Category", 
                                                     y_var       = "Index", 
                                                     
                                                     group_vars  = "Remoteness",
                                                     group_var   = "Category",
                                                     leg_pos     = 'none',
                                                     y_lab       = '',
                                                     x_lab       = '',
                                                     pallette    = brewer.pal(n = 3, name = "Paired"),
                                                     
                                                     box_size    = 1.2,
                                                     y_lim       = c(0, 1.2),
                                                     lab_angle   = 45,
                                                     lab_size    = 18,
                                                     border_size = 3,
                                                     levels      = c("Major Cities", "Regional", "Remote"))








