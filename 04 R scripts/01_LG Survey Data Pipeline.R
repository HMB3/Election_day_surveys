


# LG21 Election staff survey launch script --------------------------------



## This code runs the individual scripts to prepare, format and analyse the survey of people who worked at the 
## polling stations for the NSW Local Council elections (LG2101).




# Initialisation ----------------------------------------------------------


# Election parameters
event_group_ID      <- "LG2101"
election_day        <- '2021-12-04'
election_sunday     <- '2021-12-05'
number_of_ROs       <- 70
venue_response_map  <- FALSE


# Write to file options
dev_code                    <- FALSE # Run code that is under development?
knit_report_site            <- TRUE # knit the survey report site
copy_site                   <- FALSE # Copy report site to transfer folder

write_reminder_table        <- FALSE
write_base_count_check      <- FALSE # Export the base counts to file
write_verbatims_spreadsheet <- FALSE




# Load functions ----------------------------------------------------------

# Source DAGS production functions
load('./04 R scripts/Survey_data.RData')
source('./04 R scripts//EC_Functions.R')


source('./04 R scripts/LG_Survey_Table_Functions.R')
source('./04 R scripts/LG_Survey_Table_Group_Functions.R')
source('./04 R scripts/LG_Survey_Plotting_Functions.R')
source('./04 R scripts/LG_Survey_Plotting_Group_Functions.R')


## Load packages
survey_packages =  c('R.utils', # used for copyDirectory
                     'readxl',
                     'DT',
                     'data.table',
                     'janitor',
                     'questionr',
                     'DataCombine',
                     'colr',
                     'survey',
                     'rmarkdown',
                     'ggpubr',
                     'GGally',
                     'lemon',
                     'highcharter',
                     'plotly',
                     'gsubfn',
                     'summarywidget',
                     'kableExtra',
                     'crosstalk',
                     'gridExtra',
                     'formattable',
                     'flexdashboard',
                     'RODBC',
                     'RODBCext',
                     'leaflet',
                     'leaflet.extras',
                     'leaflet.providers',
                     'rgdal',
                     'matrixStats',
                     'Hmisc',
                     'corrplot',
                     'ggthemes',
                     'cowplot',
                     'lubridate',
                     'fuzzyjoin',
                     'stringdist',
                     'htmltools',
                     'apaTables',
                     'nlme',
                     'car',
                     'Cairo',
                     'cairoDevice',
                     'tidyverse',
                     'sp',
                     'openxlsx',
                     'rgeos',
                     'knitr',
                     'scales',
                     'reshape2',
                     'rstudioapi',
                     'snakecase',
                     'ggiraph',
                     'geosphere',
                     'jsonlite',
                     'ellipsis',
                     'fansi',
                     'chron',
                     'ggiraph',
                     'tm',
                     'SnowballC',
                     'wordcloud',
                     'wordcloud2',
                     'RColorBrewer',
                     'magicfor',
                     'revealjs',
                     'zoo',
                     'ggwordcloud',
                     'anytime',
                     'naniar',
                     'gtools',
                     'bsselectR',
                     'cowplot',
                     'flextable',
                     'DescTools',
                     'svMisc',
                     'psych',
                     'viridisLite',
                     'sf',
                     'tmap',
                     'spdep',
                     'spgwr', 
                     'berryFunctions',
                     'BAMMtools',
                     'RSQLite',
                     'mgcv',
                     'pals')


## Load the list of packages
ipak(survey_packages)






## 1). FORMAT SURVEY DATA ==============================================================================


## Create figures and tables needed for the analysis
## These steps are all that's needed for the main delivery



## Format the survey data
source('./04 R scripts/02_LG Survey Data Formatter.R',      echo = TRUE)
# source('./04 R scripts/03a_LG Survey Staff Demographics.R', echo = TRUE)
source('./04 R scripts/03b_LG Checking Base Sizes.R',       echo = TRUE)


## Create the Demographic and Basedata needed
source('./04 R scripts/04a_LG Survey CALD_ATSI by LGA.R',   echo = TRUE)
source('./04 R scripts/04b_LG Survey Projections.R',        echo = TRUE)


## Create the Tables
source('./04 R scripts/05a_LG Survey Table Formatter.R',    echo = TRUE)
source('./04 R scripts/05b_LG Survey Verbatim Responses.R', echo = TRUE)





## 2). CREATE PLOTS AND TABLES =======================================================================


## Create figures and tables needed for the analysis
source('./04 R scripts/06a_LG Survey Simple Plot Formatter.R',     echo = TRUE)
source('./04 R scripts/06b_LG Survey Complex Plot Formatter.R',    echo = TRUE)
source('./04 R scripts/06c_LG Survey Demography Plot Formatter.R', echo = TRUE)




## 3). RUN SURVEY ANALYSIS ============================================================================


## Analyse the survey data
## The research aims are broad - 
## How can we improve the health, safety and satisfaction of people who work at the elections?

## The working hypothesis is that Survey reponses (e.g. Satisfaction) don't vary across geographic and socioeconomic space :
## Ho: Responses (.e.g Satisfaction) are the same across all respondents
## Ho: Responses (.e.g Satisfaction) are the same across business units
## Ho: Responses (.e.g Satisfaction) are the same across demographics (Gender/Ethnicity/Age)


## Build the dataset, and slice it anyway you like : Age / Gender / etc
## load('./04 R scripts/Survey_data_output.RData')


## Create indexes of survey responses - this step needs more work to integrate projections, etc.
## Three analyses : categorical (ANOVA) continuous (scatterplots), and spatial (GWR). 
source('./04 R scripts/07a_LG Survey Convert Responses.R',                   echo = TRUE)
source('./04 R scripts/08a_LG Survey Analysis Box Plots Respondent.R',       echo = TRUE)
source('./04 R scripts/08b_LG Survey Analysis Scatter Plots Respondent.R',   echo = TRUE)
source('./04 R scripts/08c_LG Survey Analysis Scatter Plots Venue.R',        echo = TRUE)
source('./04 R scripts/08d_LG Survey Analysis Scatter Plots RO Office.R',    echo = TRUE)
source('./04 R scripts/09a_LG Survey Analysis Maps Respondent RO Office.R',  echo = TRUE)





## 4). KNIT REPORT =====================================================================================


## Knit the report site to html
if(knit_report_site) {
  
  rmarkdown::render_site('04 R scripts/RMD')
  
  if(copy_site) { 
    
    copyDirectory(from = paste0(survey_r_scripts, 'RMD/_site')
                  ,to  = paste0(survey_transfer_folder, '_site')
                  ,overwrite = TRUE)
    
  }
}



if(dev_code) {
  
  rmarkdown::render(paste0(survey_r_scripts, 'RMD/index.Rmd'))
  rmarkdown::render(paste0(survey_r_scripts, 'RMD/logistics analysis.Rmd'))
  rmarkdown::render(paste0(survey_r_scripts, 'RMD/analysis_training.Rmd'))
  
}





#################################################### TBC ###########################################################