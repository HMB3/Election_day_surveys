############################################ STAFF SURVEY SPATIAL ANALYSIS ################################################


## Purpose ----
## This code analyses if (and how) the survery results vary across the NSW electoral geography (i.e. across geographic space).
## We are asking a simple question - are the survey response spatially structured (i.e. related to resourcing, etc)? See 
#https://mgimond.github.io/simple_moransI_example/


## HO: Survey responses are independent of location (i.e. the Venue/District where people worked , including demographics, etc.)
## Survey responses can be qualitative (% satisfied at each venue), OR quantitative (Index values).


## We can use spatial analysis methods to test this question in two ways : univariate, or bi/multi variate.
## Univariate analyses are methods like morans I (using polygons). Bi/multi variate methods would be spatial regression
## techniques (e.g. GWR). These dedicated tools are better than just doing a GLM or GAM with lat/lon as variables -
## methods like GWR incorporate spatial structure and auto-correlation into the analyses, to test for meaningful patterns.
## That is more nuanced than trying to come up with your own proxy of spatial structure (and has decades of research behind it).
## The key point is that spatial regression techniques are tailor made for datasets like those maintained by the EC, so
## they are great research and practical tools for DAGs and the EC. Here we demonsrate their utility on the LG21 Election
## Staff Survey responses.


## Set spatial library
library(sf)
library(spdep)
library(tmap)
library(UScensus2000tract)
library(ggplot2)
library(dplyr)
library(spgwr)
library(parallel)
library(ggthemes)



## detect number of CPU cores to go parallel
number_cores <- detectCores() - 2 # leave two for the operating system
clust <- makeCluster(number_cores)   # Initiate cluster
eventID <- 'LG21'



## 1). CHECK REPONDENT-LEVEL CORRELATIONS ========================================================================


## How does satisfaction vary across space, relative to itself? 
## The Moran's I statsistic is designed to test such questions. 


## Moran's I is a global measure of spatial autocorrelation across an entire study area. The Moran's I statistic provides 
## an indication of the degree of linear association between the observation vector (xx) and a vector of spatially weighted 
## averages of neighbouring values.
## https://aurin.org.au/resources/workbench-user-guides/portal-user-guides/analysing-your-data/spatial-autocorrelation-tools/morans-i/
## https://mgimond.github.io/simple_moransI_example/


## Create a function of this


# Moran's I is not robust to outliers or strongly skewed datasets. 
# Its therefore good practice to check the distribution of the attribute values....


## These scatterplots show that Venue-level satisfaction has a better relationship with
## the topic indexes, than workload. This is an interesting result - how swamped people
## were doesn't necessairly influence their experience, at least predictably.
pairs.panels(respondent_indexes_spatial %>% as_tibble() %>% 
               
               ## 
               dplyr::select(Venue_Voting_Load, 
                             Respondent_Satisfied, 
                             Counting, 
                             IS, 
                             Logistics,
                             Operations, 
                             Recruitment, 
                             Staffing,
                             Training, 
                             Venues, 
                             WHS),
             
             ## 
             method     = "pearson", # correlation method
             hist.col   = "#00AFBB",
             density    = TRUE,      # show density plots
             ellipses   = FALSE, 
             cex        = 4, 
             # cex.cor = 2,
             cex.labels = 1.5,
             lwd = 2, 
             col = "blue")


## 
pairs.panels(respondent_indexes_spatial %>%  as_tibble() %>% 
               
               dplyr::select(Venue_Voting_Load, 
                             Respondent_Satisfied, 
                             Counting, 
                             IS, 
                             Logistics,
                             Operations, 
                             Recruitment, 
                             Staffing,
                             Training, 
                             Venues, 
                             WHS), 
             
             method   = "pearson", # correlation method
             hist.col = "#00AFBB",
             density  = TRUE,      # show density plots
             ellipses = FALSE, 
             cex = 4, 
             # cex.cor = 2,
             cex.labels = 1.5,
             lwd = 2, 
             col = "blue")


## Lets analyse the spatial variation in the Recruitment index as an example - we can do all the indexes too.
## And we can also analyse the spatial variation in Satisfaction/disatisfaction.





## 2). MULTI-VARIATE ANALYSIS OF SURVEY RESPONSES ============================================================


## We can also analyse how survey responses vary relative to other variables gathered in the survey.
## To do this, we can run a regression model to see how the relationship between Survey responses 
## and any explanatory variables changes across all locations where people worked.


# Geographically Weighted Regression (GWR) is one of several spatial regression techniques used in geography 
# and other disciplines. GWR evaluates a local model of the variable or process you are trying to understand 
# or predict by fitting a regression equation to every feature in the dataset. GWR constructs these separate 
# equations by incorporating the dependent and explanatory variables of the features falling within the 
# neighborhood of each target feature. The shape and extent of each neighborhood analyzed is based on the 
# Neighborhood Type and Neighborhood Selection Method parameters. GWR should be applied to datasets with 
# several hundred features. It is not an appropriate method for small datasets and does not work with
# multipoint data. See 
# https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/how-geographicallyweightedregression-works.htm


## Our Model hypothesizes that satisfaction varies with voting workload:
## SAT = f(Workload, X,Y).


## Turn the below into a function, so we can run it for a list of response/explanatory variables
test_data <- head(respondent_indexes_spatial, 500)


## First, set the GWR bandwidth ----
## This should go through all the points in the df, and estimate a bandwidth 
## across all of them...?


## Create a function that does the bandwidth for one run, then do the GWR
## for a list of variables - the store a list of results
## This is just a single number....? What is the context?
message('estimate GWR bandwidth for ', eventID, ' survey')
LG21_GWR_bandwidth <- gwr.sel(Respondent_Satisfied ~ Venue_Voting_Load,
                              
                              ## Coordinates
                              data   = test_data, 
                              coords = cbind(test_data$Latitude, 
                                             test_data$Longitude), 
                              
                              adapt  = TRUE)





## Then run the GWR model ----
message('run GWR bandwidth for ', eventID, ' survey')
LG21_GWR_model <- gwr(Respondent_Satisfied ~ Venue_Voting_Load,
                      
                      data      = test_data,
                      coords    = cbind(test_data$Latitude, 
                                        test_data$Longitude), 
                      
                      adapt     = LG21_GWR_bandwidth, 
                      hatmatrix = TRUE, 
                      se.fit    = TRUE,
                      cl        = clust)


## Print the results of the model
LG21_GWR_model


globalR2 <- (1 - (LG21_GWR_model$results$rss / LG21_GWR_model$gTSS))


# get spatial spatialpolygondataframe from regression results + 
# convert it into sf object. The spatial object brings the regressions 
# results within it's data component
sp <- LG21_GWR_model$SDF
sf <- st_as_sf(sp) 



# map local R2
ggplot() + geom_sf(data = sf, aes(fill = localR2)) +
  # geom_sf(data = NSWEC_RO_boundaries, aes(fill = NA))
  coord_sf() +
  # theme_map() +
  ggtitle(paste("Local R2")) +
  labs(subtitle = paste("Global R2:", round(globalR2, 2)))


# map residuals gwr.e
ggplot() + geom_sf(data = sf, aes(fill = gwr.e)) +
  coord_sf() +
  # theme_map() +
  ggtitle(paste("Residuals")) 





## 4). PLOT GWR RESULTS ========================================================================


## Create a function to run the above GWR analysis for a list of formulae
## Could use the exact same process to run regression models (e.g. GAMs, GLMs, etc) and store the output as lists.
LG21_GWR_models <- gwr_formula_analyses(analysis_data = test_data,
                                        
                                        ## A list of formulae for the GWR models
                                        formula_list  = c('Respondent_Satisfied ~ Venue_Voting_Load',
                                                          'Respondent_Dissatisfied ~ Venue_Voting_Load',
                                                          'Counting ~ Venue_Voting_Load',
                                                          'IS ~ Venue_Voting_Load',
                                                          'Counting ~ Venue_Voting_Load',
                                                          'Logistics ~ Venue_Voting_Load',
                                                          'Operations ~ Venue_Voting_Load',
                                                          'Recruitment ~ Venue_Voting_Load',
                                                          'Staffing ~ Venue_Voting_Load',
                                                          'Training ~ Venue_Voting_Load',
                                                          'Venues ~ Venue_Voting_Load',
                                                          'WHS ~ Venue_Voting_Load'),
                                        
                                        ## Use adaptive GWR bandwidth estimation, and multiple cores
                                        adapt          = TRUE,
                                        clust          = cl)





#################################################### TBC ###########################################################
