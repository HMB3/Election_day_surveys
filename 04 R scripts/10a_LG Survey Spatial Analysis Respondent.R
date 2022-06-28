############################################ STAFF SURVEY SPATIAL ANALYSIS ################################################


## Purpose ----
## This code aims to analyse how the survery results vary across the electoral geography (i.e. across geographic space).


## HO: Survey responses are independent of location (i.e. Venue/District, including demographics, etc.)
## Survey responses can be qualitative (% satisfied at each venue), OR quantitative (Index values).


## We can use spatial analysis methods to test this question in two ways : univariate, or bi/multi variate.


## 
library(sf)
library(spdep)
library(tmap)





## 1). CHECK VENUE-LEVEL CORRELATIONS ========================================================================


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
pairs.panels(Venues_workload_map %>% 
               
               dplyr::select(Venue_Voting_Load, 
                             Venue_Satisfied, 
                             Counting, 
                             IS, 
                             Logistics,
                             Operations, 
                             Recruitment, 
                             Staffing,
                             Training, 
                             Venues, 
                             WHS),
             
             method     = "pearson", # correlation method
             hist.col   = "#00AFBB",
             density    = TRUE,      # show density plots
             ellipses   = FALSE, 
             cex        = 4, 
             # cex.cor = 2,
             cex.labels = 1.5,
             lwd = 2, 
             col = "blue")


pairs.panels(Venues_workload_map %>% 
               
               dplyr::select(Venue_Voting_Load, 
                             Venue_Dissatisfied, 
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




## 2). MULTI-VARIATE ANALYSIS OF SURVEY RESPONSES ======================================================================



## We can also analyse how survey responses vary relative to other variables gathered in the survey.
## To do this, we can run a regression model to see how the relationship between Survey satisfaction and (location) varies 
## across the election.


## 'Geographically weighted regression' (GWR) is an ideal technique for our data.
## See https://rstudio-pubs-static.s3.amazonaws.com/44975_0342ec49f925426fa16ebcdc28210118.html for an example
## 


## Our Model hypothesizes that satisfaction varies with voting workload:
## SAT = f(Workload, X,Y).


## Our data for SG19 doesn't really go down to the Venue level.
## But we probably need all the Venues to run a proper spatial analysis
venue_index_gwr <- NSW_LGA_boundaries


## Turn the below into a function, so we can run it for a list of response/explanatory variables


## First, set the GWR bandwidth ----
## This should go through all the points in the df, and estimate a bandwidth 
## across all of them...?
LG21_GWR_bandwidth <- gwr.sel(Venue_Satisfied ~ Venue_Voting_Load,
                              
                              ## Coordinates
                              data   = venue_index_gwr@data, 
                              coords = cbind(venue_index_gwr$Latitude, 
                                             venue_index_gwr$Longitude), adapt = TRUE)





## Then run the GWR model ----
LG21_GWR_model <- gwr(Venue_Satisfied ~ Venue_Voting_Load,
                      
                      data      = venue_index_gwr,
                      coords    = cbind(venue_index_gwr$Latitude, 
                                        venue_index_gwr$Longitude), 
                      
                      adapt     = LG21_GWR_bandwidth, 
                      hatmatrix = TRUE, 
                      se.fit    = TRUE)


## Print the results of the model
LG21_GWR_model





## 3). PLOT THE RESULTS ========================================================================


## Attach the GWR coefficients to our original dataframe 



## Then plot the coefficients for the different variables:



## What does this mean? We probably need a separate Ad-hoc, to test if Survey responses are independent
## of location. 







#################################################### TBC ###########################################################
