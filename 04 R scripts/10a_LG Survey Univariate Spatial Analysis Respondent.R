############################################ STAFF SURVEY SPATIAL ANALYSIS ################################################


## Purpose ----
## This code analyses if (and how) the survery results vary across the NSW electoral geography (i.e. across geographic space).
## We are asking a simple question - are the survey response spatially structured (i.e. related to resourcing, etc)? 


## HO: Survey responses are independent of location (i.e. the Venue/District where people worked , including demographics, etc.)
## Survey responses can be qualitative (% satisfied at each venue), OR quantitative (Index values).


## We can use spatial analysis methods to test this question in two ways : univariate, or bi/multi variate.
## Univariate analyses are methods for analysing structure in individual variables, like morans I (using polygons). 
## Bi/multi variate methods would be spatial regression techniques (e.g. GWR). These dedicated tools are better than 
## just doing a GLM or GAM with lat/lon as variables: methods like GWR incorporate spatial structure and auto-correlation 
## into the analyses, to test for meaningful patterns above background spatial structuring. That is a more nuanced 
## approach than rying to come up with your own proxy of spatial structure (and has decades of research behind it). 
## The key point is that spatial regression techniques are tailor made for datasets like those maintained by the EC, 
## so they are great tools - both research and practial - for DAGs and the EC. Here we demonsrate their utility on the LG21 
## Election Staff Survey responses.


## Set spatial library
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(dplyr)
library(spgwr)
library(parallel)
library(ggthemes)



## detect number of CPU cores to go parallel
number_cores <- detectCores() - 2 # leave two for the operating system
cl <- makeCluster(number_cores)   # Initiate cluster
eventID <- 'LG21'



## CHECK REPONDENT-LEVEL CORRELATIONS ========================================================================


## How does satisfaction vary across space, relative to itself? 
## The Moran's I statsistic is designed to test such questions. 


## Moran's I is a global measure of spatial autocorrelation in single vartibles across an entire study area. 
## The Moran's I statistic provides an indication of the degree of linear association between the observation 
## vector (xx) and a vector of spatially weighted averages of neighbouring values.
## https://aurin.org.au/resources/workbench-user-guides/portal-user-guides/analysing-your-data/spatial-autocorrelation-tools/morans-i/
## https://mgimond.github.io/simple_moransI_example/


## Create a function of this


# Moran's I is not robust to outliers or strongly skewed datasets. 
# Its therefore good practice to check the distribution of the attribute values....



## Lets analyse the spatial variation in the Recruitment index as an example - we can do all the indexes too.
## And we can also analyse the spatial variation in Satisfaction/disatisfaction.





## CHECK UNIVARIATE RELATIONSHIPS ========================================================================


## 
# The Spatial Autocorrelation (Global Moran's I) method measures spatial autocorrelation based on both feature locations 
# and feature values simultaneously. Given a set of features and an associated attribute, it evaluates whether the 
# pattern expressed is clustered, dispersed, or random. The tool calculates the Moran's I Index value and both a 
# z-score and p-value to evaluate the significance of that Index. P-values are numerical approximations of the area 
# under the curve for a known distribution, limited by the test statistic. See :

# https://pro.arcgis.com/en/pro-app/2.8/tool-reference/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm


# The Spatial Autocorrelation (Global Moran's I) tool is an inferential statistic, which means that the results of the 
# analysis are always interpreted within the context of its null hypothesis. For the Global Moran's I statistic, the 
# null hypothesis states that the attribute being analyzed is randomly distributed among the features in your study 
# area; said another way, the spatial processes promoting the observed pattern of values is random chance. Imagine 
# that you could pick up the values for the attribute you are analyzing and throw them down onto your features, 
# letting each value fall where it may. This process (picking up and throwing down the values) is an example of 
# a random chance spatial process.


## The Moran's I statistic is not robust to outliers or strongly skewed datasets. It's therefore good practice to check 
## the distribution of the attribute values. We have effectively done this already for the response level survey indexes :
plot_grid(bp_resposes_all, 
          respondent_role_boxplots[["Staffing_boxplot"]],
          nrow = 1,
          label_size = 5, align = 'hv')


# To symbolize the polygons using the Income attribute we will first define the classification breaks (style = quantile 
# with n = 8 breaks) and the symbol colors (palette="Greens"). For the latter, the tmap package makes use of Cynthia 
# Brewer's color schemes (see her website).


## There are multiple fields we want to test, so we should create a functions to do them all
tm_shape(NSWEC_RO_boundaries) + 
  
  tm_fill(col = c("Informality"),
          style = "quantile", n = 8, 
          palette = "Greens", legend.show = FALSE) +
  tm_facets(nrow = 1)



## Step 1: Define neighboring polygons -----
# The first step in a Moran's I analysis requires that we define "neighboring" polygons. This could refer to contiguous 
# polygons, polygons within a certain distance, or it could be non-spatial in nature and defined by social, political or 
# cultural "neighbors". See :
# https://mgimond.github.io/simple_moransI_example/

# Here, we'll adopt a contiguous neighbor definition. We'll accept any contiguous polygons that share at least one vertex; 
# this is the "queen" case (if one chooses to adopt the chess analogy) and it's parameterized as queen = TRUE in the call 
# to poly2nb. If we required that just edges be shared between polygons then we would set queen = FALSE (the rook analogy).


nb <- poly2nb(NSWEC_RO_boundaries, queen = TRUE)


# For each polygon in our shape object, nb lists all neighboring polygons. For example, to see the neighbors (by ID number) 
# for the first polygon in the shape object, type:
nb[1]



## Step 2: Assign weights to the neighbors ----
# Next, we need to assign weights to each neighboring polygon. In this example, each neighboring polygon will be assigned 
# equal weight when computing the neighboring mean income values.

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

#To see the weight of the first polygon's neighbors type:

lw$weights[1]


## Step 3 Compute the (weighted) neighbor mean income values ----
# NOTE: This step does not need to be performed when running the moran or moran.test functions outlined in Steps 4 and 5. 
# This step is only needed if you wish to generate a scatter plot between the income values and their lagged counterpart.


# Next, we'll have R compute the average neighbor income value for each polygon. These values are often referred to as 
# spatially lagged values. The NA value could be due to lack of neighbors
var.lag <- lag.listw(lw, NSWEC_RO_boundaries$Informality) #%>% .[!is.na(.)]
var.lag


# You can plot the relationship between income and its spatially lagged counterpart as follows. The fitted blue line added 
# to the plot is the result of an OLS regression model.

plot(var.lag ~ NSWEC_RO_boundaries$Informality, pch = 16, asp = 1)
M1 <- lm(var.lag ~ NSWEC_RO_boundaries$Informality)
abline(M1, col="red")


# The slope of the line is the Moran's I coefficient. You can extract its value from the model object M1 as follows:
coef(M1)[2]


# The moran's I coefficient is 0.68. The positive (upward) slope suggests that as the informality value of a said polygon 
# increases, so does those of its neighboring polygons. If the slope were negative (i.e. sloping downward), this would 
# suggest a negative relationship whereby increasing values in a said polygon would be surrounded by polygons with 
# decreasing informality values.


# Step 4: Computing the Moran's I statistic ----
# The Moran's I statistic can be computed using the moran function.
# The hypothesis we are testing states that "the income values are randomly distributed across counties 
# following a completely random process". There are two methods to testing this hypothesis: an analytical 
# method and a Monte Carlo method. We'll explore both approaches in the following examples.

moran.greater <- moran.test(NSWEC_RO_boundaries$Informality, 
                            lw, 
                            #length(nb), 
                            # Szero(lw),
                            randomisation = TRUE, 
                            zero.policy   = TRUE,
                            alternative   = "greater", 
                            rank          = FALSE, 
                            na.action     = na.pass)

moran.greater.test <- moran.greater[1]

# The Moran's I statistic is 0.683 (same value that was computed using the moran function, as expected). 
# The p-value is very small. Usually, when the p-value is very small it's common practice to report it as < 0.001.
# 
# Note that ArcMap adopts this analytical approach to its hypothesis test however, it implements a two-sided test 
# as opposed to the one-sided test adopted in the above example (i.e. alternative = "greater"). A two-sided p-value 
# is nothing more than twice the one-sided p-value. Unfortunately, ArcMap does not seem to make this important 
# distinction in any of its documentation. This distinction can have important ramifications as shown in the 
# next example (Florida crime data). Fortunately, the income data is so strongly clustered that both a one-sided 
# and two-sided test produce the same outcome (a p-value close to 0).




#Monte Carlo method ----
# The analytical approach to the Moran's I analysis benefits from being fast. 
# But it may be sensitive to irregularly distributed polygons. A safer approach to 
# hypothesis testing is to run an MC simulation using the moran.mc() function. 
# The moran.mc function takes an extra argument n, the number of simulations.

MC <- moran.mc(x             = NSWEC_RO_boundaries$Informality, 
               listw         = lw, 
               nsim          = 10000, 
               zero.policy   = TRUE,
               alternative   = "greater", 
               na.action     = na.omit)


# The MC simulation generates a very small p-value, 0.001. This is not surprising given that 
# the income values are strongly clustered. We can see the results graphically by passing the 
# Moran's I model to the plot function:

# Plot the Null distribution (note that this is a density plot instead of a histogram)
plot(MC)



# The curve shows the distribution of Moran I values we could expect had the incomes been randomly 
# distributed across the counties. Note that our observed statistic, 0.683, falls way to the right 
# of the distribution suggesting that the income values are clustered (a positive Moran's I value 
# suggests clustering whereas a negative Moran's I value suggests dispersion).


# Now, had the Moran's I statistic been negative (suggesting a dispersed pattern), you would probably 
# want to set the alternative argument to less thus giving you the fraction of simulated I values more 
# dispersed than your observed I value.


# A visual exercise that you can perform to assess how "typical" or "atypical" your pattern may be 
# relative to a randomly distributed pattern is to plot your observed pattern alongside a few simulated 
# patterns generated under the null hypothesis.



## Create randomisations
set.seed(131)
NSWEC_RO_boundaries$rand1 <- sample(NSWEC_RO_boundaries$Informality, 
                                    length(NSWEC_RO_boundaries$Informality), replace = FALSE)

NSWEC_RO_boundaries$rand2 <- sample(NSWEC_RO_boundaries$Informality, 
                                    length(NSWEC_RO_boundaries$Informality), replace = FALSE)

NSWEC_RO_boundaries$rand3 <- sample(NSWEC_RO_boundaries$Informality, 
                                    length(NSWEC_RO_boundaries$Informality), replace = FALSE)


## Plot the observed distribution of Informality against the random distributions
tm_shape(NSWEC_RO_boundaries) + 
  
  tm_fill(col = c("Informality", "rand1", "rand2", "rand3"),
          style = "quantile", n = 8, 
          palette = "Greens", legend.show = FALSE) +
  tm_facets(nrow = 1)


# Can we tell the difference between our observed informality distribution and those generated from a completely 
# random process? The map on the left is our observed distribution. The three maps on the right are realizations 
# of a completely random process.





## MORANS I ACROSS TARGET VARIABLES ========================================================================


## Create a function to run the above analysis for a list of variables 
LG21_morans_variables_RO_office <- morans_i_variable_tests(analysis_data = NSWEC_RO_boundaries %>% sf:::as_Spatial(),
                                                           
                                                           var_list      = c("Informality",         
                                                                             "Venues_over_proj", 
                                                                             "Respondent_Satisfied",
                                                                             "Respondent_Dissatisfied", 
                                                                             
                                                                             "Counting",
                                                                             "IS",  
                                                                             "Logistics",
                                                                             "Operations",     
                                                                             "Recruitment",
                                                                             "Staffing",
                                                                             "Training",     
                                                                             "Venues",     
                                                                             "WHS"),
                                                           
                                                           moran_test     = "greater",
                                                           randomisations = 50000,
                                                           seed           = 131)


## Check the results...?



#################################################### TBC ###########################################################
