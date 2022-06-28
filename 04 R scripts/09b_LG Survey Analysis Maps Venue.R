#################################### STAFF SURVEY RESULTS MAPS ################################################


## Purpose ----
## The prupose of this code is to create Maps of Survey responses.
## For example, we can map Venues for SG19 that has survey responses, and Venues that didn't. 
## Ultimately we want to map the survey responses themselves, but can we recycle some of the code?


## If Venues don't have enough staff to make a venue-level index viable, then we can map to the district level.
## This would mean using polygons (districts) instead of points (venues). 


## Most of the code we need is here, it just needs to change the specifics of the labels, etc.

## To do ----

## Check the leaflet radius and color args
## Check the NA values for the map dfs - are all the missing venues expected, eg Polling places?
## Check if we can make a Polygon map using the RO level data




## 1). PREPARE SURVEY DATA FOR MAPPING ========================================================================


## Create a response rate for every Venue?
## Count which venues are missing
Venue_tally <- SurveyData_numeric_questions %>% group_by(VenueName) %>% summarise(Response_count = n())


## Join Staff count with venues
Distinct_Venues <- venue_index %>%
  left_join(Venue_tally, by = 'VenueName') %>%
  distinct(VenueName, Response_count, LocationTypeCode, Latitude, Longitude) %>%
  mutate(Survey_Response = ifelse(is.na(Response_count), FALSE, TRUE))





## 2). CREATE MAP LABELS ====================================================================================


## We need to :
# - Combined the SED geography with teh survey results
# - ReturningOffice attributes must bound to the .dbf of the .shp, 
# - Venue attributes will be point data, with lat/lon coordinates 


## Venue Response Labels ----


## Cross tabulate the venues with staff responses                           
# CrossTab(Distinct_Venues, 'LocationTypeCode', 'Survey_Response') %>%
#   filter(LocationTypeCode != 'Pre-polling Place') %>% spread(LocationTypeCode, Count)


## These labels only need to be create once
red_icon   <- makeIcon(paste0('//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI',"/marker-icon-red.png"),   
                       18, 18, iconAnchorX = 9, iconAnchorY = 18)

black_icon <- makeIcon(paste0('//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI',"/marker-icon-black.png"), 
                       18, 18, iconAnchorX = 9, iconAnchorY = 18)





## Read the LGA Shapefile ----
## Check this is all ok
NSW_LGA_boundaries       <- readOGR(dsn = paste0(survey_context,'/NSW_LGA_2020/', 
                                                 "nsw_lga.shp"), layer = "nsw_lga") %>% 
  aim.analysis::repair_geometry()


## Change the name of the LGAs to match NSWEC data
names(NSW_LGA_boundaries@data)[names(NSW_LGA_boundaries@data)=="ABB_NAME"] <- "LGAreaCode"


## 
NSW_LGA_boundaries <- NSW_LGA_boundaries %>% st_as_sf() %>% 
  
  ## Get rid of the Extra text in the council names - really annoying
  mutate(LGAreaCode = gsub(" Council",        "", LGAreaCode)) %>%
  mutate(LGAreaCode = gsub("City of ",        "", LGAreaCode)) %>%
  mutate(LGAreaCode = gsub(" City",           "", LGAreaCode)) %>%
  mutate(LGAreaCode = gsub("Council of the ", "", LGAreaCode)) %>%
  mutate(LGAreaCode = gsub("Municipal",       "", LGAreaCode)) %>%
  mutate(LGAreaCode = gsub(" Shire",          "", LGAreaCode)) %>%
  mutate(LGAreaCode = gsub(" Regional",       "", LGAreaCode)) %>%
  
  ## Convert back to a SPDF
  mutate(LGAreaCode = trimws(LGAreaCode)) %>% sf:::as_Spatial()


##
NSW_LGA_boundaries_venues <- NSW_LGA_boundaries


## What are the different LGAs between the ABS and the survey?
length(intersect(NSW_LGA_boundaries_venues@data$LGAreaCode, unique(venue_index$LGAreaCode)))
length(setdiff(NSW_LGA_boundaries_venues@data$LGAreaCode,   unique(venue_index$LGAreaCode)))
setdiff(unique(venue_index$LGAreaCode),              NSW_LGA_boundaries_venues@data$LGAreaCode)


## These councils are missing from the 2016 ABS data we used
setdiff(Basedata_councils$LGAreaCode, NSW_LGA_boundaries_venues@data$LGAreaCode)


## Join the SED geography to the Survey Results ---
NSW_LGA_boundaries_venues@data  <- NSW_LGA_boundaries_venues@data %>%
  
  inner_join(venue_index, 
             by = c('LGAreaCode')) 


## Labels and parameters
html_legend <- "<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-black.png'> Polling place<br/>
<img src='//SVRANALYTICS1/AnalyticsReports/01 Reference files/UI/marker-icon-red.png'> Pre-poll"





## Create Venue labels ----


## Venue Response labels - does that make sense?
NoResponseVenues    <- Distinct_Venues %>% filter(Survey_Response == FALSE) %>% 
  filter(LocationTypeCode != 'Pre-polling Place')


VenuesWithResponses <- Distinct_Venues %>% filter(Survey_Response == TRUE) %>% 
  filter(LocationTypeCode != 'Pre-polling Place')


Venues_workload_map  <- venue_index %>% 
  filter(LocationTypeCode != 'Pre-polling Place')


## These councils are missing from the final map data
## And these venues are different - why are non-pre polls missing?
setdiff(Basedata_councils$LGAreaCode,     Venues_workload_map$LGAreaCode)
view(setdiff(Basedata_unique_venues$VenueName, Venues_workload_map$VenueName))


## Create no response labels for mapping  
NoResponseVenues_popuplabels <- sprintf(
  "<strong>%s</strong><br># Responses: %s" ,
  NoResponseVenues$VenueName
  ,NoResponseVenues$Response_count) %>% lapply(htmltools::HTML) 


## Create response labels for mapping  
VenuesWithResponses_popuplabels <- sprintf(
  "<strong>%s</strong><br># Responses: %s" ,
  VenuesWithResponses$VenueName
  ,VenuesWithResponses$Response_count) %>% lapply(htmltools::HTML)


## Create no response labels for mapping  
WorkloadVenues_popuplabels <- sprintf(
  "<strong>%s</strong><br># Responses: %s" ,
  Venues_workload_map$VenueName
  ,Venues_workload_map$Venue_Voting_Load) %>% lapply(htmltools::HTML) 


## Create district labels for mapping
venue_labels <- sprintf(
  "<strong>%s</strong><br>",
  NSW_LGA_boundaries_venues@data$Name) %>% lapply(htmltools::HTML)





## 3). OVERALL VENUE SURVEY MAPS ========================================================================


## Create panels of maps. Create a map for each of :

## Venue Responses 
## Venue Staff Numbers
## Venue Workload
## Venue Demography
## Venue % Satisfied
## Venue Indexes


## The analysis is structured by topic. So it makes sense to present one map for each topic, but have 
## common data displayed for every topic.


## We can also have overall maps of markoffs and formality, for the Report summary page 


## Use leaflet 'groups'
## A group is a label given to a set of layers. You assign layers to groups by using 
## the 'group' parameter when adding the layers to the map


## Many layers can belong to same group. But each layer can belong to only 0 or 1 
## groups - you can't assign a layer to two or more groups.


## So can we use 'groups' to put lots of info in the same map?


## API Key
api_key <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"


if(venue_response_map) {
  
  ## Map of Venues with/without Responses
  Venue_Map_of_Survey_responses <- leaflet(NSW_LGA_boundaries_venues,
                                           height  = 500, 
                                           width   = 1600, 
                                           padding = 0, 
                                           options = leafletOptions(zoomControl = FALSE,
                                                                    minZoom     = 6, 
                                                                    maxZoom     = 20)) %>%
    ## set the boundaries
    setView(151.0000486, -33.8027653, zoom = 6) %>%
    setMaxBounds(lng1 = 131.000333332,
                 lat1 = -18.992662696,
                 lng2 = 175.612793,
                 lat2 = -43.840233) %>% 
    addTiles() %>%
    
    addResetMapButton() %>%
    
    ## Add polygons
    leaflet::addPolygons(
      weight      = 2,
      fillOpacity = 0.05,
      
      highlight      = highlightOptions(
        color        = "#514d4d",
        fillOpacity  = 0.05,
        bringToFront = FALSE),
      popup = venue_labels,
      
      labelOptions = labelOptions(
        style      = list("font-weight" = "normal", padding = "3px 8px"),
        textsize   = "15px",
        direction  = "auto"),
      group        = 'ReturningOffice')  %>%
    
    ## Use "groups"
    ## Add markers for Venues with responses
    addMarkers(data           = VenuesWithResponses,
               lat            = VenuesWithResponses$Latitude, 
               lng            = VenuesWithResponses$Longitude,
               label          = VenuesWithResponses$VenueName,
               labelOptions   = labelOptions(textsize = 15),
               popup          = VenuesWithResponses_popuplabels,
               icon           = red_icon,
               clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     maxClusterRadius    = 70),
               group = 'Venues With Responses') %>% 
    
    ## Add markers for No Response Venues
    addMarkers(data           = NoResponseVenues, 
               lat            = NoResponseVenues$Latitude, 
               lng            = NoResponseVenues$Longitude,
               label          = NoResponseVenues$VenueName,
               popup          = NoResponseVenues_popuplabels,
               icon           = black_icon,
               clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     maxClusterRadius = 70),
               group = 'Venues Without Responses') %>%
    
    ## Add markers for Venue workload
    addMarkers(data           = Venues_workload_map,
               lat            = Venues_workload_map$Latitude, 
               lng            = Venues_workload_map$Longitude,
               label          = Venues_workload_map$VenueName,
               labelOptions   = labelOptions(textsize = 15),
               popup          = Venues_workload_map,
               icon           = red_icon,
               clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     maxClusterRadius    = 70),
               
               ## Group is used to 'facet' the layers
               group = 'Venue Workloads') %>% 
    
    ## Add legend
    addControl(html = html_legend, position = "topright") %>% 
    
    ## Add search features
    addSearchFeatures(
      targetGroups = c('Venues Without Responses',
                       'Venues With Responses',
                       'Venue Workloads'),
      options                = searchFeaturesOptions(
        zoom                 = 16, 
        openPopup            = TRUE, 
        firstTipSubmit       = TRUE,
        autoCollapse         = TRUE, 
        hideMarkerOnCollapse = TRUE )) %>%
    
    addLayersControl(
      overlayGroups = c('Venues Without Responses',
                        'Venues With Responses',
                        'Venue Workloads'),
      options = layersControlOptions(collapsed = FALSE))
  
}



## Get domain of workload data
(inf_domain <- range(Venues_workload_map$Informality))
inf_pal     <- colorNumeric(palette = brewer.pal(5, 'YlOrRd'), domain = inf_domain)


# bins_returning_load     <- c(0.03, 0.68, 0.75, 0.87, 1.2, 3.0)
# bins_returning_load_pal <- colorBin(palette  = "YlOrBr", 
#                                     domain   = NSW_LGA_boundaries_venues@data$Informality, 
#                                     na.color = "transparent", 
#                                     bins     = bins_returning_load)



venue_load_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue formality ----
Venue_Map_Survey_Votes_Formality <- leaflet(NSW_LGA_boundaries_venues,
                                            height  = 500, 
                                            width   = 1600, 
                                            padding = 0, 
                                            options = leafletOptions(zoomControl = FALSE,
                                                                     minZoom     = 6, 
                                                                     maxZoom     = 20)) %>%
  
  setView(151.0000486, -33.8027653, zoom = 6) %>% 
  
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 1.5,#~ sqrt(Venues_workload_map$Total_Votes)/10,
                   color        = ~ inf_pal(Venues_workload_map$Informality),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  inf_pal, 
            values   =~ Venues_workload_map$Informality, 
            opacity  =  0.9, 
            title    = "Informality", 
            position = "bottomleft") 





## Get domain of workload data
(work_domain <- range(Venues_workload_map$Venue_Voting_Load))
work_pal     <- colorNumeric(palette = brewer.pal(9, 'YlOrRd'), domain = work_domain)


## Map of Venue Satisfaction ----
Venue_Map_Survey_Workload <- leaflet(NSW_LGA_boundaries_venues,
                                     height  = 500, 
                                     width   = 1600, 
                                     padding = 0, 
                                     options = leafletOptions(zoomControl = FALSE,
                                                              minZoom     = 6, 
                                                              maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 1.5,
                   color        = ~ work_pal(Venues_workload_map$Venue_Voting_Load)) %>% 
  
  ## Add legend
  addLegend(pal      =  work_pal, 
            values   =~ Venues_workload_map$Venue_Voting_Load, 
            opacity  =  0.9, 
            title    = "Venue Workload", 
            position = "bottomleft") 




## Get domain of workload data
(disat_domain <- range(Venues_workload_map$Venue_Dissatisfied))
disat_pal     <- colorNumeric(palette = brewer.pal(9, 'YlOrRd'), domain = disat_domain)


## Map of Venue Satisfaction ----
Venue_Map_Survey_Dissatisfaction <- leaflet(NSW_LGA_boundaries_venues,
                                            height  = 500, 
                                            width   = 1600, 
                                            padding = 0, 
                                            options = leafletOptions(zoomControl = FALSE,
                                                                     minZoom     = 6, 
                                                                     maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 1.5,
                   color        = ~ disat_pal(Venues_workload_map$Venue_Dissatisfied)) %>% 
  
  ## Add legend
  addLegend(pal      =  disat_pal, 
            values   =~ Venues_workload_map$Venue_Dissatisfied, 
            opacity  =  0.9, 
            title    = "Venue Dissatisfaction", 
            position = "bottomleft") 





## 3). VENUE INDEX MAPS ==========================================================================


## We can try mapping each Index at the Venue Level


## Get domain of workload data
(IS_domain <- range(Venues_workload_map$IS))
IS_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = IS_domain)


venue_load_IS_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "IS Index: ",         Venues_workload_map$IS,                  "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue IS Index ----
Venue_Map_Survey_IS <- leaflet(NSW_LGA_boundaries_venues,
                               height  = 500, 
                               width   = 1600, 
                               padding = 0, 
                               options = leafletOptions(zoomControl = FALSE,
                                                        minZoom     = 6, 
                                                        maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_IS_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ work_pal(Venues_workload_map$IS),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  IS_pal, 
            values   =~ Venues_workload_map$IS, 
            opacity  =  0.9, 
            title    = "Venue IS Index", 
            position = "bottomleft") 




## Get domain of workload data
(log_domain <- range(Venues_workload_map$Logistics))
log_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = log_domain)


venue_load_Logistics_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Logistics Index: ",  Venues_workload_map$Logistics,           "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Logistics Index ----
Venue_Map_Survey_Logistics <- leaflet(NSW_LGA_boundaries_venues,
                                      height  = 500, 
                                      width   = 1600, 
                                      padding = 0, 
                                      options = leafletOptions(zoomControl = FALSE,
                                                               minZoom     = 6, 
                                                               maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_Logistics_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ work_pal(Venues_workload_map$Logistics),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  log_pal, 
            values   =~ Venues_workload_map$Logistics, 
            opacity  =  0.9, 
            title    = "Venue Logistics Index", 
            position = "bottomleft") 






## Get domain of workload data
(ops_domain <- range(Venues_workload_map$Operations))
ops_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = ops_domain)


venue_load_Operations_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Operations Index: ", Venues_workload_map$Operations,           "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Operations Index ----
Venue_Map_Survey_Operations <- leaflet(NSW_LGA_boundaries_venues,
                                       height  = 500, 
                                       width   = 1600, 
                                       padding = 0, 
                                       options = leafletOptions(zoomControl = FALSE,
                                                                minZoom     = 6, 
                                                                maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_Operations_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ ops_pal(Venues_workload_map$Operations),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  ops_pal, 
            values   =~ Venues_workload_map$Operations, 
            opacity  =  0.9, 
            title    = "Venue Operations Index", 
            position = "bottomleft") 




## Get domain of workload data
(rec_domain <- range(Venues_workload_map$Recruitment))
rec_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = rec_domain)


venue_load_Recruitment_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Recruitment Index: ", Venues_workload_map$Recruitment,           "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Recruitment Index ----
Venue_Map_Survey_Recruitment <- leaflet(NSW_LGA_boundaries_venues,
                                        height  = 500, 
                                        width   = 1600, 
                                        padding = 0, 
                                        options = leafletOptions(zoomControl = FALSE,
                                                                 minZoom     = 6, 
                                                                 maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_Recruitment_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ rec_pal(Venues_workload_map$Recruitment),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  rec_pal, 
            values   =~ Venues_workload_map$Recruitment, 
            opacity  =  0.9, 
            title    = "Venue Recruitment Index", 
            position = "bottomleft") 




## Get domain of workload data
(staf_domain <- range(Venues_workload_map$Staffing))
staf_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = staf_domain)


venue_load_Staffing_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Staffing Index: ",   Venues_workload_map$Staffing,           "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Staffing Index ----
Venue_Map_Survey_Staffing <- leaflet(NSW_LGA_boundaries_venues,
                                     height  = 500, 
                                     width   = 1600, 
                                     padding = 0, 
                                     options = leafletOptions(zoomControl = FALSE,
                                                              minZoom     = 6, 
                                                              maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_Staffing_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ staf_pal(Venues_workload_map$Staffing),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  staf_pal, 
            values   =~ Venues_workload_map$Staffing, 
            opacity  =  0.9, 
            title    = "Venue Staffing Index", 
            position = "bottomleft") 





## Get domain of workload data
(train_domain <- range(Venues_workload_map$Training))
train_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = train_domain)


venue_load_Training_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Training Index: ",   Venues_workload_map$Training,           "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Training Index ----
Venue_Map_Survey_Training <- leaflet(NSW_LGA_boundaries_venues,
                                     height  = 500, 
                                     width   = 1600, 
                                     padding = 0, 
                                     options = leafletOptions(zoomControl = FALSE,
                                                              minZoom     = 6, 
                                                              maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_Training_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ staf_pal(Venues_workload_map$Training),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  train_pal, 
            values   =~ Venues_workload_map$Training, 
            opacity  =  0.9, 
            title    = "Venue Training Index", 
            position = "bottomleft")



## Get domain of workload data
(ven_domain <- range(Venues_workload_map$Venues))
ven_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = ven_domain)


Venues_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Venues Index: ",     Venues_workload_map$Venues,                "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Venues Index ----
Venue_Map_Survey_Venues <- leaflet(NSW_LGA_boundaries_venues,
                                   height  = 500, 
                                   width   = 1600, 
                                   padding = 0, 
                                   options = leafletOptions(zoomControl = FALSE,
                                                            minZoom     = 6, 
                                                            maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = Venues_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ staf_pal(Venues_workload_map$Venues),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  ven_pal, 
            values   =~ Venues_workload_map$Venues, 
            opacity  =  0.9, 
            title    = "Venues Index", 
            position = "bottomleft")





## Get domain of workload data
(whs_domain <- range(Venues_workload_map$WHS))
whs_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = whs_domain)


venue_load_WHS_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "WHS Index: ",        Venues_workload_map$WHS,                 "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue WHS Index ----
Venue_Map_Survey_WHS <- leaflet(NSW_LGA_boundaries_venues,
                                height  = 500, 
                                width   = 1600, 
                                padding = 0, 
                                options = leafletOptions(zoomControl = FALSE,
                                                         minZoom     = 6, 
                                                         maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = Venues_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ staf_pal(Venues_workload_map$Venues),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  whs_pal, 
            values   =~ Venues_workload_map$WHS, 
            opacity  =  0.9, 
            title    = "Venues WHS Index", 
            position = "bottomleft")





## Get domain of workload data
(count_domain <- range(Venues_workload_map$Counting))
count_pal     <- colorNumeric(palette = brewer.pal(5, 'RdYlBu'), domain = count_domain)


venue_load_Counting_labels <- paste(
  
  ## Create a list of variables people can see by hovering
  "RO: ",               Venues_workload_map$ReturningOffice,     "<br/>", 
  "Venue: ",            Venues_workload_map$VenueName,           "<br/>", 
  "Votes: ",            Venues_workload_map$Total_Votes,         "<br/>",
  "Workload: ",         Venues_workload_map$Venue_Voting_Load,   "<br/>",
  "Counting Index: ",   Venues_workload_map$Counting,            "<br/>",
  "Remoteness: ",       Venues_workload_map$Remoteness,          "<br/>",
  "Informal rate: ",    Venues_workload_map$Informality,         "<br/>",
  "Satisfaction: ",     Venues_workload_map$Venue_Satisfied,     "<br/>",
  "Disatisfaction: ",   Venues_workload_map$Venue_Dissatisfied,  "<br/>", 
  sep = "") %>% lapply(htmltools::HTML)


## Map of Venue Counting Index ----
Venue_Map_Survey_Counting <- leaflet(NSW_LGA_boundaries_venues,
                                     height  = 500, 
                                     width   = 1600, 
                                     padding = 0, 
                                     options = leafletOptions(zoomControl = FALSE,
                                                              minZoom     = 6, 
                                                              maxZoom     = 20)) %>%
  ## set the boundaries
  setView(151.0000486, -33.8027653, zoom = 6) %>%
  addTiles() %>%
  addResetMapButton() %>%
  
  ## Add polygons
  leaflet::addPolygons(
    weight      = 2,
    fillOpacity = 0.05,
    
    highlight      = highlightOptions(
      color        = "#514d4d",
      fillOpacity  = 0.05,
      bringToFront = FALSE),
    popup = venue_labels,
    
    labelOptions = labelOptions(
      style      = list("font-weight" = "normal", padding = "3px 8px"),
      textsize   = "15px",
      direction  = "auto"),
    group        = 'LGA') %>%
  
  ## Add markers for Venue workload
  addCircleMarkers(data         = Venues_workload_map,
                   lat          = Venues_workload_map$Latitude, 
                   lng          = Venues_workload_map$Longitude,
                   label        = venue_load_Counting_labels, 
                   labelOptions = labelOptions(textsize = 15),
                   radius       = 0.7,
                   color        = ~ count_pal(Venues_workload_map$Counting),
                   
                   
                   ## Group is used to 'facet' the layers
                   group = 'Venue Workloads') %>% 
  
  ## Add legend
  addLegend(pal      =  count_pal, 
            values   =~ Venues_workload_map$Counting, 
            opacity  =  0.9, 
            title    = "Venue Counting Index", 
            position = "bottomleft")




#################################################### TBC ###########################################################