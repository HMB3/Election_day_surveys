################################# STAFF SURVEY RESULTS MAPS ######################################


## Purpose ----
## This code creates Maps of the Survey indexes.
## We are aggregating the respondent-level data and indexes up to the RO Region boundaries





## 1). PREPARE SURVEY SPATIAL DATA ============================================================


## There are two kinds of spatial data for the survey :
## 1). Polygons : this is just the RO Regions, we join the averaged respondent-level data to the RO regions
## 2). Points : create point shapefiles of the respondent-level data, and the Venue-level data


## Do we need an API key to create the google maps? Should just work as Open maps?
# api_key <- "AIzaSyB-ltOEpu8ZwF7H-ZOIIp5UmXHwewu27eA"


## Create Polygons ----
## Read in the NSW LGA boundaries - only using these for context, not to join survey data
NSW_LGA_boundaries       <- readOGR(dsn = './02 Source data/Context_data/NSW_LGA_2020/nsw_lga.shp')


# Repair geometry function extracted from the aim.analysis package not working and package not installing
# see LG_Survey_Table_Functions.R
#repair_geometry()


## Change the name of the LGAs to match NSWEC data
names(NSW_LGA_boundaries@data)[names(NSW_LGA_boundaries@data)=="ABB_NAME"] <- "LGAreaCode"


## Update the council names, etc.
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



## Most of the code we need is here, it just needs to change the specifics of the labels, etc.
# View(NSW_LGA_boundaries@data)



## Read in the RO boundary shapefile - created by Bron in Feb 2022.
## These will be used for
NSWEC_RO_boundaries_shp  <- readOGR(dsn = './02 Source data/Context_data/RO_regions/RO_region_boundaries_LG21.shp')


NSWEC_RO_boundaries <- NSWEC_RO_boundaries_shp %>% 
  
  # Repair geometry function extracted from the aim.analysis package not working and package not installing
  # see LG_Survey_Table_Functions.R
  
  # repair_geometry() %>%
  
  ## Convert to SF for standard dplyr manipulation of spatial data
  st_as_sf() %>% 
  
  ## Get rid of the Extra text in the council names - really annoying
  rename(ReturningOffice = Region) %>% 
  mutate(ReturningOffice = paste0(ReturningOffice, " RO Office")) %>%
  
  ## Make RO names match
  mutate(ReturningOffice = gsub("Hunters Hill RO Office", "Hunter's Hill Region RO Office", ReturningOffice)) %>%
  mutate(ReturningOffice = gsub("North Sydney RO Office", "North Sydney Region RO Office",  ReturningOffice)) %>%
  mutate(ReturningOffice = gsub("Bega Valley RO Office",  "Bega Valley Region RO Office",   ReturningOffice)) %>%
  
  ## Convert back to a SPDF and reproject
  mutate(ReturningOffice = trimws(ReturningOffice)) %>%
  left_join(respondent_index_ro_average, 
            by = c('ReturningOffice')) %>% 
  select(names(respondent_index_ro_average), everything()) %>% 
  
  ## Notr the IS index is 0 for Cobar, Murray River, Waverly and Kiama.
  ## Does that make sense?
  replace_na(list(IS = 0)) %>%
  st_set_crs(3577) %>% 
  st_transform(., crs = 4283)


## Remove the NA EMs
NSWEC_RO_boundaries     <- NSWEC_RO_boundaries[!is.na(NSWEC_RO_boundaries$Counting),]
# RO_coords               <- sp::coordinates(NSWEC_RO_boundaries %>% sf:::as_Spatial()) %>% as_tibble()
# names(RO_coords)        <- c('lat', 'lon')
# NSWEC_RO_boundaries$Lat <- RO_coords$lat
# NSWEC_RO_boundaries$Lon <- RO_coords$lon
NSWEC_RO_boundaries_shp <- NSWEC_RO_boundaries %>% sf:::as_Spatial()



## Create Point Data ---- 
## Create Respondent-level shapefile of survey results
respondent_indexes_spatial <- respondent_index_ro_join %>% 
  
  SpatialPointsDataFrame(coords      = .[c("Longitude", "Latitude")],
                         data        = .,
                         proj4string = CRS('+init=epsg:4283')) %>% st_as_sf()


## Create Venue-level shapefile of survey results
venue_indexes_spatial <- respondent_index_venue_average %>%
  
  ## Now join on the context columns 
  SpatialPointsDataFrame(coords      = .[c("Longitude", "Latitude")],
                         data        = .,
                         proj4string = CRS('+init=epsg:4283')) %>% st_as_sf()





## 2). RO REGION DEMOGRAPHY POLYGON MAPS ==========================================================


## We can create maps at the district level for both election data (votes, etc), 
## and for each topic (WHS, etc.)


## This might not make sense for the Venue-level data.
## These maps are not that useful, and change for each setting so harder to use the function
demo_plot_list <- c('Venues_over_proj', 
                    'Respondent_Satisfied', 
                    'Respondent_Dissatisfied',
                    'Informality',
                    'Indigenous_prop', 
                    'CALD_prop')


RO_region_polygon_demo_maps <- 
  
  leaflet_poly_map_list(topic_list = demo_plot_list, 
                        bound_poly = NSWEC_RO_boundaries_shp,
                        jenk_no    = 5,
                        rev_leg    = FALSE,
                        leg_text   = '',
                        
                        returning_topic_labels = paste(
                          
                          ## Create a list of variables people can see by hovering
                          "ReturningOffice: ",  NSWEC_RO_boundaries_shp@data$ReturningOffice,  "<br/>", 
                          "Enrolment: ",        NSWEC_RO_boundaries_shp@data$Enrolment,        "<br/>",
                          "Remoteness: ",       NSWEC_RO_boundaries_shp@data$Remoteness,       "<br/>",
                          "CALD: ",             NSWEC_RO_boundaries_shp@data$CALD_prop,        "<br/>",
                          "ATSI: ",             NSWEC_RO_boundaries_shp@data$Indigenous_prop,  "<br/>",
                          "High SES: ",         NSWEC_RO_boundaries_shp@data$High_SEIFA_prop,  "<br/>",
                          "Low SES: ",          NSWEC_RO_boundaries_shp@data$Low_SEIFA_prop,   "<br/>",
                          "Informal rate: ",    NSWEC_RO_boundaries_shp@data$Informality,      "<br/>",
                          "Venues over prj: ",  NSWEC_RO_boundaries_shp@data$Venues_over_proj, "<br/>",
                          "Satisfaction: ",     NSWEC_RO_boundaries_shp@data$Respondent_Satisfied,     "<br/>",
                          "Disatisfaction: ",   NSWEC_RO_boundaries_shp@data$Respondent_Dissatisfied,  "<br/>", 
                          sep = "") %>% lapply(htmltools::HTML),
                        
                        view_lon   = 151.0000486,
                        view_lat   = -33.8027653, 
                        zoom_set   = 6,
                        fillOp     = 0.9,
                        fillcol    = "blue",
                        fillwt     = 2,
                        textsz     = "13px")





## 3). VENUE POINT MAPS ==========================================================


## Create a shp for creating maps
venue_indexes_spatial_shp <- venue_indexes_spatial %>% sf:::as_Spatial()


venue_map_vars <- c('Total_Votes',
                    'Venue_Voting_Load',
                    'Informality')


## 
venue_variable_polygon_maps <- 
  
  leaflet_point_map_list(topic_list = venue_map_vars,
                         bound_poly = NSW_LGA_boundaries,
                         point_data = venue_indexes_spatial_shp,
                         jenk_no    = 5,
                         rev_leg    = FALSE,
                         leg_text   = '',
                         
                         view_lon   = 151.0000486,
                         view_lat   = -33.8027653,
                         zoom_set   = 6,
                         fillOp     = 0.9,
                         fillcol    = "blue",
                         fillwt     = 2,
                         textsz     = "13px")





## Create a list of maps of the Venues
venue_index_polygon_maps <- 
  
  leaflet_point_map_list(topic_list = index_plot_columns,
                         bound_poly = NSW_LGA_boundaries,
                         point_data = venue_indexes_spatial_shp,
                         jenk_no    = 5,
                         rev_leg    = TRUE,
                         leg_text   = ' Question Index',
                         
                         view_lon   = 151.0000486,
                         view_lat   = -33.8027653,
                         zoom_set   = 6,
                         fillOp     = 0.9,
                         fillcol    = "blue",
                         fillwt     = 2,
                         textsz     = "13px")





## RO REGION COUNTING INDEX MAP =====


## Make a function to create the maps for each LGA/RO/District
RO_region_polygon_index_maps <- 
  
  leaflet_poly_map_list_index(topic_list = index_plot_columns, 
                              bound_poly = NSWEC_RO_boundaries_shp,
                              jenk_no    = 5,
                              rev_leg    = TRUE,
                              leg_text   = ' Questions Index',
                              
                              view_lon   = 151.0000486,
                              view_lat   = -33.8027653, 
                              zoom_set   = 6,
                              fillOp     = 0.9,
                              fillcol    = "blue",
                              fillwt     = 2,
                              textsz     = "13px")





## 5). SAVE SURVEY RESULTS TO TABLE  ==================================================


# ## Make the survey columns case-sensitive unique
# LG21_Survey_data_final_output <- Survey_data_complete_LG_RO_distinct
# 
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "b2",  replacement = "b2_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "c2",  replacement = "c2_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "c3",  replacement = "c3_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "c12", replacement = "c12_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "d7",  replacement = "d7_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "e2",  replacement = "e2_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "f2",  replacement = "f2_col")
# 
# names(LG21_Survey_data_final_output) <- 
#   gsub(x = names(LG21_Survey_data_final_output), pattern = "g16", replacement = "g16_col")
# 
# 
# ## truncate combo table
# Master_satis_tab_combo <- Master_satisfaction_table_combined
# 
# 
# 
# ## Write full survey data to file
# write_csv(LG21_Survey_data_final_output, 
#           paste0(survey_tabular_output, 
#                  'LG21_Staff_Survey_Responses.csv'))
# 
# 
# ## Save survey data as a table
# ## Create a workbook to store the demography analyses
# LG21_Results_workbook <- createWorkbook()
# 
# 
# ## 
# tables_list <- c('table_of_contents',
#                  'LUT_index',
#                  'Master_satis_tab_combo',
#                  'resp_satis_tab_wider_cont',
#                  'demography_satisfaction_table',
#                  'Venue_satisfaction_table_wider',
#                  'respondent_index_ro_join',
#                  'respondent_index_venue_average',
#                  'respondent_index_ro_average',
#                  'ord3_question_summary_table',
#                  'LG21_Survey_data_final_output')
# 
# 
# LG21_Results_workbook <- createWorkbook()
# 
# 
# for(file in tables_list) {
#   
#   ## Get required columns: file <- results_list[1]
#   File_to_Write <- get(file)
#   
#   write_excel_csv(File_to_Write,
#                   paste0(survey_tabular_output, file, '.csv'))
#   
#   ## Add worksheet to the spread sheet
#   message('writing ', file,  ' to results csv and spreadsheet')
#   addWorksheet(LG21_Results_workbook, file)
#   
#   ## Write the data to the corresponding worksheet
#   writeDataTable(wb         = LG21_Results_workbook,
#                  sheet      = file,
#                  x          = get(file),
#                  startCol   = 1,
#                  startRow   = 1,
#                  rowNames   = FALSE,
#                  tableStyle = "TableStyleMedium2")
#   
# }
# 
# 
# ## Save the whole workbook
# setwd(survey_root)
# saveWorkbook(LG21_Results_workbook,
#              paste0(survey_tabular_output, 'LG21_Results_workbook.xlsx'),
#              overwrite = TRUE)
# 
# survey_database_loc <- paste0(survey_spatial_output, 'LG21_Survey_Results_Database_Spatial.gpkg')
# 
# 
# if(file.exists(survey_database_loc) == TRUE) {
#   file.remove(survey_database_loc)
# }
# 
# 
# ## Save spatial data (and tables) to a geopackage ---
# st_write(respondent_indexes_spatial, 
#          dsn    = paste0(survey_spatial_output, 'LG21_Survey_Results_Database_Spatial.gpkg'), 
#          layer  = 'LG21_Survey_respondent_indexes', 
#          quiet  = TRUE,
#          append = FALSE)
# 
# 
# st_write(venue_indexes_spatial, 
#          dsn    = paste0(survey_spatial_output, 'LG21_Survey_Results_Database_Spatial.gpkg'), 
#          layer  = 'LG21_Survey_venue_indexes', 
#          quiet  = TRUE,
#          append = FALSE)
# 
# 
# st_write(NSWEC_RO_boundaries, 
#          dsn    = paste0(survey_spatial_output, 'LG21_Survey_Results_Database_Spatial.gpkg'), 
#          layer  = 'LG21_Survey_NSWEC_RO_boundaries', 
#          quiet  = TRUE,
#          append = FALSE)
# 
# 
# ## Save the tables to an SQL 
# con = dbConnect(SQLite(), dbname = paste0(survey_spatial_output, 'LG21_Survey_Results_Database_Spatial.gpkg'))
# 
# 
# ## see what's in there already:
# dbListTables(con)
# 
# 
# ## Now add all the tables
# for(table in tables_list) {
#   
#   ## Get required columns: table <- tables_list[11]
#   File_to_Write <- get(table)
#   
#   message('saving to database ', table)
#   #Error: duplicate column name: b2_1
#   dbWriteTable(con, table, File_to_Write, overwrite = TRUE)
#   
# }
# 
# 
# ## Check if it works?
# dbListTables(con)
# dbDisconnect(con)





#################################################### TBC ##################################################