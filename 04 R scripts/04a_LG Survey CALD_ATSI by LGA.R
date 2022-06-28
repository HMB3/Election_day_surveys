####################################################################################################################
######################################## ---- CALD analysis BY LGA ---- ############################################
####################################################################################################################


## This code quantifies the linguistic diversity (CALD) inside each council - (i.e. LGA), rather than within the NSWEC 
## district, as per the previous analysis for SG19.
## It also quantifies the proportion of people of ATSI background in each suburb in each LGA.


## The ABS data are drawn from the ABS table builder, using 2016 Census data.
## The ABS numbers are not the same as the EC databases - i.e. Elligible voters are not the same as enrolled voters.
## So we need to either state "Elligible" rather than "Enrolled", or use the EC data where needed.


## Rowise sorting columns of df ----
## Used in Adhoc 19069
sort_cols_rowise = function(sort_table, col_names1, col_names2){
  
  ## First, create the dataframe to store sorting
  sorted_table <- data.frame()
  
  ## For all rows in the data frame
  for (row in 1:nrow(sort_table)) {
    
    ## Report progress
    progress(row, nrow(sort_table))
    
    ## Then index each row, sort the columns in decreasing order -
    ## E.G. most popular languages in each area
    sort_1 <- sort_table[row,]
    sort_1 <- sort(sort_1, decreasing = TRUE)
    sort_1 <- data.frame(lapply(sort_1, function(x) t(data.frame(x))))
    
    ## Then change the column names of the first 5 columns 
    sort_2 <- colnames(sort_1)[1:5]
    sort_2 <- data.frame(lapply(sort_2, function(x) t(data.frame(x))))
    
    ## Just get the first five columns
    colnames(sort_2) <- col_names1
    sort_3 <- sort_1[,1:5]
    colnames(sort_3) <- col_names2
    
    ## And bind them together
    sort_4       <- cbind(sort_2, sort_3)
    sorted_table <- rbind(sorted_table, sort_4)
    
  }
  
  ## Return the sorted df
  return(sorted_table)
  
}





## 1). AGGREGATE CALD WITH LGA ======================================================================================


##
message('Running CALD LGA analysis :: aggregate CALD within LGA')
# A note RE the LGA 2016/19 boundaries ::
#   
# -	We're using the Language data from 2016 by SA1, joined to 2016 LGA. ABS doesn't supply new data for 2019, if they have them
# -	The difference between the 2016 and 2019 LGAs seems to be Rockdale and Botany Bay, which are now both part of "Bayside" :: 
#    https://www.bayside.nsw.gov.au/
# -	For the 128-odd other councils which have the same name, we are assuming they are the same, although there could be 
#   slight differences. The Gdiffernce functions suggests that all the diverse LGAs are the same
# -	Thus, I'm simply summing the Rockdale and Botany Bay LGA figures into a new Bayside LGA.
# -	In future, we can run a proper analysis to re-distribute the numbers. 
#   https://gis.stackexchange.com/questions/201883/identifying-differences-in-polygons-across-shape-files


## Also, could be difference in ::
# Northern Beaches (Pittwater, Manly, Warringah)
# Central Coast 
# Cumberland



## Read in elector data :: using the website, because it's more up-to-date that the databases
## https://roll.elections.nsw.gov.au/lg/ex35
LGA_TOT <- read_csv('./02 Source data/Context_data/LGA/LGA_ENROLENT_SUM.csv') %>% 
  
  dplyr::rename(LGA  = `Area Name`, 
                SA1s = `No of SA1s`) %>%
  mutate(LGA = gsub(" Council",        "", LGA)) %>%
  mutate(LGA = gsub("City of ",        "", LGA)) %>%
  mutate(LGA = gsub(" City",           "", LGA)) %>%
  mutate(LGA = gsub("Council of the ", "", LGA)) %>%
  mutate(LGA = gsub("Municipal",       "", LGA)) %>%
  mutate(LGA = gsub(" Shire",          "", LGA)) %>%
  mutate(LGA = gsub(" Regional",       "", LGA)) %>%
  mutate(LGA = trimws(LGA))

names(LGA_TOT) <- gsub(x = names(LGA_TOT), pattern = "\\.", replacement = "_")


## Read in CALD and LGA data ---- 
## This data is already filtered to people +18
CALD <- read.csv("./02 Source data/Context_data/LOI_by_SA1.csv", skip = 11) 


## Filter zero population from LGA
## "Count" is the poluation in each SA1
## This is missing Dubbo, Cootamundar Gundagai and Nambucca 
## Prob need to get this again
LGA_SA1  <- read.csv("./02 Source data/Context_data/LGA_SA1.csv", skip = 9) %>% 
  
  filter(Count > 0) %>%
  dplyr::select(-Counting, -X, -Count) %>%
  rename(., SA1 = "SA1..UR.",
         LGA    = "LGA..UR.") %>% 
  mutate(LGA    = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA)))



## Read in the filtered ABS population data
## This is only people over 18, who are Australian Citizens
LGA_pop <- read.csv("./02 Source data/Context_data/LGA_languages.csv", skip = 11) %>% 
  
  .[-1,] %>% 
  head(., -9) %>% 
  rename(LGA = "LANP_._1_Digit_Level",
         Elligible_Population = Total) %>%
  
  mutate(Northern_European_Languages = as.numeric(Northern_European_Languages)) %>% 
  mutate(Non_English = rowSums(dplyr::select(., -LGA, -Not_stated, -Elligible_Population, 
                                             -Supplementary_codes, -Other_Languages))) %>% 
  mutate(English_only = Elligible_Population - Non_English) %>%
  mutate(LGA = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA))) %>% 
  
  ## Remove the weird LGAs
  dplyr::select(LGA, English_only, Elligible_Population) %>% 
  filter(!LGA %in% c("Migratory - Offshore - Shipping (NSW)", "No usual address (NSW)", "Unincorporated NSW")) 


## Join SA1 with LGA
LGA_SA1 <- LGA_SA1 %>% 
  
  left_join(., LGA_pop, by = "LGA") %>% 
  
  ## Remove the weird LGAs
  dplyr::select(LGA, SA1, English_only, Elligible_Population) %>% 
  filter(!LGA %in% c("Migratory - Offshore - Shipping (NSW)", "No usual address (NSW)", "Unincorporated NSW")) 


## Rename and reorder language columns ----
## There are two "Croation" columns, but keep these.
## They are not one of the most popular languages
CALD <- head(CALD, -3)
CALD <- CALD[-1, c(1:63)]
LOI  <- c("SA1",
          "German",
          "Dutch",
          "Afrikaans",
          "French",
          "Greek",
          "Portuguese",
          "Spanish",
          "Italian",
          "Maltese",
          "Hungarian",
          "Russian",
          "Ukrainian",
          "Bosnian",
          "Croatian",
          "Macedonian",
          "Serbian",
          "Serbo_Croatian",
          "Polish",
          "Romanian",
          "Kurdish",
          "Dari",
          "Persian",
          "Hazaraghi",
          "Arabic",
          "Assyrian",
          "Chaldean",
          "Turkish",
          "Armenian",
          "Malayalam",
          "Tamil",
          "Telugu",
          "Bengali",
          "Gujarati",
          "Hindi",
          "Marathi",
          "Nepali",
          "Punjabi",
          "Sinhalese",
          "Urdu",
          "Burmese",
          "Khmer",
          "Vietnamese",
          "Lao",
          "Thai",
          "Indonesian",
          "Tagalog",
          "Filipino",
          "Chinese",
          "Cantonese",
          "Hakka",
          "Mandarin",
          "Wu",
          "MinNan",
          "Japanese",
          "Korean",
          "Dinka",
          "Fijian",
          "Samoan",
          "Tongan",
          "SignLanguages",
          "Auslan",
          "NonVerbal")


## Re-order as Language of Importance (Number of Low English Users)
LOIOrder <- c("SA1",
              "Vietnamese",
              "Cantonese",
              "Greek",
              "Mandarin",
              "Arabic",
              "Italian",
              "Macedonian",
              "Spanish",
              "Korean",
              "Khmer",
              "Turkish",
              "Serbian",
              "Croatian",
              "Russian",
              "Polish",
              "Persian",
              "Dari",
              "Assyrian",
              "MinNan",
              "Thai",
              "Portuguese",
              "Bosnian",
              "Chaldean",
              "NonVerbal",
              "Lao",
              "Hakka",
              "Maltese",
              "Auslan",
              "Hindi",
              "Burmese",
              "Punjabi",
              "Tagalog",
              "Hungarian",
              "Tamil",
              "Samoan",
              "Indonesian",
              "Hazaraghi",
              "Urdu",
              "Nepali",
              "Chinese",
              "Romanian",
              "Bengali",
              "French",
              "Armenian",
              "Croatian",
              "Filipino",
              "German",
              "Gujarati",
              "Dinka",
              "Sinhalese",
              "Tongan",
              "Ukrainian",
              "Wu",
              "Kurdish",
              "SignLanguages",
              "Japanese",
              "Dutch",
              "Telugu",
              "Malayalam",
              "Fijian",
              "Afrikaans",
              "Marathi")


## Re-order language columns
## German column was a factor
colnames(CALD) <- LOI
CALD <- CALD[ ,LOIOrder]
CALD <- CALD %>% 
  mutate(German = as.numeric(German)) %>% 
  mutate(SA1 = as.factor(SA1))


## Convert values to numeric, a matrix
CALD1 <- CALD
CALD1 <- CALD1[,2:63]
CALD1 <- sapply(CALD1, function(x) as.numeric(x))


## Now join the CALD to the LGA data
LGA_SA1_CALD <- CALD %>% 
  left_join(., LGA_SA1, by = "SA1") %>% 
  dplyr::select(LGA, SA1, Elligible_Population, English_only, everything()) %>% 
  filter(!LGA %in% c("Migratory - Offshore - Shipping (NSW)", "No usual address (NSW)", "Unincorporated NSW")) 



## Group by LGA, aggregate and sum up all languages
## 133 LGAs in NSW - calculate Top 5/3 languages in each LGA
length(unique(LGA_SA1_CALD$LGA))


## Group by LGA and sum languages spoken ----
## Worth checking if this works
## The group_by is not working to combine the council data
LGA_SA1_LOI <- LGA_SA1_CALD                           %>% 
  
  dplyr::select(-Elligible_Population, -English_only) %>% 
  group_by(LGA)                    %>% 
  dplyr::select(-SA1)              %>% 
  summarize_if(., is.numeric, sum) %>% 
  na.omit()                        %>% 
  left_join(., LGA_pop)            %>%
  dplyr::select(LGA, Elligible_Population, English_only, everything()) %>% 
  as.data.frame()
row.names(LGA_SA1_LOI) <- LGA_SA1_LOI$LGA


## Combine data for Botany Bay and Rockdale councils ----
bayside_CALD            <- LGA_SA1_LOI["Rockdale", ][-1] + LGA_SA1_LOI["Botany Bay", ][-1]
bayside_CALD$LGA        <- "Bayside" 
row.names(bayside_CALD) <- "Bayside" 
bayside_CALD            <- bayside_CALD %>% dplyr::select(LGA, everything()) 


## Bayside should still be amoung the most linguistically diverse LGAs
LGA_SA1_LOI <- LGA_SA1_LOI %>%
  
  .[!rownames(.) %in% c("Rockdale", "Botany Bay"), ] %>% 
  bind_rows(., bayside_CALD) %>%
  arrange(LGA) %>% 
  mutate(CALD_population = rowSums(dplyr::select(., -LGA, -Elligible_Population, -English_only))) %>% 
  filter(!LGA %in% c("Migratory - Offshore - Shipping", "No usual address", "Unincorporated")) 



## Include Sanity check on data
## These numbers match up with the ABS quick stats
## https://quickstats.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/LGA12900?opendocument
length(unique(LGA_SA1_LOI$LGA))
summary(LGA_SA1_LOI$Elligible_Population)


## So the LGA populations look reasonable?
LGA_LOI_plot <- split(LGA_SA1_LOI, sample(1:3, nrow(LGA_SA1_LOI), replace = T))


## Consider the distribution of questions - the aggregation scheme will affect the results
ggplot(LGA_LOI_plot[[1]], aes(x = reorder(LGA, -Elligible_Population), Elligible_Population)) + 
  geom_bar(stat ="identity", fill = "Blue") +
  theme(axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = comma)

ggplot(LGA_LOI_plot[[2]], aes(x = reorder(LGA, -Elligible_Population), Elligible_Population)) + 
  geom_bar(stat ="identity", fill = "Blue") +
  theme(axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  scale_y_continuous(labels = comma)

ggplot(LGA_LOI_plot[[3]], aes(x = reorder(LGA, -Elligible_Population), Elligible_Population)) + 
  geom_bar(stat ="identity", fill = "Blue") +
  theme(axis.text.x  = element_text(size = 7, angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  scale_y_continuous(labels = comma)


## Convert the LOI table to a matrix, so we can sort it
LGA_SA1_LOI_matrix  <- LGA_SA1_LOI[,c(4:65)]
LGA_SA1_LOI_matrix  <- sapply(LGA_SA1_LOI_matrix, as.numeric)


## Sort and count languages ----
LGA_SA1_LOI_sort = sort_cols_rowise(sort_table = LGA_SA1_LOI_matrix,
                                    
                                    col_names1 = c("LAN1",  "LAN2", "LAN3",  "LAN4",  "LAN5"),
                                    col_names2 = c("nLAN1", "nLAN2","nLAN3", "nLAN4", "nLAN5")) %>% 
  cbind(LGA_SA1_LOI, .) %>% 
  
  ## Join on the Enrolled Elector Populations
  left_join(., LGA_TOT, by = "LGA") %>% 
  
  ## Calculate the % for the total population for each langauge
  ## Note, we are using the total enrollment numbers from the EC website, not from the ABS
  ## https://roll.elections.nsw.gov.au/lg/ex35
  ## So Elligble_population is subbed in for Total_Enrolment
  mutate(LAN_1perc = paste0(round((nLAN1/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(LAN_1perc = formattable::percent(LAN_1perc)) %>%
  
  mutate(LAN_2perc = paste0(round((nLAN2/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(LAN_2perc = formattable::percent(LAN_2perc)) %>%
  
  mutate(LAN_3perc = paste0(round((nLAN3/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(LAN_3perc = formattable::percent(LAN_3perc)) %>%
  
  mutate(LAN_4perc = paste0(round((nLAN4/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(LAN_4perc = formattable::percent(LAN_4perc)) %>%
  
  mutate(LAN_5perc = paste0(round((nLAN5/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(LAN_5perc = formattable::percent(LAN_5perc)) 





## 2). CALCULATE LOW - ENGLISH PROFICIENCY ==============================================================================


## Read in table of low English proficiency from the ABS
message('Running CALD LGA analysis :: CALCULATE LOW - ENGLISH PROFICIENCY ')

SA1_low_eng <- read.csv("./02 Source data/Context_data/SA1_low_english_18_Aus.csv",  skip = 11) %>% 
  
  .[-1,] %>% 
  head(., -9) %>%
  
  rename(SA1                          = "ENGP.Proficiency.in.Spoken.English",
         Very_well                    = "Very.well",
         Not_well                     = "Not.well",
         Not_at_all                   = "Not.at.all", 
         LANP_and_ENGP_not_stated     = "Not.stated...both.language..LANP..and.proficiency..ENGP..not.stated",
         LANP_stated_ENGP_not_stated  = "Not.stated...language..LANP..stated..proficiency..ENGP..not.stated",
         Not_applicable               = "Not.applicable",
         Elligible_Population         = Total) %>% 
  
  # mutate(Per_no_eng = paste0(round((Not_at_all/Elligible_Population) * 100, digits = 1), "%")) %>% 
  # mutate(Per_no_eng  = formattable::percent(Per_no_eng)) %>% 
  mutate(Loweng_num = Not_well + Not_at_all) %>%  
  
  left_join(., dplyr::select(LGA_SA1, c(LGA, SA1)), by = "SA1") %>% 
  dplyr::select(., LGA, SA1, everything())


## Read in table of which langauges were spoken by people who identified as having low-english proficiency
ENGLP       <- read.csv("./02 Source data/Context_data/LOI_by_SA1_ENGLP.csv", skip = 12) 
ENGLP       <- head(ENGLP, -3)
ENGLP       <- ENGLP[-1, c(1:63)]
LOI         <- colnames(ENGLP) <- LOI


## Re-order as Language of Importance (Number of Low English Users)
ENGLP <- ENGLP[, LOIOrder] 


## Now join the CALD to the LGA data
SA1_ENGLP <- ENGLP %>% 
  
  left_join(., LGA_SA1, by = "SA1") %>% 
  dplyr::select(LGA, SA1, Elligible_Population, English_only, everything()) %>% 
  
  ## Now sum all the people who identify as as low English Proficiency
  mutate(Loweng_lan = rowSums(dplyr::select(., -LGA, -SA1, -Elligible_Population, -English_only))) %>% 
  left_join(., dplyr::select(SA1_low_eng, c(SA1, Loweng_num, Not_well, Not_at_all)), by = "SA1") %>%
  
  ## Remove extraneous Councils
  dplyr::select(LGA, SA1, Elligible_Population, English_only, 
                Loweng_lan, Loweng_num, Not_well, Not_at_all, everything()) %>% 
  filter(!LGA %in% c("Migratory - Offshore - Shipping", "No usual address", "Unincorporated"))  


## Group by LGA, aggregate and sum up all languages
## 133 LGAs in NSW - calculate Top 5/3 languages in each LGA
length(unique(SA1_ENGLP$LGA))


## Group by LGA and sum languages spoken ----
## Worth checking if this works
LGA_SA1_ENGLP <- SA1_ENGLP %>% 
  
  dplyr::select(-Elligible_Population, -English_only) %>% 
  group_by(LGA)       %>% 
  dplyr::select(-SA1) %>% 
  summarize_if(., is.numeric, sum) %>% 
  na.omit() %>% 
  left_join(., LGA_pop) %>%
  dplyr::select(LGA, Elligible_Population, English_only, Loweng_lan, everything()) %>%
  as.data.frame()
row.names(LGA_SA1_ENGLP) <- LGA_SA1_ENGLP$LGA


## Combine data for Botany Bay and Rockdale councils  ----
bayside            <- LGA_SA1_ENGLP["Rockdale", ][-1] + LGA_SA1_ENGLP["Botany Bay", ][-1]
bayside$LGA        <- "Bayside" 
row.names(bayside) <- "Bayside" 
bayside            <- bayside %>% dplyr::select(LGA, everything()) 


## Bayside should still be amoung the most linguistically diverse LGAs
LGA_SA1_ENGLP <- LGA_SA1_ENGLP %>%
  
  .[!rownames(.) %in% c("Rockdale", "Botany Bay"), ] %>% 
  bind_rows(., bayside) %>%
  arrange(LGA)


## Check the grouping has worked
length(unique(LGA_SA1_ENGLP$LGA))
summary(LGA_SA1_ENGLP$Elligible_Population)





## 3). CALCULATE TOP 5 LANGUAGES in LGA ======================================================================
message('Running CALD LGA analysis :: CALCULATE TOP 5 LANGUAGES in LGA')


## Convert the LOI table to a matrix, so we can sort it
LGA_SA1_ENGLP_matrix  <- LGA_SA1_ENGLP[,c(4:65)]
LGA_SA1_ENGLP_matrix  <- sapply(LGA_SA1_ENGLP_matrix, as.numeric)


## Sort the dataframe, then bind on the actual languages
## arguments imply differing number of rows: 132, 130
LGA_LOI_ENGLP_sort = sort_cols_rowise(sort_table   = LGA_SA1_ENGLP_matrix,
                                      col_names1   = c("LENGP1", "LENGP2", "LENGP3", "LENGP4", "LENGP5"),
                                      col_names2   = c("nLENGP1","nLENGP2","nLENGP3","nLENGP4","nLENGP5")) %>% 
  cbind(LGA_SA1_LOI_sort, .) 


## Then just select the language calculated columns
LGA_LOI_ENGLP <- LGA_LOI_ENGLP_sort[ ,c(1, 2, 3, 66:88)] %>% 
  
  ## And sum up the non-english language columns
  mutate(SUMnLAN     = rowSums(dplyr::select(., starts_with("nLAN"))))   %>% 
  mutate(SUMnLENGP   = rowSums(dplyr::select(., starts_with("nLENGP")))) %>% 
  
  ## Then calculate the total % for the top languages using the total population
  mutate(PercentLAN  = paste0(round((SUMnLAN/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(PercentLAN  = formattable::percent(PercentLAN)) %>% 
  
  ## Calculate the % of low english proficiency
  mutate(PercentENGP = paste0(round((SUMnLENGP/SUMnLAN)  * 100, digits = 1), "%")) %>% 
  mutate(PercentENGP = formattable::percent(PercentENGP)) %>%
  
  ## Calculate the % of English only speakers - original number from the ABS
  mutate(English_only_per = paste0(round((English_only/Total_Enrolment) * 100, digits = 1), "%")) %>%
  mutate(English_only_per = formattable::percent(English_only_per)) %>%
  
  ## Diversity is 100 - English only
  mutate(Diversity = formattable::percent(1) - English_only_per) %>%
  
  ## 'Other languages' are 100 - (english + top 5)
  mutate(Other_languages = (formattable::percent(1) - (English_only_per + LAN_1perc 
                                                       + LAN_2perc + LAN_3perc 
                                                       + LAN_4perc + LAN_5perc))) %>% 
  
  ## Now take a master CALD calculation that we can relate back to the survey responses
  mutate(CALD_prop = paste0(round((CALD_population/Elligible_Population) * 100, digits = 1), "%")) %>%
  mutate(CALD_prop = formattable::percent(CALD_prop)) %>%
  mutate(CALD = ifelse(CALD_prop > formattable::percent(0.30), "Yes", "No")) %>% 
  dplyr::select(LGA, Elligible_Population, CALD_population, CALD_prop, CALD) %>%
  arrange(-CALD_prop)





## 4). ABS DATA ======================================================================================


## For ABS data, we want to get indicators of 
## - SES LGA
## - Remoteness LGA
## - CALD LGA

## The SA1 is the main ABS unit of analysis
#https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003June%202020?OpenDocument


## Transform LGA SEIFA
LGA_SEIFA <- read_excel(path =  '02 Source data/Context_data/SEIFA_LGA.xlsx', 
                        sheet = 'Table 2', skip = 5)[1:16] %>%
  
  ## Get rid of the weird columns....
  select(-'...1', -'...3') %>% 
  dplyr::rename(., LGA                 = '...2',
                SEIFA_total     ='...15',
                Non_SEIFA_total ='...16') %>%
  
  ## Fix the LGA
  mutate(LGA = gsub("[()]", "", LGA)) %>%
  mutate(LGA = gsub(" A",   "", LGA)) %>%
  mutate(LGA = gsub(" C",   "", LGA)) %>%
  mutate(LGA = trimws(LGA)) %>% 
  
  ## Now get a summary of the ses scores in the SA1s
  mutate(`High_SEIFA` = rowSums(select(., `Decile 8 SA1s`, `Decile 9 SA1s`, `Decile 10 SA1s`)),
         `Low_SEIFA`  = rowSums(select(., `Decile 1 SA1s`, `Decile 1 SA1s`, `Decile 3 SA1s`)),
         
         High_SEIFA_prop = paste0(round((High_SEIFA/SEIFA_total) * 100, digits = 2), "%"),
         High_SEIFA_prop = formattable::percent(High_SEIFA_prop),
         
         Low_SEIFA_prop = paste0(round((Low_SEIFA/SEIFA_total) * 100, digits = 2), "%"),
         Low_SEIFA_prop = formattable::percent(Low_SEIFA_prop)) %>% 
  
  select(LGA, High_SEIFA_prop, Low_SEIFA_prop) %>% 
  
  mutate(LGA = gsub('Campbelltown NSW', 'Campbelltown',  LGA),
         LGA = gsub('Laneove',          'Lane Cove',     LGA),
         LGA = gsub('Centraloast NSW',  'Central Coast', LGA)) 
  





## ABS Remoteness Data ----


## Get the SEDs * SA1's
SED_SA1  <- read.csv("./02 Source data/Context_data/2020_sed_nsw_en.csv") %>% 
  
  ## Just get the columns we want
  select(ValueCode, GroupName) %>% 
  dplyr::rename(District            = GroupName,
                SA1_7DIGITCODE_2016 = ValueCode) %>% 
  mutate(SA1_7DIGITCODE_2016 = as.factor(SA1_7DIGITCODE_2016)) 


## Get the LGAs * SA1's
LGA_SA1  <- read.csv("./02 Source data/Context_data/LGA_SA1.csv", skip = 9) %>% 
  filter(Count > 0) %>%
  dplyr::select(-Counting, -X, -Count) %>%
  dplyr::rename(., SA1_7DIGITCODE_2016 = "SA1..UR.",
                LGA = "LGA..UR.") %>% 
  mutate(LGA = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA))) %>% 
  mutate(SA1_7DIGITCODE_2016 = as.factor(SA1_7DIGITCODE_2016))  %>% 
  dplyr::select(LGA, SA1_7DIGITCODE_2016) %>% distinct()


## Join the SEDs and the LGAs using SA1s
LGA_SED_SA1 <- LGA_SA1 %>% left_join(., SED_SA1, by = "SA1_7DIGITCODE_2016") %>% 
  select(SA1_7DIGITCODE_2016, LGA, District)


## GET the SA1's * Remoteness
SA1_REMOTENESS <- read_csv('./02 Source data/Context_data/RA_2016_AUST.csv') %>% 
  
  mutate(SA1_7DIGITCODE_2016 = as.factor(SA1_7DIGITCODE_2016))   %>% 
  dplyr::select(SA1_7DIGITCODE_2016, RA_CODE_2016, RA_NAME_2016) %>% distinct()


## Get the LGAs * Remoteness
SA1_LGA_REMOTENESS  <- LGA_SED_SA1 %>% inner_join(., SA1_REMOTENESS, by = "SA1_7DIGITCODE_2016")





## Because some Districts contain SA1s which are in different remoteness categories, 
## we need to classify the Districts. This means deciding if each LGA is majority remote, 
## or majority very remote, etc.
LGA_REMOTENESS_CLASSIFY <- sort(unique(SA1_LGA_REMOTENESS$LGA)) %>%
  
  ## Pipe the list into lapply
  lapply(function(x) {
    
    ## x = sort(unique(SA1_LGA_REMOTENESS$LGA))[4]
    message("Classifying Districts into one remoteness category for ", x)
    LGA_df      <- filter(SA1_LGA_REMOTENESS, LGA == x) 
    
    ## Create a frequency table to determine which categry it belongs to
    LGA_remote  <- table(LGA_df$RA_NAME_2016) %>% as.data.frame() %>% arrange(-Freq) %>% 
      head(1) %>% .$Var1 %>% as.character()
    
    ## Create the Language table
    dist_remote_df <- tibble(LGA        = x,
                             ## Enter headline values
                             Remoteness = LGA_remote)
    
    ## Return the table
    return(dist_remote_df)
    
  }) %>% bind_rows() %>% 
  
  ## Convert Botany Bay and Rockdale to Bayside
  mutate(LGA = ifelse(LGA == "Botany Bay", "Bayside", LGA),
         LGA = ifelse(LGA == "Rockdale",   "Bayside", LGA)) %>% distinct()





## 5). AGGREGATE ATSI DATA WITH LGA =================================================================================


## Summarise ATSI at the LGA level too
LGA_ATSI = read.csv("./02 Source data/Context_data/LGA_ATSI.csv", skip = 11) %>% 
  
  .[-1,]      %>% 
  head(., -9) %>% 
  rename(Non_Indigenous = "Non.Indigenous",
         Indigenous     = "Both.Aboriginal.and.Torres.Strait.Islander",
         TSI            = "Torres.Strait.Islander",
         LGA            = "INGP.Indigenous.Status",
         Not_stated     = "Not.stated",
         Elligible_population = Total) %>%
  
  ## Check this calculation - some add to more than 100%, so they must not be mutually exclusive
  mutate(Indigenous_Population = Aboriginal + TSI) %>% 
  dplyr::select(LGA, Indigenous_Population, Elligible_population) %>% 
  
  ## Rename Western Regional Council, 
  ## and remove Central Darling, because it's in Administration
  mutate(LGA = gsub("\\s*\\([^\\)]+\\)","", as.character(LGA))) %>% 
  
  # mutate(LGA = ifelse(LGA == 'Western Plains Regional', "Dubbo", LGA)) %>% 
  # filter(LGA != 'Central Darling') %>% 
  mutate(Indigenous_prop = paste0(round((Indigenous_Population/Elligible_population) * 100, digits = 2), "%")) %>%
  mutate(Indigenous_prop = formattable::percent(Indigenous_prop)) %>% 
  mutate(Rank = dense_rank(desc(Indigenous_prop))) %>% 
  dplyr::select(LGA, Indigenous_Population, Indigenous_prop)

## Assign the Row names as the LGA names
row.names(LGA_ATSI) <- LGA_ATSI$LGA


## Combine data for Botany Bay and Rockdale councils ----
bayside_ATSI            <- LGA_ATSI["Rockdale", ][-1] + LGA_ATSI["Botany Bay", ][-1]
bayside_ATSI$LGA        <- "Bayside" 
row.names(bayside_ATSI) <- "Bayside" 
bayside_ATSI            <- bayside_ATSI %>% dplyr::select(LGA, everything())


## Bayside has same perecent Indigenous as the whole country
LGA_ATSI <- LGA_ATSI %>%
  
  .[!rownames(.) %in% c("Rockdale", "Botany Bay"), ] %>% 
  bind_rows(., bayside_ATSI) %>%
  arrange(LGA)               %>% 
  mutate(Indigenous_Population = rowSums(dplyr::select(., -LGA, -Indigenous_prop))) %>% 
  filter(!LGA %in% c("Migratory - Offshore - Shipping", "No usual address", "Unincorporated")) %>% 
  
  ## Add discrete ATSI column
  mutate(Indigenous = ifelse(Indigenous_prop > 0.10, 'Yes', 'No'))





## Master table of ABS context data at SG level for Survey ----
## EMS has the AEC's name, and ABS's name.
## Cootamundra Gundagai, Dubbo, etc
LGA_CALD_ABS_CONTEXT <- left_join(LGA_LOI_ENGLP, LGA_SEIFA, by = 'LGA') %>%
  
  left_join(., LGA_ATSI,                                    by = 'LGA') %>% 
  left_join(., LGA_REMOTENESS_CLASSIFY,                     by = 'LGA') %>% 
  
  ## Re-order the columns
  dplyr::select(LGA, Remoteness, CALD, CALD_prop, Indigenous, Indigenous_prop, High_SEIFA_prop, Low_SEIFA_prop) %>% 
  
  ## Get rid of council terms that don't match EC data
  mutate(LGA = gsub(" Council",        "", LGA)) %>%
  mutate(LGA = gsub("City of ",        "", LGA)) %>%
  mutate(LGA = gsub(" City",           "", LGA)) %>%
  mutate(LGA = gsub("Council of the ", "", LGA)) %>%
  mutate(LGA = gsub("Municipal",       "", LGA)) %>%
  mutate(LGA = gsub(" Shire",          "", LGA)) %>%
  mutate(LGA = gsub(" Regional",       "", LGA)) %>%
  mutate(LGA = trimws(LGA))





####################################################################################################################
#################################################### ---- TBC ---- #################################################
####################################################################################################################