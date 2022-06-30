############################################################################################
################################# TABLE FUNCTIONS ---- #####################################
############################################################################################




## DATA WRANGLING FUNCTIONS  ===============================================================


## Install and load packages if they aren't already
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}


# Repair geometry function from the aim.analysis package on github
# https://github.com/nstauffer/aim.analysis

#' Identify and repair invalid geometry in spatial polygons data frames
#' @description Using functions from sf, check the geometry of a set of polygons. If the geometry invalid, it attempts to buffer the polygons with \code{sf::st_buffer(dist = 0)}. If the geometry is corrupt or fine, it does nothing.
#' @param polygons Spatial polygons (either sf or spatial polygons data frame). The polygons to be checked. Note that the function will halt if the geometry is corrupt and not return a value.
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @param force Logical. If \code{TRUE} then both valid and invalid polygons will be buffered by 0. This shouldn't be necessary, but is a feature for the paranoid.
#' @return The spatial polygons data frame \code{polygons1}. This will be unchanged if the geometry was valid or repaired if it was invalid.
#' @export
repair_geometry <- function(polygons,
                            verbose = FALSE,
                            force = FALSE) {
  if(class(polygons) == "SpatialPolygonsDataFrame") {
    polygons_sf <- sf::st_as_sf(polygons)
    spdf <- TRUE
  } else if ("sf" %in% class(polygons)) {
    polygons_sf <- polygons
    spdf <- FALSE
  } else {
    stop("polygons must either be a spatial polygons data frame or an sf object")
  }
  
  validity_check <- sf::st_is_valid(polygons_sf)
  
  if (any(is.na(validity_check))) {
    stop("The geometry of the polygons is corrupt. Unable to repair.")
  }
  
  if (!all(validity_check)) {
    if (verbose) {
      message("Invalid geometry found. Attempting to repair.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else if (force) {
    if (verbose) {
      message("No invalid geometry found. Attempting to repair anyway.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else {
    if (verbose) {
      message("No invalid geometry found.")
    }
    output <- polygons_sf
  }
  
  if (spdf) {
    output <- methods::as(output, "Spatial")
  }
  
  return(output)
}





## Knit the RMD files for individual survey questions ----
knit_SG_survey_pages <- function(knit_summary, survey_report_folder, 
                                 survey_sections, survey_root, survey_transfer_folder) {
  
  ## If knitting is true
  if (knit_summary == TRUE) {
    
    ## Switch to the report folder on the C:drive...
    setwd(survey_report_folder)
    
    ## section = survey_sections[2]
    ## Pipe the list into Lapply
    survey_sections %>%
      
      ## Pipe the list into lapply
      lapply(function(section) {
        
        ## Copy the html file to the G:drive
        num = which(survey_sections == section)
        
        message('knitting file for ', num, ' ', section)
        rmarkdown::render(paste0(survey_root, 
                                 paste0('04 R scripts/RMD/', num, 'SG Survey IndividualQs ', 
                                        section, ' Questions.Rmd')))
        
        file.copy(paste0(num, 'SG-Survey-IndividualQs-', section, '-Questions.html'),
                  paste0(survey_transfer_folder,'/', num, 'SG_Survey_IndividualQs_', 
                         section, '_Questions.html'), overwrite = TRUE)
        
        ## Collect garbage
        gc()
        
      })
  }
}





## Knit the RMD files for individual survey questions ----
knit_SB_survey_pages <- function(knit_summary, survey_report_folder, 
                                 survey_sections, survey_root, survey_transfer_folder) {
  
  ## If knitting is true
  if (knit_summary == TRUE) {
    
    ## Switch to the report folder on the C:drive...
    setwd(survey_report_folder)
    
    ## section = survey_sections[2]
    ## Pipe the list into Lapply
    survey_sections %>%
      
      ## Pipe the list into lapply
      lapply(function(section) {
        
        ## Copy the html file to the G:drive
        num = which(survey_sections == section)
        
        message('knitting file for ', num, ' ', section)
        rmarkdown::render(paste0(survey_root, 
                                 paste0('04 R scripts/RMD/', num, 'SB Survey IndividualQs ', 
                                        section, ' Questions.Rmd')))
        
        file.copy(paste0(num, 'SB-Survey-IndividualQs-', section, '-Questions.html'),
                  paste0(survey_transfer_folder,'/', num, 'SB_Survey_IndividualQs_', 
                         section, '_Questions.html'), overwrite = TRUE)
        
        ## Collect garbage
        gc()
        
      })
  }
}





knit_LG_survey_pages <- function(knit_summary, survey_report_folder, 
                                 survey_sections, survey_root, survey_transfer_folder) {
  
  ## If knitting is true
  if (knit_summary == TRUE) {
    
    ## Switch to the report folder on the C:drive...
    setwd(survey_report_folder)
    
    ## section = survey_sections[1]
    ## Pipe the list into Lapply
    survey_sections %>%
      
      ## Pipe the list into lapply
      lapply(function(section) {
        
        ## Copy the html file to the G:drive
        num = which(survey_sections == section)
        
        message('knitting file for ', #num, ' ', 
                section)
        rmarkdown::render(paste0(survey_root, 
                                 paste0('04 R scripts/RMD/', #num, 
                                        'SG Survey IndividualQs ', 
                                        section, ' Questions.Rmd')))
        
        file.copy(paste0('SG-Survey-IndividualQs-', section, '-Questions.html'),
                  paste0(survey_transfer_folder,'/', 'SG Survey IndividualQs ', 
                         section, ' Questions.html'), overwrite = TRUE)
        
        ## Collect garbage
        gc()
        
      })
  }
}





## Twee function to create a map of directory structure ---- 
## quick-and-dirty ersatz Unix tree command in R
## inspired by this one-liner:
## ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
## found here (among many other places):
## http://serverfault.com/questions/143954/how-to-generate-an-ascii-representation-of-a-unix-file-hierarchy
tree_dir_structure <- function(path  = getwd(), 
                               level = Inf) {
  
  fad <-
    list.files(path = path, 
               recursive = TRUE, 
               no.. = TRUE, 
               include.dirs = TRUE)
  
  fad_split_up <- strsplit(fad, "/")
  too_deep     <- lapply(fad_split_up, length) > level
  fad_split_up[too_deep] <- NULL
  
  jfun <- function(x) {
    n <- length(x)
    if(n > 1)
      x[n - 1] <- "|__"
    if(n > 2)
      x[1:(n - 2)] <- "   "
    x <- if(n == 1) c("-- ", x) else c("   ", x)
    x
  }
  
  fad_subbed_out <- lapply(fad_split_up, jfun)
  cat(unlist(lapply(fad_subbed_out, paste, collapse = "")), sep = "\n")
  
}





## Convert to numeric ----
tonumeric <- function(var) {
  as.numeric(levels(var))[var]
}





## Complete data for one column ----
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}




## Complete data for one column ----
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}





## Capitalise first letter ----
simpleCap <- function(x) {
  
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
  
}


## Round df ----
round_df     <- function(df, digits) {
  
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}


## Spread table by row ----
## Convert each row into a column and bind the original column headers
spreadByRow <- function(df) {
  
  result <- bind_cols(apply(X = df,
                            MARGIN = 1,
                            FUN = function(x) {
                              
                              tibble(Value = x)
                            }
  ),
  tibble(Metric = names(df)))
  return(result)
}


## Replace values in a data frame ----
replace_values <- function(df, 
                           from_vals, 
                           to_vals) {
  
  
  if(length(from_vals) == length(to_vals)) {
    temp <- df %>% dplyr::mutate_if(is.factor, as.character) %>% as.data.frame()
    
    for(i in seq_along(from_vals)) {
      temp[temp == from_vals[i]] <- to_vals[i]
    }
    return(temp)
  } else {
    cat(red("\nWarning: unequal number of original values 
            and replacement values provided. Data frame 
            values have not been updated \n"))
  }
}


## Substring from the right ----
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


## value_round -------
## Function to round up to nearest value
value_round <- function(x, nearest_val) {
  new_val <- ceiling(x/nearest_val)*nearest_val
  return(new_val)
}


## RowShift function ----
## Modified to allow indexing in both directions of the vector. Originally from...
## http://stackoverflow.com/questions/14689424/use-a-value-from-the-previous-row-in-an-r-data-table-calculation
rowShift <- function(x, shiftLen = 1L) {
  
  r <- (1L + shiftLen):(length(x) + shiftLen)
  if(shiftLen <= 0) {r[r<1] <- 1
  
  } else {r[r>length(x)] <- length(x)}
  return(x[r])
  
}


## Make column a true NA value ----
make_true_NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x %in% c("NA", "<NA>"); x} else {
    x}



## Make first letter uppercase ----
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


## Remove duplicates from a string ----
str_remove_dupes <- function(s, w, S = el(strsplit(s, " ")), t = tolower) 
{cat(S[!duplicated(x<-t(S))|!x%in%t(w)])}


## Grid draw function ----
## A function definition similar to what ggplot2 has for ggmatrix
grid.draw.ggmatrix <- function(x, recording = TRUE) {
  print(x)
}





## PERCENT FUNCTIONS =======================================================================


## Adding percentage sign to numeric ----
addpercent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


## Format all numeric columns in a dataframe to % ----
percent_formatter <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, paste0(formatC(100 * x, format = "f", digits = 2), "%")) 
  } else x 
}


## Covert all numeric columns in a dataframe to % ----
percent_numeric <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = 2, ...), "%")
}


## Format all numeric columns in a dataframe to % ----
percent_num_form <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, round(x*100L, 2)) 
  } else x 
  
}





## CHOICE TABLES =======================================================================


## Create a single choice table  ----
SingleChoiceTable <- function(DataSet, 
                              QID, 
                              Percent) {
  
  Cnum <- which(colnames(DataSet) == QID)
  
  if(Percent == TRUE) {
    
    ram1 <- data.frame(table(DataSet[,Cnum], useNA = 'no')) %>%
      dplyr::select(Var1, Count = Freq) %>%
      left_join(data.frame(prop.table(table(DataSet[,Cnum], 
                                            useNA = 'no'))) %>% 
                  dplyr::select(Var1, Percentage = Freq)) %>% 
      
      dplyr::mutate(Percentage = formattable::percent(Percentage,2)) %>% 
      dplyr::rename(!!QID := Var1)
    
  }
  
  else if(Percent == FALSE) {
    ram1 <- data.frame(table(DataSet[,Cnum], useNA = 'no')) %>% 
      dplyr::select(QID, Count = Freq) %>%
      
      left_join(data.frame(prop.table(table(DataSet[,Cnum], 
                                            useNA = 'no'))) %>% 
                  dplyr::select(QID))
  }
  colnames(ram1)[1] <- colnames(DataSet)[Cnum]
  return(ram1)
}





## Create a grid choice table  ----
GridChoiceTable <- function(DataSet,
                            QID, 
                            Percent) {
  
  QIDs           <- as.data.frame(QID)
  colnames(QIDs) <- 'Cname'
  
  ## Find Cnum
  ram2 <- data.frame()
  
  for (i in 1:length(QIDs$Cname)) {
    
    ram1 <- data.frame(Cnum = which(colnames(DataSet) == QIDs$Cname[i]))
    ram2 <- ram2 %>% rbind(ram1)
  }
  
  QIDs <- QIDs %>% cbind(ram2)
  
  ram2 <- data.frame()
  
  for(i in 1:length(QIDs$Cnum)) {
    
    if(Percent == TRUE) {
      ram1 <- QIDs$Cnum[i] %>% 
        cbind(as.data.frame(table(DataSet[,QIDs$Cnum[i]], useNA = 'no'))) %>% 
        dplyr::mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
        dplyr::rename(Answer = 1)
      
      colnames(ram1)[1:3] <- c('Cnum', 'Answer', 'Count')
      
    } else {
      ram1 <- QIDs$Cnum[i] %>% 
        cbind(as.data.frame(table(DataSet[,QIDs$Cnum[i]], 
                                  useNA = 'no')))}
    ram2 <- ram2 %>% rbind(ram1)
  }
  
  colnames(ram2)[1:3] <- c('Cnum', 'Answer', 'Count')
  
  return(QIDs %>% 
           left_join(ram2, by = 'Cnum') %>% 
           dplyr::select(-Cnum))
  
}


## Create a Multi-Choice Unanswered table ----
## This function....
MultiChoiceUnanswered <- function(DataSet, QID) {
  
  ## 
  QIDs <- as.data.frame(QID)
  colnames(QIDs) <- 'Cname'
  
  # find Cnum
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cname)) {
    
    ram1 <- data.frame(Cnum = which(colnames(DataSet) == QIDs$Cname[i]))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  QIDs <- QIDs %>% cbind(ram2)
  
  ## Calculate dummy variable
  ram2 <- data.frame(RespondentID=rownames(DataSet))
  for (i in 1:length(QIDs$Cnum)) {
    
    ram1 <- DataSet %>% dplyr::mutate(tick = ifelse(is.na(.[,QIDs$Cnum[i]]),0,1)) %>% 
      dplyr::select(tick)
    colnames(ram1) <- QIDs$Cname[i]
    ram2 <- ram2 %>% 
      cbind(ram1)
    
  }
  
  ram2 <- ram2 %>% dplyr::mutate(Answered = rowSums(.[2:(nrow(QIDs)+1)]))
  return(nrow(ram2 %>% filter(Answered == 0)))
  
}



## Create a Multi-Choice answered table ----
## This function is reliant on MultiChoiceUnanswered function
MultiChoiceTable <- function(DataSet, QID, Percent = FALSE) { 
  
  QIDs <- as.data.frame(QID)
  colnames(QIDs) <- 'Cname'
  
  ## Find Cnum
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cname)) {
    
    ram1 <- data.frame(Cnum = which(colnames(DataSet) == QIDs$Cname[i]))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  
  ## 
  QIDs <- QIDs %>% cbind(ram2)
  
  ## Count true
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cnum)) {
    
    ram1 <- data.frame(Count = ifelse(all(is.na(DataSet[,QIDs$Cnum[i]])), 0, 
                                      table(!is.na(DataSet[,QIDs$Cnum[i]]))['TRUE']))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  QIDs <- QIDs %>% cbind(ram2)
  
  if(Percent == TRUE) {
    return(
      QIDs %>% dplyr::mutate(Percentage = round(Count/(nrow(DataSet) - 
                                                         MultiChoiceUnanswered(DataSet,QID)),4)) %>% 
        dplyr::select(-Cnum)
    )
    
  }
  else if(Percent == FALSE) {
    return(
      QIDs %>% dplyr::select(-Cnum)
    )
  }
}





## CROSS-TABULATE =======================================================================


## Creae a Word Cloud using free-text survey responses ----
SurveyWordCloud <- function(texts, exclude = c('')) {
  
  ## Load functions needed for the word cloud
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("wordcloud2")
  library("RColorBrewer")
  
  ## Analyse text
  docs    <- Corpus(VectorSource(texts))
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  
  ## 
  docs    <- tm_map(docs, toSpace, "/")
  docs    <- tm_map(docs, toSpace, "@")
  docs    <- tm_map(docs, toSpace, "\\|")
  
  ## Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  ## Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  ## Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  ## Remove your own stop word
  ## specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, exclude) 
  
  ## Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  ## Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  # Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  set.seed(1234)
  
  
  return(wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                   max.words=200, random.order=FALSE, rot.per=0.35,
                   main="Title",
                   colors=brewer.pal(8, "Dark2"))
  )
}





## Create a multiSpread value ----
multiSpread <- function(df, key, value) {
  
  ## quote key
  keyq <- rlang::enquo(key)
  
  ## Break value vector into quotes
  valueq <- rlang::enquo(value)
  s      <- rlang::quos(!!valueq)
  
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
  
}





## Likertise table ----
likertise <- function(tb, 
                      isgrid = FALSE,
                      customorder = c()) {  
  
  ## Do the numbers make this function hard-wired?
  if (isgrid == FALSE) {
    
    if (nlevels(tb[,1]) != 5 & nlevels(tb[,1]) != 3 & length(customorder) == 0) {
      return(print('Question can not be likertised or please revise function'))
    }
    
    if (nlevels(tb[,1]) == 5) {
      Order <- c(4,2,1,3,5)
    }
    
    else if(nlevels(tb[,1]) == 3) {
      Order <- c(3,2,1)
    }
    
    if(length(customorder) > 0 ){
      Order <- customorder
    }
    
    
    tb[1] <- factor(tb[,1],
                    levels(tb[,1])[Order]
    )
    return(tb[order(tb[,1]),])
  }
  
  else if(isgrid == TRUE) {
    
    if (nlevels(tb[,2]) != 5 & nlevels(tb[,2]) != 3 & length(customorder) == 0) {
      return(print('Question can not be likertised or please revise function'))
    }
    
    if (nlevels(tb[,2]) == 5) {
      Order <- c(4,2,1,3,5)
    }
    
    else if(nlevels(tb[,2]) == 3) {
      Order <- c(3,2,1)
    }
    
    if(length(customorder) > 0 ){
      Order <- customorder
    }
    
    
    tb[2] <- factor(tb[,2],
                    levels(tb[,2])[Order]
    )
    
    return(tb[order(tb[,1],tb[,2]),])
  }
}





## Cross tabulate Survey responses ----



CrossTab <- function(DataSet, 
                     QID, 
                     Group = NULL, 
                     Percent = FALSE) {
  
  Cnum <- which(colnames(DataSet) == QID)
  Gnum <- which(colnames(DataSet) == Group)
  
  ram1 <- data.frame(table(pull(DataSet[,Cnum]), pull(DataSet[,Gnum]), useNA = 'ifany')) %>% 
    dplyr::select(Var1, Var2, Count = Freq) 
  
  if(Percent == TRUE) {
    
    ram1 <- ram1 %>% 
      left_join(data.frame(round(prop.table(table(pull(DataSet[,Cnum]), 
                                                  pull(DataSet[,Gnum]), 
                                                  useNA = 'ifany'),2), digits=4)) %>% 
                  select(Var1, Var2, Percentage = Freq)) 
    
  }
  
  colnames(ram1)[1:2] <- c(colnames(DataSet)[Cnum],colnames(DataSet)[Gnum])
  
  
  return(ram1)
}





## Cross tabulate selected Survey responses ----
CrossTabSelect <- function(DataSet, QID, Group = NULL, Score = c('')) {
  
  ram5 <- DataSet %>% dplyr::select(Group) %>% distinct() %>% na.omit()
  
  for (j in 1:length(QID)) {
    
    ram1 <- CrossTab(DataSet, QID[j], Group, Percent = TRUE)
    ram3 <- data.frame()
    
    for (i in 1: length(Score)) {
      ram2 <- ram1 %>% filter(ram1[,1] == Score[i]) %>% na.omit()
      ram3 <- ram3 %>% rbind(ram2)
    }
    
    ram4 <- ram3 %>% group_by_at(which(colnames(ram3) == Group)) %>%
      summarise(Qname = sum(Percentage))
    
    colnames(ram4)[2] <- QID[j]
    ram5 <- ram5 %>% left_join(ram4)
    
  }
  return(ram5)
}





############################################################################################
############################################ ---TBC---- ####################################
############################################################################################