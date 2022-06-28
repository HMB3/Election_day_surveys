# Functions ---------------------------------------------------------------

# xlsx.writeMultipleData
#+++++++++++++++++++++++++++++
# file : the path to the output file
# ... : a list of data to write to the workbook
xlsx.writeMultipleData <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names = FALSE)
    else write.xlsx(objects[[i]], file, sheetName = objnames[i], 
                    append = TRUE)
  }
}


tonumeric <- function(var) {
  as.numeric(levels(var))[var]
}


completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}



ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="https://cran.csiro.au/")
  
  sapply(pkg, require, character.only = TRUE)
  
}


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Capitalise first letter
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


# encript  for ids this only works for 1 bit
# monkey export doesn't like ---#?&;


encrypt <- function(x) {
  
  
  
  dict <- data.frame(Alpha = c(0:9
                               ,letters
                               ,LETTERS)
                     ,Beta = c( 
                       letters[6:10]
                       ,3:6
                       ,letters[1:5]
                       ,letters[11:15]
                       ,0:2
                       ,letters[16:20]
                       ,letters[21:26]
                       ,7:9
                       ,LETTERS[6:10]
                       ,LETTERS[11:15]
                       ,LETTERS[1:5]
                       ,LETTERS[21:26]
                       ,LETTERS[16:20]
                     )) 
  
  
  
  enc_all <- NULL
  for(k in 1:length(x)) {
    
    enc <- NULL
    
    for (i in 1:nchar(as.character(x[k]))) {
      
      enc <- paste0(enc, ifelse(substr(x[k],i,i) %in% dict$Alpha,as.character(dict[dict$Alpha==substr(x[k],i,i),2]),substr(x[k],i,i)))
    }
    enc_all[k] <- paste0(enc)
    
  }
  return(enc_all)
}

###

decipher <- function(x) {
  
  
  dict <- data.frame(Alpha = c(0:9
                               ,letters
                               ,LETTERS)
                     ,Beta = c( 
                       letters[6:10]
                       ,3:6
                       ,letters[1:5]
                       ,letters[11:15]
                       ,0:2
                       ,letters[16:20]
                       ,letters[21:26]
                       ,7:9
                       ,LETTERS[6:10]
                       ,LETTERS[11:15]
                       ,LETTERS[1:5]
                       ,LETTERS[21:26]
                       ,LETTERS[16:20]
                     )) 
  
  
  dec_all <- NULL
  for(k in 1:length(x)) {
    
    dec <- NULL
    
    for (i in 1:nchar(as.character(x[k]))) {
      
      dec <- paste0(dec, ifelse(substr(x[k],i,i) %in% dict$Beta, as.character(dict[dict$Beta==substr(x[k],i,i),1]),substr(x[k],i,i)))
    }
    dec_all[k] <- paste0(dec)
    
  }
  return(dec_all)
}



## This funtion has issues which are corrected by decipher above,
## but it was alread used on the staff data for LG21  
decipher_LG21 <- function(x) {
  
  
  spcharacters <- c(')','(','*',',','^','%','$','~','@','!')
  dict <- data.frame(Alpha = c(0:9,spcharacters,letters,LETTERS)
                     ,Beta = c( letters[6:10]
                                ,spcharacters[5:7]
                                ,3:6
                                ,letters[1:5]
                                ,letters[11:15]
                                ,0:2
                                ,letters[16:20]
                                ,spcharacters[8:10]
                                ,letters[21:26]
                                ,7:9
                                ,LETTERS[6:10]
                                ,spcharacters[3:4]
                                ,LETTERS[11:15]
                                ,LETTERS[1:5]
                                ,LETTERS[16:20]
                                ,spcharacters[1:2]
                                
                                ,LETTERS[21:26])) 
  
  
  dec_all <- NULL
  for(k in 1:length(x)) {
    
    dec <- NULL
    
    for (i in 1:nchar(as.character(x[k]))) {
      
      dec <- paste0(dec, ifelse(substr(x[k],i,i) %in% dict$Beta, as.character(dict[dict$Beta==substr(x[k],i,i),1]),substr(x[k],i,i)))
    }
    dec_all[k] <- paste0(dec)
    
  }
  return(dec_all)
}





## Correlation matrix chart a=dataset, x=list of variables -----------------
cormatrix <- function(a, x, title, text_size) {
  
  ## 
  corVOI <- rcorr(as.matrix(a[,x]), type = 'spearman')
  
  ## 
  cex.before <- par("cex")
  par(cex = text_size)
  
  ## Replace all non-finite values with 0
  corVOI$r[!rowSums(!is.finite(corVOI$r)),]
  corVOI$r[!is.finite(corVOI$r)] <- 0
  
  corrplot(corVOI$r, method = "color", bg = "pink",
           order = "hclust", number.cex = .7,
           addCoef.col = "white",
           tl.col = "black", tl.srt = 90,
           p.mat = corVOI$P, sig.level = 0.01, insig = "blank", 
           diag = TRUE, 
           type = 'upper',
           title = title)
  
  ## Update here 
  par(cex = cex.before)
  
}



# cormat : matrix of the correlation coefficients -----------------
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, cols) {
  
  corVOI <- rcorr(as.matrix(cormat[,cols]), type = 'spearman')
  ut     <- upper.tri(corVOI$r)
  
  data.frame(
    row    = rownames(corVOI$r)[row(corVOI$r)[ut]],
    column = rownames(corVOI$r)[col(corVOI$r)[ut]],
    cor    =(corVOI$r)[ut],
    p      = corVOI$P[ut]
  )   %>% arrange(-cor) %>% 
    .[complete.cases(.), ]
}


# A function definition similar to what ggplot2 has for ggmatrix:-----------------
grid.draw.ggmatrix <- function(x, recording = TRUE) {
  print(x)
}


## adding percentage sign to numeric ---------------------------------------
addpercent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


## Covert all numeric columns in a dataframe to % ---------------------------------------
percent_formatter <- function(x) {
  
  if(is.numeric(x)){ 
    
    ifelse(is.na(x), x, paste0(formatC(100 * x, format = "f", digits = 2), "%")) 
    
  } else x 
  
}


percent_numeric <- function(x, digits = 2, format = "f", ...) {
  
  paste0(formatC(100 * x, format = format, digits = 2, ...), "%")
  
}


## Covert all numeric columns in a dataframe to % ---------------------------------------
percent_num_form <- function(x) {
  
  if(is.numeric(x)){ 
    
    ifelse(is.na(x), x, round(x*100L, 2)) 
    
  } else x 
  
}

# spreadByRow -------------------------------------------------------------


# Convert each row into a column and bind the original column headers

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



# File backups ------------------------------------------------------------


# file back ups
file_backup <- function(file, dest) {
  
  dest = paste(dest, sub(".*/", "", file), sep = "/")
  
  file.copy(file, dest)
  
} 



# replace_values ----------------------------------------------------------


replace_values <- function(df, from_vals, to_vals) {
  
  
  if(length(from_vals) == length(to_vals)) {
    
    temp <- df %>% mutate_if(is.factor, as.character) %>% as.data.frame()
    
    for(i in seq_along(from_vals)) {
      
      temp[temp == from_vals[i]] <- to_vals[i]
      
    }
    
    return(temp)
    
    
  } else {
    
    cat(red("\nWarning: unequal number of original values and replacement values provided. Data frame values have not been updated \n"))
  }
  
  
}



# substring from the right ------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# value_round -------------------------------------------------------------

# Function to round up to nearest value

value_round <- function(x, nearest_val) {
  
  new_val <- ceiling(x/nearest_val)*nearest_val
  
  
  return(new_val)
  
}






# RowShift function -------------------------------------------------------


# Modified to allow indexing in both directions of the vector. Originally from...
## http://stackoverflow.com/questions/14689424/use-a-value-from-the-previous-row-in-an-r-data-table-calculation


rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  if(shiftLen <= 0) {r[r<1] <- 1
  } else {r[r>length(x)] <- length(x)}
  
  return(x[r])
}





# RowShift function -------------------------------------------------------
make_true_NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x %in% c("NA", "<NA>"); x} else {
    x}




# r4spss ------------------------------------------------------------------

SingleChoiceTable <- function(DataSet, QID, Percent = FALSE) {
  
  
  Cnum <- which(colnames(DataSet) == QID)
  
  
  if(Percent == TRUE) {
    
    ram1 <- data.frame(table(DataSet[,Cnum], useNA = 'ifany')) %>% select(Var1, Count = Freq) %>%
      left_join(data.frame(prop.table(table(DataSet[,Cnum], useNA = 'ifany'))) %>% select(Var1, Percentage = Freq)) %>% 
      mutate(Percentage = formattable::percent(Percentage,2))
    
  }
  
  else if(Percent == FALSE) {
    ram1 <- data.frame(table(DataSet[,Cnum], useNA = 'ifany')) %>% select(Var1, Count = Freq) %>%
      
      left_join(data.frame(prop.table(table(DataSet[,Cnum], useNA = 'ifany'))) %>% select(Var1))
  }
  
  colnames(ram1)[1] <- colnames(DataSet)[Cnum]
  
  return(ram1)
}





## GridChoiceTable ----
GridChoiceTable <- function(DataSet,QID, Percent = FALSE) {
  
  QIDs <- as.data.frame(QID)
  colnames(QIDs) <- 'Cname'
  
  # find Cnum
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cname)) {
    
    ram1 <- data.frame(Cnum = which(colnames(DataSet) == QIDs$Cname[i]))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  QIDs <- QIDs %>% cbind(ram2)
  
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cnum)) {
    if(Percent == TRUE) {
      ram1 <- QIDs$Cnum[i] %>% cbind(as.data.frame(table(DataSet[,QIDs$Cnum[i]], useNA = 'ifany'))) %>% mutate(Percent = round(Freq/sum(Freq),4))
    }
    else if(Percent == FALSE) {
      ram1 <- QIDs$Cnum[i] %>% cbind(as.data.frame(table(DataSet[,QIDs$Cnum[i]], useNA = 'ifany')))
      
    }
    
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  colnames(ram2)[1:3] <- c('Cnum', 'Answer', 'Count')
  
  return(QIDs %>% left_join(ram2, by='Cnum') %>% select(-Cnum))
  
}


## MultiChoiceUnanswered ----
MultiChoiceUnanswered <- function(DataSet, QID) {
  
  QIDs <- as.data.frame(QID)
  colnames(QIDs) <- 'Cname'
  
  # find Cnum
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cname)) {
    
    ram1 <- data.frame(Cnum = which(colnames(DataSet) == QIDs$Cname[i]))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  QIDs <- QIDs %>% cbind(ram2)
  
  # calculate dummy var
  ram2 <- data.frame(RespondentID=rownames(DataSet))
  for (i in 1:length(QIDs$Cnum)) {
    
    ram1 <- DataSet %>% mutate(tick = ifelse(is.na(.[,QIDs$Cnum[i]]),0,1)) %>% select(tick)
    colnames(ram1) <- QIDs$Cname[i]
    ram2 <- ram2 %>% cbind(ram1)
    
  }
  
  ram2 <- ram2 %>% mutate(Answered = rowSums(.[2:(nrow(QIDs)+1)]))
  
  return(nrow(ram2 %>% filter(Answered == 0)))
}



## MultiChoiceTable ----
MultiChoiceTable <- function(DataSet, QID, Percent = FALSE) { # this function is reliant on MultiChoiceUnanswered function
  
  QIDs <- as.data.frame(QID)
  colnames(QIDs) <- 'Cname'
  
  # find Cnum
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cname)) {
    
    ram1 <- data.frame(Cnum = which(colnames(DataSet) == QIDs$Cname[i]))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  QIDs <- QIDs %>% cbind(ram2)
  
  # Count true
  ram2 <- data.frame()
  for (i in 1:length(QIDs$Cnum)) {
    
    ram1 <- data.frame(Count = ifelse(all(is.na(DataSet[,QIDs$Cnum[i]])),0,table(!is.na(DataSet[,QIDs$Cnum[i]]))['TRUE']))
    ram2 <- ram2 %>% rbind(ram1)
    
  }
  QIDs <- QIDs %>% cbind(ram2)
  
  if(Percent == TRUE) {
    return(
      QIDs %>% mutate(Percentage = round(Count/(nrow(DataSet) - MultiChoiceUnanswered(DataSet,QID)),4)) %>% select(-Cnum)
    )
    
  }
  else if(Percent == FALSE) {
    return(
      QIDs %>% select(-Cnum)
    )
    
  }
  
}





## SurveyWordCloud ----
SurveyWordCloud <- function(texts,exclude = c('')) {
  
  
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("wordcloud2")
  library("RColorBrewer")
  
  docs <- Corpus(VectorSource(texts))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, exclude) 
  
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  # Eliminate extra white spaces
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
                   colors=brewer.pal(8, "Dark2"))
  )
  
}


multiSpread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}





## likertise ----
likertise <- function(tb, isgrid = FALSE, customorder = c()) {  
  
  
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




## CrossTab ----
CrossTab <- function(DataSet, QID, Group = NULL, Percent = FALSE) {
  
  DataSet <- as_tibble(DataSet)
  Cnum <- which(colnames(DataSet) == QID)
  Gnum <- which(colnames(DataSet) == Group)
  
  ram1 <- data.frame(table(pull(DataSet[,Cnum]), pull(DataSet[,Gnum]), useNA = 'ifany')) %>% dplyr::select(Var1, Var2, Count = Freq) 
  
  if(Percent == TRUE) {
    
    ram1 <- ram1 %>% left_join(data.frame(round(prop.table(table(pull(DataSet[,Cnum]), pull(DataSet[,Gnum]), useNA = 'ifany'),2), digits=4)) %>% select(Var1, Var2, Percentage = Freq)) 
    
  }
  
  colnames(ram1)[1:2] <- c(colnames(DataSet)[Cnum],colnames(DataSet)[Gnum])
  
  
  return(ram1)
}





## CrossTabSelect ----
CrossTabSelect <- function(DataSet, QID, Group = NULL, Score = c('')) {
  
  ram5 <- DataSet %>% select(Group) %>% distinct() %>% na.omit()
  
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




# hcharts -----------------------------------------------------------------
# df       = Q11_1_plot
# xcol     = "Response"
# ycol     = "Percentage"
# ytitle   = "Percentage of respondents"
# title    =  Q11_1_label
# categor  = unique(Q11_1_plot$Response)
# subtitle = "This is a single response question"


## simple_percentage_bar ---- 
simple_percentage_bar = function(df, xcol, ycol, 
                                 ytitle, title, 
                                 categor, subtitle) {
  
  ## Make the df formattable  
  formattable(df)
  
  highchart() %>% 
    
    ## Data for plotting
    hc_add_series(Q4_table, "bar", hcaes(x = xcol, y = ycol), name = 'Responses') %>%
    
    ## Options for each type of series
    hc_plotOptions(
      series = list(
        showInLegend = FALSE,
        pointFormat = "{point.y}%"
      ),
      column = list(
        colorByPoint = TRUE
      )
    ) %>%
    
    ## Axis
    hc_yAxis(
      title  = list(text   = ytitle),
      labels = list(format = "{value}")
    ) %>% 
    
    hc_xAxis(categories = categor) %>%
    
    ## Titles and credits
    hc_title(
      text = title
    ) %>%
    
    hc_subtitle(text = "This is a single response question") %>%
    
    hc_credits(
      enabled = TRUE, text = paste0(sum(df$Count), " Respondents"),
      href = "#",
      style = list(fontSize = "12px")
    )
  
}





## stacked_bar_percent ---- 
## https://ox-it.github.io/OxfordIDN_htmlwidgets/charts/StackedBarCharts/
stacked_bar_percent = function(df, categories_column, 
                               measure_columns, title, resp) {
  
  generated_chart <- highchart() %>%
    
    hc_chart(type = "column") %>% 
    hc_title(text = title) %>% 
    hc_subtitle(text = paste0('n = ', resp)) %>% 
    
    hc_xAxis(categories = df[, categories_column],
             title = categories_column)
  
  invisible(lapply(measure_columns, function(column) {
    generated_chart <<-
      hc_add_series(hc = generated_chart, name = column,
                    data = df[, column])
  }))
  
  generated_chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = "percent")) %>%
    hc_yAxis(title = list(text = "Percentage %")) %>%
    hc_legend(reversed = TRUE) 
  
}





## stacked_bar_counts ---- 
# stacked_bar_counts = function(df, categories_column, 
#                               measure_columns, title) {
#   
#   generated_chart <- highchart() %>%
#     
#     hc_chart(type = "column") %>% 
#     hc_title(text = title) %>% 
#     
#     hc_xAxis(categories = df[, categories_column],
#              title = categories_column)
#   
#   invisible(lapply(measure_columns, function(column) {
#     
#     generated_chart <<-
#       hc_add_series(hc = generated_chart, name = column,
#                     data = df[, column])
#     
#   }))
#   
#   generated_chart <- generated_chart %>%
#     hc_chart(type = "bar") %>%
#     hc_plotOptions(series = list(stacking = "normal"))
#   generated_chart
#   
# }


## A function to transform the numbers on the axes to be 1 significanty didgit 
number_ticks <- function(n) {function(limits) pretty(limits, n)}

round_df     <- function(df, digits) {
  
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

# df          = df
# cols        = 1:(ncol(df)-1)
# alpha       = 0.5 
# alignPer    = 0.8
# upper_size  = 2.5 
# lower_size  = 0.7 
# axis_size   = 6 
# title_size  = 7 
# leg_size    = 5 
# title       = ''


## Scatterplot matrix ---- 
scatter_matrix_metro = function(df, cols,        #col_cat, 
                                alpha, alignPer, 
                                upper_size, lower_size, axis_size, 
                                title_size, leg_size, title) {
  
  ggpairs(df,
          
          ## Remove the legend and histogram
          legend  = ncol(df),
          columns = cols, 
          mapping = ggplot2::aes(colour = Region),
          
          ## Create upper and lower panels
          upper   = list(combo      = wrap("box_no_facet",    alpha = alpha),
                         continuous = wrap("cor",     size = upper_size, alignPercent = alignPer)),
          lower   = list(continuous = wrap("smooth", alpha = alpha, size = lower_size)),
          diag    = list(continuous = wrap("densityDiag", alpha = 0.5 ))) + 
    
    ## Use the classic theme
    theme_classic() +
    
    ## Change the axes sizes, etc.
    theme(axis.title.x     = element_text(colour = "black", size = axis_size),
          axis.text.x      = element_text(size = axis_size),
          
          axis.title.y     = element_text(colour = "black", size = axis_size),
          axis.text.y      = element_text(size = axis_size),
          
          panel.background = element_blank(),
          panel.border     = element_rect(colour = "black", fill = NA, size = 1.2),
          plot.title       = element_text(size   = title_size, face = "bold"),
          legend.text      = element_text(size   = leg_size),
          legend.title     = element_blank(),
          legend.position  = "right",
          legend.key.size  = unit(1.5, "cm"))
  
  ## And title
  #ggtitle(title)
  
}


## Boxplots ---- 
role_boxplots = function(df, x_var, y_var, pallette,
                         box_size, x_lab, y_lab, y_lim, 
                         lab_size, lab_angle, border_size) {
  
  ggboxplot(df, 
            x = x_var, y = y_var, 
            fill = "Region",
            palette = pallette, size = box_size) +
    geom_hline(yintercept = 0, col = 'black', linetype = "dashed") +
    
    ## Use the classic theme
    theme_classic() +
    labs(y = y_lab,
         x = '') +
    scale_y_continuous(limits = y_lim) +
    
    ## Change the axes sizes, etc.
    theme(axis.title.x     = element_text(colour = 'black', size = lab_size),
          axis.text.x      = element_text(size = lab_size, angle = lab_angle, hjust = 1),
          
          axis.title.y     = element_text(colour = 'black', size = lab_size),
          axis.text.y      = element_text(size = lab_size),
          
          panel.border     = element_rect(colour = 'black', fill = NA, size = border_size),
          plot.title       = element_text(size   = lab_size, face = 'bold'),
          legend.text      = element_text(size   = lab_size),
          legend.title     = element_blank(),
          legend.key.size  = unit(1.5, 'cm')) +
    
    ## And title
    ggtitle(paste0(''))
  
}





## bar chart horizontal ---- 
single_bar_chart_horiz = function(df, title, ylab, xlab, ymin, ymax,
                                  tsize, caption, xsize, ysize, ycol, 
                                  lab_size, bar_width, capt_size) {
  
  #ggplot(df, aes(x = Response, y = Percentage, fill = Response)) +
  ggplot(df, aes(x = reorder(Response, Percentage), y = Percentage, fill = Response)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_colour_brewer(palette = "Set2") +
    #scale_fill_manual(na.value = "grey") +
    
    geom_text(aes(label = Percentage, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1,1,1,1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
}





## single_bar_chart_horiz_unord ---- 
single_bar_chart_horiz_unord = function(df, title, ylab, xlab, ymin, ymax,
                                        tsize, caption, xsize, ysize, ycol, 
                                        lab_size, bar_width, capt_size) {
  
  ggplot(df, aes(x = Response, y = Percentage, fill = Response)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_colour_brewer(palette = "Set2", na.value = "grey") +
    
    geom_text(aes(label = Percentage, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1,1,1,1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
}





## single_bar_chart_horiz_small ---- 
single_bar_chart_horiz_small = function(df, title, ylab, xlab, ymin, ymax,
                                        tsize, caption, xsize, ysize, ycol, colours,
                                        lab_size, bar_width, capt_size) {
  
  ggplot(df, aes(x = Response, y = Percentage, fill = Response)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_fill_manual(values = colours, na.value = "grey") +
    
    geom_text(aes(label = Percentage, hjust = + 0.5), 
              hjust = -0.5, 
              position = position_dodge(width = 1),
              inherit.aes = TRUE,
              size      = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab(ylab) +
    ggtitle(title) +
    xlab(xlab) +
    labs(caption = caption) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1,1,1,1), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size = xsize),
          axis.title.x    = element_text(size = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y  = element_text(size = ysize, face = "bold"),
          axis.text.y   = element_text(size = ysize, color = ycol),
          plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
}





## https://github.com/etiennebr/visualraster
#' Qualitative colour scale
#' 
#' Overrides the default discrete gradient to use color brewer Set 3
#' @param values a set of aesthetic values to map data values to. If this is a 
#' named vector, then the values will be matched based on the names. If unnamed,
#'  values will be matched in order (usually alphabetical) with the limits of 
#'  the scale. Any data values that don't match will be given na.value.
#' @param ... Other arguments passed on to \code{\link{scale_fill_manual}}
#' @import RColorBrewer
#' @export
visras_scale_fill_discrete = function(values = brewer.pal(11, "Set3"), ...) {
  scale_fill_manual(values = values, ...)
}





## stacked_liker_percent ---- 
stacked_liker_percent = function(highs,  lows,
                                 high_label, low_label, scale_cols,
                                 title, count, lab_size, leg_size, leg_order,
                                 tsize, ysize, xsize, width, neg_min) {
  
  ## Start the gg device
  ggplot() + 
    
    ## Create the bar charts
    ## use the 'Role' column for the order
    ## Legend happens with fill = response reorder(Response, -Percentage)
    geom_bar(data = highs, aes(x  = reorder(Role, desc(Role)), y = Percentage, fill = Response), 
             position = "stack", stat = "identity", width = width) +
    
    geom_bar(data = lows,  aes(x  = reorder(Role, desc(Role)), y = Percentage, fill = Response),
             position = "stack", stat = "identity", width = width) +
    
    ## Create bars between the response categories
    geom_hline(yintercept  =  0, color  = "black", size  =  1) +
    theme_light() +
    
    ## Make them the same everytime
    visras_scale_fill_discrete(breaks = leg_order, values = scale_cols) +
    #scale_fill_manual(values  =  scale_cols) +
    
    ## Add values for responses
    geom_text(data = high_label, aes(x = Role, y = Percentage, label = Percentage), size = lab_size, hjust =  1) +
    geom_text(data = low_label,  aes(x = Role, y = Percentage, label = Percentage), size = lab_size, hjust =  1.5) +
    
    ## Plot the bars horizontally
    coord_flip() +
    labs(title =  title, y = '', x = '', fill = count) +
    
    ## Create titles and legend
    theme(plot.title      =  element_text(size  = tsize, hjust = 0.5,   face = "bold"),
          axis.text.y     =  element_text(hjust = 0,     size  = ysize, face = "bold"),
          axis.text.x     =  element_text(size  = xsize),
          legend.title    =  element_text(size = leg_size,  face = "bold"),
          legend.text     =  element_text(size = leg_size),
          legend.position =  "bottom") +
    
    ## Scale between +/- 50%
    scale_y_continuous(breaks  = seq(neg_min, 1, 0.25), limits = c(neg_min, 1))
  
  
}





## stacked_liker_percent_single ---- 
stacked_liker_percent_single = function(highs,  lows,
                                        high_label, low_label, scale_cols,
                                        title, count, lab_size, leg_size, leg_order,
                                        tsize, ysize, xsize, width, neg_min, order) {
  
  ## Start the gg device
  ggplot() + 
    
    ## Create the bar charts
    ## Legend happens with fill = response
    geom_bar(data = highs, aes(x  = Role, y = Percentage, fill = Response), 
             position = "stack", stat = "identity", width = width) +
    
    geom_bar(data = lows,  aes(x  = Role, y = Percentage, fill = Response),
             position = "stack", stat = "identity", width = width) +
    
    ## Create bars between the response categories
    geom_hline(yintercept  =  0, color  = "black", size  =  1) +
    theme_light() +
    
    ## Make them the same everytime
    visras_scale_fill_discrete(breaks = leg_order, values = scale_cols) +
    #scale_fill_manual(values  =  scale_cols) +
    
    ## Add values for responses
    geom_text(data = high_label, aes(x = Role, y = Percentage, label = Percentage), size = lab_size, hjust =  1) +
    geom_text(data = low_label,  aes(x = Role, y = Percentage, label = Percentage), size = lab_size, hjust =  1.5) +
    
    ## Plot the bars horizontally
    coord_flip() +
    labs(title =  title, y = '', x = '', fill = count) +
    
    ## Create titles and legend
    theme(plot.title      =  element_text(size  = tsize, hjust = 0.5,   face = "bold"),
          axis.text.y     =  element_text(hjust = 0,     size  = ysize, face = "bold"),
          axis.text.x     =  element_text(size  = xsize),
          legend.title    =  element_text(size = leg_size,  face = "bold"),
          legend.text     =  element_text(size = leg_size),
          legend.position =  "bottom") +
    
    ## Scale between +/- 50%
    scale_y_continuous(breaks  = seq(neg_min, 1, 0.25), limits = c(neg_min, 1))
  
  
}





## hi_ind.bar ---- 
hi_ind.bar <- function(a) { 
  
  ind.venue <- as.character({area_markoff %>% distinct(VenueName)}[a,])
  
  
  ind.bar.data <- area_markoff %>% 
    filter(VenueName == ind.venue) %>%
    arrange(Date) 
  
  if (nrow(UsedBP) > 0) {
    
    ind.bar.data <- ind.bar.data %>% left_join(UsedBP, by=c('VenueName',
                                                            'Date'))
  }
  
  ind.bar <- hchart(ind.bar.data, 'line',hcaes(x=OpeningDate,y=`Early Voting`, group=paste0(VenueName,' Mark off'))) %>%
    hc_add_series(ind.bar.data, type = "line", dashStyle = "longdash",
                  hcaes(x=OpeningDate,y=Projection),color = 'pink',name=paste0(ind.venue,' Projected')) %>%
    
    hc_title(
      text = paste0(ind.venue,' Votes by date'),
      useHTML = TRUE) %>%
    hc_yAxis(title = list(text='Counts')) %>%
    hc_tooltip(table = TRUE, sort = FALSE) %>%
    hc_legend(enabled = F) %>%
    hc_tooltip(crosshairs=c(TRUE,TRUE)) %>%
    hc_plotOptions(column = list(
      dataLabels = list(enabled = FALSE),
      enableMouseTracking = TRUE),
      area = list(marker = list(fillColor = "rgba(255, 255, 255, 0)")),
      line = list(marker = list(fillColor = "rgba(255, 255, 255, 0)")))
  
  if (nrow(UsedBP) > 0) {
    
    ind.bar <- ind.bar %>% hc_add_series(ind.bar.data, type = "line",hcaes(x=OpeningDate,y=Prepoll_Ordinary), color=c('purple'), name='AoBP')
    
  } else {
    
    ind.bar <- ind.bar
  }
  
  return(ind.bar)
}





## hi_ind.cumsum ---- 
hi_ind.cumsum <- function(a) {
  
  ind.venue <- as.character({area_markoff %>% distinct(VenueName)}[a,])
  
  ind.cumsum.data <- area_markoff %>% 
    filter(VenueName == ind.venue) %>% 
    group_by(VenueName) %>%
    mutate(CumulativeSum = cumsum(`Early Voting`),
           CumulativeProjection = cumsum(Projection)) %>%
    arrange(Date)
  
  
  if (nrow(UsedBP) > 0) {
    
    ind.cumsum.data <- ind.cumsum.data %>% left_join(UsedBP %>% group_by(VenueName) %>%
                                                       mutate(AoBP = cumsum(Prepoll_Ordinary)), by=c('VenueName',
                                                                                                     'Date'))
    
  }
  
  plot_cumsum <- hchart(ind.cumsum.data, 'line',hcaes(x=OpeningDate, y=CumulativeSum, group=paste0(VenueName,' Mark off'))) %>%
    hc_title(
      text = paste0(ind.venue," Cumulative Votes"),
      useHTML = TRUE) %>%
    hc_tooltip(table = TRUE, sort = FALSE) %>%
    hc_legend(enabled = F) %>%
    hc_tooltip(crosshairs=TRUE) %>%
    hc_add_series(ind.cumsum.data, type = "line", dashStyle = "longdash",
                  hcaes(x=OpeningDate,y=CumulativeProjection),color = 'pink',name=paste0(ind.venue,' Projected'))
  if (nrow(UsedBP) > 0) {
    
    plot_cumsum <- plot_cumsum %>% hc_add_series(ind.cumsum.data, type = "line",hcaes(x=OpeningDate,y=AoBP), color=c('purple'), name='AoBP')
    
  } else {
    
    plot_cumsum <- plot_cumsum
    
  }
  
  return(plot_cumsum)
}




readFromGoogledRam <- function(DataSet,Address) {
  
  Cnum <- which(colnames(DataSet) == Address)
  
    
  if (read_json("ram.html")$status == 'OK') {
    
    
    GeoCode_temp <- as.data.frame(read_json("ram.html")) %>%
      select(lat = results.geometry.location.lat,
             long = results.geometry.location.lng) %>%
      mutate(Address =  as.character(DataSet[i,Cnum]))
    
  } else {
    
    GeoCode_temp <- data.frame(lat=NA,
                               long=NA,
                               Address = as.character(DataSet[i,Cnum]))
    
  }
  
  return(GeoCode_temp)

}

## Twee function to create a map of directory structure ---- 
## quick-and-dirty ersatz Unix tree command in R
## inspired by this one-liner:
## ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
## found here (among many other places):
## http://serverfault.com/questions/143954/how-to-generate-an-ascii-representation-of-a-unix-file-hierarchy
twee <- function(path = getwd(), level = Inf) {
  
  fad <-
    list.files(path = path, recursive = TRUE,no.. = TRUE, include.dirs = TRUE)
  
  fad_split_up <- strsplit(fad, "/")
  
  too_deep <- lapply(fad_split_up, length) > level
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





















