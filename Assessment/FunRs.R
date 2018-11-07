#############################
#This Function is used to extract the title-topic from urls
topic_extract <- function(url){
  REG_EXPR <- "[^/]+?/$"
  first_part <- sub(REG_EXPR,"",url)
  last_part <- sub(first_part,"",url)
  last_part <- sub("/$","",last_part)
  return(last_part)
}

is_in_there <- function(topic,s){
  return(str_detect(topic,s))
}

Forq6_8 <- function(clname,df=NewsData){
  res <- list()
  d <-c("weekday_is_sunday", "weekday_is_monday", "weekday_is_tuesday","weekday_is_wednesday",
        "weekday_is_thursday","weekday_is_friday","weekday_is_saturday")
  x <- df[,d]*df[,clname]
  dayNum <-apply(x,2,sum)
  res$dayofweek <- as.data.frame(dayNum); 
  avweekd <-res[[1]][c("weekday_is_monday", "weekday_is_tuesday","weekday_is_wednesday",
                         "weekday_is_thursday","weekday_is_friday"),]
  avweekendd <- res[[1]][c("weekday_is_saturday","weekday_is_sunday"),]
  aved <- res[[1]][d,]
  res$avweekd <- sum(avweekd)/5
  res$avweekendd <- sum(avweekendd)/2
  res$aved <- sum(aved)/7
  
  return(res)
  
}

phrase_num <- function(phrase,L=tables){
  Date <- names(L)
  numofph <- vector()
  for(i in 1:length(L)){
    numofph[i] <-L[[i]][phrase,]
  }
  
  df <- cbind.data.frame(Date,numofph)
  colnames(df) <- c("Date",phrase)
  return(df)
}

