#' This function read the complete version of Reed-Smith dataset of Japanese House of Representative election. 

ReadReedSmithData <- function(){
  library(tidyverse)
  path <- "./data_raw/jhred.csv"
  data <- read_csv(
    path, 
    #header = TRUE, 
    #sep = ",", 
    #stringsAsFactors = FALSE
  )
  
  # modification
  data$totcwinsT <- data$totcwins - ifelse(data$result == 0, 0, 1) 
  # binary version for the incumbency variable
  data$incBinary <- ifelse(data$inc == 0, 0, 1)  
  data$prcode[data$name_jp == "坂本剛二" & data$year == 1996] <- 0
  
  return(data)
}