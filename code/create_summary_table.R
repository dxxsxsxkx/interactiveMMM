# This file creates a summary table for the ageNomination paper. 

# import libraries and functions
library(tidyverse)
source("../../util/read_data.R")  # ReadReedSmithData()
source("./modify_dataset.R")  # functions for filtering and adding new variables to data

# some parameters
kVarsForRegression <- c("age", "female", "totcwinsT", "incBinary", "pr_rank")


#' Create a summary table of the variables used in regression analysis
#' @param data A data frame for which a summary table is created.
#' @return None. 
#' @export Resulting table is saved as a tex file.
CreateSummaryTable <- function(data){
  data.candidate <- data %>% 
    dplyr::select(all_of(kVarsForRegression)) %>% 
    mutate(
      female = as.factor(ifelse(female == 1, "Female", "Male")), 
      incBinary = as.factor(ifelse(incBinary == 1, "Incumbent", "Non-incumbent"))
    )
  colnames(data.candidate) <- kVarsForRegression
  tab.summary <- sumtable(
    data.candidate, 
    add.median = TRUE, 
    col.align = c(
      "left", rep("center", 8)
    ), 
    out = "latex", 
    file = paste0(getwd(), "/agenomination/table/tab.summary.tex")
  )
}

if (sys.nframe() == 0){
  # read data
  data.small <- ReadReedSmithData() %>% 
    FilterData() %>%
    AddVariables()
  
  # create a summary table
  CreateSummaryTable(data.small)
}