# This file 

# import libraries and functions
library(tidyverse)
source("./code/read_data.R")  # ReadReedSmithData()
source("./code/modify_dataset.R")  # functions for filtering and adding new variables to data

# some parameters
kThresholdHighElectability <- 0.6
kThresholdLowElectability <- 0.4


#' Calculate the number of PR seats obtained in the previous election. 
CalculatePastPRSeats <- function(data){
  data$pr_partyseatsBefore <- NA
  for (legis in unique(data$legis)){
    for (region in unique(data$region)) {
      for (party in unique(data$party_en)) {
        conditionNow <- (data$legis == legis & data$region == region & data$party_en == party)
        conditionBefore <- (data$legis == legis - 1 & data$region == region & data$party_en == party)
        if(any(conditionBefore)){
          data$pr_partyseatsBefore[conditionNow] <- unique(data$pr_partyseats[conditionBefore])
        }else{
          data$pr_partyseatsBefore[conditionNow] <- 0
        }
      }
    }
  }
  return(data)
}

#' Categorize party ranks according to the predicted election probability
CategorizeRanks <- function(data){
  # for each rank in a given party's list in a given district-year
  # calculate the proportion of elected candidates
  data.ranks <- aggregate(
    data = data, 
    resultPR ~ legis + region + party_en + pr_partyseatsBefore + pr_ncand + pr_m + pr_rank, 
    FUN = mean
  ) 
  
  # fit a prediction model
  fit <- lm(
    data = data.ranks, 
    resultPR ~ pr_partyseatsBefore + pr_m + pr_ncand
  )
  
  # calculate predicted probabilities of election
  data$pr_electability <- predict(
    object = fit, 
    newdata = data, 
    type = "response"
  )
  
  # categorize ranks according to the predicted probabilities
  data$pr_electability_lab <- ifelse(
    data$pr_electability > kThresholdHighElectability, "fair", 
    ifelse(
      data$pr_electability > kThresholdLowElectability, "medium", "tough"
    )
  )
  data$pr_electability_lab <- ordered(
    data$pr_electability_lab, 
    c("fair", "medium", "tough")
  )
  return(data)
}

#' Create a LaTex table of electability
CreateElectabilityTable <- function(data){
  table(data$resultPR, data$pr_electability_lab) %>% 
    as.data.frame() %>% 
    pivot_wider(names_from = Var2, values_from = Freq) %>% 
    mutate(Var1 = c("Loss", "Win")) %>% 
    rename(
      "Result" = "Var1", 
      "Fair" = "fair", 
      "Medium" = "medium", 
      "Tough" = "tough"
    ) %>% 
    xtable(
      align = "ll|ccc", 
      display = c("s", "s", "d", "d", "d")
    ) %>% 
    print(
      type = "latex", 
      floating = FALSE, 
      include.rownames = FALSE, 
      hlines.after = c(-1, 0), 
      # add.to.row = addtorow, 
      booktabs = TRUE, 
      file = "table/tab.electability.tex"
      # tabular.environment = "threeparttable", 
      # only.contents = TRUE
    )
}

if (sys.nframe() == 0){
  # read data
  data.small <- ReadReedSmithData() %>% 
    FilterData() %>%
    AddVariables() %>% 
    CalculatePastPRSeats() %>% 
    CategorizeRanks()
  
  # create a summary table
  CreateElectabilityTable(data.small)
}