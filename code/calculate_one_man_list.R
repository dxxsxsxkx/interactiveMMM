# This file calculates the frequency of one-man PR list, 
#   i.e., a PR list with only one candidate.
# kOneManPR represents the number of such lists. 

# Last update: 2023-09-24. 
# kOneManPR = 65 (out of 2337 lists). 


# import functions
source("../../util/read_data.R")  # ReadReedSmithData()
source("./modify_dataset.R")  # functions for filtering and adding new variables to data


# read data
data.small <- ReadReedSmithData() %>% 
  FilterData() %>%
  AddVariables()

# calculation
tab.cand.pr <- aggregate(
  data = data.small, 
  pr_ncand ~ legis + year + region + party_jp + party_en, 
  FUN = unique
)
tab.cand.pr[tab.cand.pr$pr_ncand == 1, ]
tab.cand.list <- tab.cand.pr$pr_ncand
kOneManPR <- 0
for (i in 1:length(tab.cand.list)) {
  kOneManPR <- kOneManPR + ifelse(tab.cand.list[[i]] == 1, 1, 0)
}
