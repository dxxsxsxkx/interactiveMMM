# This file calculate the frequency of rank sharing, 
#  i.e., multiple candidates sharing a single PR rank, 
#  in the Japanese House of Representatives elections.
# "kListDup" represents the number of PR lists with rank sharing,
# "kRankDup" stands for the number of ranks shared by multiple candidates. 

# Last update: 2023-09-24. 
# kListDup = 348 (out of 664), 
# kRankDup = 377 (out of 2337).

# import functions
source("../../util/read_data.R")  # ReadReedSmithData()
source("./modify_dataset.R")  # functions for filtering and adding new variables to data


# prepare the dataset
data <- ReadReedSmithData()
data.small <- data %>% 
  FilterData() %>%
  AddVariables()


# calculate rank sharing
tab.rank.agg <- aggregate(
  data = data.small, 
  pr_rank ~ legis + region + party_jp, 
  FUN = table
)
tab.rank.list <- tab.rank.agg$pr_rank
kRankDup <- 0
kListDup <- 0
kCountRank <- 0
kCountList <- 0
for (i in 1:length(tab.rank.list)) {
  kRankDup = kRankDup + sum(tab.rank.list[[i]] > 1)
  kListDup = kListDup + sum(any(tab.rank.list[[i]] > 1))
  kCountRank = kCountRank + length(tab.rank.list[[i]])
  kCountList = kCountList + 1
}

