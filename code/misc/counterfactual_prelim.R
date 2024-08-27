# overview

# read packages
library(tidyverse)
source("./code/create_models.R")

# read data
data.small <- read_csv("./data_analysis/data_small.csv")

# check data
data.dual <- data.small %>%  # there are 5292 dually-nominated candidates
  filter(
    is_dual == 1
  )

# create a small dataframe
data.analysis <- data.small %>% 
  select(  # leave only necessary columns
    name_jp,  # identity
    year, legis,  # election
    party_en,  # party
    # pr district info
    kuname, pr_m, pr_partyseats, pr_rank, pr_ncand, pr_rerank, 
    is_dual,  # dual nomination status
    age, female, totcwinsT, celeb, incBinary,  # attributes
    result  # election result
  )

# plan A: simply omitting dually-nominated candidates
# 1. create a subset of data for each election-district-party.
# 2. omit dually-nominated candidates.
# 3. rerank the rest of the candidates.
data.wo.dual <- data.analysis %>% 
  # step 1.
  group_by(year, kuname, party_en) %>% 
  # step 2.
  mutate(
    rank = if_else(
      is_dual == 1, 
      NA_integer_, 
      pr_rank
    )
  ) %>%
  filter(
    !is.na(rank)
  ) %>% 
  # step 3.
  mutate(
    rank_dense = min_rank(rank)
  ) %>% 
  mutate(  # what if there are only pure PR candidates?
    result_updated = ifelse(
      rank_dense <= pr_partyseats,
      3, 
      0
    )
  ) %>% 
  ungroup()

data.check <- data.wo.dual %>% 
  filter(
    year == 1996
  )
summary(data.wo.dual)
data.wo.dual$name_jp[data.wo.dual$totcwinsT == 19]

# analysis
data.wo.dual %>%  # positive relationship bw. age and rank_dense
  glm.nb(
    data = ., 
    (rank_dense - 1) ~ age
  ) %>% 
  summary()

# positive relationship bw. age and rank_dense; 
# negative relationship bw. rank_dense - totcwinsT and rank_dense - incBinary.
data.wo.dual %>% 
  glm.nb(
    data = ., 
    (rank_dense - 1) ~ age + female + totcwinsT + as.factor(party_en) + celeb + 
      incBinary + pr_m + as.factor(legis)
  ) %>% 
  summary()

z# plan B: imputing attributes for dually-nominated candidates
# 1. create a subset of data for each election-district-party.
# 2. impute attributes for dually-nominated candidates.


