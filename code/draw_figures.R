# This file contains functions to create figures. 
library(tidyverse)

# import functions
source("./code/read_data.R")  # ReadReedSmithData()
source("./code/modify_dataset.R")  # functions for filtering and adding new variables to data

# read data
data.small <- ReadReedSmithData() %>% 
  FilterData() %>%
  AddVariables()


#' Distribution of candidates' list ranks
DrawListRanks <- function(data){
  tab.rank <- as.data.frame(table(data$pr_rank - 1)) %>% 
    mutate(
      Var1 = as.numeric(Var1) - 1, 
      Rate = Freq / sum(Freq), 
      Pois = dpois(Var1, lambda = 1), 
      NB = dnbinom(Var1, prob = 0.6, size = 1.5)
    )
  tab.rank %>% 
    pivot_longer(cols = c(Rate, Pois, NB)) %>% 
    ggplot() + 
    geom_line(aes(
      x = Var1, 
      y = value,  
      linetype = name
    )) +
    labs(
      x = "Rank",
      y = "Frequency",
      linetype = "Line type"  # Customize the legend title
    ) +
    scale_linetype_manual(
      labels = c(
        "Rate" = "Data", 
        "Pois" = "Poisson(1)", 
        "NB" = "NB(0.6, 1.5)"
      ), 
      values = c(
        "Rate" = "solid", 
        "Pois" = "dashed", 
        "NB" = "dotted"
      )
    ) + 
    scale_linewidth_manual(
      values = c(
        "Rate" = 1, 
        "Pois" = 1.5, 
        "NB" = 1.5
      )
    ) +
    theme_bw() + 
    theme(
      axis.title = element_text(size = 20), 
      axis.text = element_text(size = 20), 
      legend.position = c(0.9, 0.9),  # Adjust the position of the legend
      legend.justification = c(1, 1),  # Adjust the justification of the legend
      legend.background = element_rect(fill = "white", color = "black")  # Customize the legend background
    )
  ggsave("./figure/dist.rank.pdf", width = 8, height = 6)
}

#' Distribution of the lowest ranks of elected candidates
DrawLowestRanksElected <- function(data){
  tab.rank.likelihood <- aggregate(
    data = data.small[data.small$resultPR == 1, ],  # only elected candidates
    pr_rank ~ legis + region + party_jp, 
    FUN = max
  )
  
  ggplot(tab.rank.likelihood) + 
    geom_histogram(aes(x = pr_rank)) + 
    scale_x_continuous(
      breaks = seq(0, max(tab.rank.likelihood$pr_rank), 5)
    ) + 
    labs(
      x = "Rank", 
      y = "Frequency"
    ) + 
    theme_minimal() + 
    theme(
      axis.title = element_text(size = 20), 
      axis.text = element_text(size = 20)
    )
  ggsave("./figure/dist.length.list.pdf", width = 8, height = 6)
}


#' Distribution of the number of PR seats obtained
DrawNofPrSeats <- function(data){
  tab.seat.likelihood <- aggregate(
    data = data.small, 
    pr_partyseats ~ legis + region + party_jp, 
    FUN = max
  )
  
  ggplot(tab.seat.likelihood) +
    geom_bar(aes(x = pr_partyseats)) + 
    labs(
      x = "Seats", 
      y = "Frequency"
    ) + 
    scale_x_continuous(
      breaks = seq(0, max(tab.seat.likelihood$pr_partyseats), 5)
    ) + 
    theme_minimal() + 
    theme(
      axis.title = element_text(size = 20), 
      axis.text = element_text(size = 20)
    )
  ggsave("./figure/dist.seats.obtained.pdf", width = 8, height = 6)
}