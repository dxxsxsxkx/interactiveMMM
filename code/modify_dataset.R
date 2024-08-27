# This file includes functions to create a filtered dataset for ageNomination. 

#' Filter a dataset by given sets of variables and values, 
#'  and returns a smaller dataset.
#' @param data A dataset to be filtered.
#' @return A filtered dataset.
#' TODO: make this function more generic
FilterData <- function(data){
  data.small <- data %>% 
    filter(
      year >= 1994,  # leave only post-reform candidates
      year <= 2020,  # exclude candidates of 2021 due to missing data
      prcode != 0,  # leave only candidates who ran in PR districts
      byelection != 1  # exclude by-elections
    )
  return(data.small)
}

#' Add variables to a dataset.
#' @param data A dataset to be modified.
#' @return A dataset with added variables.
#' TODO: make this function more generic
AddVariables <- function(data){
  data.w.variables <- data %>% 
    mutate(
      # create a dummy representing dually-nominated candidates
      is_dual = ifelse(!is.na(ken), 1, 0),
      pr_rank.fct = as.factor(pr_rank),
      # create a coarsened pr_rank variable
      pr_rank.crs = ifelse(pr_rank > 12, 13, pr_rank), 
      pr_rank.crs.fct = as.factor(pr_rank.crs), 
      # create a PR result variable
      resultPR = ifelse(result %in% c(1, 2, 3), 1, 0)
    )
  
  data.w.variables$party_onemanlist <- ifelse(
    data$party_jp %in% 
      unique(data$party_jp[data$pr_ncand == 1]), 
    1, 0
  )
  data.w.variables$is_LDP <- ifelse(data$party_id == 1, 1, 0)
  
  return(data.w.variables)
}

CreateLDPData <- function(data){
  data.ldp <- data %>% 
    filter(party_id == 1)
}

CreateNonLDPData <- function(data){
  data.non.ldp <- data %>% 
    filter(party_id != 1)
}

#' NOTE: this function is usable but not recommended. 
#'   as filter() in tidyverse is more flexible and powerful.
#' This function filter a dataset by given sets of variables and values, 
#'   and returns a smaller dataset. 
#' @data A dataset to be filtered.
#' @list A list of three elements: variables, values to be filtered, and method of filtering.
#' @return A filtered dataset.

FilterDataDeprecated <- function(data, 
                       list = c("variable", "value", "method of filtering")){
  library(tidyverse)
  # TODO: do without for loops
  for (i in 1:length(list)){
    if (list[[i]][3] == "equal"){
      data <- data[data[, list[[i]][1]] == list[[i]][2], ]
    } else if(list[[i]][3] == "not equal"){
      data <- data[data[, list[[i]][1]] != list[[i]][2], ]
    } else if(list[[i]][3] == "greater than"){
      data <- data[data[, list[[i]][1]] > list[[i]][2], ]
    } else if(list[[i]][3] == "less than"){
      data <- data[data[, list[[i]][1]] < list[[i]][2], ]
    } else if(list[[i]][3] == "greater than or equal to"){
      data <- data[data[, list[[i]][1]] >= list[[i]][2], ]
    } else if(list[[i]][3] == "less than or equal to"){
      data <- data[data[, list[[i]][1]] <= list[[i]][2], ]
    } else if(list[[i]][3] == "in"){
      data <- data[data[, list[[i]][1]] %in% list[[i]][2], ]
    } else if(list[[i]][3] == "not in"){
      data <- data[!data[, list[[i]][1]] %in% list[[i]][2], ]
    } else if(list[[i]][3] == "between"){
      data <- data[data[, list[[i]][1]] >= list[[i]][2] & data[, list[[i]][1]] <= list[[i]][3], ]
    } else if(list[[i]][3] == "not between"){
      data <- data[data[, list[[i]][1]] < list[[i]][2] | data[, list[[i]][1]] > list[[i]][3], ]
    } else if(list[[i]][3] == "is NA"){
      data <- data[is.na(data[, list[[i]][1]]), ]
    } else if(list[[i]][3] == "is not NA"){
      data <- data[!is.na(data[, list[[i]][1]]), ]
    } else if(list[[i]][3] == "is NULL"){
      data <- data[is.null(data[, list[[i]][1]]), ]
    } else if(list[[i]][3] == "is not NULL"){
      data <- data[!is.null(data[, list[[i]][1]]), ]
    } else{
      stop("Invalid method of filtering.")
    }
    return(data)
  }
}