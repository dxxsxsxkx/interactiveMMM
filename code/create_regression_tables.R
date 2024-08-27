# This file includes functions creating regression tables. 
library(tidyverse)
library(texreg)

#' Create a regression table for count models.
#' @param ... A list of regression models. Defaults to 4. 
CreateRegTableCount <- function(...){
  tabreg.count <- texreg(
    list(list(...)[[1]], list(...)[[2]], list(...)[[3]], list(...)[[4]]), 
    caption = "All candidates", 
    custom.model.names = c(
      "Univariate NB", 
      "NB", 
      "Univariate Zero-inflated NB", 
      "Zero-inflated NB"
    ), 
    custom.coef.map = list(
      "(Intercept)" = NA, 
      "age" = "Age", 
      "female" = "Female", 
      "celeb" = "Celebrity candidate", 
      "totcwinsT" = "N of elections before", 
      "is_dual" = "Dual nomination", 
      "incBinary" = "Incumbency", 
      "pr_m" = "District magnitude", 
      "Log(theta)" = NA, 
      "Zero model: Intercept" = "(Intercept) ", 
      "Zero model: party_onemanlist" = "Parties with one-person list"
    ), 
    groups = list(
      "Coefficients" = 1:9, 
      "Zero model" = 10:11
    ), 
    custom.gof.rows = list(
      "Party FE" = c("Yes", "Yes", "No", "No"), 
      "Year FE" = c("Yes", "Yes", "Yes", "Yes")
    ), 
    custom.note = paste0(
      "\n\\item %stars.",  
      "\\item P-values are not displayed for the Zero-inflated NB models.", 
      "\n\\item Dependent variables are (candidates' PR ranks - 1)."
    ),
    center = FALSE, 
    threeparttable = TRUE, 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    scalebox = 0.8, 
    label = "table:coefCount"
  )
  writeLines(tabreg.count, "./table/tabreg_count.tex")
}

#' Create a regression table for count models, without including the party FE.
#' @param ... A list of regression models. Defaults to 2.
CreateRegTableCountwoParty <- function(...){
  tabreg.count <- texreg(
    list(
      list(...)[[1]], list(...)[[2]]
    ), 
    caption = "LDP candidates",
    custom.model.names = c(
      "Univariate NB", 
      "NB"
    ),
    custom.coef.map = list(
      "(Intercept)" = NA, 
      "age" = "Age", 
      "female" = "Female", 
      "celeb" = "Celebrity candidate", 
      "totcwinsT" = "N of elections before", 
      "is_dual" = "Dual nomination",
      "incBinary" = "Incumbency", 
      "pr_m" = "District magnitude"
    ), 
    custom.gof.rows = list(
      "Year FE" = c("Yes", "Yes")
    ), 
    custom.note = paste0(
      "\n\\item %stars.",  
      "\n\\item Dependent variables are (candidates' PR ranks - 1)."), 
    center = FALSE, 
    threeparttable = TRUE, 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    scalebox = 0.8, 
    label = "table:coefCountLDP"
  )
  writeLines(tabreg.count, "./table/tabreg_count_ldp.tex")
}

CreateRegTableCountNonLDP <- function(...){
  tabreg.count <- texreg(
    list(list(...)[[1]], list(...)[[2]], list(...)[[3]], list(...)[[4]]), 
    caption = "Non-LDP candidates", 
    custom.model.names = c(
      "Univariate NB", 
      "NB", 
      "Univariate Zero-inflated NB", 
      "Zero-inflated NB"
    ), 
    custom.coef.map = list(
      "(Intercept)" = NA, 
      "age" = "Age", 
      "female" = "Female", 
      "celeb" = "Celebrity candidate", 
      "totcwinsT" = "N of elections before", 
      "is_dual" = "Dual nomination", 
      "incBinary" = "Incumbency", 
      "pr_m" = "District magnitude", 
      "Log(theta)" = NA, 
      "Zero model: Intercept" = "(Intercept) ", 
      "Zero model: party_onemanlist" = "Parties with one-person list"
    ), 
    groups = list(
      "Coefficients" = 1:9, 
      "Zero model" = 10:11
    ), 
    custom.gof.rows = list(
      "Party FE" = c("Yes", "Yes", "No", "No"), 
      "Year FE" = c("Yes", "Yes", "Yes", "Yes")
    ), 
    custom.note = paste0(
      "\n\\item %stars.",  
      "\\item P-values are not displayed for the Zero-inflated NB models.", 
      "\n\\item Dependent variables are (candidates' PR ranks - 1)."
    ),
    center = FALSE, 
    threeparttable = TRUE, 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    scalebox = 0.8, 
    label = "table:coefCountNonLDP"
  )
  writeLines(tabreg.count, "./table/tabreg_count_nonldp.tex")
}

#' Create a regression table for ordinal models.
#' @param ... A list of regression models/texregs.
CreateRegTableChoice <- function(...){
  tabreg.choice <- texreg(
    list(
      list(...)[[1]], list(...)[[2]], list(...)[[3]], list(...)[[4]], 
      list(...)[[5]], list(...)[[6]]
    ), 
    caption = "Ordered / multinomial model (All candidates)", 
    custom.model.names = c(
      " ",  " ", 
      "Fair | Medium", "Medium | Tough", "Fair | Medium", "Medium | Tough"
    ), 
    custom.coef.map = list(
      "age" = "Age", 
      "female" = "Female", 
      "celeb" = "Celebrity candidate", 
      "totcwinsT" = "N of elections before", 
      "is_dual" = "Dual nomination",
      "incBinary" = "Incumbency", 
      "fair|medium" = "Fair | Medium", 
      "medium|tough" = "Medium | Tough"
    ), 
    groups = list(
      "Coefficients" = 1:6, 
      "Thresholds" = 7:8
    ), 
    custom.header = list(
      "Ordered logit" = 1:2, 
      "Cumulative link" = 3:4, 
      "Multinomial logit" = 5:6
    ), 
    custom.gof.rows = list(
      "Year FE" = c("No", "Yes", "Yes", " ", "Yes", " ")
    ), 
    custom.note = paste0(
      "\n\\item %stars.",  
      "\\item P-values are not displayed for the cumulative link and multinomial models.", 
      "\n\\item Dependent variables are categories of ranks in party lists."), 
    digits = 3, 
    threeparttable = TRUE, 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    scalebox = 0.7, 
    label = "table:coefChoice"
  )
  writeLines(tabreg.choice, "./table/tabreg_choice.tex")
}

#' Create a regression table for ordinal models without Party FEs.
#' @param ... A list of regression models/texregs.
CreateRegTableChoicewoParty <- function(...){
  tabreg.choice <- texreg(
    list(
      list(...)[[1]], list(...)[[2]], list(...)[[3]], list(...)[[4]], 
      list(...)[[5]], list(...)[[6]]
    ), 
    custom.model.names = c(
      " ",  " ", 
      "Fair | Medium", "Medium | Tough", "Fair | Medium", "Medium | Tough"
    ), 
    custom.coef.map = list(
      "age" = "Age", 
      "female" = "Female", 
      "celeb" = "Celebrity candidate", 
      "totcwinsT" = "N of elections before", 
      "is_dual" = "Dual nomination",
      "incBinary" = "Incumbency", 
      "fair|medium" = "Fair | Medium", 
      "medium|tough" = "Medium | Tough"
    ), 
    groups = list(
      "Coefficients" = 1:6, 
      "Thresholds" = 7:8
    ), 
    custom.header = list(
      "Ordered logit" = 1:2, 
      "Cumulative link" = 3:4, 
      "Multinomial logit" = 5:6
    ), 
    custom.gof.rows = list(
      "Year FE" = c("No", "Yes", "Yes", " ", "Yes", " ")
    ), 
    custom.note = paste0(
      "\n\\item %stars.",  
      "\\item P-values are not displayed for the cumulative link and multinomial models.", 
      "\n\\item Dependent variables are categories of ranks in party lists."), 
    digits = 3,
    caption = "Ordered / multinomial model (LDP candidates)", 
    threeparttable = TRUE, 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    scalebox = 0.7, 
    label = "table:coefChoiceLDP"
  )
  writeLines(tabreg.choice, "./table/tabreg_choice_ldp.tex")
}

#' Create a regression table for ordinal models for non-LDP candidates.
#' @param ... A list of regression models/texregs.
CreateRegTableChoiceNonLDP <- function(...){
  tabreg.choice <- texreg(
    list(
      list(...)[[1]], list(...)[[2]], list(...)[[3]], list(...)[[4]], 
      list(...)[[5]], list(...)[[6]]
    ), 
    caption = "Ordered / multinomial model (Non-LDP candidates)", 
    custom.model.names = c(
      " ",  " ", 
      "Fair | Medium", "Medium | Tough", "Fair | Medium", "Medium | Tough"
    ), 
    custom.coef.map = list(
      "age" = "Age", 
      "female" = "Female", 
      "celeb" = "Celebrity candidate", 
      "totcwinsT" = "N of elections before", 
      "is_dual" = "Dual nomination",
      "incBinary" = "Incumbency", 
      "fair|medium" = "Fair | Medium", 
      "medium|tough" = "Medium | Tough"
    ), 
    groups = list(
      "Coefficients" = 1:6, 
      "Thresholds" = 7:8
    ), 
    custom.header = list(
      "Ordered logit" = 1:2, 
      "Cumulative link" = 3:4, 
      "Multinomial logit" = 5:6
    ), 
    custom.gof.rows = list(
      "Year FE" = c("No", "Yes", "Yes", " ", "Yes", " ")
    ), 
    custom.note = paste0(
      "\n\\item %stars.",  
      "\\item P-values are not displayed for the cumulative link and multinomial models.", 
      "\n\\item Dependent variables are categories of ranks in party lists."), 
    digits = 3, 
    threeparttable = TRUE, 
    booktabs = TRUE, 
    dcolumn = TRUE, 
    use.packages = FALSE, 
    scalebox = 0.7, 
    label = "table:coefChoiceNonLDP"
  )
  writeLines(tabreg.choice, "./table/tabreg_choice_nonldp.tex")
}