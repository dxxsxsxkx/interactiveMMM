# This file includes functions that create regression models. 
# NOTE: functions are intended to be used for specific classes of data or model specifications. 
# NOTE: all functions expect a data frame which is either the full version of or a subset of
#  the Reed-Smith dataset.

#' Fit univariate negative binomial model. 
FitUnivariateNB <- function(data){
  fit.nb <- glm.nb(
    data = data, 
    pr_rank - 1 ~ age
  )
  return(fit.nb)
}

#' Fit negative binomial regression model.
FitNB <- function(data){
  fit.nb <- glm.nb(
    data = data, 
    pr_rank - 1 ~ age + female + totcwinsT + as.factor(party_en) + is_dual + celeb + 
      incBinary + pr_m + as.factor(legis)
  )
  return(fit.nb)
}

#' Fit negative binomial regression model, without including the party FE. 
FitNBwoParty <- function(data){
  fit.nb <- glm.nb(
    data = data, 
    pr_rank - 1 ~ age + female + totcwinsT + is_dual + celeb +
      incBinary + pr_m + as.factor(legis) 
  )
  return(fit.nb)
}

#' Fit univariate zero-inflated negative binomial model.
FitUnivariateZeroInfl <- function(data){
  fit.zeroinfl <- zeroinfl(
    data = data, 
    (pr_rank - 1) ~ age | party_onemanlist,
    dist = "negbin"
  )
  return(fit.zeroinfl)
}

#' Fit zero-inflated negative binomial regression model.
FitZeroInfl <- function(data){
  fit.zeroinfl <- zeroinfl(
    data = data, 
    (pr_rank - 1) ~ age + female + totcwins + is_dual + celeb + 
      incBinary + pr_m + as.factor(legis) | party_onemanlist, 
    dist = "negbin"
  )
  return(fit.zeroinfl)
}

#' Fit a simple ordered logit model.
FitUnivariateOrdLogit <- function(data){
  fit.uni.ord <- clm(
    data = data, 
    pr_electability_lab ~ age, 
    link = "logit"
  )
  return(fit.uni.ord)
}

#' Fit a multivariate ordered logit model.
FitMultivariateOrdLogit <- function(data){
  fit.multi.ord <- clm(
    data = data, 
    pr_electability_lab ~ 
      age + female + totcwinsT + celeb + incBinary + as.factor(legis) + is_dual, 
    link = "logit"
  )
  return(fit.multi.ord)
}

#' Fit a cumulative link model. 
FitCLM <- function(data){
  fit.clm <- clm(
    data = data, 
    pr_electability_lab ~ 
      female + celeb + incBinary + as.factor(legis) + is_dual, 
    nominal = ~ age + totcwinsT  # nominal part of the model
  )
  return(fit.clm)
}

#' Fit a multinomial logit model.
FitMultiLogit <- function(data){
  fit.mlogit <- multinom(
    data = data, 
    as.factor(pr_electability_lab) ~ age + female + totcwinsT + is_dual + celeb + 
      incBinary + as.factor(legis)
  ) 
  return(fit.mlogit)
}