# This file includes functions creating Texreg objects for different regression models. 

#' Create a Texreg object for univariate zero-inflated negative binomial model.
CreateTexregUnivariateZeroinfl <- function(model){
  texreg.zeroinfl <- createTexreg(
    coef.names = c(
      "(Intercept)", "age", "Log(theta)", "Zero model: Intercept", 
      "Zero model: party_onemanlist"
    ),
    coef = append(
      model$coefficients$count, 
      append(log(model$theta), model$coefficients$zero)
    ),
    se = append(
      sqrt(diag(model$vcov)), 
      model$SE.logtheta, 
      after = 2
    ),
    gof.names = c("AIC", "Log Likelihood", "Num. obs."),
    gof = c(AIC(model), logLik(model), model$n),
    gof.decimal = c(TRUE, TRUE, FALSE)
  )
  return(texreg.zeroinfl)
}

#' Create a Texreg object for zero-inflated negative binomial model.
CreateTexregZeroinfl <- function(model){
  texreg.zeroinfl <- createTexreg(
    coef.names = c(
      "(Intercept)", "age", "female", "totcwinsT", "is_dual", "celeb", "incBinary", "pr_m", 
      "as.factor(legis)42", "as.factor(legis)43", "as.factor(legis)44", 
      "as.factor(legis)45", "as.factor(legis)46", "as.factor(legis)47", 
      "as.factor(legsi)48", 
      "Log(theta)", 
      "Zero model: Intercept", "Zero model: party_onemanlist"
    ), 
    coef = append(
      model$coefficients$count, 
      append(log(model$theta), model$coefficients$zero)
    ), 
    se = append(
      sqrt(diag(model$vcov)), 
      model$SE.logtheta, 
      after = 14
    ), 
    gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
    gof = c(AIC(model), logLik(model), model$n), 
    gof.decimal = c(TRUE, TRUE, FALSE)
  )
  return(texreg.zeroinfl)
}

#' Create a Texreg object for the fair-medium part of cumulative link model.
CreateTexregClmMed <- function(model){
  texreg.clm.med <- createTexreg(
    coef.names = c(
      "fair|medium", "age", "totcwinsT", "female", "incBinary",
      "as.factor(legis)42", "as.factor(legis)43", "as.factor(legis)44", 
      "as.factor(legis)45", "as.factor(legis)46", "as.factor(legis)47", 
      "as.factor(legis)48", "is_dual", "celeb"
    ), 
    coef = c(
      coef(model)[1], coef(model)[3], coef(model)[5], 
      coef(model)[7:17]
    ), 
    se = c(
      sqrt(diag(model$vcov))[1], sqrt(diag(model$vcov))[3],
      sqrt(diag(model$vcov))[5], sqrt(diag(model$vcov))[7:17] 
    ), 
    gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
    gof = c(AIC(model), logLik(model), 6935), 
    gof.decimal = c(TRUE, TRUE, FALSE)
  )
  return(texreg.clm.med)
}

#' Create a Texreg object for the medium-tough part of cumulative link model.
CreateTexregClmtough <- function(model){
  texreg.clm.tough <- createTexreg(
    coef.names = c(
      "medium|tough", "age", "totcwinsT", "female", "incBinary",
      "as.factor(legis)42", "as.factor(legis)43", "as.factor(legis)44", 
      "as.factor(legis)45", "as.factor(legis)46", "as.factor(legis)47", 
      "as.factor(legsi)48", "is_dual", "celeb"
    ), 
    coef = c(
      coef(model)[2], coef(model)[4], coef(model)[6], 
      coef(model)[7:17]
    ), 
    se = c(
      sqrt(diag(model$vcov))[2], sqrt(diag(model$vcov))[4],
      sqrt(diag(model$vcov))[6], sqrt(diag(model$vcov))[7:17] 
    )
  )
}

#' Create a Texreg object for the fair-medium part of multinominal logit model.
CreateTexregMlogitMed <- function(model){
  texreg.mlogit.med <- createTexreg(
    coef.names = c(
      "(Intercept)", "age", "female", 
      "totcwinsT", "is_dual", "celeb", "incBinary", 
      "as.factor(legis)42", "as.factor(legis)43", 
      "as.factor(legis)44", "as.factor(legis)45", 
      "as.factor(legis)46", "as.factor(legis)47", 
      "as.factor(legsi)48"
    ), 
    coef = coef(model)[1, ], 
    se = sqrt(diag(vcov(model)[1:14, 1:14])), 
    gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
    gof = c(AIC(model), logLik(model), 6935), 
    gof.decimal = c(TRUE, TRUE, FALSE)
  )
  return(texreg.mlogit.med)
}

#' Create a Texreg object for the medium-tough part of multinominal logit model.
CreateTexregMlogitTough <- function(model){
  texreg.mlogit.tough <- createTexreg(
    coef.names = c(
      "(Intercept)", "age", "female", 
      "totcwinsT", "is_dual", "celeb", "incBinary", 
      "as.factor(legis)42", "as.factor(legis)43", 
      "as.factor(legis)44", "as.factor(legis)45", 
      "as.factor(legis)46", "as.factor(legis)47", 
      "as.factor(legsi)48"
    ), 
    coef = coef(model)[2, ], 
    se = sqrt(diag(vcov(model)[15:28, 15:28]))
  )
  return(texreg.mlogit.tough)
}