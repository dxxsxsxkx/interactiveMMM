# read packages
library(tidyverse)
library(MASS)
library(ordinal)
library(nnet)
library(texreg)
library(estimatr)
library(pscl)
library(boot)
library(lmtest)
library(margins)
library(marginaleffects)
library(xtable)
library(vtable)
library(sandwich)
library(cowplot)
library(effects)
library(texreg)

# import functions
source("./code/read_data.R")  # ReadReedSmithData()
source("./code/modify_dataset.R")  # functions for filtering and adding new variables to data
source("./code/create_models.R")  # functions for creating models
source("./code/create_texregs.R")  # functions for creating texreg objects
source("./code/create_regression_tables.R")  # functions for creating regression tables
source("./code/draw_figures.R")  # functions for drawing figures
source("./code/draw_figures_interpretation.R")  # functions for drawing figures for interpretation
source("./code/calculate_PR_electability.R")

# data preparation
data.1947.2017 <- read_csv(
  "./data_raw/jhred.csv"
) |> 
  filter(year >= 1947 & year <= 2017) |>
  mutate(
    totcwinsT = totcwins - ifelse(result == 0, 0, 1), 
    incBinary = ifelse(inc == 0, 0, 1), 
    is_smd = ifelse(prcode == 0, 1, 0), 
    is_dual = ifelse(
      !is.na(ken) & !is.na(prcode), 
      1,
      0
    ) |> as.factor()
  )
data.2021 <- read_csv(
  "./data_raw/jhred_2021.csv"
) |> 
  mutate(
    totcwinsT = elected - ifelse(result == 0, 0, 1), 
    incBinary = ifelse(inc == 0, 0, 1), 
    is_smd = ifelse(is.na(prcode), 1, 0),
    is_dual = dual |> as.factor(), 
    kobo = as.double(kobo), 
    byelection = 0, 
    ifelse(is.na(prcode), 0, prcode), 
    party_en = party
  )
data <- bind_rows(data.1947.2017, data.2021)  # this dataframe is used for all analysis

# create small dataframes from `data`
data.smd <- data |> 
  filter(is_smd == 1)
data.pr.only <- data |>  # pure-PR
  filter(
    is_smd == 0, 
    is_dual == 0
  )
data.pr <- data |>  # PR (including dual-listed candidates)
  filter(
    year >= 1994, 
    prcode != 0, 
    byelection != 1
  ) |> 
  AddVariables() |> 
  CalculatePastPRSeats() |> 
  CategorizeRanks() |>   
  select(
    name_jp, legis, year, 
    party_en, prcode, pr_m, pr_rank, 
    totcwinsT, is_dual, incBinary, female, age
  ) |> 
  group_by(legis, party_en, prcode, pr_rank) |> 
  mutate(
    # check if there is another candidate from the same party in the same district in the same election
    # who has the same pr_rank. 
    is_tie = (n() > 1)
  ) |> 
  ungroup()

# election - / party-specific dataframes
data.ldp.2005 <- data.pr %>% 
  filter(
    year == 2005, 
    party_en == "LDP"
  )
data.ldp.2012 <- data.pr %>% 
  filter(
    year == 2012, 
    party_en == "LDP"
  )
data.ldp <- data.pr %>% 
  filter(
    party_en == "LDP"
  )
data.dpj.cdp <- data.pr %>% 
  filter(
    party_en %in% c("DPJ", "CDP")
  )
data.komeito <- data.pr %>% 
  filter(
    party_en == "Komeito"
  )
data.jcp <- data.pr %>% 
  filter(
    party_en == "JCP"
  )

# Main analysis
# H1: Senior candidates are more likely to be ranked higher
# H2: Incumbents are more likely to be ranked higher
# H5: Dual-listed candidates are more likely to be ranked higher
fit.rank.seniority <- glm.nb(
  data = data.pr, 
  pr_rank ~ totcwinsT + as.factor(legis) + as.factor(party_en)
)
fit.rank.incumbency <- glm.nb(
  data = data.pr, 
  pr_rank ~ incBinary + as.factor(legis) + as.factor(party_en)
)
fit.rank.dual <- glm.nb(
  data = data.pr, 
  pr_rank ~ is_dual + as.factor(legis) + as.factor(party_en)
)
fit.rank <- glm.nb(
  data = data.pr, 
  pr_rank ~ totcwinsT + female + is_tie + incBinary + is_dual + 
    is_tie:totcwinsT + is_tie:incBinary + is_tie:is_dual + 
    pr_m + as.factor(legis) + as.factor(party_en)
)

# H3: Senior candidates are more likely to be dual-listed
# H4: Incumbents are more likely to be dual-listed
fit.dual.seniority <- glm(
  data = data.pr, 
  is_dual ~ totcwinsT + as.factor(legis) + as.factor(party_en), 
  family = binomial(link = "logit")
)
fit.dual.incumbency <- glm(
  data = data.pr, 
  is_dual ~ incBinary + as.factor(legis) + as.factor(party_en), 
  family = binomial(link = "logit")
)
fit.dual <- glm(
  data = data.pr, 
  is_dual ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis) + as.factor(party_en), 
  family = binomial(link = "logit")
)
fit.tie.seniority <- glm(
  data = data.pr, 
  is_tie ~ totcwinsT + as.factor(legis) + as.factor(party_en), 
  family = binomial(link = "logit")
)
fit.tie.incumbency <- glm(
  data = data.pr, 
  is_tie ~ incBinary + as.factor(legis) + as.factor(party_en), 
  family = binomial(link = "logit")
)
fit.tie <- glm(
  data = data.pr, 
  is_tie ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis) + as.factor(party_en), 
  family = binomial(link = "logit")
)

# Create texreg objects; need to incorporate party-clustered SEs
texreg.rank.seniority <- createTexreg(
  coef.names = names(summary(fit.rank.seniority)$coefficients[,1]), 
  coef = coef(fit.rank.seniority), 
  se = sqrt(diag(vcovCL(fit.rank.seniority, cluster = ~ party_en))),
  # manually calculate p-values
  p = 2 * (1 - pnorm(abs(coef(fit.rank.seniority)) / sqrt(diag(vcovCL(fit.rank.seniority, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.rank.seniority), logLik(fit.rank.seniority), nobs(fit.rank.seniority)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.rank.incumbency <- createTexreg(
  coef.names = names(summary(fit.rank.incumbency)$coefficients[,1]), 
  coef = coef(fit.rank.incumbency), 
  se = sqrt(diag(vcovCL(fit.rank.incumbency, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.rank.incumbency)) / sqrt(diag(vcovCL(fit.rank.incumbency, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.rank.incumbency), logLik(fit.rank.incumbency), nobs(fit.rank.incumbency)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.rank.dual <- createTexreg(
  coef.names = names(summary(fit.rank.dual)$coefficients[,1]), 
  coef = coef(fit.rank.dual), 
  se = sqrt(diag(vcovCL(fit.rank.dual, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.rank.dual)) / sqrt(diag(vcovCL(fit.rank.dual, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.rank.dual), logLik(fit.rank.dual), nobs(fit.rank.dual)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.rank <- createTexreg(
  coef.names = names(summary(fit.rank)$coefficients[,1]), 
  coef = coef(fit.rank), 
  se = sqrt(diag(vcovCL(fit.rank, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.rank)) / sqrt(diag(vcovCL(fit.rank, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.rank), logLik(fit.rank), nobs(fit.rank)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.dual.seniority <- createTexreg(
  coef.names = names(summary(fit.dual.seniority)$coefficients[,1]), 
  coef = coef(fit.dual.seniority), 
  se = sqrt(diag(vcovCL(fit.dual.seniority, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.dual.seniority)) / sqrt(diag(vcovCL(fit.dual.seniority, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.dual.seniority), logLik(fit.dual.seniority), nobs(fit.dual.seniority)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.dual.incumbency <- createTexreg(
  coef.names = names(summary(fit.dual.incumbency)$coefficients[,1]), 
  coef = coef(fit.dual.incumbency), 
  se = sqrt(diag(vcovCL(fit.dual.incumbency, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.dual.incumbency)) / sqrt(diag(vcovCL(fit.dual.incumbency, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.dual.incumbency), logLik(fit.dual.incumbency), nobs(fit.dual.incumbency)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.dual <- createTexreg(
  coef.names = names(summary(fit.dual)$coefficients[,1]), 
  coef = coef(fit.dual), 
  se = sqrt(diag(vcovCL(fit.dual, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.dual)) / sqrt(diag(vcovCL(fit.dual, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.dual), logLik(fit.dual), nobs(fit.dual)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.tie.seniority <- createTexreg(
  coef.names = names(summary(fit.tie.seniority)$coefficients[,1]), 
  coef = coef(fit.tie.seniority), 
  se = sqrt(diag(vcovCL(fit.tie.seniority, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.tie.seniority)) / sqrt(diag(vcovCL(fit.tie.seniority, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.tie.seniority), logLik(fit.tie.seniority), nobs(fit.tie.seniority)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.tie.incumbency <- createTexreg(
  coef.names = names(summary(fit.tie.incumbency)$coefficients[,1]), 
  coef = coef(fit.tie.incumbency), 
  se = sqrt(diag(vcovCL(fit.tie.incumbency, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.tie.incumbency)) / sqrt(diag(vcovCL(fit.tie.incumbency, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.tie.incumbency), logLik(fit.tie.incumbency), nobs(fit.tie.incumbency)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)
texreg.tie <- createTexreg(
  coef.names = names(summary(fit.tie)$coefficients[,1]), 
  coef = coef(fit.tie), 
  se = sqrt(diag(vcovCL(fit.tie, cluster = ~ party_en))),
  p = 2 * (1 - pnorm(abs(coef(fit.tie)) / sqrt(diag(vcovCL(fit.tie, cluster = ~ party_en))))), 
  gof.names = c("AIC", "Log Likelihood", "Num. obs."), 
  gof = c(AIC(fit.tie), logLik(fit.tie), nobs(fit.tie)), 
  gof.decimal = c(TRUE, TRUE, FALSE)
)

# regression tables
screenreg(
  list(
    texreg.rank.seniority, 
    texreg.rank.incumbency, 
    texreg.rank.dual, 
    texreg.rank,
    texreg.dual.seniority, 
    texreg.dual.incumbency, 
    texreg.dual, 
    texreg.tie.seniority, 
    texreg.tie.incumbency, 
    texreg.tie
  ),
  custom.header = list(
    "List Rank" = 1:4, 
    "Dual Listing" = 5:7,
    "Dual Listing (Tie)" = 8:10
  ), 
  custom.coef.map = list(
    "totcwinsT" = "Total Wins", 
    "incBinary" = "Incumbency", 
    "is_dual1" = "Dual Listing", 
    "is_tieTRUE" = "Tie", 
    "female"  = "Female", 
    "pr_m" = "Block Magnitude", 
    "totcwinsT:is_tieTRUE" = "Total Wins x Tie",
    "is_tieTRUE:incBinary" = "Tie x Incumbency",
    "is_tieTRUE:is_dual1" = "Tie x Dual Listing"
  ), 
  custom.gof.rows = list(
    "Year FE" = c(rep("Yes", 10)),
    "Party FE" = c(rep("Yes", 10))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors clustered at the party level in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (columns 1-4) ", 
    "dual listing status (columns 5-7), and whether the candidate has a tie on the list (columns 8-10).\n", 
    "\\item Estimated models: negatige binomial (columns 1-4) and logit (columns 5-10)."
  ), 
  caption = "Regression Results"
)

# marginal effects plot
# List rank
marginal_effects_rank <- slopes(
  fit.rank, 
  newdata = datagrid(
    totcwinsT = seq(
      min(data.pr$totcwinsT, na.rm = TRUE),
      max(data.pr$totcwinsT, na.rm = TRUE),
      1
    ), 
    female = 0, 
    incBinary = c(0, 1), 
    is_dual = c(as.factor(0), as.factor(1)), 
    is_tie = c(TRUE, FALSE), 
    pr_m = median(data.pr$pr_m, na.rm = TRUE),
    legis = as.factor(46), 
    party_en = as.factor("LDP")
  ), 
  vcov = ~ party_en
)
# Plot marginal effects
marginal_effects_rank |> 
  mutate(
    interaction_label = interaction(incBinary, is_dual, is_tie, sep = ", ")
  ) |> 
  filter(
    term == "totcwinsT", 
    !(is_dual == 0 & is_tie == TRUE)
  ) |>
  ggplot(
    aes(
      x = totcwinsT, 
      y = predicted, 
      color = interaction_label,
      linetype = interaction_label
    )
  ) +
  geom_line() +
  geom_ribbon(
    aes(
      ymin = predicted_lo, 
      ymax = predicted_hi
    ), 
    alpha = 0.2
  ) +
  scale_color_manual(
    name = "Incumbency, Dual Listing, and Tie Status", 
    breaks = c(
      "0, 0, FALSE", 
      "0, 1, FALSE", 
      "0, 1, TRUE", 
      "1, 0, FALSE", 
      "1, 1, FALSE", 
      "1, 1, TRUE"
    ),
    labels = c(
      "Not Incumbent, Not Dual Listed, No Tie",
      "Not Incumbent, Dual Listed, No Tie",
      "Not Incumbent, Dual Listed, Tie",
      "Incumbent, Not Dual Listed, No Tie",
      "Incumbent, Dual Listed, No Tie",
      "Incumbent, Dual Listed, Tie"
    ), 
    values = c("#F8766D", "#00BFC4", "#7CAE00", "#F8766D", "#00BFC4", "#7CAE00")
  ) +
  scale_linetype_manual(
    name = "Incumbency, Dual Listing, and Tie Status", 
    breaks = c(
      "0, 0, FALSE", 
      "0, 1, FALSE", 
      "0, 1, TRUE", 
      "1, 0, FALSE", 
      "1, 1, FALSE", 
      "1, 1, TRUE"
    ),
    labels = c(
      "Not Incumbent, Not Dual Listed, No Tie",
      "Not Incumbent, Dual Listed, No Tie",
      "Not Incumbent, Dual Listed, Tie",
      "Incumbent, Not Dual Listed, No Tie",
      "Incumbent, Dual Listed, No Tie",
      "Incumbent, Dual Listed, Tie"
    ),
    values = c("solid", "solid", "solid", "dashed", "dashed", "dashed")
  ) +
  labs(
    title = NULL,
    x = "N of Past Wins",
    y = "Predicted List Rank"
  ) +
  theme_minimal() + 
  theme(legend.position = "right")

# Dual listing
marginal_effects_dual <- slopes(
  fit.dual, 
  newdata = datagrid(
    totcwinsT = seq(
      min(data.pr$totcwinsT, na.rm = TRUE),
      max(data.pr$totcwinsT, na.rm = TRUE),
      1
    ), 
    female = 0, 
    incBinary = c(0, 1), 
    pr_m = median(data.pr$pr_m, na.rm = TRUE),
    legis = as.factor(46), 
    party_en = as.factor("LDP")
  ), 
  vcov = ~ party_en
)
# Plot marginal effects
marginal_effects_dual |> 
  filter(
    term == "totcwinsT"
  ) |>
  ggplot(
    aes(
      x = totcwinsT, 
      y = predicted, 
      color = as.factor(incBinary)
    )
  ) +
  geom_line() +
  geom_ribbon(
    aes(
      ymin = predicted_lo, 
      ymax = predicted_hi
    ), 
    alpha = 0.2
  ) +
  scale_color_manual(
    name = "Incumbency", 
    breaks = c(0, 1),
    labels = c("Not Incumbent", "Incumbent"), 
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = NULL,
    x = "N of Past Wins",
    y = "Predicted Probability of Dual Listing"
  ) +
  theme_minimal() + 
  theme(legend.position = "right")

marginal_effects_tie <- slopes(
  fit.tie, 
  newdata = datagrid(
    totcwinsT = seq(
      min(data.pr$totcwinsT, na.rm = TRUE),
      max(data.pr$totcwinsT, na.rm = TRUE),
      1
    ), 
    female = 0, 
    incBinary = c(0, 1), 
    pr_m = median(data.pr$pr_m, na.rm = TRUE),
    legis = as.factor(46), 
    party_en = as.factor("LDP")
  ), 
  vcov = ~ party_en
)
# Plot marginal effects
marginal_effects_tie |> 
  filter(
    term == "totcwinsT"
  ) |>
  ggplot(
    aes(
      x = totcwinsT, 
      y = predicted, 
      color = as.factor(incBinary)
    )
  ) +
  geom_line() +
  geom_ribbon(
    aes(
      ymin = predicted_lo, 
      ymax = predicted_hi
    ), 
    alpha = 0.2
  ) +
  scale_color_manual(
    name = "Incumbency", 
    breaks = c(0, 1),
    labels = c("Not Incumbent", "Incumbent"), 
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = NULL,
    x = "N of Past Wins",
    y = "Predicted Probability of Tie"
  ) +
  theme_minimal() + 
  theme(legend.position = "right")
