# read packages
library(tidyverse)
library(MASS)
library(texreg)
library(estimatr)
library(marginaleffects)
library(sandwich)
library(cowplot)
library(effects)
library(texreg)

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
  dplyr::select(
    name_jp, legis, year, 
    party_en, prcode, pr_m, pr_rank, result, 
    totcruns, totcwinsT, is_dual, incBinary, female, age
  ) |> 
  group_by(legis, party_en, prcode, pr_rank) |> 
  mutate(
    # check if there is another candidate from the same party in the same district in the same election
    # who has the same pr_rank. 
    is_tie = (n() > 1)
  ) |> 
  ungroup()

# Summary statistics
# A1
# total
data.pr |> 
  summarize(
    n = n(), 
    n_inc = sum(incBinary == 1) / n, 
    n_dual = sum(is_dual == 1) / n, 
    n_tie = sum(is_dual == 1 & is_tie == 1) / n,
    n_female = sum(female == 1) / n, 
    mean_n_win = mean(totcwinsT, na.rm = TRUE),
    sd_n_win = sd(totcwinsT, na.rm = TRUE), 
    med_n_win = median(totcwinsT, na.rm = TRUE)
  )
# by election year
data.pr |> 
  group_by(year) |> 
  summarize(
    n = n(), 
    n_inc = sum(incBinary == 1) / n, 
    n_dual = sum(is_dual == 1) / n, 
    n_tie = sum(is_dual == 1 & is_tie == 1) / n, 
    n_female = sum(female == 1) / n, 
    mean_n_win = mean(totcwinsT, na.rm = TRUE),
    sd_n_win = sd(totcwinsT, na.rm = TRUE), 
    med_n_win = median(totcwinsT, na.rm = TRUE)
  )

# A2: PR block magnitudes
data.pr |> 
  group_by(year, prcode) |> 
  # pr_m is the same for all candidates in the same PR block
  summarize(
    n = n(), 
    pr_m = first(pr_m)
  ) |> 
  filter(
    year == 2021  # modify this to see other years
  )

# A3: Distribution of list rank
data.pr |> 
  # calculate proportion for each rank
  group_by(pr_rank) |>
  summarize(
    n = n(), 
  ) |>
  mutate(
    prop = n / sum(n)
  ) |> 
  ggplot(
    aes(
      x = pr_rank, 
      y = prop
    )
  ) +
  geom_histogram(
    stat = "identity",
    bins = 30, 
    color = "white"
  ) +
  labs(
    title = NULL,
    x = "List Rank",
    y = "Frequency"
  ) +
  theme_minimal(
    base_size = 16
  )
ggsave(
  "./figure/paper/pr_rank_distribution.pdf", 
  width = 8, 
  height = 6
)

# Frequency of dual listing
data.dual.total <- data.pr %>% 
  filter(
    result %in% c(2, 3)
  ) %>% 
  group_by(year) %>% 
  summarize(
    n = n(), 
    n_dual = sum(as.numeric(is_dual)[result == 2] - 1)
  ) %>%
  mutate(
    party_en = "Total",
    year = year,
    prop_dual = n_dual / n
  ) %>% 
  ungroup()
data.dual.party <- data.pr %>% 
  filter(
    result %in% c(2, 3)
  ) %>% 
  group_by(year, party_en) %>% 
  summarize(
    n = n(), 
    n_dual = sum(as.numeric(is_dual)[result == 2] - 1)
  ) %>%
  mutate(
    year = year,
    prop_dual = n_dual / n
  ) %>% 
  ungroup() %>% 
  # change party names 
  # DPJ + CDP = DPJ, 1996 - 2014 and CDP, 2017
  mutate(
    party_en = as.character(party_en) %>% 
      ifelse(
        . %in% c("DPJ", "CDP"), 
        "DPJ + CDP",
        .
      )
  )

data.dual <- data.dual.total %>%
  bind_rows(data.dual.party) %>% 
  filter(
    party_en %in% c(
      "Total", 
      "LDP", 
      "Komeito", 
      "DPJ + CDP", 
      "JCP"
    )
  )

# Figure 1
data.dual %>% 
  filter(party_en == "Total") |>  # I changed my mind ...
  ggplot() +
  geom_line(
    aes(
      x = year, 
      y = prop_dual#, 
      #color = party_en
    ), 
    linewidth = 1
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Proportion"
  ) +
  scale_color_discrete(
    name = NULL
  ) + 
  guides(
    color = guide_legend(
      title = "Party", 
      reverse = TRUE
    ), 
    alpha = "none"
  ) + 
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    axis.title.x = element_text(
      margin = margin(0, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    ), 
    # legend
    legend.title = element_blank(), 
    legend.position = "bottom"
  )
ggsave("./figure/paper/dual_nomination.pdf", width = 8, height = 6)


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

# B1
# regression tables
texreg(
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
  caption = "Regression Results", 
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!htbp",
  scalebox = 0.6,
  file = "./table/regression_results.tex", 
  label = "tab:regression_results"
)

# Marginal effects plot
# Figure 2
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
    name = NULL, 
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
    name = NULL, 
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
  theme(
    legend.position = "bottom", 
    text = element_text(size = 12)
  )
ggsave(
  "./figure/paper/marginal_effects_rank.pdf", 
  width = 8, 
  height = 6
)

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
plot.dual <- marginal_effects_dual |> 
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
  theme(
    legend.position = "bottom"
  )

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
plot.tie <- marginal_effects_tie |> 
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
  theme(legend.position = "bottom")

# Figure 3
plot_grid(
  plot.dual, 
  plot.tie, 
  ncol = 1
)
ggsave(
  "./figure/paper/marginal_effects_dual_tie.pdf", 
  width = 6, 
  height = 8
)

# Party- and election-specific analysis
# LDP
fit.rank.seniority.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ totcwinsT + as.factor(legis)
)
fit.rank.incumbency.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ incBinary + as.factor(legis) 
)
fit.rank.dual.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ is_dual + as.factor(legis) 
)
fit.rank.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ totcwinsT + female + is_tie + incBinary + is_dual + 
    is_tie:totcwinsT + is_tie:incBinary + is_tie:is_dual + 
    pr_m + as.factor(legis)
)
fit.dual.seniority.ldp <- glm(
  data = data.ldp, 
  is_dual ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.incumbency.ldp <- glm(
  data = data.ldp, 
  is_dual ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.ldp <- glm(
  data = data.ldp, 
  is_dual ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.seniority.ldp <- glm(
  data = data.ldp, 
  is_tie ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.incumbency.ldp <- glm(
  data = data.ldp, 
  is_tie ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.ldp <- glm(
  data = data.ldp, 
  is_tie ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis), 
  family = binomial(link = "logit")
)

# DPJ + CDP
fit.rank.seniority.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ totcwinsT + as.factor(legis)
)
fit.rank.incumbency.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ incBinary + as.factor(legis) 
)
fit.rank.dual.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ is_dual + as.factor(legis) 
)
fit.rank.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ totcwinsT + female + is_tie + incBinary + is_dual + 
    is_tie:totcwinsT + is_tie:incBinary + is_tie:is_dual + 
    pr_m + as.factor(legis), 
)
fit.dual.seniority.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_dual ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.incumbency.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_dual ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_dual ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis),
  family = binomial(link = "logit")
)
fit.tie.seniority.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_tie ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.incumbency.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_tie ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_tie ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis),
  family = binomial(link = "logit")
)

# Komeito
fit.rank.seniority.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ totcwinsT + as.factor(legis)
)
fit.rank.incumbency.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ incBinary + as.factor(legis) 
)
fit.rank.dual.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ is_dual + as.factor(legis) 
)
fit.rank.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ totcwinsT + female + is_tie + incBinary + is_dual + 
    is_tie:totcwinsT + is_tie:incBinary + is_tie:is_dual + 
    pr_m + as.factor(legis)
)
fit.dual.seniority.komeito <- glm(
  data = data.komeito, 
  is_dual ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.incumbency.komeito <- glm(
  data = data.komeito, 
  is_dual ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.komeito <- glm(
  data = data.komeito, 
  is_dual ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis),
  family = binomial(link = "logit")
)
fit.tie.seniority.komeito <- glm(
  data = data.komeito, 
  is_tie ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.incumbency.komeito <- glm(
  data = data.komeito, 
  is_tie ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.komeito <- glm(
  data = data.komeito, 
  is_tie ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis),
  family = binomial(link = "logit")
)

# JCP
fit.rank.seniority.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ totcwinsT + as.factor(legis)
)
fit.rank.incumbency.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ incBinary + as.factor(legis) 
)
fit.rank.dual.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ is_dual + as.factor(legis) 
)
fit.rank.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ totcwinsT + female + is_tie + incBinary + is_dual + 
    is_tie:totcwinsT + is_tie:incBinary + is_tie:is_dual + 
    pr_m + as.factor(legis)
)
fit.dual.seniority.jcp <- glm(
  data = data.jcp, 
  is_dual ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.incumbency.jcp <- glm(
  data = data.jcp, 
  is_dual ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.dual.jcp <- glm(
  data = data.jcp, 
  is_dual ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis),
  family = binomial(link = "logit")
)
fit.tie.seniority.jcp <- glm(
  data = data.jcp, 
  is_tie ~ totcwinsT + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.incumbency.jcp <- glm(
  data = data.jcp, 
  is_tie ~ incBinary + as.factor(legis), 
  family = binomial(link = "logit")
)
fit.tie.jcp <- glm(
  data = data.jcp, 
  is_tie ~ totcwinsT + female + incBinary + pr_m + 
    as.factor(legis),
  family = binomial(link = "logit")
)

# 2005 and 2012 LDP
fit.rank.ldp.2005 <- glm.nb(
  data = data.ldp.2005, 
  pr_rank ~ totcwinsT + female + is_tie + incBinary + is_dual + 
    is_tie:totcwinsT + is_tie:incBinary + is_tie:is_dual + 
    pr_m
)
fit.dual.ldp.2005 <- glm(
  data = data.ldp.2005, 
  is_dual ~ totcwinsT + female + incBinary + pr_m,
  family = binomial(link = "logit")
)
fit.tie.ldp.2005 <- glm(
  data = data.ldp.2005, 
  is_tie ~ totcwinsT + female + incBinary + pr_m,
  family = binomial(link = "logit")
)
fit.rank.ldp.2012 <- glm.nb(
  data = data.ldp.2012, 
  pr_rank ~ totcwinsT + female + incBinary + is_dual + 
    is_dual:totcwinsT + is_dual:incBinary + 
    pr_m
)
fit.dual.ldp.2012 <- glm(
  data = data.ldp.2012, 
  is_dual ~ totcwinsT + female + incBinary + pr_m,
  family = binomial(link = "logit")
)

# Regression tables
# Table 2: LDP
texreg(
  list(
    fit.rank.seniority.ldp, 
    fit.rank.incumbency.ldp, 
    fit.rank.dual.ldp, 
    fit.rank.ldp,
    fit.dual.seniority.ldp, 
    fit.dual.incumbency.ldp, 
    fit.dual.ldp, 
    fit.tie.seniority.ldp, 
    fit.tie.incumbency.ldp, 
    fit.tie.ldp
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
    "Party FE" = c(rep("No", 10))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (columns 1-4) ", 
    "dual listing status (columns 5-7), and whether the candidate has a tie on the list (columns 8-10).\n", 
    "\\item Estimated models: negatige binomial (columns 1-4) and logit (columns 5-10)."
  ), 
  caption = "Regression Results for LDP Candidates", 
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!htbp",
  scalebox = 0.6, 
  file = "./table/regression_results_ldp.tex", 
  label = "tab:ldp"
)

# Table 3: DPJ + CDP
texreg(
  list(
    fit.rank.seniority.dpj.cdp, 
    fit.rank.incumbency.dpj.cdp, 
    fit.rank.dual.dpj.cdp, 
    fit.rank.dpj.cdp,
    fit.dual.seniority.dpj.cdp, 
    fit.dual.incumbency.dpj.cdp, 
    fit.dual.dpj.cdp, 
    fit.tie.seniority.dpj.cdp, 
    fit.tie.incumbency.dpj.cdp, 
    fit.tie.dpj.cdp
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
    "Party FE" = c(rep("No", 10))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (columns 1-4) ", 
    "dual listing status (columns 5-7), and whether the candidate has a tie on the list (columns 8-10).\n", 
    "\\item Estimated models: negatige binomial (columns 1-4) and logit (columns 5-10)."
  ), 
  caption = "Regression Results for DPJ / CDP Candidates", 
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!htbp",
  scalebox = 0.6, 
  file = "./table/regression_results_dpj_cdp.tex", 
  label = "tab:dpj_cdp"
)

# B2: Komeito
texreg(
  list(
    fit.rank.seniority.komeito, 
    fit.rank.incumbency.komeito, 
    fit.rank.dual.komeito, 
    fit.rank.komeito,
    fit.dual.seniority.komeito, 
    fit.dual.incumbency.komeito, 
    fit.dual.komeito, 
    fit.tie.seniority.komeito, 
    fit.tie.incumbency.komeito, 
    fit.tie.komeito
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
    "Party FE" = c(rep("No", 10))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (columns 1-4) ", 
    "dual listing status (columns 5-7), and whether the candidate has a tie on the list (columns 8-10).\n", 
    "\\item Estimated models: negatige binomial (columns 1-4) and logit (columns 5-10)."
  ), 
  caption = "Regression Results for Komeito Candidates", 
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  scalebox = 0.7, 
  float.pos = "!htbp",
  file = "./table/regression_results_komeito.tex",
  label = "tab:komeito"
)

# B3: JCP
texreg(
  list(
    fit.rank.seniority.jcp, 
    fit.rank.incumbency.jcp, 
    fit.rank.dual.jcp, 
    fit.rank.jcp,
    fit.dual.seniority.jcp, 
    fit.dual.incumbency.jcp, 
    fit.dual.jcp, 
    fit.tie.seniority.jcp, 
    fit.tie.incumbency.jcp, 
    fit.tie.jcp
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
    "Party FE" = c(rep("No", 10))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (columns 1-4) ", 
    "dual listing status (columns 5-7), and whether the candidate has a tie on the list (columns 8-10).\n", 
    "\\item Estimated models: negatige binomial (columns 1-4) and logit (columns 5-10)."
  ), 
  caption = "Regression Results for JCP Candidates", 
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!htbp",
  scalebox = 0.7,
  file = "./table/regression_results_jcp.tex",
  label = "tab:jcp"
)

# Table 4: 2005 and 2012 LDP
texreg(
  list(
    fit.rank.ldp.2005, 
    fit.dual.ldp.2005, 
    fit.tie.ldp.2005,
    fit.rank.ldp.2012, 
    fit.dual.ldp.2012
  ),
  custom.header = list(
    "2005" = 1:3, 
    "2012" = 4:5
  ), 
  custom.model.names = c(
    "List Rank",
    "Dual Listing",
    "Tie", 
    "List Rank",
    "Dual Listing"
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
    "Year FE" = c(rep("Yes", 5)),
    "Party FE" = c(rep("No", 5))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (columns 1 / 4), ", 
    "dual listing status (columns 2 / 5), and whether the candidate has a tie on the list (column 3).\n", 
    "\\item Estimated models: negatige binomial (columns 1 / 4) and logit (columns 2, 3, and 5). \n", 
    "\\textit{Note.} All dual-listed LDP candidates in the 2012 general election had ties on the list."
  ), 
  caption = "Regression Results for LDP Candidates in 2005 and 2012",
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!htbp",
  scalebox = NULL, 
  file = "./table/regression_results_ldp_2005_2012.tex",
  label = "tab:ldp_2005_2012"
)

# Age of winners
data.winner <- data %>% 
  filter(
    result %in% c(1, 2, 3)
  ) %>% 
  mutate(
    win_in = ifelse(
      result == 1, 
      "Majoritarian", 
      "PR"
    )
  )

# Figure 4: Age composition of legislators elected from each tier
data.winner %>% 
  filter(
    # only post-reform winners
    year >= 1994
  ) |> 
  group_by(win_in) %>%
  # summarize by five years in age
  mutate(
    age = cut(
      age, 
      breaks = seq(20, 100, 5)
    )
  ) %>%
  count(age) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = age, 
      y = prop,
      fill = win_in
    )
  ) +
  geom_histogram(
    stat = "identity",
    position = "dodge", 
    alpha = 0.7,
    binwidth = 5
  ) +
  labs(
    title = NULL,
    x = "Age",
    y = NULL
  ) +
  scale_x_discrete(
    labels = c(
      "20-25", 
      "25-30", 
      "30-35", 
      "35-40", 
      "40-45", 
      "45-50", 
      "50-55", 
      "55-60", 
      "60-65", 
      "65-70", 
      "70-75", 
      "75-80", 
      "80-85", 
      "85-90"
    )
  ) + 
  theme_minimal() +
  theme(
    legend.position = "inside", 
    legend.position.inside = c(0.9, 0.8), 
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "white", 
      color = "black"
    ),
    text = element_text(size = 15), 
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1
    ), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    )
  )
ggsave("./figure/paper/age_smd_vs_pr_winners.pdf", width = 8, height = 6)

# Figure 4: Age comparison, average vs. new candidates
age.first.run <- data %>% 
  filter(
    totcruns == 1, 
    year >= 1994, 
    byelection != 1
  ) %>% 
  group_by(year) %>% 
  summarize(
    first_run = mean(age)
  )
age.all <- data %>% 
  filter(
    year >= 1994, 
    byelection != 1
  ) %>% 
  group_by(year) %>%
  summarize(
    all = mean(age)
  )
age.pr.first.run <- data.pr %>% 
  filter(
    totcruns == 1, 
    year >= 1994
  ) %>%
  group_by(year) %>%
  summarize(
    first_run_pr = mean(age)
  )
age.pr.all <- data.pr %>% 
  filter(
    year >= 1994, 
  ) %>% 
  group_by(year) %>% 
  summarize(
    all_pr = mean(age)
  )

# 2*2: first-time candidates vs. all candidates, SMD + PR vs. PR only
age.first.run %>%
  left_join(age.all, by = "year") %>%
  left_join(age.pr.first.run, by = "year") %>%
  left_join(age.pr.all, by = "year") %>%
  pivot_longer(
    cols = c(-year), 
    names_to = "variable", 
    values_to = "age"
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = year, 
      y = age, 
      color = variable, 
      linetype = variable
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      all = "red", 
      first_run = "red", 
      all_pr = "blue", 
      first_run_pr = "blue"
    ), 
    labels = c(
      all = "SMD + PR", 
      first_run = "First Run, SMD + PR", 
      all_pr = "PR", 
      first_run_pr = "First Run, PR"
    )
  ) + 
  scale_linetype_manual(
    name = NULL,
    values = c(
      all = "solid", 
      first_run = "dashed", 
      all_pr = "solid", 
      first_run_pr = "dashed"
    ), 
    labels = c(
      all = "SMD + PR", 
      first_run = "First Run, SMD + PR", 
      all_pr = "PR", 
      first_run_pr = "First Run, PR"
    )
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean Age"
  ) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20), 
    legend.text = element_text(size = 13),
    axis.title.x = element_text(
      margin = margin(0, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    ), 
    legend.position = "bottom"
  )
ggsave("./figure/paper/age_first_run.pdf", width = 8, height = 6)  


# Data of winners
age.first.win <- data.winner %>% 
  filter(
    # only post-reform winners
    year >= 1994, 
    byelection != 1, 
    totcwinsT == 0
  ) %>% 
  group_by(year) %>%
  summarize(
    first_win = mean(age)
  )
age.all.win <- data.winner %>%
  filter(
    # only post-reform winners
    year >= 1994, 
    byelection != 1
  ) %>% 
  group_by(year) %>% 
  summarize(
    all_win = mean(age)
  )
age.pr.first.win <- data.winner %>%
  filter(
    # only post-reform winners
    year >= 1994, 
    byelection != 1, 
    totcwinsT == 0, 
    result %in% c(2, 3)
  ) %>%
  group_by(year) %>%
  summarize(
    first_win_pr = mean(age)
  )
age.pr.all.win <- data.winner %>%
  filter(
    # only post-reform winners
    year >= 1994, 
    byelection != 1, 
    result %in% c(2, 3)
  ) %>% 
  group_by(year) %>% 
  summarize(
    all_win_pr = mean(age)
  )

# Table 6: data of first-time winners
# mean age of first-time winners
data.winner %>% 
  filter(
    # only post-reform winners
    byelection != 1, 
    totcwinsT == 0
  ) %>% 
  group_by(year) %>%
  summarize(
    first_win = mean(age)
  ) |> 
  group_by(year) |> 
  summarize(
    mean(first_win)
  )

# proportion of first-time winners
data.winner |> 
  filter(
    byelection != 1
  ) |> 
  group_by(year) |> 
  # count the number of first-time winners (totcwinsT = 0)
  summarize(
    n_first_win = sum(totcwinsT == 0), 
    n_all_win = n()
  ) |>
  mutate(
    prop_first_win = n_first_win / n_all_win
  ) |> 
  print(n = 27)

# Mean age of all winners
data.winner %>% 
  filter(
    # only post-reform winners
    byelection != 1
  ) %>% 
  group_by(year) %>%
  summarize(
    first_win = mean(age)
  ) |> 
  group_by(year) |> 
  summarize(
    mean(first_win)
  ) |>  
  print(n = 27)

# A4: Age comparison, average vs. new winners
# 2*2: first-time winners vs. all winners, SMD + PR vs. PR only
age.first.win %>%
  left_join(age.all.win, by = "year") %>%
  left_join(age.pr.first.win, by = "year") %>%
  left_join(age.pr.all.win, by = "year") %>%
  pivot_longer(
    cols = c(-year), 
    names_to = "variable", 
    values_to = "age"
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = year, 
      y = age, 
      color = variable, 
      linetype = variable
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      all_win = "red", 
      first_win = "red", 
      all_win_pr = "blue", 
      first_win_pr = "blue"
    ), 
    labels = c(
      all_win = "SMD + PR", 
      first_win = "First Win, SMD + PR", 
      all_win_pr = "PR", 
      first_win_pr = "First Win, PR"
    )
  ) + 
  scale_linetype_manual(
    name = NULL,
    values = c(
      all_win = "solid", 
      first_win = "dashed", 
      all_win_pr = "solid", 
      first_win_pr = "dashed"
    ), 
    labels = c(
      all_win = "SMD + PR", 
      first_win = "First Win, SMD + PR", 
      all_win_pr = "PR", 
      first_win_pr = "First Win, PR"
    )
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean Age"
  ) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20), 
    legend.text = element_text(size = 13),
    axis.title.x = element_text(
      margin = margin(0, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    ), 
    legend.position = "bottom"
  )
ggsave("./figure/paper/age_first_win.pdf", width = 8, height = 6)













