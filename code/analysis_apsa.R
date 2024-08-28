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
library(gtsummary)
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

# read and clean data
data <- ReadReedSmithData() %>% 
  filter(
    year >= 1994,
    year <= 2020
  ) %>% 
  mutate(
    is_smd = ifelse(prcode == 0, 1, 0), 
    is_novice = ifelse(totcruns == 1, 1, 0) %>% 
      as.factor(),
    is_dual = ifelse(
      !is.na(ken) & !is.na(prcode), 
      1,
      0
    ) %>% 
      as.factor(),
    party_en = as.factor(party_en)
  )

data.smd <- data %>% 
  filter(
    is_smd == 1
  )
data.pr.only <- data %>% 
  filter(
    is_smd == 0, 
    is_dual == 0
  )

data.pr <- data %>%  # PR
  filter(
    year >= 1994, 
    year <= 2020, 
    prcode != 0, 
    byelection != 1
  ) %>%
  AddVariables() %>% 
  CalculatePastPRSeats() %>% 
  CategorizeRanks() %>% 
  mutate(
    is_smd = ifelse(prcode == 0, 1, 0), 
    is_novice = ifelse(totcruns == 1, 1, 0) %>% 
      as.factor(),
    is_dual = as.factor(is_dual),
    incBinary = as.factor(incBinary),
    party_en = as.factor(party_en), 
    legis = as.factor(legis)
  )

# election- / party-specific data
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

# LDP
fit.h1.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ is_dual + female + pr_m + legis
)
fit.h2.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ incBinary + female + pr_m + legis
)
fit.h3.ldp <- glm.nb(
  data = data.ldp, 
  pr_rank ~ totcwinsT + female + pr_m + legis
)
fit.h4.ldp <- glm(
  data = data.ldp, 
  is_dual ~ incBinary + female + pr_m + legis,
  family = binomial(link = "logit")
)
fit.h5.ldp <- glm(
  data = data.ldp, 
  is_dual ~ totcwinsT + female + pr_m + legis, 
  family = binomial(link = "logit")
)
texreg(
  list(
    fit.h5.ldp, 
    fit.h4.ldp, 
    fit.h3.ldp, 
    fit.h2.ldp, 
    fit.h1.ldp
  ), 
  custom.header = list(
    "Dual Listing" = 1:2, 
    "List Rank" = 3:5
  ), 
  custom.model.names = c(
    "H1", "H2", "H3", "H4", "H5"
  ), 
  custom.coef.map = list(
    "totcwinsT" = "Total Wins", 
    "incBinary1" = "Incumbency", 
    "is_dual1" = "Dual Listing", 
    "female"  = "Female", 
    "pr_m" = "Block Magnitude"
  ), 
  custom.gof.rows = list(
    "Year FE" = c(rep("Yes", 5)),
    "Party FE" = c(rep("Yes", 5))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (H1-2) ", 
    "and dual nomination status (H3-5).\n", 
    "\\item Estimated models: logit (H1-2) and negative binomial (H3-5)."
  ), 
  caption = "Regression Results for LDP Candidates",
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!bth", 
  label = "tab:regLDP",
  file = "./table/regression_results_ldp.tex"
)

# DPJ + CDP
fit.h1.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ is_dual + female + pr_m + legis
)
fit.h2.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ incBinary + female + pr_m + legis
)
fit.h3.dpj.cdp <- glm.nb(
  data = data.dpj.cdp, 
  pr_rank ~ totcwinsT + female + pr_m + legis
)
fit.h4.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_dual ~ incBinary + female + pr_m + legis,
  family = binomial(link = "logit")
)
fit.h5.dpj.cdp <- glm(
  data = data.dpj.cdp, 
  is_dual ~ totcwinsT + female + pr_m + legis, 
  family = binomial(link = "logit")
)
texreg(
  list(
    fit.h5.dpj.cdp, 
    fit.h4.dpj.cdp, 
    fit.h3.dpj.cdp, 
    fit.h2.dpj.cdp, 
    fit.h1.dpj.cdp
  ), 
  custom.header = list(
    "Dual Listing" = 1:2, 
    "List Rank" = 3:5
  ), 
  custom.model.names = c(
    "H1", "H2", "H3", "H4", "H5"
  ), 
  custom.coef.map = list(
    "totcwinsT" = "Total Wins", 
    "incBinary1" = "Incumbency", 
    "is_dual1" = "Dual Listing", 
    "female"  = "Female", 
    "pr_m" = "Block Magnitude"
  ), 
  custom.gof.rows = list(
    "Year FE" = c(rep("Yes", 5)),
    "Party FE" = c(rep("Yes", 5))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (H1-2) ", 
    "and dual nomination status (H3-5).\n", 
    "\\item Estimated models: logit (H1-2) and negative binomial (H3-5)."
  ), 
  caption = "Regression Results for DPJ / CDP Candidates",
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!bth", 
  label = "tab:regDPJCDP",
  file = "./table/regression_results_dpj_cdp.tex"
)

# Komeito
fit.h1.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ is_dual + female + pr_m + legis
)
fit.h2.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ incBinary + female + pr_m + legis
)
fit.h3.komeito <- glm.nb(
  data = data.komeito, 
  pr_rank ~ totcwinsT + female + pr_m + legis
)
fit.h4.komeito <- glm(
  data = data.komeito, 
  is_dual ~ incBinary + female + pr_m + legis,
  family = binomial(link = "logit")
)
fit.h5.komeito <- glm(
  data = data.komeito, 
  is_dual ~ totcwinsT + female + pr_m + legis, 
  family = binomial(link = "logit")
)
texreg(
  list(
    fit.h5.komeito, 
    fit.h4.komeito, 
    fit.h3.komeito, 
    fit.h2.komeito, 
    fit.h1.komeito
  ), 
  custom.header = list(
    "Dual Listing" = 1:2, 
    "List Rank" = 3:5
  ), 
  custom.model.names = c(
    "H1", "H2", "H3", "H4", "H5"
  ), 
  custom.coef.map = list(
    "totcwinsT" = "Total Wins", 
    "incBinary1" = "Incumbency", 
    "is_dual1" = "Dual Listing", 
    "female"  = "Female", 
    "pr_m" = "Block Magnitude"
  ), 
  custom.gof.rows = list(
    "Year FE" = c(rep("Yes", 5)),
    "Party FE" = c(rep("Yes", 5))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (H1-2) ", 
    "and dual nomination status (H3-5).\n", 
    "\\item Estimated models: logit (H1-2) and negative binomial (H3-5)."
  ), 
  caption = "Regression Results for Komeito Candidates",
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!bth", 
  label = "tab:regKomeito",
  file = "./table/regression_results_komeito.tex"
)

# JCP
fit.h1.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ is_dual + female + pr_m + legis
)
fit.h2.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ incBinary + female + pr_m + legis
)
fit.h3.jcp <- glm.nb(
  data = data.jcp, 
  pr_rank ~ totcwinsT + female + pr_m + legis
)
fit.h4.jcp <- glm(
  data = data.jcp, 
  is_dual ~ incBinary + female + pr_m + legis,
  family = binomial(link = "logit")
)
fit.h5.jcp <- glm(
  data = data.jcp, 
  is_dual ~ totcwinsT + female + pr_m + legis, 
  family = binomial(link = "logit")
)
texreg(
  list(
    fit.h5.jcp, 
    fit.h4.jcp, 
    fit.h3.jcp, 
    fit.h2.jcp, 
    fit.h1.jcp
  ), 
  custom.header = list(
    "Dual Listing" = 1:2, 
    "List Rank" = 3:5
  ), 
  custom.model.names = c(
    "H1", "H2", "H3", "H4", "H5"
  ), 
  custom.coef.map = list(
    "totcwinsT" = "Total Wins", 
    "incBinary1" = "Incumbency", 
    "is_dual1" = "Dual Listing", 
    "female"  = "Female", 
    "pr_m" = "Block Magnitude"
  ), 
  custom.gof.rows = list(
    "Year FE" = c(rep("Yes", 5)),
    "Party FE" = c(rep("Yes", 5))
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (H1-2) ", 
    "and dual nomination status (H3-5).\n", 
    "\\item Estimated models: logit (H1-2) and negative binomial (H3-5)."
  ), 
  caption = "Regression Results for JCP Candidates",
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!bth", 
  label = "tab:regJCP",
  file = "./table/regression_results_jcp.tex"
)

# 2005 LDP
fit.h3.ldp.2005 <- glm.nb(
  data = data.ldp.2005, 
  pr_rank ~ totcwinsT + female + pr_m
)
fit.h4.ldp.2005 <- glm(
  data = data.ldp.2005, 
  is_dual ~ totcwinsT + female + pr_m, 
  family = binomial(link = "logit")
)
data.h3.ldp.2005 <- data.frame(
  totcwinsT = seq(
    min(data.ldp.2005$totcwinsT), 
    max(data.ldp.2005$totcwinsT), 
    1
  ), 
  female = 0, 
  pr_m = median(data.pr$pr_m)
) %>%
  cbind(
    predict(
      fit.h3.ldp.2005, 
      newdata = ., 
      type = "link", 
      se.fit = TRUE
    )
  ) %>% 
  within(
    data = ., 
    {
      rank = exp(fit)
      lwr = exp(fit - 1.96 * se.fit)
      upr = exp(fit + 1.96 * se.fit)
    }
  )

data.h3.ldp.2005 %>% 
  ggplot(aes(
    x = totcwinsT, 
    y = rank
  )) +
  geom_ribbon(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    alpha = 0.3
  ) +
  geom_line() +
  labs(
    title = NULL,
    x = "Total Wins",
    y = "Predicted Rank"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 20), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )

data.h4.ldp.2005 <- data.frame(
  totcwinsT = seq(
    min(data.ldp.2005$totcwinsT), 
    max(data.ldp.2005$totcwinsT),
    1
  ), 
  female = 0,
  pr_m = median(data.pr$pr_m)
) %>% 
  cbind(
    predict(
      fit.h4.ldp.2005, 
      newdata = ., 
      type = "response", 
      se.fit = TRUE
    )
  ) %>% 
  within(
    data = ., 
    {
      is_dual = fit
      lwr = fit - 1.96 * se.fit
      upr = fit + 1.96 * se.fit
    }
  )

data.h4.ldp.2005 %>%
  ggplot(aes(
    x = totcwinsT, 
    y = is_dual
  )) +
  geom_ribbon(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    alpha = 0.3
  ) +
  geom_line() +
  labs(
    title = NULL,
    x = "Total Wins",
    y = "Predicted Probability of Dual Candidacy"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 20), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )

# 2012 LDP
fit.h3.ldp.2012 <- glm.nb(
  data = data.ldp.2012, 
  pr_rank ~ totcwinsT + female + pr_m
)
fit.h4.ldp.2012 <- glm(
  data = data.ldp.2012, 
  is_dual ~ totcwinsT + female + pr_m, 
  family = binomial(link = "logit")
)
data.h3.ldp.2012 <- data.frame(
  totcwinsT = seq(
    min(data.ldp.2012$totcwinsT), 
    max(data.ldp.2012$totcwinsT), 
    1
  ),
  female = 0, 
  pr_m = median(data.pr$pr_m)
) %>%
  cbind(
    predict(
      fit.h3.ldp.2012, 
      newdata = ., 
      type = "link", 
      se.fit = TRUE
    )
  ) %>% 
  within(
    data = ., 
    {
      rank = exp(fit)
      lwr = exp(fit - 1.96 * se.fit)
      upr = exp(fit + 1.96 * se.fit)
    }
  )
data.h3.ldp.2012 %>% 
  ggplot(aes(
    x = totcwinsT, 
    y = rank
  )) +
  geom_ribbon(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    alpha = 0.3
  ) +
  geom_line() +
  labs(
    title = NULL,
    x = "Total Wins",
    y = "Predicted Rank"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 20), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )

data.h4.ldp.2012 <- data.frame(
  totcwinsT = seq(
    min(data.ldp.2012$totcwinsT), 
    max(data.ldp.2012$totcwinsT),
    1
  ),
  female = 0,
  pr_m = median(data.pr$pr_m)
) %>% 
  cbind(
    predict(
      fit.h4.ldp.2012, 
      newdata = ., 
      type = "response", 
      se.fit = TRUE
    )
  ) %>% 
  within(
    data = ., 
    {
      is_dual = fit
      lwr = fit - 1.96 * se.fit
      upr = fit + 1.96 * se.fit
    }
  )

data.h4.ldp.2012 %>%
  ggplot(aes(
    x = totcwinsT, 
    y = is_dual
  )) +
  geom_ribbon(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    alpha = 0.3
  ) +
  geom_line() +
  labs(
    title = NULL,
    x = "Total Wins",
    y = "Predicted Probability of Dual Candidacy"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 20), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )

# reg table for election-/party-specific analysis
texreg(
  list(
    fit.h3.ldp.2005, 
    fit.h4.ldp.2005, 
    fit.h3.ldp.2012, 
    fit.h4.ldp.2012
  ),
  custom.header = list(
    "2005 LDP" = 1:2, 
    "2012 LDP" = 3:4
  ), 
  custom.model.names = c("H3", "H4", "H3", "H4"),
  custom.coef.map = list(
    "totcwinsT" = "Total Wins", 
    "incBinary1" = "Incumbency", 
    "is_dual1" = "Dual Listing", 
    "female"  = "Female", 
    "pr_m" = "Block Magnitude"
  ), 
  custom.note = paste0(
    "\\item %stars. Standard errors in parentheses.\n", 
    "\\item Dependent variable: candidate $i$'s list rank (H3) ", 
    "and dual nomination status (H4).\n", 
    "\\item Estimated models: negative binomial (H3) and logit (H4)."
  ), 
  caption = "Regression Results for LDP Candidates in 2005 and 2012",
  booktabs = TRUE, 
  dcolumn = TRUE, 
  threeparttable = TRUE, 
  use.packages = FALSE, 
  float.pos = "!bth", 
  label = "tab:regLDP2005_2012",
  file = "./table/regression_results_ldp_2005_2012.tex"
)

# analysis with rank safety index


