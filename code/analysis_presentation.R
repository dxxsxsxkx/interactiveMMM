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

data.pr.only <- data.pr %>% 
  filter(
    is_dual == 0
  )

# Intro
# Compare age distributions of SMD and PR candidates
data %>% 
  group_by(is_smd) %>% 
  # how many observations are there for each age?
  count(age) %>%
  # what is the proportion for each age?
  mutate(
    prop = n / sum(n)
  ) %>% 
  ggplot( 
    aes(
      x = age, 
      y = prop,
      fill = factor(is_smd)
    )
  ) +
  geom_histogram(
    stat = "identity",
    position = "identity", 
    alpha = 0.7,
  ) +
  labs(
    title = "SMD(blue) vs. PR(red)",
    x = "Age",
    y = NULL
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none", 
    text = element_text(size = 30)
  )
ggsave("./figure/age_smd_vs_pr.pdf", width = 6, height = 8)

# t-test
t.test(
  data.smd %>% 
    pull(age),
  data.pr %>% 
    pull(age), 
  alternative = "greater"
)

data.smd %>% 
  bind_rows(data.pr.only) %>% 
  group_by(is_smd) %>% 
  count(age) %>% 
  mutate(
    prop = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = age, 
      y = prop,
      fill = factor(is_smd)
    )
  ) +
  geom_histogram(
    stat = "identity",
    position = "identity", 
    alpha = 0.7,
  ) +
  labs(
    title = "SMD vs. PR Only",
    x = "Age",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    text = element_text(size = 30)
  )
ggsave("./figure/age_smd_vs_pr_only.pdf", width = 6, height = 8)

# t-test
t.test(
  data.smd %>% 
    pull(age),
  data.pr.only %>% 
    pull(age), 
  alternative = "greater"
)

# Compare age distributions of SMD and PR winners
#   result == 1 vs. result == 2 or 3
data.winner <- data %>% 
  filter(
    result %in% c(1, 2, 3)
  ) %>% 
  mutate(
    win_in = ifelse(
      result == 1, 
      "SMD", 
      "PR"
    )
  )
data.winner %>% 
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
    text = element_text(size = 30), 
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1
    ), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    )
  )
ggsave("./figure/age_smd_vs_pr_winners.pdf", width = 8, height = 6)

# t-test
t.test(
  data.winner %>% 
    filter(
      win_in == "SMD"
    ) %>% 
    pull(age),
  data.winner %>% 
    filter(
      win_in == "PR"
    ) %>% 
    pull(age), 
  alternative = "greater"
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

data.dual %>% 
  ggplot() +
  geom_line(
    aes(
      x = year, 
      y = prop_dual, 
      color = party_en, 
      alpha = party_en
    ), 
    linewidth = 1
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Proportion"
  ) +
  scale_color_manual(
    values = c(
      "Total" = "black", 
      "LDP" = "blue", 
      "Komeito" = "green", 
      "DPJ + CDP" = "red", 
      "JCP" = "purple"
    )
  ) +
  scale_alpha_manual(
    values = c(
      "Total" = 1, 
      "LDP" = 0.5, 
      "Komeito" = 0.5, 
      "DPJ + CDP" = 0.5, 
      "JCP" = 0.5
    )
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
    text = element_text(size = 20), 
    axis.title.x = element_text(
      margin = margin(0, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    ), 
    # legend
    legend.title = element_blank(), 
    legend.position = "top"
  )
ggsave("./figure/dual_nomination.pdf", width = 8, height = 6)

# H1 - H3
fit.h1 <- glm.nb(
  data = data.pr, 
  pr_rank ~ is_dual + female + pr_m + legis + party_en
)
fit.h2 <- glm.nb(
  data = data.pr, 
  pr_rank ~ incBinary + female + pr_m + legis + party_en
)
fit.h3 <- glm.nb(
  data = data.pr, 
  pr_rank ~ totcwinsT + female + pr_m + legis + party_en
)

# incumbency and dual nomination status
fit.h4 <- glm(
  data = data.pr, 
  is_dual ~ incBinary + female + pr_m + legis + party_en,
  family = binomial(link = "logit")
)

# seniority and dual nomination status
fit.h5 <- glm(
  data = data.pr, 
  is_dual ~ totcwinsT + female + pr_m + legis + party_en, 
  family = binomial(link = "logit")
)

# results
# H1
data.h1 <- data.frame(
  is_dual = c(as.factor(0), as.factor(1)), 
  female = 0, 
  pr_m = median(data.pr$pr_m),
  legis = as.factor(46),
  party_en = as.factor("LDP")
) %>%
  cbind(
    predict(
      fit.h1, 
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
plot.h1 <- data.h1 %>%
  ggplot(aes(
    x = is_dual, 
    y = rank, 
  )) +
  geom_point(
    size = 5
  ) + 
  geom_errorbar(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    width = 0.1, 
    linewidth = 3
  ) +
  geom_line() +
  labs(
    title = NULL, 
    x = "Dual Listing",
    y = "Predicted Rank"
  ) +
  scale_x_discrete(
    labels = c("No", "Yes")
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 30), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )
ggsave("./figure/h1_dual_nomination.pdf", width = 8, height = 6)

# H2
data.h2 <- data.frame(
  incBinary = c(as.factor(0), as.factor(1)), 
  female = 0, 
  pr_m = median(data.pr$pr_m),
  legis = as.factor(46),
  party_en = as.factor("LDP")
) %>% 
  cbind(
    predict(
      fit.h2, 
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
plot.h2 <- data.h2 %>%
  ggplot(aes(
    x = incBinary, 
    y = rank
  )) +
  geom_point(
    size = 5
  ) + 
  geom_errorbar(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    width = 0.1, 
    size = 3
  ) +
  labs(
    title = NULL,
    x = "Incumbency",
    y = NULL
  ) +
  scale_x_discrete(
    labels = c("No", "Yes")
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 30), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )
ggsave("./figure/h2_incumbency.pdf", width = 8, height = 6)


# H3
data.h3 <- data.frame(
  totcwinsT = seq(
    min(data.pr$totcwinsT), 
    max(data.pr$totcwinsT), 
    1
  ), 
  female = 0, 
  pr_m = median(data.pr$pr_m),
  legis = as.factor(46),
  party_en = as.factor("LDP")
) %>%
  cbind(
    predict(
      fit.h3, 
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
plot.h3 <- data.h3 %>%
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
  geom_line(
    size = 1
  ) +
  labs(
    title = NULL,
    x = "Total Wins",
    y = NULL
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 30), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )
ggsave("./figure/h3_total_wins.pdf", width = 8, height = 6)

cowplot::plot_grid(
  plotlist = list(
    plot.h3, 
    plot.h2, 
    plot.h1
  ), 
  nrow = 1
)
ggsave("./figure/slide/h3_h4_h5.pdf", width = 8, height = 6)

# H4
data.h4 <- data.frame(
  incBinary = c(as.factor(0), as.factor(1)), 
  female = 0, 
  pr_m = median(data.pr$pr_m),
  legis = as.factor(46),
  party_en = as.factor("LDP")
) %>% 
  cbind(
    predict(
      fit.h4, 
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
plot.h4 <- data.h4 %>%
  ggplot(aes(
    x = incBinary, 
    y = is_dual
  )) +
  geom_point(
    size = 5
  ) + 
  geom_errorbar(
    aes(
      ymin = lwr, 
      ymax = upr
    ), 
    width = 0.1, 
    size = 2
  ) +
  labs(
    title = NULL,
    x = "Incumbency",
    y = "Dual Listing"
  ) +
  scale_x_discrete(
    labels = c("No", "Yes")
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 30), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )
ggsave("./figure/h4_incumbency.pdf", width = 8, height = 6)

data.h5 <- data.frame(
  totcwinsT = seq(
    min(data.pr$totcwinsT), 
    max(data.pr$totcwinsT),
    1
  ), 
  female = 0,
  pr_m = median(data.pr$pr_m),
  legis = as.factor(46),
  party_en = as.factor("LDP")
) %>% 
  cbind(
    predict(
      fit.h5, 
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
plot.h5 <- data.h5 %>%
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
  geom_line(
    size = 1
  ) +
  labs(
    title = NULL,
    x = "Total Wins",
    y = NULL
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 30), 
    axis.title.x = element_text(
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(
      margin = margin(0, 10, 0, 0)
    )
  )
ggsave("./figure/h5_total_wins.pdf", width = 8, height = 6)

cowplot::plot_grid(
  plotlist = list(
    plot.h5, 
    plot.h4
  ), 
  nrow = 1
)
ggsave("./figure/slide/h1_h2.pdf", width = 8, height = 6)

# Discussion
data.first.run <- data %>% 
  filter(
    totcruns == 1, 
    year >= 1994, 
    byelection != 1
  ) %>% 
  group_by(year) %>% 
  summarize(
    n = n(), 
    age_mean = mean(age)
  ) %>%
  left_join(
    data %>% 
      group_by(year) %>% 
      summarize(
        n_total = n(), 
        age_mean_total = mean(age)
      )
  ) %>%
  mutate(
    prop_novice = n / n_total
  )
data.pr.first.run <- data.pr %>% 
  filter(
    totcruns == 1, 
    year >= 1994
  ) %>% 
  group_by(year) %>% 
  summarize(
    n = n(), 
    age_mean = mean(age)
  ) %>% 
  left_join(
    data.pr %>% 
      group_by(year) %>% 
      summarize(
        n_total = n(), 
        age_mean_total = mean(age)
      )
  ) %>% 
  mutate(
    prop_novice = n / n_total
  )
