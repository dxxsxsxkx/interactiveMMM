#' Hypothesis 1
DrawFiguresInterpretationH1 <- function(model){
  data.hyp.cand <- data.frame(
    age = c(35, 45, 55, 65), 
    female = c(0, 0, 0, 0), 
    totcwinsT = c(2, 2, 2, 2), 
    party_en = c("LDP", "LDP", "LDP", "LDP"), 
    incBinary = c(1, 1, 1, 1), 
    is_dual = c(1, 1, 1, 1), 
    celeb = c(0, 0, 0, 0),
    legis = c(46, 46, 46, 46), 
    pr_m = c(6, 6, 6, 6)
  )
  data.hyp.cand$mu <- predict(model, newdata = data.hyp.cand, type = "response")
  
  df <- data.frame(
    x = 1:11,
    y1 = dnbinom(0:10, size = model$theta, mu = data.hyp.cand$mu[1]),
    y2 = dnbinom(0:10, size = model$theta, mu = data.hyp.cand$mu[2]),
    y3 = dnbinom(0:10, size = model$theta, mu = data.hyp.cand$mu[3]),
    y4 = dnbinom(0:10, size = model$theta, mu = data.hyp.cand$mu[4])
  )
  
  # Create the plot using ggplot
  ggplot(data = df, aes(x = x)) +
    geom_line(aes(y = y1, color = "35"), linetype = "solid") +
    geom_line(aes(y = y2, color = "45"), linetype = "solid") +
    geom_line(aes(y = y3, color = "55"), linetype = "solid") +
    geom_line(aes(y = y4, color = "65"), linetype = "solid") +
    labs(
      x = "Predicted rank", 
      y = "Probability", 
      color = "Age"
    ) +
    scale_x_continuous(
      breaks = seq(0, 10, 1)
    ) +
    scale_y_continuous(
      limits = c(0, max(df[, 2:5])), expand = c(0, 0)
    ) + 
    scale_color_manual(
      values = c("black", "red", "blue", "green"), 
      labels = c("35", "45", "55", "65")
    ) + 
    theme_minimal() + 
    theme(
      legend.position = c(0.9, 0.9),  # Adjust the position of the legend
      legend.justification = c(1, 1),  # Adjust the justification of the legend
      legend.background = element_rect(fill = "white", color = "black")
    )
  ggsave("./figure/pred.rank.pdf", width = 8, height = 6)
}

#' Hypothesis 2
DrawFiguresInterpretationH2 <- function(model){
  data.hyp.cand.h2 <- data.frame(
    name.cand = c(rep("A", 36), rep("B", 36), rep("C", 36)), 
    age = rep(c(25:60), 3), 
    female = c(rep(0, 36), rep(1, 36), rep(0, 36)), 
    totcwinsT = c(rep(0, 36), rep(1, 36), rep(3, 36)), 
    is_dual = c(rep(0, 36), rep(0, 36), rep(0, 36)),
    celeb = c(rep(0, 36), rep(0, 36), rep(0, 36)),
    incBinary = c(rep(0, 36), rep(1, 36), rep(1, 36)), 
    legis = rep(46, 36*3) 
    # pr_electability_lab = rep(c("fair", "medium", "tough"), 3)
  )
  
  prd.hyp.cand.h2 <- cbind(
    data.hyp.cand.h2, 
    predict(object = model, newdata = data.hyp.cand.h2)
  )
  
  prd.hyp.cand.h2 %>% 
    dplyr::filter(name.cand == "A") %>% 
    pivot_longer(cols = c(fit.fair, fit.medium, fit.tough)) %>% 
    ggplot(aes(x = age, y = value, color = name)) + 
    geom_line() + 
    labs(
      title = "Male non-incumbent with no prior win, 2012"
    ) + 
    scale_color_discrete(
      labels = c(
        "fit.fair" = "Fair", 
        "fit.medium" = "Medium", 
        "fit.tough" = "Tough"
      )
    ) + 
    theme_minimal() + 
    theme(
      legend.position = "none"
    )
  ggsave("figure/pred.cat.1.pdf", width = 8, height = 6)
  prd.hyp.cand.h2 %>% 
    dplyr::filter(name.cand == "B") %>% 
    pivot_longer(cols = c(fit.fair, fit.medium, fit.tough)) %>% 
    ggplot(aes(x = age, y = value, color = name)) + 
    geom_line() + 
    labs(
      title = "Female incumbent elected once before, 2012"
    ) + 
    scale_color_discrete(
      labels = c(
        "fit.fair" = "Fair", 
        "fit.medium" = "Medium", 
        "fit.tough" = "Tough"
      )
    ) + 
    theme_minimal() + 
    theme(
      legend.position = "none"
    )
  ggsave("./figure/pred.cat.2.pdf", width = 8, height = 6)
  prd.hyp.cand.h2 %>% 
    dplyr::filter(name.cand == "C") %>% 
    pivot_longer(cols = c(fit.fair, fit.medium, fit.tough)) %>% 
    ggplot(aes(x = age, y = value, color = name)) + 
    geom_line() + 
    labs(
      title = "Male incumbent elected three times before, 2012"
    ) + 
    scale_color_discrete(
      labels = c(
        "fit.fair" = "Fair", 
        "fit.medium" = "Medium", 
        "fit.tough" = "Tough"
      )
    ) + 
    theme_minimal() + 
    theme(
      legend.position = c(0.9, 1),  # Adjust the position of the legend
      legend.justification = c(1, 1),  # Adjust the justification of the legend
      legend.background = element_rect(fill = "white", color = "black")  # Customize the legend background
    )
  ggsave("./figure/pred.cat.3.pdf", width = 8, height = 6)
}
