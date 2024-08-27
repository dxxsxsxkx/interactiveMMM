library(tidyverse)
library(ggdag)
library(dagitty)

# draw a DAG
dag <- dagify(
  exposure = "dual", 
  outcome = "nWin", 
  rank ~ age + nWin + dual + inc + seshu + female + M + party, 
  age ~ seshu, 
  nWin ~ age + female + party, 
  dual ~ nWin + party, 
  inc ~ female + party, 
  seshu ~ female
) %>% 
  tidy_dagitty()
ggdag(dag)

ggdag_adjustment_set(dag)
