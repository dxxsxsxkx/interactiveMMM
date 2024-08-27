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
library(gridExtra)
library(xtable)
library(vtable)
library(gtsummary)


# import functions
source("./code/read_data.R")  # ReadReedSmithData()
source("./code/modify_dataset.R")  # functions for filtering and adding new variables to data
source("./code/create_models.R")  # functions for creating models
source("./code/create_texregs.R")  # functions for creating texreg objects
source("./code/create_regression_tables.R")  # functions for creating regression tables
source("./code/draw_figures.R")  # functions for drawing figures
source("./code/draw_figures_interpretation.R")  # functions for drawing figures for interpretation
source("./code/calculate_PR_electability.R")


# read data
data <- ReadReedSmithData()


# filter data and add new variables. 
data.small <- data %>% 
  FilterData() %>%
  AddVariables() %>% 
  CalculatePastPRSeats() %>% 
  CategorizeRanks()

data.ldp <- data.small %>% 
  CreateLDPData()

data.non.ldp <- data.small %>% 
  CreateNonLDPData()


# fit regression models
# for hypothesis 1
fit.nb.uni <- FitUnivariateNB(data.small)
fit.nb <- FitNB(data.small)
fit.zeroinfl.uni <- FitUnivariateZeroInfl(data.small)
fit.zeroinfl <- FitZeroInfl(data.small)
texreg.univariate.zeroinfl <- CreateTexregUnivariateZeroinfl(fit.zeroinfl.uni)
texreg.zeroinfl <- CreateTexregZeroinfl(fit.zeroinfl)

# LDP candidates only
fit.nb.uni.ldp <- FitUnivariateNB(data.ldp)
fit.nb.ldp <- FitNBwoParty(data.ldp)

# non-LDP candidates only
fit.nb.nonldp.uni <- FitUnivariateNB(data.non.ldp)
fit.nb.nonldp <- FitNB(data.non.ldp)
fit.zeroinfl.uni.nonldp <- FitUnivariateZeroInfl(data.non.ldp)
fit.zeroinfl.nonldp <- FitZeroInfl(data.non.ldp)
texreg.univariate.zeroinfl.nonldp <- CreateTexregUnivariateZeroinfl(fit.zeroinfl.uni.nonldp)
texreg.zeroinfl.nonldp <- CreateTexregZeroinfl(fit.zeroinfl.nonldp)

# for hypothesis 2
fit.uni.ord <- FitUnivariateOrdLogit(data.small)
fit.multi.ord <- FitMultivariateOrdLogit(data.small)
fit.clm <- FitCLM(data.small)
fit.mlogit <- FitMultiLogit(data.small)
texreg.clm.med <- CreateTexregClmMed(fit.clm)
texreg.clm.tough <- CreateTexregClmtough(fit.clm)
texreg.mlogit.med <- CreateTexregMlogitMed(fit.mlogit)
texreg.mlogit.tough <- CreateTexregMlogitTough(fit.mlogit)

# LDP candidates only
fit.uni.ord.ldp <- FitUnivariateOrdLogit(data.ldp)
fit.multi.ord.ldp <- FitMultivariateOrdLogit(data.ldp)
fit.clm.ldp <- FitCLM(data.ldp)
fit.mlogit.ldp <- FitMultiLogit(data.ldp)
texreg.clm.med.ldp <- CreateTexregClmMed(fit.clm.ldp)
texreg.clm.tough.ldp <- CreateTexregClmtough(fit.clm.ldp)
texreg.mlogit.med.ldp <- CreateTexregMlogitMed(fit.mlogit.ldp)
texreg.mlogit.tough.ldp <- CreateTexregMlogitTough(fit.mlogit.ldp)

# non-LDP candidates only
fit.uni.ord.nonldp <- FitUnivariateOrdLogit(data.non.ldp)
fit.multi.ord.nonldp <- FitMultivariateOrdLogit(data.non.ldp)
fit.clm.nonldp <- FitCLM(data.non.ldp)
fit.mlogit.nonldp <- FitMultiLogit(data.non.ldp)
texreg.clm.med.nonldp <- CreateTexregClmMed(fit.clm.nonldp)
texreg.clm.tough.nonldp <- CreateTexregClmtough(fit.clm.nonldp)
texreg.mlogit.med.nonldp <- CreateTexregMlogitMed(fit.mlogit.nonldp)
texreg.mlogit.tough.nonldp <- CreateTexregMlogitTough(fit.mlogit.nonldp)

# create regression tables
CreateRegTableCount(fit.nb.uni, fit.nb, texreg.univariate.zeroinfl, texreg.zeroinfl)   # H1
CreateRegTableCountwoParty(fit.nb.uni.ldp, fit.nb.ldp)  # H1, LDP candidates only
CreateRegTableCountNonLDP(fit.nb.nonldp.uni, fit.nb.nonldp, texreg.univariate.zeroinfl.nonldp, texreg.zeroinfl.nonldp)  # H1, non-LDP candidates only

# hypothesis2
CreateRegTableChoice(fit.uni.ord, fit.multi.ord, texreg.clm.med, texreg.clm.tough, texreg.mlogit.med, texreg.mlogit.tough)
CreateRegTableChoicewoParty(fit.uni.ord.ldp, fit.multi.ord.ldp, texreg.clm.med.ldp, texreg.clm.tough.ldp, texreg.mlogit.med.ldp, texreg.mlogit.tough.ldp)
CreateRegTableChoiceNonLDP(fit.uni.ord.nonldp, fit.multi.ord.nonldp, texreg.clm.med.nonldp, texreg.clm.tough.nonldp, texreg.mlogit.med.nonldp, texreg.mlogit.tough.nonldp)

# draw figures
DrawListRanks(data.small)
DrawLowestRanksElected(data.small)
DrawNofPrSeats(data.small)

# draw interpretation figures
DrawFiguresInterpretationH1(fit.nb)
DrawFiguresInterpretationH2(fit.clm)

# save some objects for later use in other scripts. 
write_csv(data.small, "./data_analysis/data_small.csv")
write_rds(fit.nb, "./model/model_nb.rds")
write_rds(fit.nb.ldp, "./model/model_nb_ldp.rds")
write_rds(fit.nb.nonldp, "./model/model_nb_nonldp.rds")

