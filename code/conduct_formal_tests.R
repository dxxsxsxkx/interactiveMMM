# This scripts conduct tests for overdispersion and parallel regression assumption. 

# Load libraries
library("MASS")
library("pscl")
library("tidyverse")

# Load data and regression models
data.small <- read_csv("./ageNomination/data_analysis/data_small.csv")
fit.nb <- read_rds("./ageNomination/model/model_nb.rds")
fit.nb.ldp <- read_rds("./ageNomination/model/model_nb_ldp.rds")
fit.nb.nonldp <- read_rds("./ageNomination/model/model_nb_nonldp.rds")

# Overdispersion test
pscl::odTest(fit.nb)
pscl::odTest(fit.nb.ldp)  # not working
pscl::odTest(fit.nb.nonldp)  # not working

