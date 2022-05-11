

library(dplyr)
library(tidyverse)
library(igraph)
library(coda)
library(R6)
library(nimble)
nimbleOptions(disallow_multivariate_argument_expressions = FALSE)


## Load and prepare data
# load('allometry_database.RData')

allometry_complete_database <- read.csv("data/allometry_complete_database.csv", sep = ",")
NFI_data = readRDS(file = "data/NFI_TNRS_check.rds")

# extracting species list from NFI data (191 species)
sampling <- NFI_data %>% 
  filter(continent == "E_U" & nplot >= 100 & ntree >= 1000 | continent == "N_A" & nplot >= 150 & ntree >= 3000)


my.constants <- list()
my.data <- list()

diameter.model <- nimbleCode({
  
  for (i in 1:n_species) {
  
  # prior
  a1 ~ 
  a2 ~
  sigma ~   
  
  # likelihood
})

initial.values <- function () list()

parameters.to.save <- c("")

n.iter <- 110000
n.burnin <- 10000
n.chains <- 3
n.thin <- 10
  
mcmc.output <- nimbleMCMC(code = ,
                          data = my.data,
                          constants = my.constants,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          thin = n.thin,
                          niter = n.iter,
                          nburnin = n.burnin,
                          nchains = n.chains)
