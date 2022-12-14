##title: "01_load_packages_data"
##author: "Keqi YUE"
##date: '2022-12-08'

## R script to prepare data and lagged variables for INLA-DLNM modelling
# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

# load INLA
library(INLA)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

## load data
## Climate data and mosquitoes index data included for 2010 to 2022.08
setwd("D:/Downloads/Dengue")
df <- read.csv("Raw Data/Three districts all dataset-1.csv")
df$Date <- as.Date(df$Date)
df$year <- as.factor(format(as.Date(df$Date), "%Y"))
df$month <- as.factor(format(as.Date(df$Date), "%m"))
df$District <- as.factor(df$District)
str(df)

## Create lagged variables
## Data from 2010 to 2022.08
# define matrices of lagged terms for monthly mean climate variables
# Mean Temperature
nlag <- 2
lag_temp <- tsModel::Lag(df$Meantemp, k = 0:nlag)
lagknot = equalknots(0:nlag, 2)
var <- lag_temp
basis_mt <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(df$Meantemp, 2)),
                       arglag = list(fun = "ns", knots = lagknot))

# Total Rainfall
nlag <- 6
lag_train <- tsModel::Lag(df$Totalrain, k = 0:nlag)
lagknot = equalknots(0:nlag, 2)
var <- lag_train
basis_tr <- crossbasis(var,
                       argvar = list(fun = "ns", knots = equalknots(df$Totalrain, 2)),
                       arglag = list(fun = "ns", knots = lagknot))

# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_mt) = paste0("basis_mt.", colnames(basis_mt))
colnames(basis_tr) = paste0("basis_tr.", colnames(basis_tr))

## Create lagged variables
## Data from 2020.04 to 2022.08
df1 <- df[df$Date > "2020-03-01",]
# Mean Temperature
nlag <- 2
lag_temp <- tsModel::Lag(df1$Meantemp, k = 0:nlag)
lagknot = equalknots(0:nlag, 2)
var <- lag_temp
basis_mt1 <- crossbasis(var,
                        argvar = list(fun = "ns", knots = equalknots(df1$Meantemp, 2)),
                        arglag = list(fun = "ns", knots = lagknot))

# Total Rainfall
nlag <- 6
lag_train <- tsModel::Lag(df1$Totalrain, k = 0:nlag)
lagknot = equalknots(0:nlag, 2)
var <- lag_train
basis_tr1 <- crossbasis(var,
                        argvar = list(fun = "ns", knots = equalknots(df1$Totalrain, 2)),
                        arglag = list(fun = "ns", knots = lagknot))

# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_mt1) = paste0("basis_mt1.", colnames(basis_mt1))
colnames(basis_tr1) = paste0("basis_tr1.", colnames(basis_tr1))

# set up data and priors for INLA model
# set data for models
Y1  <- df$Number.of.positive.traps # model 1 response variable
Y2 <- df$Nummosquito.per.1000.traps # model 2 response variable and variable for model 1
# random variable
T1 <- df$month # for random effect to account for inter-annual variability 
T2 <- df$year # for random effect to account for annual cycle (seasonality)
S <- df$District # for district difference
Date <- df$Date

trap <- df$Total.traps

# create dataframe for model testing
df2 <- data.frame(Y1, Y2, T1, T2, S, Date)

# define priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

# inla model function
# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
mymodel <- function(formula, data = df2, family = "binomial", config = FALSE){
  model <- inla(formula = formula, data = data, family = family, Ntrials = df$Total.traps,
                control.inla = list(strategy = 'adaptive'), 
                control.compute = list(dic = TRUE, config = config, 
                                       cpo = TRUE, return.marginals = FALSE, waic = TRUE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = FALSE)
  model <- inla.rerun(model)
  return(model)
}

## data for model 3
df3 <- df2[df2$Date > "2020-03-01",]
