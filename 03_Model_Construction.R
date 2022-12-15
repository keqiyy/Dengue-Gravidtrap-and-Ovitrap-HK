##title: '03_Model_Construction'
##author: 'Keqi YUE'
##date: '2022-12-06'

#############################################
## load packages and data
source("D:/Downloads/Dengue/01_load_packages_data.R")
setwd("D:/Downloads/Dengue")
###################################################
## Model1: To estimate the area ovitrap/gravidtrap index
#define formulas with random effects (T1, T2, S) and different combinations of mean temperature (basis_mt) and total rainfall (basis_tr)
formula0 <- Y1 ~ 1 + f(T1, model = "iid")
formula0.1 <- Y1 ~ 1 + f(T2, model = "iid")
formula0.2 <- Y1 ~ 1 + f(S, model = "iid")
formula0.3 <- Y1 ~ 1 + basis_mt
formula0.4 <- Y1 ~ 1 + basis_tr
formula1.1 <- Y1 ~ 1 + f(T2, model = "iid") + basis_tr
formula1.2 <- Y1 ~ 1 + f(T2, model = "iid") + basis_mt
formula1.3 <- Y1 ~ 1 + basis_mt + basis_tr
formula1.4 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid")
formula1.5 <- Y1 ~ 1 + f(T1, model = "iid") + basis_tr
formula1.6 <- Y1 ~ 1 + f(T1, model = "iid") + basis_mt
formula1.7 <- Y1 ~ 1 + f(T1, model = "iid") + f(S, model = "iid")
formula1.8 <- Y1 ~ 1 + f(T2, model = "iid") + f(S, model = "iid")
formula2.1 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr
formula2.2 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_mt
formula2.3 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + f(S, model = "iid")
formula2.4 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr + basis_mt
formula3 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr + basis_mt + f(S, model = "iid")

# create a list of formulas
formulas <- list(formula0, formula0.1, formula0.2, formula0.3, formula0.4, formula1.1, formula1.2, formula1.3, formula1.4, 
                 formula1.5, formula1.6, formula1.7, formula1.8, formula2.1, formula2.2, formula2.3, formula2.4, formula3)
# create model label string
lab <- c("model0","model0.1", "model0.2", "model0.3","model0.4", "model1.1", "model1.2", "model1.3", "model1.4",
         "model1.5", "model1.6", "model1.7", "model1.8", "model2.1", "model2.2", "model2.3", "model2.4", "model3")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], data = df2, family = "binomial")
                   save(model, file = paste0("Model/AOI/", lab[i],".RData"))})

table_aoi <- data.table(Model  = c("T1","T2","S","mt", "tr", "T2+tr", "T2+mt", "mt+tr", "T1+T2","T1+tr", "T1+mt", "T1+S", "T2+S",
                                  "T2+T1+tr","T2+T1+mt","T2+T1+S", "T1+T2+mt+tr", "T2+T1+tr+mt+S"), 
                       DIC = NA,
                       logscore = NA,
                       WAIC = NA)

for(i in 1:length(formulas)){
  load(paste0("Model/AOI/",lab[i],".RData"))
  table_aoi$DIC[i] <- round(model$dic$dic, 4)
  table_aoi$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_aoi$WAIC[i] <- round(model$waic$waic, 4)
}

# View table
table_aoi

# Write results of model selection
fwrite(table_aoi, file = "Model/AOI/AOI model selection.csv", quote = FALSE, 
       row.names = FALSE)

###################################################
## Model2: To estimate the number of mosquitoes per 1000 traps
formula0 <- Y2 ~ 1 + f(T1, model = "iid")
formula0.1 <- Y2 ~ 1 + f(T2, model = "iid")
formula0.2 <- Y2 ~ 1 + f(S, model = "iid")
formula0.3 <- Y2 ~ 1 + basis_mt1
formula0.4 <- Y2 ~ 1 + basis_tr1

formula1.1 <- Y2 ~ 1 + f(T2, model = "iid") + basis_tr1
formula1.2 <- Y2 ~ 1 + f(T2, model = "iid") + basis_mt1
formula1.3 <- Y2 ~ 1 + basis_mt1 + basis_tr1
formula1.4 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid")
formula1.5 <- Y2 ~ 1 + f(T1, model = "iid") + basis_tr1
formula1.6 <- Y2 ~ 1 + f(T1, model = "iid") + basis_mt1
formula1.7 <- Y2 ~ 1 + f(T1, model = "iid") + f(S, model = "iid")
formula1.8 <- Y2 ~ 1 + f(T2, model = "iid") + f(S, model = "iid")

formula2.1 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1
formula2.2 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_mt1
formula2.3 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + f(S, model = "iid")
formula2.4 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1
formula3 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid")

# create a list of formulas
formulas <- list(formula0, formula0.1, formula0.2, formula0.3, formula0.4, formula1.1, formula1.2, formula1.3, formula1.4, 
                 formula1.5, formula1.6, formula1.7, formula1.8, formula2.1, formula2.2, formula2.3, formula2.4, formula3)
# create model label string
lab <- c("model0","model0.1", "model0.2", "model0.3","model0.4", "model1.1", "model1.2", "model1.3", "model1.4",
         "model1.5", "model1.6", "model1.7", "model1.8", "model2.1", "model2.2", "model2.3", "model2.4", "model3")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
mymodel <- function(formula, data = df3, family = "nbinomial", config = FALSE){
  model <- inla(formula = formula, data = data, family = family,
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

models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], data = df3, family = "nbinomial")
                   save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/", lab[i],".RData"))})

table_nb <- data.table(Model  = c("T1","T2","S","mt", "tr", "T2+tr", "T2+mt", "mt+tr", "T1+T2","T1+tr", "T1+mt", "T1+S", "T2+S",
                                  "T2+T1+tr","T2+T1+mt","T2+T1+S", "T2+mt+tr", "T2+T1+tr+mt+S"), 
                       DIC = NA,
                       logscore = NA,
                       WAIC = NA)

table_po <- table_nb

for(i in 1:length(formulas)){
  load(paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/",lab[i],".RData"))
  table_nb$DIC[i] <- round(model$dic$dic, 4)
  table_nb$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_nb$WAIC[i] <- round(model$waic$waic, 4)
}

models <- lapply(1:length(formulas),
                 function(i) {
                   model <- mymodel(formulas[[i]], data = df3, family = "Poisson")
                   save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/poisson/", lab[i],".RData"))})

for(i in 1:length(formulas)){
  load(paste0("Model/Number of mosquitoes per 1000 traps/poisson/",lab[i],".RData"))
  table_po$DIC[i] <- round(model$dic$dic, 4)
  table_po$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_po$WAIC[i] <- round(model$waic$waic, 4)
}

# View table
table_po
table_nb

# Write results of model selection
fwrite(table_po, file = "Model/Number of mosquitoes per 1000 traps/nummosquitoes_model_selection(poisson).csv", quote = FALSE, 
       row.names = FALSE)
fwrite(table_nb, file = "model/Number of mosquitoes per 1000 traps/nummosquitoes_model_selection(nbinomial).csv", quote = FALSE, 
       row.names = FALSE)

#############################################################
## Model3: To estimate AOI, adding number of mosquitoes per 1000 traps as a predictor

formula0 <- Y1 ~ 1 + f(T1, model = "iid")
formula0.1 <- Y1 ~ 1 + f(T2, model = "iid")
formula0.2 <- Y1 ~ 1 + f(S, model = "iid")
formula0.3 <- Y1 ~ 1 + basis_mt1
formula0.4 <- Y1 ~ 1 + basis_tr1
formula1.1 <- Y1 ~ 1 + f(T2, model = "iid") + basis_tr1
formula1.2 <- Y1 ~ 1 + f(T2, model = "iid") + basis_mt1
formula1.3 <- Y1 ~ 1 + basis_mt1 + basis_tr1
formula1.4 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid")
formula1.5 <- Y1 ~ 1 + f(T1, model = "iid") + basis_tr1
formula1.6 <- Y1 ~ 1 + f(T1, model = "iid") + basis_mt1
formula1.7 <- Y1 ~ 1 + f(T1, model = "iid") + f(S, model = "iid")
formula1.8 <- Y1 ~ 1 + f(T2, model = "iid") + f(S, model = "iid")
formula2.1 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1
formula2.2 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_mt1
formula2.3 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + f(S, model = "iid")
formula2.4 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1
formula3 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid")
formula4 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + Y2

# create a list of formulas
formulas <- list(formula0, formula0.1, formula0.2, formula0.3, formula0.4, formula1.1, formula1.2, formula1.3, formula1.4, 
                 formula1.5, formula1.6, formula1.7, formula1.8, formula2.1, formula2.2, formula2.3, formula2.4, formula3, formula4)
# create model label string
lab <- c("model0","model0.1", "model0.2", "model0.3","model0.4", "model1.1", "model1.2", "model1.3", "model1.4",
         "model1.5", "model1.6", "model1.7", "model1.8", "model2.1", "model2.2", "model2.3", "model2.4", "model3", "model4")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
mymodel <- function(formula, data = df3, family = "binomial", config = FALSE){
  model <- inla(formula = formula, data = data, family = family, Ntrials = df1$Total.traps,
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

models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], data = df3, family = "binomial")
                   save(model, file = paste0("Model/AOI full model/", lab[i],".RData"))})

table_aoi1 <- data.table(Model  = c("T1","T2","S","mt", "tr", "T2+tr", "T2+mt", "mt+tr", "T1+T2","T1+tr", "T1+mt", "T1+S", "T2+S",
                                   "T2+T1+tr","T2+T1+mt","T2+T1+S", "T1+T2+mt+tr", "T2+T1+tr+mt+S", "T2+T1+tr+mt+S+Nummosq"), 
                        DIC = NA,
                        logscore = NA,
                        WAIC = NA)

for(i in 1:length(formulas)){
  load(paste0("Model/AOI full model/",lab[i],".RData"))
  table_aoi1$DIC[i] <- round(model$dic$dic, 4)
  table_aoi1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_aoi1$WAIC[i] <- round(model$waic$waic, 4)
}

# View table
table_aoi1

# Write results of model selection
fwrite(table_aoi1, file = "Model/AOI full model/AOI full model selection.csv", quote = FALSE, 
       row.names = FALSE)
