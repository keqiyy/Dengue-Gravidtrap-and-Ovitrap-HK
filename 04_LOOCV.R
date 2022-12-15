##title: '04_LOOCV'
##author: 'Keqi YUE'
##date: '2022-12-13'

## Note: We will validate the three models by leave-one-month-out corss validation, and plot the observed AOI, model fitted values, and predictions.
source("01_load_packages_data.R")
######################################
## LOOCV for Model 1
## input the best fitting model
setwd("D:/Downloads/Dengue")
load("Model/AOI/model3.RData")
model0 <- model
df0 <- df

formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr + basis_mt + f(District, model = "iid")

n <- dim(df)[1] # number of samples
loo_pred <- rep(NA, n)
loo_predmin <- rep(NA, n)
loo_predmax <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0

## function
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

## loocv
for(i in 1:n) {
  if(i%%3 == 1){
    print(i)
    nummosquito_i <- replace(df0$Number.of.positive.traps, i, "NA")
    nummosquito_i <- replace(nummosquito_i, i+1, "NA")
    nummosquito_i <- replace(nummosquito_i, i+2, "NA")
    df$Number.of.positive.traps <- as.numeric(nummosquito_i)
    dlnm_i <- mymodel(formula, data = df, family = "binomial") #glm with out ith row
    loo_pred[i] <- dlnm_i$summary.fitted.values$mean[i]
    loo_pred[i+1] <- dlnm_i$summary.fitted.values$mean[i+1]
    loo_pred[i+2] <- dlnm_i$summary.fitted.values$mean[i+2]
    loo_predmin[i] <- dlnm_i$summary.fitted.values$`0.025quant`[i]
    loo_predmin[i+1] <- dlnm_i$summary.fitted.values$`0.025quant`[i+1]
    loo_predmin[i+2] <- dlnm_i$summary.fitted.values$`0.025quant`[i+2]
    loo_predmax[i] <- dlnm_i$summary.fitted.values$`0.975quant`[i]
    loo_predmax[i+1] <- dlnm_i$summary.fitted.values$`0.975quant`[i+1]
    loo_predmax[i+2] <- dlnm_i$summary.fitted.values$`0.975quant`[i+2]
  }
}

MSE_Va <- mean((df0$meanAOI - loo_pred) ^ 2)
MSE_Tr <- mean((df0$meanAOI - model$summary.fitted.values$mean) ^ 2)
MSE_Va
MSE_Tr


## plot
df0$fittednum <- model0$summary.fitted.values$mean
df0$predictednum <- loo_pred
df0$predictedmin <- loo_predmin
df0$predictedmax <- loo_predmax

colors <- c("Observed AOI" = "blue", "Model fitted value" = "red", "Cross-validation prediction" = "green3")

ggplot(df0)+
  geom_line(aes(x=Date, y=meanAOI, color = "Observed AOI"), size = 1.2, alpha = 0.6)+
  geom_line(aes(x=Date, y=fittednum, color = "Model fitted value"), size = 1.2, alpha = 0.6) +
  geom_line(aes(x=Date, y=predictednum, color = "Cross-validation prediction"), size = 1.2, alpha = 0.6)+
  geom_ribbon(aes(x=Date, ymin=predictedmin, ymax=predictedmax), alpha=0.2)+
  labs(x = "Date",
       y = "Area ovitrap/gravidtrap index",
       color = "Legend")+
  facet_wrap( ~ District, ncol = 1)+
  theme(legend.position = c(0.85,1), 
        legend.title= element_blank(),
        axis.title = element_text(size=30), 
        axis.text.x = element_text(size=25, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 30),
        legend.text = element_text(size=19),
        strip.text = element_text(size=25),
        panel.grid.major.x = element_line(size = 1))+
  scale_color_manual(values = colors)+
  scale_x_date(date_breaks = "1 year", labels = label_date(format = "%Y/%m"), expand = c(0,0))

######################################
## LOOCV for Model 2
## input the best fitting model
load("Model/Number of mosquitoes per 1000 traps/nbinomial/model3.RData")
model1 <- model
df <- df0
str(df1)
df0 <- df1

formula <- Nummosquito.per.1000.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid")

n <- dim(df1)[1] # number of samples
loo_pred <- rep(NA, n)
loo_predmin <- rep(NA, n)
loo_predmax <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0

## function
mymodel <- function(formula, data = df2, family = "nbinomial", config = FALSE){
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

## loocv
for(i in 1:n) {
  if(i%%3 == 1){
    print(i)
    nummosquito_i <- replace(df0$Nummosquito.per.1000.traps, i, "NA")
    nummosquito_i <- replace(nummosquito_i, i+1, "NA")
    nummosquito_i <- replace(nummosquito_i, i+2, "NA")
    df1$Nummosquito.per.1000.traps <- as.numeric(nummosquito_i)
    dlnm_i <- mymodel(formula, data = df1, family = "nbinomial") #glm with out ith row
    loo_pred[i] <- round(dlnm_i$summary.fitted.values$mean[i])
    loo_pred[i+1] <- round(dlnm_i$summary.fitted.values$mean[i+1])
    loo_pred[i+2] <- round(dlnm_i$summary.fitted.values$mean[i+2])
    loo_predmin[i] <- round(dlnm_i$summary.fitted.values$`0.025quant`[i])
    loo_predmin[i+1] <- round(dlnm_i$summary.fitted.values$`0.025quant`[i+1])
    loo_predmin[i+2] <- round(dlnm_i$summary.fitted.values$`0.025quant`[i+2])
    loo_predmax[i] <- round(dlnm_i$summary.fitted.values$`0.975quant`[i])
    loo_predmax[i+1] <- round(dlnm_i$summary.fitted.values$`0.975quant`[i+1])
    loo_predmax[i+2] <- round(dlnm_i$summary.fitted.values$`0.975quant`[i+2])
  }
}

MSE_Va <- mean((df0$Nummosquito.per.1000.traps - loo_pred) ^ 2)
MSE_Tr <- mean((df0$Nummosquito.per.1000.traps - round(model1$summary.fitted.values$mean)) ^ 2)
MSE_Va
MSE_Tr

## plot
df0$fittednum <- round(model1$summary.fitted.values$mean)
df0$predictednum <- loo_pred
df0$predictedmin <- loo_predmin
df0$predictedmax <- loo_predmax

colors <- c("Observed number of mosquitoes per 1000 traps" = "blue", "Model fitted value" = "red", "Cross-validation prediction" = "green3")

ggplot(df0)+
  geom_line(aes(x=Date, y=Nummosquito.per.1000.traps, color = "Observed number of mosquitoes per 1000 traps"), size = 1.2, alpha = 0.6)+
  geom_line(aes(x=Date, y=fittednum, color = "Model fitted value"), size = 1.2, alpha = 0.6) +
  geom_line(aes(x=Date, y=predictednum, color = "Cross-validation prediction"), size = 1.2, alpha = 0.6)+
  geom_ribbon(aes(x=Date, ymin=predictedmin, ymax=predictedmax), alpha=0.2)+
  labs(x = "Date",
       y = "Number of mosquitoes per 1000 traps",
       color = "Legend")+
  facet_wrap( ~ District, ncol = 1)+
  theme(legend.position = c(0.73,0.95), 
        legend.title= element_blank(),
        axis.title = element_text(size=30), 
        axis.text.x = element_text(size=25, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 30),
        legend.text = element_text(size=19),
        strip.text = element_text(size=25),
        panel.grid.major.x = element_line(size = 1))+
  scale_color_manual(values = colors)+
  scale_x_date(date_breaks = "6 month", labels = label_date(format = "%Y/%m"), expand = c(0,0))

######################################
## LOOCV for Model 3
## input the best fitting model
load("Model/AOI full model/model4.RData")
model2 <- model
str(df0)
df1 <- df0

formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + Nummosquito.per.1000.traps 

n <- dim(df1)[1] # number of samples
loo_pred <- rep(NA, n)
loo_predmin <- rep(NA, n)
loo_predmax <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0

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

## loocv
for(i in 1:n) {
  if(i%%3 == 1){
    print(i)
    nummosquito_i <- replace(df0$Number.of.positive.traps, i, "NA")
    nummosquito_i <- replace(nummosquito_i, i+1, "NA")
    nummosquito_i <- replace(nummosquito_i, i+2, "NA")
    df1$Number.of.positive.traps <- as.numeric(nummosquito_i)
    dlnm_i <- mymodel(formula, data = df1, family = "binomial") #glm with out ith row
    loo_pred[i] <- dlnm_i$summary.fitted.values$mean[i]
    loo_pred[i+1] <- dlnm_i$summary.fitted.values$mean[i+1]
    loo_pred[i+2] <- dlnm_i$summary.fitted.values$mean[i+2]
    loo_predmin[i] <- dlnm_i$summary.fitted.values$`0.025quant`[i]
    loo_predmin[i+1] <- dlnm_i$summary.fitted.values$`0.025quant`[i+1]
    loo_predmin[i+2] <- dlnm_i$summary.fitted.values$`0.025quant`[i+2]
    loo_predmax[i] <- dlnm_i$summary.fitted.values$`0.975quant`[i]
    loo_predmax[i+1] <- dlnm_i$summary.fitted.values$`0.975quant`[i+1]
    loo_predmax[i+2] <- dlnm_i$summary.fitted.values$`0.975quant`[i+2]
  }
}

MSE_Va <- mean((df0$meanAOI - loo_pred) ^ 2)
MSE_Tr <- mean((df0$meanAOI - model2$summary.fitted.values$mean) ^ 2)
MSE_Va
MSE_Tr

## plot
df0$fittednum <- model2$summary.fitted.values$mean
df0$predictednum <- loo_pred
df0$predictedmin <- loo_predmin
df0$predictedmax <- loo_predmax

colors <- c("Observed AOI" = "blue", "Model fitted value" = "red", "Cross-validation prediction" = "green3")

ggplot(df0)+
  geom_line(aes(x=Date, y=meanAOI, color = "Observed AOI"), size = 1.2, alpha = 0.6)+
  geom_line(aes(x=Date, y=fittednum, color = "Model fitted value"), size = 1.2, alpha = 0.6) +
  geom_line(aes(x=Date, y=predictednum, color = "Cross-validation prediction"), size = 1.2, alpha = 0.6)+
  geom_ribbon(aes(x=Date, ymin=predictedmin, ymax=predictedmax), alpha=0.2)+
  labs(x = "Date",
       y = "Area ovitrap/gravidtrap index",
       color = "Legend")+
  facet_wrap( ~ District, ncol = 1)+
  theme(legend.position = c(0.85,1), 
        legend.title= element_blank(),
        axis.title = element_text(size=30), 
        axis.text.x = element_text(size=25, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 30),
        legend.text = element_text(size=19),
        strip.text = element_text(size=25),
        panel.grid.major.x = element_line(size = 1))+
  scale_color_manual(values = colors)+
  scale_x_date(date_breaks = "6 month", labels = label_date(format = "%Y/%m"), expand = c(0,0))
