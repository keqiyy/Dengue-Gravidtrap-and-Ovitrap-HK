##title: '04_LOOCV'
##author: 'Keqi YUE'
##date: '2022-12-13'

## Note: We will validate the three models by leave-one-month-out corss validation, and plot the observed AOI, model fitted values, and predictions.
source("01_load_packages_data.R")
######################################
## LOOCV for Model 1
## input the best fitting model
source("Model/aoi inla logit/model3.R")
model0 <- model
df0 <- df

formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr + basis_mt + f(District, model = "iid")

n <- dim(df)[1] # number of samples
loo_pred <- rep(NA, n)
loo_predmin <- rep(NA, n)
loo_predmax <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0

## loocv
for(i in 1:n) {
  if(i%%3 == 1){
    print(i)
    nummosquito_i <- replace(df0$Number.of.positive.traps, i, "NA")
    nummosquito_i <- replace(nummosquito_i, i+1, "NA")
    nummosquito_i <- replace(nummosquito_i, i+2, "NA")
    df$Number.of.positive.traps <- as.numeric(nummosquito_i)
    dlnm_i <- mymodel(formula, data = df, family = "binomial", Ntrials = df0$Total.traps) #glm with out ith row
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
        strip.text = element_text(size=25))+
  scale_color_manual(values = colors)+
  scale_x_date(date_breaks = "1 year", labels = label_date(format = "%Y/%m"), expand = c(0,0))

######################################
## LOOCV for Model 2
## input the best fitting model
source("Model/Number of mosquitoes/nb/model3.R")
model1 <- model
df <- df0

formula <- Nummosquito.per.1000.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr + basis_mt + f(District, model = "iid")

n <- dim(df)[1] # number of samples
loo_pred <- rep(NA, n)
loo_predmin <- rep(NA, n)
loo_predmax <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0

## loocv
for(i in 1:n) {
  if(i%%3 == 1){
    print(i)
    nummosquito_i <- replace(df0$Nummosquito.per.1000.traps, i, "NA")
    nummosquito_i <- replace(nummosquito_i, i+1, "NA")
    nummosquito_i <- replace(nummosquito_i, i+2, "NA")
    df$Nummosquito.per.1000.traps <- as.numeric(nummosquito_i)
    dlnm_i <- mymodel(formula, data = df, family = "nbinomial") #glm with out ith row
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
  theme(legend.position = c(0.85,1), 
        legend.title= element_blank(),
        axis.title = element_text(size=30), 
        axis.text.x = element_text(size=25, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 30),
        legend.text = element_text(size=19),
        strip.text = element_text(size=25))+
  scale_color_manual(values = colors)+
  scale_x_date(date_breaks = "1 year", labels = label_date(format = "%Y/%m"), expand = c(0,0))

######################################
## LOOCV for Model 3
## input the best fitting model
source("Model/aoi inla logit/full model/model4.R")
model2 <- model
df <- df0
df <- df[df$Date > "2020-03-01",]
df0 <- df0[df0$Date > "2020-03-01",]

formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + Nummosquito.per.1000.traps 

n <- dim(df)[1] # number of samples
loo_pred <- rep(NA, n)
loo_predmin <- rep(NA, n)
loo_predmax <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0

## loocv
for(i in 1:n) {
  if(i%%3 == 1){
    print(i)
    nummosquito_i <- replace(df0$Number.of.positive.traps, i, "NA")
    nummosquito_i <- replace(nummosquito_i, i+1, "NA")
    nummosquito_i <- replace(nummosquito_i, i+2, "NA")
    df$Number.of.positive.traps <- as.numeric(nummosquito_i)
    dlnm_i <- mymodel(formula, data = df, family = "binomial", Ntrials = df0$Total.traps) #glm with out ith row
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
        strip.text = element_text(size=25))+
  scale_color_manual(values = colors)+
  scale_x_date(date_breaks = "1 year", labels = label_date(format = "%Y/%m"), expand = c(0,0))

