##title: '05_lag_nonlinear_Output.R'
##author: 'Keqi YUE'
##date: '2022-12-13'

source("01_load_packages_data.R")

#######################
## Model 1
source("Model/aoi inla logit/model3.R")
model0 <- model

## plot lag effect of mean temperature
nlag <- 2
# extract full coef and vcov and create indicators for each term
coef <- model0$summary.fixed$mean
vcov <- model0$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mt", model0$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (24 deg C)
predt <- crosspred(basis_mt, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = 'log', bylag = 0.25, cen = round(mean(df$Meantemp), 0))

png(file = "Figures/single_temp_model1.png", width = 3000, height = 2500, res = 300)
plot(predt, "contour", xlab="Mean temperature", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="Mean temperature (°C)",ylab="Lag (months)",cex.main=2,cex.lab=1.5))
dev.off()

## plot lag effect of total rainfall
nlag <- 6
# extract full coef and vcov and create indicators for each term
coef <- model0$summary.fixed$mean
vcov <- model0$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_tr", model0$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (173mm)
predt <- crosspred(basis_tr, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = round(mean(df$Totalrain), 0))

png(file = "Figures/single_rain_model1.png", width = 3000, height = 2500, res = 300)
plot(predt, "contour", xlab="Total rainfall", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="Total rainfall (mm)",ylab="Lag (months)",cex.main=2,cex.lab=1.5))
dev.off()

#######################
## Model 2
source("Model/Number of mosquitoes/nb/model3.R")
model1 <- model

## plot lag effect of mean temperature
nlag <- 2
# extract full coef and vcov and create indicators for each term
coef <- model1$summary.fixed$mean
vcov <- model1$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mt", model1$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (24 deg C)
predt <- crosspred(basis_mt, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = 'log', bylag = 0.25, cen = round(mean(df$Meantemp), 0))

png(file = "Figures/single_temp_model2.png", width = 3000, height = 2500, res = 300)
plot(predt, "contour", xlab="Mean temperature", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="Mean temperature (°C)",ylab="Lag (months)",cex.main=2,cex.lab=1.5))
dev.off()

## plot lag effect of total rainfall
nlag <- 6
# extract full coef and vcov and create indicators for each term
coef <- model1$summary.fixed$mean
vcov <- model1$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_tr", model1$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (173mm)
predt <- crosspred(basis_tr, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = round(mean(df$Totalrain), 0))

png(file = "Figures/single_rain_model2.png", width = 3000, height = 2500, res = 300)
plot(predt, "contour", xlab="Total rainfall", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="Total rainfall (mm)",ylab="Lag (months)",cex.main=2,cex.lab=1.5))
dev.off()

#######################
## Model 3
source("Model/aoi inla logit/full model/model4.R")
model2 <- model
df1 <- df[df$Date > "2020-03-01",]

## plot lag effect of mean temperature
nlag <- 2
# extract full coef and vcov and create indicators for each term
coef <- model2$summary.fixed$mean
vcov <- model2$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mt1", model2$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (24 deg C)
predt <- crosspred(basis_mt1, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = 'log', bylag = 0.25, cen = round(mean(df1$Meantemp), 0))

png(file = "Figures/single_temp_model3.png", width = 3000, height = 2500, res = 300)
plot(predt, "contour", xlab="Mean temperature", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="Mean temperature (°C)",ylab="Lag (months)",cex.main=2,cex.lab=1.5))
dev.off()

## plot lag effect of total rainfall
nlag <- 6
# extract full coef and vcov and create indicators for each term
coef <- model2$summary.fixed$mean
vcov <- model2$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_tr1", model2$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (173mm)
predt <- crosspred(basis_tr1, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = round(mean(df1$Totalrain), 0))

png(file = "Figures/single_rain_model3.png", width = 3000, height = 2500, res = 300)
plot(predt, "contour", xlab="Total rainfall", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="Total rainfall (mm)",ylab="Lag (months)",cex.main=2,cex.lab=1.5))
dev.off()
