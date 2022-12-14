---
title: "00_Data_preprocess"
author: "Keqi YUE"
date: '2022-12-06'
---
  
##Basic Settings
#install.packages("readxl")
library(readxl)
#install.packages("data.table")
library(data.table)
#install.packages("zoo")
library(zoo)
#install.packages("reshape2")
library(reshape2)
#read every sheet in a excel file
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


## Processing index data into three districts: Hong Kong Island & Kowloon, New Territories East, and New Territories West
setwd("D:/Downloads/Dengue/Data/Raw Data/Index")
#processing area ovitrap index
aoi <- read_excel_allsheets("2010-2020 AOI.xlsx") #area ovitraps index data from January 2010 to March 2020
hkk <- c() #Hong Kong Island & Kowloon
nte <- c() #New Territories East
ntw <- c() #New Territories West
for(i in 1:length(aoi)){
  a <- setDT(aoi[[i]])
  df <- aggregate(a[,4:15], list(a$District1), FUN=sum, na.rm=TRUE)
  for(j in 2:ncol(df)){
    hkk <- c(hkk, as.numeric(df[1,][j]))
    nte <- c(nte, as.numeric(df[2,][j]))
    ntw <- c(ntw, as.numeric(df[3,][j]))
  }
}

#processing area gravidtrap index and area density index
agi <- read_excel_allsheets("2020-2022 AGI&ADI.xlsx") #data from April 2020 to September 2022
hkk1 <- c()
nte1 <- c()
ntw1 <- c()
hkk_adi <- c()
nte_adi <- c()
ntw_adi <- c()

agi2020 <- setDT(agi[[1]]) #2020 agi & adi
df <- aggregate(agi2020[,6:14], list(agi2020$District1, agi2020$Indices), FUN=mean, na.rm=TRUE)
df
hkk_adi <- c(hkk_adi, as.numeric(df[1,][3:11]))
nte_adi <- c(nte_adi, as.numeric(df[2,][3:11]))
ntw_adi <- c(ntw_adi, as.numeric(df[3,][3:11]))

df <- aggregate(agi2020[,6:14], list(agi2020$District1, agi2020$Indices), FUN=sum, na.rm=TRUE)
df
hkk1 <- c(hkk1, as.numeric(df[4,][3:11]))
nte1 <- c(nte1, as.numeric(df[5,][3:11]))
ntw1 <- c(ntw1, as.numeric(df[6,][3:11]))

agi2021 <- setDT(agi[[2]]) #2021 agi & adi
df <- aggregate(agi2021[,6:17], list(agi2021$District1, agi2021$Indices), FUN=mean, na.rm=TRUE)
df
hkk_adi <- c(hkk_adi, as.numeric(df[1,][3:14]))
nte_adi <- c(nte_adi, as.numeric(df[2,][3:14]))
ntw_adi <- c(ntw_adi, as.numeric(df[3,][3:14]))

df <- aggregate(agi2021[,6:17], list(agi2021$District1, agi2021$Indices), FUN=sum, na.rm=TRUE)
df
hkk1 <- c(hkk1, as.numeric(df[4,][3:14]))
nte1 <- c(nte1, as.numeric(df[5,][3:14]))
ntw1 <- c(ntw1, as.numeric(df[6,][3:14]))

agi2022 <- setDT(agi[[3]]) #2022 agi & adi
df <- aggregate(agi2022[,6:17], list(agi2022$District1, agi2022$Indices), FUN=mean, na.rm=TRUE)
df
hkk_adi <- c(hkk_adi, as.numeric(df[1,][3:14]))
nte_adi <- c(nte_adi, as.numeric(df[2,][3:14]))
ntw_adi <- c(ntw_adi, as.numeric(df[3,][3:14]))

df <- aggregate(agi2022[,6:17], list(agi2022$District1, agi2022$Indices), FUN=sum, na.rm=TRUE)
df
hkk1 <- c(hkk1, as.numeric(df[4,][3:14]))
nte1 <- c(nte1, as.numeric(df[5,][3:14]))
ntw1 <- c(ntw1, as.numeric(df[6,][3:14]))

#Merge all index data
HKK <- c(hkk[1:123], hkk1[1:30])
NTE <- c(nte[1:123], nte1[1:30])
NTW <- c(ntw[1:123], ntw1[1:30])
Date <- seq(as.Date('2010-01-01'), as.Date('2022-09-01'), by = "months")
df <- data.frame(Date = Date, HKK, NewEast = NTE, NewWest = NTW)
str(df)
write.csv(df, "Three Districts AOI AGI.csv")

Date <- seq(as.Date('2020-04-01'), as.Date('2022-09-01'), by = "months")
df1 <- data.frame(Date, HKK = hkk_adi[1:30],NewEast = nte_adi[1:30], NewWest = ntw_adi[1:30])
str(df1)
write.csv(df1, "Three Districts ADI.csv")
```

## Processing monthly mean temperature data into three districts: Hong Kong Island & Kowloon, New Territories East, and New Territories West
##Note: We select several weather stations from the Hong Kong Observatory data for three districts in Hong Kong.
##Stations for HKK: Happy Valley, Wong Chuk Hang, King's park
##Stations for NTE: Sha Tin, Tai Mei Tuk, Ta Kwu Ling
##Stations for NTW: New Tsing Yi Station, Cheung Chau, Sha Lo Wan, Tuen Mun Children and Juvenile Home, Wetland park

#Monthly mean temprature of different stations is calculated by the daily mean temperature of these stations.
setwd("D:/Downloads/Dengue/Data/Raw Data/")
#happy valley
happyv <- read_excel_allsheets("Mean Temperature/Daily Temperature/HKIsland/Happy Valley.xlsx")
for(i in 1:length(happyv)){
  for(j in 1:ncol(happyv[[i]])){
    happyv[[i]][,j] <- as.numeric(gsub("#","",as.character(happyv[[i]][,j])))
  }
}
happyv1 <- c()
for(i in 1:length(happyv)){
  for(j in 2:ncol(happyv[[i]])){
    happyv1 <- c(happyv1, mean(happyv[[i]][,j], na.rm = TRUE))
  }
}
str(happyv1)
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
happyv_df <- data.frame(Date = Date, Meantemp = happyv1[1:152])
write.csv(happyv_df,"Mean Temperature/Monthly Temperature/hki/Happy Valley.csv")

#Wong Chuk Hang
wch <- read_excel_allsheets("Mean Temperature/Daily Temperature/HKIsland/Wong Chuk Hang.xlsx")
for(i in 1:length(wch)){
  for(j in 1:ncol(wch[[i]])){
    wch[[i]][,j] <- as.numeric(gsub("#","",as.character(wch[[i]][,j])))
  }
}
wch1 <- c()
for(i in 1:length(wch)){
  for(j in 2:ncol(wch[[i]])){
    wch1 <- c(wch1, mean(wch[[i]][,j], na.rm = TRUE))
  }
}
wch1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = wch1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/hki/Wong Chuk Hang.csv")

##King's park
kp <- read_excel_allsheets("Mean Temperature/Daily Temperature/Kowloon/King's park.xlsx")
for(i in 1:length(kp)){
  for(j in 1:ncol(kp[[i]])){
    kp[[i]][,j] <- as.numeric(gsub("#","",as.character(kp[[i]][,j])))
  }
}

kp1 <- c()
for(i in 1:length(kp)){
  for(j in 2:ncol(kp[[i]])){
    kp1 <- c(kp1, mean(kp[[i]][,j], na.rm = TRUE))
  }
}
kp1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = kp1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/kl/King's park.csv")

#New Tsing Yi Station
nty <- read_excel_allsheets("Mean Temperature/Daily Temperature/Kowloon/New Tsing Yi Station.xlsx")
for(i in 1:length(nty)){
  for(j in 1:ncol(nty[[i]])){
    nty[[i]][,j] <- as.numeric(gsub("#","",as.character(nty[[i]][,j])))
  }
}

nty1 <- c()
for(i in 1:length(nty)){
  for(j in 2:ncol(nty[[i]])){
    nty1 <- c(nty1, mean(nty[[i]][,j], na.rm = TRUE))
  }
}
nty1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = nty1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/kl/New Tsing Yi Station.csv")

##Cheung Chau
cc <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Cheung Chau.xlsx")
for(i in 1:length(cc)){
  for(j in 1:ncol(cc[[i]])){
    cc[[i]][,j] <- as.numeric(gsub("#","",as.character(cc[[i]][,j])))
  }
}

cc1 <- c()
for(i in 1:length(cc)){
  for(j in 2:ncol(cc[[i]])){
    cc1 <- c(cc1, mean(cc[[i]][,j], na.rm = TRUE))
  }
}
cc1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = cc1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/nt/Cheung Chau.csv")

##Sha Lo Wan
slw <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Sha Lo Wan.xlsx")
for(i in 1:length(slw)){
  for(j in 1:ncol(slw[[i]])){
    slw[[i]][,j] <- as.numeric(gsub("#","",as.character(slw[[i]][,j])))
  }
}

slw1 <- c()
for(i in 1:length(slw)){
  for(j in 2:ncol(slw[[i]])){
    slw1 <- c(slw1, mean(slw[[i]][,j], na.rm = TRUE))
  }
}
slw1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = slw1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/nt/Sha Lo Wan.csv")

##Sha Tin
st <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Sha Tin.xlsx")
for(i in 1:length(st)){
  for(j in 1:ncol(st[[i]])){
    st[[i]][,j] <- as.numeric(gsub("#","",as.character(st[[i]][,j])))
  }
}

st1 <- c()
for(i in 1:length(st)){
  for(j in 2:ncol(st[[i]])){
    st1 <- c(st1, mean(st[[i]][,j], na.rm = TRUE))
  }
}
st1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = st1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/nt/Sha Tin.csv")

##Ta Kwu Ling
tkl <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Ta Kwu Ling.xlsx")
for(i in 1:length(tkl)){
  for(j in 1:ncol(tkl[[i]])){
    tkl[[i]][,j] <- as.numeric(gsub("#","",as.character(tkl[[i]][,j])))
  }
}

tkl1 <- c()
for(i in 1:length(tkl)){
  for(j in 2:ncol(tkl[[i]])){
    tkl1 <- c(tkl1, mean(tkl[[i]][,j], na.rm = TRUE))
  }
}
tkl1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = tkl1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/nt/Ta Kwu Ling.csv")

##Tai Mei Tuk
tmt <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Tai Mei Tuk.xlsx")
for(i in 1:length(tmt)){
  for(j in 1:ncol(tmt[[i]])){
    tmt[[i]][,j] <- as.numeric(gsub("#","",as.character(tmt[[i]][,j])))
  }
}

tmt1 <- c()
for(i in 1:length(tmt)){
  for(j in 2:ncol(tmt[[i]])){
    tmt1 <- c(tmt1, mean(tmt[[i]][,j], na.rm = TRUE))
  }
}
tmt1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = tmt1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/NT/Tai Mei Tuk.csv")


##Tuen Mun Children and Juvenile Home
tm <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Tuen Mun Children and Juvenile Home.xlsx")
for(i in 1:length(tm)){
  for(j in 1:ncol(tm[[i]])){
    tm[[i]][,j] <- as.numeric(gsub("#","",as.character(tm[[i]][,j])))
  }
}

tm1 <- c()
for(i in 1:length(tm)){
  for(j in 2:ncol(tm[[i]])){
    tm1 <- c(tm1, mean(tm[[i]][,j], na.rm = TRUE))
  }
}
tm1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = tm1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/nt/Tuen Mun Children and Juvenile Home.csv")

##Wetland Park
wt <- read_excel_allsheets("Mean Temperature/Daily Temperature/NT/Wetland Park.xlsx")
for(i in 1:length(wt)){
  for(j in 1:ncol(wt[[i]])){
    wt[[i]][,j] <- as.numeric(gsub("#","",as.character(wt[[i]][,j])))
  }
}

wt1 <- c()
for(i in 1:length(wt)){
  for(j in 2:ncol(wt[[i]])){
    wt1 <- c(wt1, mean(wt[[i]][,j], na.rm = TRUE))
  }
}
wt1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Meantemp = wt1[1:152])
write.csv(df,"Mean Temperature/Monthly Temperature/nt/Wetland Park.csv")

##Merge data and calculate the districtly monthly mean temperature
hkk <- data.frame(Date = Date, hv = happyv1[1:152], wch = wch1[1:152], kp = kp1[1:152])
nte <- data.frame(Date = Date, tkl = tkl1[1:152], st = st1[1:152], tmt = tmt1[1:152])
ntw <- data.frame(Date = Date, nty = nty1[1:152], cc = cc1[1:152], slw = slw1[1:152], tm = tm1[1:152], wt = wt1[1:152])

hkk <- data.frame(Date=hkk[,1], HKK=rowMeans(hkk[,-1], na.rm = TRUE))
nte <- data.frame(Date=nte[,1], NewEast=rowMeans(nte[,-1], na.rm = TRUE))
ntw <- data.frame(Date=ntw[,1], NewWest=rowMeans(ntw[,-1], na.rm = TRUE))

alldat <- merge(hkk, nte, by = c("Date"))
alldat <- merge(alldat, ntw, by = c("Date"))
alldat
write.csv(alldat, "Mean Temperature/Monthly Temperature/Three districts monthly mean temperature.csv")


## Processing monthly total rainfall data into three districts: Hong Kong Island & Kowloon, New Territories East, and New Territories West
##Note
##Stations for HKK: Happy Valley, Quarry Bay, Cape D'Aguilar, King's park
##Stations for NTE: Sha Tin, Ta Kwu Ling, Ta Mei Tuk
##Stations for NTW: Chaung Chau, Sha Lo Wan, Wetland Park, Tuen Mun Children and Juvenile Home

setwd("D:/Downloads/Dengue/Data/Raw Data/Total Rainfall")
##Cape D'Aguilar
cda <- read_excel_allsheets("Daily/HKIsland/Cape D'Aguilar.xlsx")
for(i in 1:length(cda)){
  for(j in 1:ncol(cda[[i]])){
    cda[[i]][,j] <- as.numeric(gsub("#","",as.character(cda[[i]][,j])))
  }
}

cda1 <- c()
for(i in 1:length(cda)){
  for(j in 2:ncol(cda[[i]])){
    cda1 <- c(cda1, sum(cda[[i]][,j], na.rm = TRUE))
  }
}
cda1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = cda1[1:152])
write.csv(happyv_df,"Monthly/HKIsland/Cape D'Aguilar.csv")

##Happy Valley
happyv <- read_excel_allsheets("Daily/HKIsland/Happy Valley.xlsx")
for(i in 1:length(happyv)){
  for(j in 1:ncol(happyv[[i]])){
    happyv[[i]][,j] <- as.numeric(gsub("#","",as.character(happyv[[i]][,j])))
  }
}

happyv1 <- c()
for(i in 1:length(happyv)){
  for(j in 2:ncol(happyv[[i]])){
    happyv1 <- c(happyv1, sum(happyv[[i]][,j], na.rm = TRUE))
  }
}
happyv1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
happyv_df <- data.frame(Date = Date, Totalrain = happyv1[1:152])
write.csv(happyv_df,"Monthly/HKIsland/Happy Valley.csv")

##Quarry Bay
qb <- read_excel_allsheets("Daily/HKIsland/Quarry Bay.xlsx")
for(i in 1:length(qb)){
  for(j in 1:ncol(qb[[i]])){
    qb[[i]][,j] <- as.numeric(gsub("#","",as.character(qb[[i]][,j])))
  }
}

qb1 <- c()
for(i in 1:length(qb)){
  for(j in 2:ncol(qb[[i]])){
    qb1 <- c(qb1, sum(qb[[i]][,j], na.rm = TRUE))
  }
}
qb1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = qb1[1:152])
write.csv(df,"Monthly/HKIsland/Quarry Bay.csv")

##King's park
kp <- read_excel_allsheets("Daily/Kowloon/King's park.xlsx")
for(i in 1:length(kp)){
  for(j in 1:ncol(kp[[i]])){
    kp[[i]][,j] <- as.numeric(gsub("#","",as.character(kp[[i]][,j])))
  }
}

kp1 <- c()
for(i in 1:length(kp)){
  for(j in 2:ncol(kp[[i]])){
    kp1 <- c(kp1, sum(kp[[i]][,j], na.rm = TRUE))
  }
}
kp1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = kp1[1:152])
write.csv(df,"Monthly/KL/King's park.csv")

##Cheung Chau
cc <- read_excel_allsheets("Daily/NT/Cheung Chau.xlsx")
for(i in 1:length(cc)){
  for(j in 1:ncol(cc[[i]])){
    cc[[i]][,j] <- as.numeric(gsub("#","",as.character(cc[[i]][,j])))
  }
}

cc1 <- c()
for(i in 1:length(cc)){
  for(j in 2:ncol(cc[[i]])){
    cc1 <- c(cc1, sum(cc[[i]][,j], na.rm = TRUE))
  }
}
cc1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = cc1[1:152])
write.csv(df,"Monthly/nt/Cheung Chau.csv")

##Sha Lo Wan
slw <- read_excel_allsheets("Daily/NT/Sha Lo Wan.xlsx")
for(i in 1:length(slw)){
  for(j in 1:ncol(slw[[i]])){
    slw[[i]][,j] <- as.numeric(gsub("#","",as.character(slw[[i]][,j])))
  }
}

slw1 <- c()
for(i in 1:length(slw)){
  for(j in 2:ncol(slw[[i]])){
    slw1 <- c(slw1, sum(slw[[i]][,j], na.rm = TRUE))
  }
}
slw1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = slw1[1:152])
write.csv(df,"Monthly/nt/Sha Lo Wan.csv")

##Sha Tin
st <- read_excel_allsheets("Daily/NT/Sha Tin.xlsx")
for(i in 1:length(st)){
  for(j in 1:ncol(st[[i]])){
    st[[i]][,j] <- as.numeric(gsub("#","",as.character(st[[i]][,j])))
  }
}

st1 <- c()
for(i in 1:length(st)){
  for(j in 2:ncol(st[[i]])){
    st1 <- c(st1, sum(st[[i]][,j], na.rm = TRUE))
  }
}
st1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = st1[1:152])
write.csv(df,"Monthly/nt/Sha Tin.csv")

##Ta Kwu Ling
tkl <- read_excel_allsheets("Daily/NT/Ta Kwu Ling.xlsx")
for(i in 1:length(tkl)){
  for(j in 1:ncol(tkl[[i]])){
    tkl[[i]][,j] <- as.numeric(gsub("#","",as.character(tkl[[i]][,j])))
  }
}

tkl1 <- c()
for(i in 1:length(tkl)){
  for(j in 2:ncol(tkl[[i]])){
    tkl1 <- c(tkl1, sum(tkl[[i]][,j], na.rm = TRUE))
  }
}
tkl1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = tkl1[1:152])
write.csv(df,"Monthly/nt/Ta Kwu Ling.csv")

##Tai Mei Tuk
tmt <- read_excel_allsheets("Daily/NT/Tai Mei Tuk.xlsx")
for(i in 1:length(tmt)){
  for(j in 1:ncol(tmt[[i]])){
    tmt[[i]][,j] <- as.numeric(gsub("#","",as.character(tmt[[i]][,j])))
  }
}

tmt1 <- c()
for(i in 1:length(tmt)){
  for(j in 2:ncol(tmt[[i]])){
    tmt1 <- c(tmt1, sum(tmt[[i]][,j], na.rm = TRUE))
  }
}
tmt1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = tmt1[1:152])
write.csv(df,"Monthly/NT/Tai Mei Tuk.csv")


##Tuen Mun Children and Juvenile Home
tm <- read_excel_allsheets("Daily/NT/Tuen Mun Children and Juvenile Home.xlsx")
for(i in 1:length(tm)){
  for(j in 1:ncol(tm[[i]])){
    tm[[i]][,j] <- as.numeric(gsub("#","",as.character(tm[[i]][,j])))
  }
}

tm1 <- c()
for(i in 1:length(tm)){
  for(j in 2:ncol(tm[[i]])){
    tm1 <- c(tm1, sum(tm[[i]][,j], na.rm = TRUE))
  }
}
tm1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = tm1[1:152])
write.csv(df,"Monthly/nt/Tuen Mun Children and Juvenile Home.csv")

##Wetland Park
wt <- read_excel_allsheets("Daily/NT/Wetland Park.xlsx")
for(i in 1:length(wt)){
  for(j in 1:ncol(wt[[i]])){
    wt[[i]][,j] <- as.numeric(gsub("#","",as.character(wt[[i]][,j])))
  }
}

wt1 <- c()
for(i in 1:length(wt)){
  for(j in 2:ncol(wt[[i]])){
    wt1 <- c(wt1, sum(wt[[i]][,j], na.rm = TRUE))
  }
}
wt1
Date <- seq(as.Date('2010-01-01'), as.Date('2022-08-01'), by = "months")
df <- data.frame(Date = Date, Totalrain = wt1[1:152])
write.csv(df,"Monthly/nt/Wetland Park.csv")

#calculate districtly mean total rainfall
hkk <- data.frame(Date = Date, hv = happyv1[1:152], qb = qb1[1:152], cda = cda1[1:152], kp = kp1[1:152])
nte <- data.frame(Date = Date, st = st1[1:152], tkl = tkl1[1:152], tmt = tmt1[1:152])
ntw <- data.frame(Date, cc = cc1[1:152], slw = slw1[1:152], tm = tm1[1:152], wt = wt1[1:152])                  

hkk <- data.frame(Date=hkk[,1], HKK=rowMeans(hkk[,-1], na.rm = TRUE))
nte <- data.frame(Date=nte[,1], NewEast=rowMeans(nte[,-1], na.rm = TRUE))
ntw <- data.frame(Date=ntw[,1], NewWest=rowMeans(ntw[,-1], na.rm = TRUE))

alldat <- merge(hkk, nte, by = c("Date"))
alldat <- merge(alldat, ntw, by = c("Date"))
alldat
write.csv(alldat, "Monthly/Three districts monthly total rainfall.csv")



## Merge all data to get the data sheet, including AOI/AGI, ADI, mean temperature, total rainfall
setwd("D:/Downloads/Dengue/Data")
temp <- read.csv("Mean Temperature/Monthly Temperature/Three districts monthly mean temperature.csv")
rain <- read.csv("Total Rainfall/Monthly/Three districts monthly total rainfall.csv")
ind <- read.csv("Index/Three Districts AOI AGI.csv")
adi <- read.csv("Index/Three Districts ADI.csv")
temp1 <- melt(temp, id = "Date")
rain1 <- melt(rain, id = "Date")
ind1 <- melt(ind, id = "Date")
adi1 <- melt(adi, id = "Date")
adi1
df <- data.frame(Date = temp1$Date, District = temp1$variable, Meantemp = temp1$value)
df <- merge(df, rain1, by.x = c("Date", "District"), by.y = c("Date", "variable"))
df <- merge(df, ind1, by.x = c("Date", "District"), by.y = c("Date", "variable"))
df1 <- merge(df, adi1, by.x = c("Date", "District"), by.y = c("Date", "variable"))
df
colnames(df) <- c("Date","District","Meantemp","Totalrain","sumAOI")
colnames(df1) <- c("Date","District","Meantemp","Totalrain","sumAOI", "meanADI")
write.csv(df, "Three districts all dataset.csv")
write.csv(df1, "Three districts adi dataset.csv")

## Add other variables, such as year, month
##Note: We also counted the sites that set traps in each district, and got a column entitled "Number of sites". Then, we calculated the number of total traps, number of positive traps, and number of mosquitoes per 1000 traps based on AOI/AGI and ADI.
##There are 55 traps at each site. Therefore, the calculations are as below:
#Number of total traps = Number of sites * 55
#Number of positive traps = mean AOI(AGI) * Number of total traps (round to integers)
#Number of mosquitoes per 1000 traps = ADI * Number of positive traps * 1000 (round to integers)

#The completed data frame
df <- read.csv("Three districts all dataset-1.csv")
df$Date <- as.Date(df$Date)
df$year <- as.factor(format(as.Date(df$Date), "%Y"))
df$month <- as.factor(format(as.Date(df$Date), "%m"))
df$District <- as.factor(df$District)
str(df)