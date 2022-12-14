##title: "02_Data_Visualization"
##author: "Keqi YUE"
##date: '2022-12-08'

setwd("D:/Downloads/Dengue")
source("01_load_packages_data.R")

## box plot for the distribution of monthly mean temperature and total rainfall during 2010 to 2022 August
# mean temperature
ggplot(df, aes(x = month, y = Meantemp))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(col=Period, size = Period), position = "jitter")+
  scale_size_manual(values=c(2, 5, 5, 5))+
  theme_classic()+
  labs(x = "Month",
       y = "Mean Temperature (°C)")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = c(0.9,0.1),
        strip.text = element_text(size = 25))+
  facet_wrap( ~ District)

# total rainfall
ggplot(df, aes(x = month, y = Totalrain))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(col=Period, size = Period), position = "jitter")+
  scale_size_manual(values=c(2, 5, 5, 5))+
  theme_classic()+
  labs(x = "Month",
       y = "Total rainfall (mm)")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = c(0.92,0.9),
        strip.text = element_text(size = 25))+
  facet_wrap( ~ District)

