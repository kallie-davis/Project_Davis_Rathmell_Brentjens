library(ggplot2)
library(tidyverse)
library(dplyr)
#install.packages("data.table")
library(data.table)


erie_data_2022 <- read.csv("./Data/Processed/Erie_2022_processed.csv")
erie_data_2012_2018 <- read.csv("./Data/Processed/Erie_2012_2018_processed.csv")

erie_data_2012_2018$Date <- as.Date(erie_data_2012_2018$Date, format="%m/%d/%Y")
erie_data_2012_2018 <- erie_data_2012_2018 %>%
  mutate(year=year(Date)) %>%
  mutate(month=month(Date))

##setting ggplot theme
ggplot_theme <- theme_linedraw(base_size = 12) +
  theme(panel.grid=element_line(colour="gray"),
        axis.text = element_text(color = "black"), 
        legend.position = "top")
theme_set(ggplot_theme)

##summary of 2012-2018 data
summary(erie_data_2012_2018)
#Chla (unit?): min = 0.710, max = 6784.000, mean = 34.014, NAs = 3
#PC (unit?): min = 0.01, max = 8227.92, mean = 29.16, NAs = 36
#MC (unit?): min = 0.0800, max = 1.9400, mean = 0.2527, NAs = 659
#DO (mg/L): min = 3.790, max = 13.040, mean = 7.516, NAs = 94
#CTD Temp (celsius): min = 2.40, max = 29.70, mean = 21.97, NAs = 97
#nitrate.nitrite (mg/L): min = 0.0020, max = 9.4530, mean = 0.9134, NAs = 371
#TDP (unit?): min = 0.16, max = 273.58, mean = 19.10, NAs = 330

summary_df_2012_2018 <- data_frame(Variable = c("Chlorophyll-a", "Phycocyanin", "Microcystin", "Dissolved oxygen", "Temperature", "Nitrate + Nitrite", "Total dissolved phosphorus"),
                              Minimum = c(0.710, 0.01, 0.0800, 3.790, 2.40, 0.0020, 0.16),
                              Maximum = c(6784.000, 8227.92, 1.9400, 13.040, 29.70, 9.4530, 273.58), 
                              Mean = c(34.014, 29.16, 0.2527, 7.516, 21.97, 0.9134, 19.10))
colnames(summary_df_2012_2018)[1] <- ""
summary_table_2012_2018 <- as.data.table(summary_df_2012_2018)
summary_table_2012_2018 ##ADD UNITS TO TABLE

##summary of 2022 data
summary(erie_data_2022)
#Chla (unit?): min = 1.430, max = 271.590, mean = 37.961, NAs = 0
#PC (unit?): min = 0.0186, max = 403.0800, mean = 30.8265, NAs = 19
#MC (unit?): min = 0.1000, max = 1.0400, mean = 0.3032, NAs = 32
#DO (mg/L): min = 4.557, max = 12.473, mean = 7.907, NAs = 0
#Temp (celsius): min = 8.236, max = 26.205, mean = 21.066, NAs = 0

summary_df_2022 <- data_frame(Variable = c("Chlorophyll-a", "Phycocyanin", "Microcystin", "Dissolved oxygen", "Temperature"),
                              Minimum = c(1.430, 0.0186, 0.1000, 4.557, 8.236),
                              Maximum = c(271.590, 403.0800, 1.0400, 12.473, 26.205), 
                              Mean = c(37.961, 30.8265, 0.3032, 7.907, 21.066))
colnames(summary_df_2022)[1] <- ""
summary_table_2022 <- as.data.table(summary_df_2022)
summary_table_2022


##histograms
#chla
chla_hist_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=Extracted.Chlorophyll.a.b5g.L))+
  geom_histogram(bins=50, color="black", fill="lightgray") +
  xlab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))+
  ggtitle("2012-2018")
chla_hist_2012_2018

chla_hist_no_outlier <- ggplot(data=subset(erie_data_2012_2018, !Extracted.Chlorophyll.a.b5g.L>6000), aes(x=Extracted.Chlorophyll.a.b5g.L))+
  geom_histogram(bins=50, color="black", fill="lightgray") +
  xlab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))+
  ggtitle("2012-2018")
chla_hist_no_outlier
##easier to see distribution of chl a when outlier is removed

chla_hist_2022 <- ggplot(data=erie_data_2022, aes(x=Chla))+
  geom_histogram(bins=50, color="black", fill="lightgray") +
  xlab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))+
  ggtitle("2022")
chla_hist_2022


#temp
temp_hist_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=CTD.Temperature.b0C))+
  geom_histogram(bins=50, color="black", fill="lightgray") +
  xlab(expression("Temperature" ~ "("*degree*C*")"))+
  ggtitle("2012-2018")
temp_hist_2012_2018

temp_hist_2022 <- ggplot(data=erie_data_2022, aes(x=Temp))+
  geom_histogram(bins=50, color="black", fill="lightgray")+
  xlab(expression("Temperature" ~ "("*degree*C*")"))+
  ggtitle("2022")
temp_hist_2022


#MC
mc_hist_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=Dissolved.Microcystin.b5g.L))+
  geom_histogram(bins=50, color="black", fill="lightgray")+
  xlab(expression("Microcystin" ~ "("*mu*"g/L)"))+
  ggtitle("2012-2018")
mc_hist_2012_2018

mc_hist_2022 <- ggplot(data=erie_data_2022, aes(x=MC))+
  geom_histogram(bins=50, color="black", fill="lightgray")+
  xlab(expression("Microcystin" ~ "("*mu*"g/L)"))+
  ggtitle("2022")
mc_hist_2022


#DO
do_hist_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=CTD.Dissolved.Oxygen.mg.L))+
  geom_histogram(bins=50, color="black", fill="lightgray")+
  xlab("Dissolved Oxygen (mg/L)") +
  ggtitle("2012-2018")
do_hist_2012_2018

do_hist_2022 <- ggplot(data=erie_data_2022, aes(x=DO))+
  geom_histogram(bins=50, color="black", fill="lightgray")+
  xlab("Dissolved Oxygen (mg/L)") +
  ggtitle("2022")
do_hist_2022



##year box plots
# chla_year <- ggplot(data=erie_data_2012_2018, aes(x=as.factor(year), y=Extracted.Chlorophyll.a.b5g.L))+
#   geom_boxplot()
# chla_year
# 
# chla_year_no_outlier <- ggplot(data=subset(erie_data_2012_2018, !Extracted.Chlorophyll.a.b5g.L>6000), aes(x=as.factor(year), y=Extracted.Chlorophyll.a.b5g.L))+
#   geom_boxplot()
# chla_year_no_outlier
# 
# temp_year <- ggplot(data=erie_data_2012_2018, aes(x=as.factor(year), y=CTD.Temperature.b0C))+
#   geom_boxplot()
# temp_year



##surface v. bottom box plots
chla_depth_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=as.factor(Sample.Depth.category), y=Extracted.Chlorophyll.a.b5g.L))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))+
  ggtitle("2012-2018")
chla_depth_2012_2018

##??
chla_depth_no_outlier <- ggplot(data=subset(erie_data_2012_2018, !Extracted.Chlorophyll.a.b5g.L>750), aes(x=as.factor(Sample.Depth.category), y=Extracted.Chlorophyll.a.b5g.L))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))+
  ggtitle("2012-2018")
chla_depth_no_outlier

chla_depth_2022 <- ggplot(data=erie_data_2022, aes(x=as.factor(Depth_category), y=Chla))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))+
  ggtitle("2022")
chla_depth_2022

MC_depth_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=as.factor(Sample.Depth.category), y=Dissolved.Microcystin.b5g.L))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Microcystin" ~ "("*mu*"g/L)"))+
  ggtitle("2012-2018")
MC_depth_2012_2018

MC_depth_2022 <- ggplot(data=erie_data_2022, aes(x=as.factor(Depth_category), y=MC))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Microcystin" ~ "("*mu*"g/L)"))+
  ggtitle("2022")
MC_depth_2022

# DO_depth_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=as.factor(Sample.Depth.category), y=CTD.Dissolved.Oxygen.mg.L))+
#   geom_boxplot()
# DO_depth_2012_2018
# 
# DO_depth_2022 <- ggplot(data=erie_data_2022, aes(x=as.factor(Depth_category), y=DO))+
#   geom_boxplot()
# DO_depth_2022

Temp_depth_2012_2018 <- ggplot(data=erie_data_2012_2018, aes(x=as.factor(Sample.Depth.category), y=CTD.Temperature.b0C))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Temperature" ~ "("*degree*C*")"))+
  ggtitle("2012-2018")
Temp_depth_2012_2018

Temp_depth_2022 <- ggplot(data=erie_data_2022, aes(x=as.factor(Depth_category), y=Temp))+
  geom_boxplot()+
  xlab("Sample Depth Category")+
  ylab(expression("Temperature" ~ "("*degree*C*")"))+
  ggtitle("2022")
Temp_depth_2022
