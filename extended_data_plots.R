library(tidyverse)
#install.packages("lubridate")
library(lubridate)
#install.packages("zoo")
library(zoo)
#install.packages("trend")
library(trend)
library(ggplot2)
#install.packages("Kendall")
library(Kendall)

##setting ggplot theme
ggplot_theme <- theme_linedraw(base_size = 12) +
  theme(panel.grid=element_line(colour="gray"),
        axis.text = element_text(color = "black"), 
        legend.position = "top")
theme_set(ggplot_theme)

##load data
Erie_data <- read.csv("./2022_WLE_Weekly_Datashare_CSV.csv",
                      stringsAsFactors = T)
View(Erie_data)

##data wrangling
class(Erie_data$Temp_C)
Erie_data$Temp_C <- as.numeric(as.character(Erie_data$Temp_C))

class(Erie_data$DO_mgL.1)
Erie_data$DO_mgL.1 <- as.numeric(as.character(Erie_data$DO_mgL.1))

class(Erie_data$Dissolved_Microcystin_ugL.1)
Erie_data$Dissolved_Microcystin_ugL.1 <- 
  as.numeric(as.character(Erie_data$Dissolved_Microcystin_ugL.1))

class(Erie_data$Extracted_CHLa_ugL.1)

class(Erie_data$Extracted_PC_ugL.1)
Erie_data$Extracted_PC_ugL.1 <- as.numeric(as.character(Erie_data$Extracted_PC_ugL.1))

class(Erie_data$Date)
Erie_data$Date <- as.Date(as.character(Erie_data$Date), format="%m/%d/%Y")

Erie_data_subset <- data_frame(Erie_data$Date, Erie_data$Sample_Depth_category, 
                               Erie_data$Temp_C, Erie_data$DO_mgL.1,
                               Erie_data$Dissolved_Microcystin_ugL.1,
                               Erie_data$Extracted_CHLa_ugL.1, 
                               Erie_data$Extracted_PC_ugL.1)

colnames(Erie_data_subset) <- c("Date", "Depth_category", "Temp", "DO", "MC", "Chla", "PC")
View(Erie_data_subset)

Erie_data_surface <- Erie_data_subset %>%
  filter(Depth_category=="Surface") %>%
  group_by(Date) %>%
  summarize(mean_chla = mean(Chla),
            mean_temp = mean(Temp),
            mean_mc = mean(MC), 
            mean_do = mean(DO))

Erie_data_bottom <- Erie_data_subset %>%
  filter(Depth_category=="Bottom")

chla_plot <- ggplot() +
  geom_line(data=Erie_data_surface, aes(x=Date, y=mean_chla))

mc_plot <- ggplot() +
  geom_line(data=Erie_data_surface, aes(x=Date, y=mean_mc))

chla_plot


##2012-2018 Data
Erie_Data_2012_2018 <- 
  read.csv("./lake_erie_habs_field_sampling_results_2012_2018_v2.csv",
           check.names = F)

colnames(Erie_Data_2012_2018) <- gsub(" ", ".", colnames(Erie_Data_2012_2018))
colnames(Erie_Data_2012_2018) <- gsub("/", ".", colnames(Erie_Data_2012_2018))
colnames(Erie_Data_2012_2018) <- gsub("[//+]", "", colnames(Erie_Data_2012_2018))
colnames(Erie_Data_2012_2018) <- gsub("[()]", "", colnames(Erie_Data_2012_2018))
colnames(Erie_Data_2012_2018) <- gsub("[<>]", "", colnames(Erie_Data_2012_2018))

View(Erie_Data_2012_2018)
colnames(Erie_Data_2012_2018)

colnames(Erie_Data_2012_2018)[19] <- "Photo.active.radiation"

Erie_Data_2012_2018 <- data_frame(Erie_Data_2012_2018$Date,
                                  Erie_Data_2012_2018$Site,
                                  Erie_Data_2012_2018$Sample.Depth.category,
                                  Erie_Data_2012_2018$Dissolved.Organic.Carbon.mg.L,
                                  Erie_Data_2012_2018$Urea.b5g.N.L,
                                  Erie_Data_2012_2018$Nitrate..Nitrite.mg.N.L,
                                  Erie_Data_2012_2018$Ammonia.b5g.N.L,
                                  Erie_Data_2012_2018$Soluble.Reactive.Phosphorus.b5g.P.L,
                                  Erie_Data_2012_2018$Total.Dissolved.Phosphorus.b5g.P.L,
                                  Erie_Data_2012_2018$Total.Phosphorus.b5g.P.L,
                                  Erie_Data_2012_2018$Extracted.Chlorophyll.a.b5g.L,
                                  Erie_Data_2012_2018$Extracted.Phycocyanin.b5g.L,
                                  Erie_Data_2012_2018$Dissolved.Microcystin.b5g.L,
                                  Erie_Data_2012_2018$CTD.Dissolved.Oxygen.mg.L,
                                  Erie_Data_2012_2018$Photo.active.radiation,
                                  Erie_Data_2012_2018$Sample.Temperature.b0C,
                                  Erie_Data_2012_2018$CTD.Temperature.b0C)
colnames(Erie_Data_2012_2018) <- gsub("Erie_Data_2012_2018", "", colnames(Erie_Data_2012_2018))
colnames(Erie_Data_2012_2018) <- gsub("[//$]", "", colnames(Erie_Data_2012_2018))
View(Erie_Data_2012_2018)

lapply(Erie_Data_2012_2018, class)
Erie_Data_2012_2018$Dissolved.Microcystin.b5g.L <- as.numeric(Erie_Data_2012_2018$Dissolved.Microcystin.b5g.L)
Erie_Data_2012_2018$Extracted.Phycocyanin.b5g.L <- as.numeric(Erie_Data_2012_2018$Extracted.Phycocyanin.b5g.L)
Erie_Data_2012_2018$Soluble.Reactive.Phosphorus.b5g.P.L <- as.numeric(Erie_Data_2012_2018$Soluble.Reactive.Phosphorus.b5g.P.L)
Erie_Data_2012_2018$Ammonia.b5g.N.L <- as.numeric(Erie_Data_2012_2018$Ammonia.b5g.N.L)
Erie_Data_2012_2018$Nitrate..Nitrite.mg.N.L <- as.numeric(Erie_Data_2012_2018$Nitrate..Nitrite.mg.N.L)
Erie_Data_2012_2018$Urea.b5g.N.L <- as.numeric(Erie_Data_2012_2018$Urea.b5g.N.L)
Erie_Data_2012_2018$Date <- as.Date(Erie_Data_2012_2018$Date, format = "%m/%d/%Y")

Erie_data_surface_ext <- Erie_Data_2012_2018 %>%
  filter(Sample.Depth.category=="Surface") %>%
  group_by(Date) %>%
  summarize(mean_chla = mean(Extracted.Chlorophyll.a.b5g.L),
            mean_temp = mean(CTD.Temperature.b0C),
            mean_mc = mean(Dissolved.Microcystin.b5g.L), 
            mean_do = mean(CTD.Dissolved.Oxygen.mg.L)) %>%
  mutate(year = year(Date)) %>%
  mutate(month = month(Date)) %>%
  mutate(day = day(Date)) %>%
  month_day=paste(month,"/", day)
View(Erie_data_surface_ext)

class(Erie_data_surface_ext$month)
#Erie_data_surface_ext$month <- as.Date(Erie_data_surface_ext$month)

chla_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_surface_ext, year=="2014"), aes(x=month, y=mean_chla)) +
  geom_line(data=filter(Erie_data_surface_ext, year=="2012"), aes(x=month, y=mean_chla))

temp_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_surface_ext, year=="2017"), aes(x=Date, y=mean_temp))

chla_plot_ext
temp_plot_ext

##combining datasets
