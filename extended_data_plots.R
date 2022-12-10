library(tidyverse)
#install.packages("lubridate")
library(lubridate)

##setting ggplot theme
ggplot_theme <- theme_linedraw(base_size = 12) +
  theme(panel.grid=element_line(colour="gray"),
        axis.text = element_text(color = "black"), 
        legend.position = "top")
theme_set(ggplot_theme)

##load data
Erie_data <- read.csv("./Data/Processed/Erie_2012_2018_processed.csv")
View(Erie_data)

##data wrangling
class(Erie_data$Date)
Erie_data$Date <- as.Date(Erie_data$Date, format="%m/%d/%Y")

Erie_data_subset <- data_frame(Erie_data$Date, Erie_data$Sample.Depth.category, 
                               Erie_data$CTD.Temperature.b0C, 
                               Erie_data$CTD.Dissolved.Oxygen.mg.L,
                               Erie_data$Dissolved.Microcystin.b5g.L,
                               Erie_data$Extracted.Chlorophyll.a.b5g.L, 
                               Erie_data$Extracted.Phycocyanin.b5g.L, 
                               Erie_data$Total.Dissolved.Phosphorus.b5g.P.L,
                               Erie_data$Ammonia.b5g.N.L,
                               Erie_data$Nitrate..Nitrite.mg.N.L,
                               Erie_data$Urea.b5g.N.L)

colnames(Erie_data_subset) <- c("Date", "Depth_category", "Temp", "DO", "MC", "Chla", "PC", "TDP", "NH3", "NO23", "urea")
View(Erie_data_subset)

Erie_data_date_means <- Erie_data_subset %>%
  group_by(Date) %>%
  summarize(mean_chla = mean(Chla, na.rm=T),
            mean_temp = mean(Temp, na.rm=T),
            mean_mc = mean(MC,na.rm=T),
            mean_do = mean(DO,na.rm=T),
            mean_tdp = mean(TDP, na.rm=T),
            mean_no23 = mean(NO23, na.rm=T)) %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date))

View(Erie_data_date_means)

chla_plot <- ggplot() +
  geom_line(data=Erie_data_date_means, aes(x=Date, y=mean_chla))

chla_plot

temp_plot <- ggplot() +
  geom_line(data=Erie_data_date_means, aes(x=Date, y=mean_temp))

temp_plot

tdp_plot <- ggplot() +
  geom_line(data=Erie_data_date_means, aes(x=Date, y=mean_tdp))

tdp_plot

mc_plot <- ggplot() +
  geom_line(data=Erie_data_date_means, aes(x=Date, y=mean_mc))

mc_plot

  
  
Erie_data_month_means <- Erie_data_subset %>%
    mutate(month = month(Date)) %>%
    mutate(year = year(Date)) %>%
    group_by(year, month) %>%
    summarize(mean_chla = mean(Chla, na.rm=T),
              mean_temp = mean(Temp, na.rm=T),
              mean_mc = mean(MC, na.rm=T), 
              mean_do = mean(DO, na.rm=T), 
              mean_tdp = mean(TDP, na.rm=T),
              mean_no23 = mean(NO23, na.rm=T)) 
  
View(Erie_data_month_means)
  
chla_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_month_means, year=="2012"), aes(x=month, y=mean_chla),
            color="red") +
  geom_line(data=filter(Erie_data_month_means, year=="2013"), aes(x=month, y=mean_chla)) +
  geom_line(data=filter(Erie_data_month_means, year=="2014"), aes(x=month, y=mean_chla)) +
  geom_line(data=filter(Erie_data_month_means, year=="2015"), aes(x=month, y=mean_chla)) +
  geom_line(data=filter(Erie_data_month_means, year=="2016"), aes(x=month, y=mean_chla)) +
  geom_line(data=filter(Erie_data_month_means, year=="2017"), aes(x=month, y=mean_chla)) +
  geom_line(data=filter(Erie_data_month_means, year=="2018"), aes(x=month, y=mean_chla))

chla_plot_ext

temp_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_surface_ext, year=="2017"), aes(x=Date, y=mean_temp))

temp_plot_ext


