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

# Days <- as.data.frame(seq(as.Date("2012-03-01"), as.Date("2018-11-30"), "days"))
# colnames(Days)[1] <- "Date"
# 
# Erie_data_subset2 <- left_join(Days, Erie_data_date_means, by = "Date")
# View(Erie_data_subset2)

chla_plot <- ggplot() +
  geom_point(data=Erie_data_date_means, aes(x=Date, y=mean_chla),
             size=0.75)

chla_plot

Erie_data_date_means2 <- Erie_data_subset %>%
  filter(Chla < 6000) %>%
  group_by(Date) %>%
  summarize(mean_chla = mean(Chla),
            mean_temp = mean(Temp),
            mean_mc = mean(MC),
            mean_do = mean(DO),
            mean_tdp = mean(TDP),
            mean_no23 = mean(NO23)) %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date))
View(Erie_data_date_means2)

Erie_data_date_means3 <- rbind(Erie_data_date_means2[1:15, ],
                               NA,
                               Erie_data_date_means2[16:37, ], 
                               NA,
                               Erie_data_date_means2[38:58, ],
                               NA,
                               Erie_data_date_means2[59:81, ],
                               NA,
                               Erie_data_date_means2[82:100, ],
                               NA,
                               Erie_data_date_means2[101:122, ],
                               NA,
                               Erie_data_date_means2[123:140, ])

Date_2012 <- as.Date("12/31/2012", format = "%m/%d/%Y")
#Date_2012 <- year(Date_2012)

Date_2013 <- as.Date("12/31/2013", format = "%m/%d/%Y")
#Date_2013 <- year(Date_2013)

Date_2014 <- as.Date("12/31/2014", format = "%m/%d/%Y")
#Date_2014 <- year(Date_2014)

Date_2015 <- as.Date("12/31/2015", format = "%m/%d/%Y")
#Date_2015 <- year(Date_2015)

Date_2016 <- as.Date("12/31/2016", format = "%m/%d/%Y")
#Date_2016 <- year(Date_2016)

Date_2017 <- as.Date("12/31/2017", format = "%m/%d/%Y")
#Date_2017 <- year(Date_2017)

Erie_data_date_means3[16, 1] <- Date_2012
Erie_data_date_means3[39, 1] <- Date_2013
Erie_data_date_means3[61, 1] <- Date_2014
Erie_data_date_means3[85, 1] <- Date_2015
Erie_data_date_means3[105, 1] <- Date_2016
Erie_data_date_means3[128, 1] <- Date_2017

View(Erie_data_date_means3)


chla_plot <- ggplot() +
  geom_line(data= Erie_data_date_means3, aes(x=Date, y=mean_chla),
            na.rm=T) +
  ylab(expression("Chlorophyll a" ~ "("*mu*"g/L)")) +
  xlab("Year")

chla_plot

temp_plot <- ggplot() +
  geom_line(data=Erie_data_date_means3, aes(x=Date, y=mean_temp),
            na.rm=T) +
  ylab(expression("Temperature" ~ "("*degree*C*")")) +
  xlab("Year")

temp_plot

tdp_plot <- ggplot() +
  geom_line(data=Erie_data_date_means3, aes(x=Date, y=mean_tdp),
            na.rm=T)

tdp_plot

mc_plot <- ggplot() +
  geom_line(data=Erie_data_date_means3, aes(x=Date, y=mean_mc),
            na.rm=T)

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

Erie_data_2022 <- read.csv("./Data/Processed/Erie_2022_processed.csv")
View(Erie_data_2022)

class(Erie_data_2022$Date)
Erie_data_2022$Date <- as.Date(Erie_data_2022$Date, format="%m/%d/%Y")

Erie_data_2022_means <- Erie_data_2022 %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, month) %>%
  summarize(mean_chla = mean(Chla, na.rm=T),
            mean_temp = mean(Temp, na.rm=T),
            mean_mc = mean(MC, na.rm=T),
            mean_do = mean(DO, na.rm=T))
  
chla_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_month_means, year=="2012"), aes(x=month, y=mean_chla),
            color="black", linewidth=0.75) +
  geom_line(data=filter(Erie_data_month_means, year=="2014"), aes(x=month, y=mean_chla),
            color="tomato", linewidth=0.75) +
  geom_line(data=filter(Erie_data_month_means, year=="2018"), aes(x=month, y=mean_chla),
            color="#009E73", linewidth=0.75) +
  geom_line(data=Erie_data_2022_means, aes(x=month, y=mean_chla),
            color="#0072B2",linewidth=0.75)

Erie_data_month_means2 <- Erie_data_subset %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(month) %>%
  summarize(mean_chla = mean(Chla, na.rm=T),
            mean_temp = mean(Temp, na.rm=T),
            mean_mc = mean(MC, na.rm=T), 
            mean_do = mean(DO, na.rm=T), 
            mean_tdp = mean(TDP, na.rm=T),
            mean_no23 = mean(NO23, na.rm=T))

chla_plot_ext <- ggplot() +
  geom_line(data=Erie_data_month_means2, aes(x=month, y=mean_chla),
            color="black", linewidth=0.75)

chla_plot_ext


temp_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_month_means, year=="2012"), aes(x=month, y=mean_temp),
            color="black") +
  geom_line(data=filter(Erie_data_month_means, year=="2015"), aes(x=month, y=mean_temp),
            color="tomato") +
  geom_line(data=filter(Erie_data_month_means, year=="2017"), aes(x=month, y=mean_temp),
            color="#009E73") +
  geom_line(data=filter(Erie_data_2022_means), aes(x=month, y=mean_temp),
            color="#0072B2")

temp_plot_ext

mc_plot_ext <- ggplot() +
  geom_line(data=filter(Erie_data_month_means, year=="2012"), aes(x=month, y=mean_mc),
            color="black",linewidth=0.75) +
  geom_line(data=filter(Erie_data_month_means, year=="2015"), aes(x=month, y=mean_mc),
            color="tomato",linewidth=0.75) +
  geom_line(data=filter(Erie_data_month_means, year=="2017"), aes(x=month, y=mean_mc),
            color="#009E73", linewidth=0.75) +
  geom_line(data=filter(Erie_data_2022_means), aes(x=month, y=mean_mc),
            color="#0072B2", linewidth=0.75)

mc_plot_ext


