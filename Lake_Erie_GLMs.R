##load packages
library(tidyverse)
library(ggplot2)
#install.packages("corrplot")
library(corrplot)



##setting ggplot theme
ggplot_theme <- theme_linedraw(base_size = 12) +
  theme(panel.grid=element_line(colour="gray"),
        axis.text = element_text(color = "black"), 
        legend.position = "top")
theme_set(ggplot_theme)



##load data
Erie_data <- read.csv("./Data/Raw/2022_WLE_Weekly_Datashare_CSV.csv")
View(Erie_data)



##data wrangling
# Erie_data <- Erie_data %>%
#   na.omit(Temp_C) %>%
#   na.omit(DO_mgL.1) %>%
#   na.omit(Dissolved_Microcystin_ugL.1) %>%
#   na.omit(Extracted_CHLa_ugL.1) %>%
#   na.omit(Extracted_PC_ugL.1)

class(Erie_data$Temp_C)
Erie_data$Temp_C <- as.numeric(Erie_data$Temp_C)

class(Erie_data$DO_mgL.1)
Erie_data$DO_mgL.1 <- as.numeric(Erie_data$DO_mgL.1)

class(Erie_data$Dissolved_Microcystin_ugL.1)
Erie_data$Dissolved_Microcystin_ugL.1 <- 
  as.numeric(Erie_data$Dissolved_Microcystin_ugL.1)

class(Erie_data$Extracted_CHLa_ugL.1)

class(Erie_data$Extracted_PC_ugL.1)
Erie_data$Extracted_PC_ugL.1 <- as.numeric(Erie_data$Extracted_PC_ugL.1)

Erie_data_subset <- data_frame(Erie_data$Sample_Depth_category, 
                               Erie_data$Temp_C, Erie_data$DO_mgL.1,
                          Erie_data$Dissolved_Microcystin_ugL.1,
                          Erie_data$Extracted_CHLa_ugL.1, 
                          Erie_data$Extracted_PC_ugL.1)

colnames(Erie_data_subset) <- c("Depth_category", "Temp", "DO", "MC", "Chla", "PC")

write.csv(Erie_data_subset, "./Data/Processed/Erie_2022_processed.csv")
Erie_data_processed <- read.csv("./Data/Processed/Erie_2022_processed.csv")
View(Erie_data_processed)

Erie_data_cor <- data_frame(Erie_data_processed$Temp, 
                            Erie_data_processed$DO,
                          Erie_data_processed$MC,
                          Erie_data_processed$Chla, 
                          Erie_data_processed$PC)

colnames(Erie_data_cor) <- c("Temp", "DO", "MC", "Chla", "PC")

View(Erie_data_cor)

Erie_data_surface <- Erie_data_processed %>%
  filter(Depth_category=="Surface")

Erie_data_bottom <- Erie_data_processed %>%
  filter(Depth_category=="Bottom")



##GLMs
Erie_cor <- cor(Erie_data_cor)
corrplot(Erie_cor, method = "ellipse")
corrplot.mixed(Erie_cor, upper = "ellipse")

###change to processed data
Erie_chla_lm <- lm(data=Erie_data, Extracted_CHLa_ugL.1 ~ Temp_C)
summary(Erie_chla_lm)

Erie_chla_s_lm <- lm(data=Erie_data_surface, Chla ~ Temp)
summary(Erie_chla_s_lm)

Erie_chla_b_lm <- lm(data=Erie_data_bottom, Chla ~ Temp)
summary(Erie_chla_b_lm)


Erie_mc_lm <- lm(data=Erie_data, Dissolved_Microcystin_ugL.1 ~ Temp_C)
summary(Erie_mc_lm)

Erie_mc_s_lm <- lm(data=Erie_data_surface, MC ~ Temp)
summary(Erie_mc_s_lm)

Erie_mc_b_lm <- lm(data=Erie_data_bottom, MC ~ Temp)
summary(Erie_mc_b_lm)


Erie_do_lm <- lm(data=Erie_data, DO_mgL.1 ~ Temp_C)
summary(Erie_do_lm)

Erie_do_s_lm <- lm(data=Erie_data_surface, DO ~ Temp)
summary(Erie_do_s_lm)

Erie_do_b_lm <- lm(data=Erie_data_bottom, DO ~ Temp)
summary(Erie_do_b_lm)


##surface vs. bottom comparison
surface_bottom_temp <- t.test(data=Erie_data_subset, Temp ~ Depth_category)
surface_bottom_temp


surface_bottom_chla <- t.test(data=filter(Erie_data_subset, !Depth_category=="Scum"), Chla ~ Depth_category)
surface_bottom_chla

chla_boxplot <- ggplot(data=filter(Erie_data_subset, !Depth_category=="Scum"), aes(x=Depth_category, y=Chla)) +
  geom_boxplot()
chla_boxplot

surface_bottom_mc <- t.test(data=filter(Erie_data_subset, !Depth_category=="Scum"), MC ~ Depth_category)
surface_bottom_mc

surface_bottom_do <- t.test(data=filter(Erie_data_subset, !Depth_category=="Scum"), DO ~ Depth_category)
surface_bottom_do

do_boxplot <- ggplot(data=filter(Erie_data_subset, !Depth_category=="Scum"), aes(x=Depth_category, y=DO)) +
  geom_boxplot()
do_boxplot


##plotting data
chla_plot <- ggplot(data=Erie_data, aes(x=Temp_C, y=Extracted_CHLa_ugL.1)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="#009E73") +
  ylim(0, 300) +
  #scale_y_continuous(trans='log10') +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))
chla_plot

chla_s_plot <- ggplot(data=Erie_data_surface, aes(x=Temp, y=Chla)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="#009E73") +
  ylim(0, 300) +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab(expression("Chlorophyll a" ~ "("*mu*"g/L)"))
chla_s_plot


mc_plot <- ggplot(data=Erie_data, aes(x=Temp_C, y=Dissolved_Microcystin_ugL.1)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="tomato") +
  ylim(0, 1.25) +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab(expression("Dissolved microcystin" ~ "("*mu*"g/L)")) +
  ggtitle("Dissolved Microcystin Concentration by Temperature")
mc_plot

mc_s_plot <- ggplot(data=Erie_data_surface, aes(x=Temp, y=MC)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="tomato") +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab(expression("Dissolved microcystin" ~ "("*mu*"g/L)")) 
mc_s_plot


do_plot <- ggplot(data=Erie_data, aes(x=Temp_C, y=DO_mgL.1)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="deepskyblue3") +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab("Dissolved oxygen (mg/L)")
do_plot

do_s_plot <- ggplot(data=Erie_data_surface, aes(x=Temp, y=DO)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="deepskyblue3") +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab("Dissolved oxygen (mg/L)")
do_s_plot

do_b_plot <- ggplot(data=Erie_data_bottom, aes(x=Temp, y=DO)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="deepskyblue3") +
  xlab(expression("Temperature" ~ "("*degree*C*")")) + 
  ylab("Dissolved oxygen (mg/L)")
do_b_plot