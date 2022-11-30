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
Erie_data <- read.csv("./2022_WLE_Weekly_Datashare_CSV.csv",
                      stringsAsFactors = T)
View(Erie_data)



##data wrangling
# Erie_data <- Erie_data %>%
#   na.omit(Temp_C) %>%
#   na.omit(DO_mgL.1) %>%
#   na.omit(Dissolved_Microcystin_ugL.1) %>%
#   na.omit(Extracted_CHLa_ugL.1) %>%
#   na.omit(Extracted_PC_ugL.1)

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

Erie_data_subset <- data_frame(Erie_data$Sample_Depth_category, 
                               Erie_data$Temp_C, Erie_data$DO_mgL.1,
                          Erie_data$Dissolved_Microcystin_ugL.1,
                          Erie_data$Extracted_CHLa_ugL.1, 
                          Erie_data$Extracted_PC_ugL.1)

colnames(Erie_data_subset) <- c("Depth_category", "Temp", "DO", "MC", "Chla", "PC")

Erie_data_cor <- data_frame(Erie_data$Temp_C, Erie_data$DO_mgL.1,
                          Erie_data$Dissolved_Microcystin_ugL.1,
                          Erie_data$Extracted_CHLa_ugL.1, 
                          Erie_data$Extracted_PC_ugL.1)

colnames(Erie_data_cor) <- c("Temp", "DO", "MC", "Chla", "PC")

View(Erie_data_cor)

Erie_data_surface <- Erie_data_subset %>%
  filter(Depth_category=="Surface")

Erie_data_bottom <- Erie_data_subset %>%
  filter(Depth_category=="Bottom")



##GLMs
Erie_cor <- cor(Erie_data_cor)
corrplot(Erie_cor, method = "ellipse")
corrplot.mixed(Erie_cor, upper = "ellipse")

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
#Erie_Data_2012_2018$Date <- as.Date(Erie_Data_2012_2018$Date, format = "%d/%m/%Y")
##date conversion not working?

##?????
Erie_chla_lm_lt <- lm(data=Erie_Data_2012_2018, Extracted.Chlorophyll.a.b5g.L ~ CTD.Temperature.b0C)
summary(Erie_chla_lm_lt)

Erie_mc_lm_lt <- lm(data=Erie_Data_2012_2018, Dissolved.Microcystin.b5g.L ~ CTD.Temperature.b0C)
summary(Erie_mc_lm_lt) ##significant

Erie_do_lm_lt <- lm(data=Erie_Data_2012_2018, CTD.Dissolved.Oxygen.mg.L ~ CTD.Temperature.b0C)
summary(Erie_do_lm_lt) ##significant

Erie_par_lm_lt <- lm(data=Erie_Data_2012_2018, Photo.active.radiation ~ CTD.Temperature.b0C)
summary(Erie_par_lm_lt) ##significant

##nutrient models
##NO23
Erie_chla_n023_lm <- lm(data=Erie_Data_2012_2018, Extracted.Chlorophyll.a.b5g.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_chla_n023_lm)

Erie_phyco_n023_lm <- lm(data=Erie_Data_2012_2018, Extracted.Phycocyanin.b5g.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_phyco_n023_lm)

Erie_mc_n023_lm <- lm(data=Erie_Data_2012_2018, Dissolved.Microcystin.b5g.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_mc_n023_lm)

Erie_do_n023_lm <- lm(data=Erie_Data_2012_2018, CTD.Dissolved.Oxygen.mg.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_do_n023_lm) ##significant

Erie_par_no23_lm <- lm(data=Erie_Data_2012_2018, Photo.active.radiation ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_par_no23_lm) ##significant

#plots
mc_nitrate_plot <- ggplot(data=Erie_Data_2012_2018, aes(x=Nitrate..Nitrite.mg.N.L, y=Dissolved.Microcystin.b5g.L)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="#009E73") +
  xlab("NO23") + 
  ylab("mc") +
  ylim(0, 2)
mc_nitrate_plot

##TDP
Erie_chla_tdp_lm <- lm(data=Erie_Data_2012_2018, Extracted.Chlorophyll.a.b5g.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_chla_tdp_lm)

Erie_phyco_tdp_lm <- lm(data=Erie_Data_2012_2018, Extracted.Phycocyanin.b5g.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_phyco_tdp_lm)

Erie_mc_tdp_lm <- lm(data=Erie_Data_2012_2018, Dissolved.Microcystin.b5g.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_mc_tdp_lm)

##ammonia
Erie_chla_NH3_lm <- lm(data=Erie_Data_2012_2018, Extracted.Chlorophyll.a.b5g.L ~ Ammonia.b5g.N.L)
summary(Erie_chla_NH3_lm)

Erie_phyco_NH3_lm <- lm(data=Erie_Data_2012_2018, Extracted.Phycocyanin.b5g.L ~ Ammonia.b5g.N.L)
summary(Erie_phyco_NH3_lm)

Erie_mc_NH3_lm <- lm(data=Erie_Data_2012_2018, Dissolved.Microcystin.b5g.L ~ Ammonia.b5g.N.L)
summary(Erie_mc_NH3_lm)

