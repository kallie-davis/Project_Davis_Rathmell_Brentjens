##2012-2018 Data
Erie_Data_2012_2018 <- 
  read.csv("./Data/Raw/lake_erie_habs_field_sampling_results_2012_2018_v2.csv",
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

Erie_Data_2012_2018 <- Erie_Data_2012_2018 %>%
  filter(!Sample.Depth.category=="Scum")
View(Erie_Data_2012_2018)

#write.csv(Erie_Data_2012_2018, "./Data/Processed/Erie_2012_2018_processed.csv")
erie_extended_processed <- read.csv("./Data/Processed/Erie_2012_2018_processed.csv")
View(erie_extended_processed)

class(erie_extended_processed$Date)
erie_extended_processed$Date <- as.Date(erie_extended_processed$Date, format="%m/%d/%Y")

##data wrangling
erie_extended_processed <- erie_extended_processed %>%
  mutate(year = year(Date))


##?????
Erie_chla_lm_lt <- lm(data=erie_extended_processed, Extracted.Chlorophyll.a.b5g.L ~ CTD.Temperature.b0C)
summary(Erie_chla_lm_lt)

Erie_phyco_lm_lt <- lm(data=erie_extended_processed, Extracted.Phycocyanin.b5g.L ~ CTD.Temperature.b0C)
summary(Erie_phyco_lm_lt)

Erie_mc_lm_lt <- lm(data=erie_extended_processed, Dissolved.Microcystin.b5g.L ~ CTD.Temperature.b0C)
summary(Erie_mc_lm_lt) ##significant negative

Erie_do_lm_lt <- lm(data=erie_extended_processed, CTD.Dissolved.Oxygen.mg.L ~ CTD.Temperature.b0C)
summary(Erie_do_lm_lt) ##significant negative

Erie_par_lm_lt <- lm(data=erie_extended_processed, Photo.active.radiation ~ CTD.Temperature.b0C)
summary(Erie_par_lm_lt) ##significant positive

##nutrient models
##NO23
Erie_chla_n023_lm <- lm(data=erie_extended_processed, Extracted.Chlorophyll.a.b5g.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_chla_n023_lm)

#Erie_chla_n023_lm_2017 <- lm(data=filter(erie_extended_processed$year=="2012"), Extracted.Chlorophyll.a.b5g.L ~ Nitrate..Nitrite.mg.N.L)
#summary(Erie_chla_n023_lm)
###filter not working?

Erie_phyco_n023_lm <- lm(data=erie_extended_processed, Extracted.Phycocyanin.b5g.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_phyco_n023_lm)

Erie_mc_n023_lm <- lm(data=erie_extended_processed, Dissolved.Microcystin.b5g.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_mc_n023_lm)

Erie_do_n023_lm <- lm(data=erie_extended_processed, CTD.Dissolved.Oxygen.mg.L ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_do_n023_lm) ##significant negative

Erie_par_no23_lm <- lm(data=erie_extended_processed, Photo.active.radiation ~ Nitrate..Nitrite.mg.N.L)
summary(Erie_par_no23_lm) ##significant negative

##TDP
Erie_chla_tdp_lm <- lm(data=erie_extended_processed, Extracted.Chlorophyll.a.b5g.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_chla_tdp_lm)

Erie_phyco_tdp_lm <- lm(data=erie_extended_processed, Extracted.Phycocyanin.b5g.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_phyco_tdp_lm)

Erie_mc_tdp_lm <- lm(data=erie_extended_processed, Dissolved.Microcystin.b5g.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_mc_tdp_lm)

Erie_do_tdp_lm <- lm(data=erie_extended_processed, CTD.Dissolved.Oxygen.mg.L ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_do_tdp_lm) ##significant negative

Erie_par_tdp_lm <- lm(data=erie_extended_processed, Photo.active.radiation ~ Total.Dissolved.Phosphorus.b5g.P.L)
summary(Erie_par_tdp_lm) ##significant negative

##ammonia
Erie_chla_NH3_lm <- lm(data=Erie_Data_2012_2018, Extracted.Chlorophyll.a.b5g.L ~ Ammonia.b5g.N.L)
summary(Erie_chla_NH3_lm)

Erie_phyco_NH3_lm <- lm(data=Erie_Data_2012_2018, Extracted.Phycocyanin.b5g.L ~ Ammonia.b5g.N.L)
summary(Erie_phyco_NH3_lm)

Erie_mc_NH3_lm <- lm(data=Erie_Data_2012_2018, Dissolved.Microcystin.b5g.L ~ Ammonia.b5g.N.L)
summary(Erie_mc_NH3_lm)

#plots
mc_nitrate_plot <- ggplot(data=erie_extended_processed, aes(x=Nitrate..Nitrite.mg.N.L, y=Dissolved.Microcystin.b5g.L)) +
  geom_point() +
  geom_smooth(method=lm, se=F, color="#009E73") +
  xlab("NO23") + 
  ylab("mc") +
  ylim(0, 2)
mc_nitrate_plot

