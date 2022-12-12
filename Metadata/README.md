# Project_Davis_Rathmell_Brentjens

## Summary
This data was used in the final project for Environmental Data Analytics (ENV 872L) at Duke University, Fall 2022. The data will be evaluated using multiple linear regression, time series analysis, and comparison of means (Wilcoxin Rank Sum) tests. 

## Investigators

Kallie Davis, MEM candidate at Duke University

Emma Brentjens, MEM candidate at Duke University

Dori Rathmell, MEM candidate at Duke University

## Keywords

Harmful algal blooms, microcystins, dissolved oxygen, chlorophyll a, nitrate, nitrite, phosphorus, Western Lake Erie, Great Lakes

## Database Information

The raw data are from Western Lake Erie water quality data collected by NOAA Great Lakes Environmental Research Laboratory (GLERL) between 2012 and 2018 and in 2022. Data was accessed on November 29, 2022 from the following website: https://www.glerl.noaa.gov/res/HABs_and_Hypoxia/habsMon.html 

## Folder structure, file formats, and naming conventions 

Folder structure

* Code: contains coding files for data wrangling and data analysis
* Data: contains separate subfolders for raw and processed data
* Metadata: contains metadata file for the repository
* Output: contains knit files (PDF format)

## Metadata

### Raw Data
lake_erie_habs_field_sampling_2012_2018_v2.csv contains the following parameters:

* Date in format month/day/year; character class
* Site is the location where sample was collected; character class
* Station Depth measured in meters; numeric class
* Sample Depth measured in meters; numeric class
* Sample Depth by category; character class
* Local Time in Eastern Time Zone; character class
* Latitude in decimal degree; numeric class
* Longitude in decimal degree; numeric class
* wind speed measured in knots; character class
* wave height measured in feet; numeric class
* weather conditions during sample collection; character class
* secchi depth measured in meters; character class
* Sample temperature measured in degrees celsius; numeric class
* CTD temperature measured in degrees celsius (CTD: Refers to an instrument which measures conductivity, temperature, and depth); numeric class
* CTD specific conductivity measured in microsiemens per meter; numeric class
* CTD beam attenuation measured in meters; character class
* CTD transmission measured as a percent; character class
* CTD dissolved oxygen measured in milligrams per liter; numeric class
* CTD photosynthetically active radiation measured in microeinsteins per second per square meter; numeric class
* turbidity measured in Nephelometric Turbidity unit; numeric class
* particulate microcystin measured in micrograms per liter; character class
* dissolved microcystin measured in micrograms per liter; character class
* extracted phycocyanin measured in micrograms per liter; character class
* extracted chlorophyll-a measured in micrograms per liter; numeric class
* total phosphorus measured in micrograms per liter; numeric class
* total dissolved phosphorus measured in micrograms per liter; numeric class
* soluble reactive phosphorus measured in micrograms per liter; character class
* ammonia measured in micrograms per liter; character class
* nitrate and nitrite measured in milligrams per liter; character class
* urea measured in micrograms per liter; character class
* particulate organic carbon measured in milligrams per liter; numeric class
* particulate organic nitrogen measured in milligrams per liter; numeric class
* dissolved organic carbon measured in milligrams per liter; numeric class
* colored dissolved organic material absorbance at 400 nm; numeric class
* total suspended solids measured in milligrams per liter; numeric class
* volatile suspended solids measured in milligrams per liter; numeric class

2022_WLE_Weekly_Datashare_CSV.csv

* Date in format month/day/year; character class
* Site is the location where sample was collected; character class
* Station Depth measured in meters; numeric class
* Sample Depth measured in meters; numeric class
* Sample Depth by category; character class
* Arrival_Time; character class
* Departure_Time; character class
* Lat_degree is latitude in decimal degree; numeric class
* Long_degree is longitude in decimal degree; numeric class
* wind_speed_knots is the wind speed measured in knots; character class
* Wave_Ht_ft is the wave height measured in feet; numeric class
* Sky reports  weather conditions during sample collection; character class
* Secchi_Depth_m is the secchi depth measured in meters; character class
* Temp_C is sample temperature measured in degrees celsius; character class
* SpCond_uScm-1 is specific conductivity measured in microsiemens per meter; character class
* BeamAtten_m-1 is beam attenuation measured in meters; character class
* Transmiss_pct is the transmission measured as a percent; character class
* DO_mgL-1 is dissolved oxygen measured in milligrams per liter; character class
* PAR_uEcm-2s-1 is photosynthetically active radiation measured in microeinsteins per second per square meter; character class
* Turbidity_NTU is turbidity measured in Nephelometric Turbidity unit; character class
* Particulate_Microcystin_ugL-1 is particulate microcystin measured in micrograms per liter; character class
* Dissolved_Microcystin_ugL-1 is dissolved microcystin measured in micrograms per liter; character class
* Extracted_PC_ugL-1 is extracted phycocyanin measured in micrograms per liter; character class
* Extracted_CHLa_ugL-1 is extracted chlorophyll-a measured in micrograms per liter; numeric class
* Total_cyanobacteria_genes_copiesmL-1 is the total number of cyanobacteria gene copies detected per mL of sample
*  mycE_genes_copiesmL-1 is the number of microcystis gene copies detected per mL of sample

### Processed Data

Erie_2012_2018_processed.csv 

* Date in format month/day/year; character class
* Site is the location where sample was collected; character class
* Sample Depth by category; character class
* Dissolved.Organic.Carbon.mg.L; numeric class
* Urea.b5g.N.L is urea concentration measured in micrograms per liter; numeric class
* Nitrate..Nitrite.mg.N.L is nitrate and nitrite concentrations measured in milligrams per liter; numeric class
* Ammonia.b5g.N.L is ammonia concentration measured in micrograms per liter; numeric class
* Soluble.Reactive.Phosphorus.b5g.P.L is soluble reactive phosphorus measured in micrograms per liter; numeric class
* Total.Dissolved.Phosphorus.b5g.P.L is total dissolved phosphorus measured in micrograms per liter; numeric class
* Total.Phosphorus.b5g.P.L is total phosphorus measured in micrograms per liter; numeric class
* Extracted.Chlorophyll.a.b5g.L is extracted chlorophyll a measured in micrograms per liter; numeric class
* Extracted.Phycocyanin.b5g.L is extracted phycocyanin measured in micrograms per liter; numeric class
* Dissolved.Microcystin.b5g.L is dissolved microcystin measured in micrograms per liter; numeric class
* CTD.Dissolved.Oxygen.mg.L is dissolved oxygen measured in micrograms per liter; numeric class
* Photo.active.radiation is photosynthetically active radiation measured in microeinsteins per second per square meter; numeric class
* Sample.Temperature.b0C is temperature of sample measured in degrees celsius; numeric class
* CTD.Temperature.b0C is temperature measured by  instrument which measures conductivity, temperature, and depth; numeric class

Erie_2022_processed.csv

* Date in format month/day/year; character class
* Depth_category is the sample depth category; character class
* Temp is the temperature measured in degrees celsius; numeric class
* DO is dissolved oxygen measured in milligrams per liter; numeric class
* MC is microcystin concentration measured in micrograms per liter; numeric class
* Chla is extracted chlorophyll a concentration measured in micrograms per liter; numeric class
* PC is extracted phycocyanin measured in micrograms per liter; numeric class

## Scripts and code

erie_habs_data_extended.R: an R code file used for wrangling the raw lake_erie_habs_field_sampling_2012_2018_v2.csv dataset; used to develop preliminary models and plots

exploratory_data_analysis.R: an R code file used for the exploratory data analysis for the report

extended_data_plots.R: an R code file used to create plots using the extended dataset (Erie_2012_2018_processed.csv)

Lake_Erie_GLMs.R: an R code file used to create generalized linear models

KD_Project_Template.Rmd: RMD file used to evaluate sample depth category analyses and compile report preliminarily

timeseries.Rmd: an RMD file used to conduct time series analysis

## Quality assurance/quality control

Data wrangling and changes to data file were inspected individually by group members.

