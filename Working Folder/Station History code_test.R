# Purpose: Code for staton history period of record trends and summary tables
# Created: 05/10/2023
# Author: Amanda Maguire amanda.maguire@water.ca.gov
# Revisions: 5/25/23 - streamline workflow to reduce edits & update to filter out calendar year 2023 water quality data

#Shortcut Key Codes
#Alt + 0176 = °   (degree symbol)
#Alt + 230 =  µ   (micro symbol)

#I. LIBRARIES---------
library(readxl)
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(scales)
library(dplyr)

#setting working directory
setwd("Y:/Stations/Station Information/Station Histories/Station Update Code/Working Folder")

#II. DATA IMPORT------------------------
#R will look in the WORKING DIRECTORY to find files. can find updated water year info here https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#WY Index import
wyt <- read_excel(
  path = "WY Type Indices.xlsx",
  sheet = "Sacramento Valley Index",
  col_names = TRUE)

#WY Index dataframe clean -- only need to clean once. Will reuse for each station.
SAC_WYIndex <- wyt %>% filter(WY%in%c(2014:2023)) %>% select(WY,Index,`Yr-type`) #Chane years to reflect the years the station has been collecting data
SAC_WYIndex[SAC_WYIndex == "BN"] <- "Normal"
SAC_WYIndex[SAC_WYIndex == "AN"] <- "Normal"
SAC_WYIndex[SAC_WYIndex == "D"] <- "Dry"
SAC_WYIndex[SAC_WYIndex == "C"] <- "Dry"
SAC_WYIndex[SAC_WYIndex == "W"] <- "Wet"
unique(SAC_WYIndex$`Yr-type`)


#Continuous WQ import
df_raw_L <- read_csv(
  file = "LIS_por_raw.csv",  #######*add file name----
  col_names = FALSE,     #add column names later
  skip = 3,              #skip top 3 rows
  col_types = "cdddddddddddd")  # "c" = character, "d" = numeric, "-" = skip

#III. DATA CLEAN & SUMMARIZE---------------------------
#Name the columns for the parameters your station has
names(df_raw_L) <- c(
  "DateTime",
  "Specific Conductance (µS/cm)", "SpCnd_q",
  "Water Temperature (°C)", "WT_q",
  "Turbidity (FNU)", "Turb_q",
  "Dissolved Oxygen (mg/L)", "DO_q",
  "pH units", "pH_q",
  "Chlorophyll Fluorescence (µg/L)", "Chla_q",
  "Depth (ft)", "Depth_q",
  "fDOM (QSU)", "fDOM_q")

df_clean_L <- df_raw_L %>% 
  mutate(DateTime = mdy_hm(DateTime)) %>% 
  mutate(DateTime = floor_date(DateTime, "15 minute")) %>%
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min")) %>%
  mutate(Date= as.Date(floor_date(DateTime, "day"))) %>%
  select(c(2,4,6,8,10,12,14,16,18)) %>%     ##update for the number of parameters. Need to add an extra column so with three parameters need columns 2 to 8
  filter(Date < '2024-09-30') %>% #update date
  pivot_longer(cols = c(1:8), names_to = "Parameter", values_to = "Value") %>%
  addWaterYear() %>% 
  rename(WY = waterYear) %>%
  mutate(StationCode = "LIS") %>%     #####*UPDATE station name------
  mutate(CDate=as.Date(paste0("1904","-",month(Date),"-",day(Date))))

#IV. CREATE DATA FRAME WITH CONTINUOUS WATER QUALITY AND WATER YEAR TYPE INDEX----
df_cleanWY_L <- full_join(df_clean_L,SAC_WYIndex) %>% 
  group_by(StationCode, Parameter, `Yr-type`, CDate) %>% 
  summarize(Daily_avg = mean(Value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #seq along dates starting with the beginning of the water year
  mutate(CDate=as.Date(paste0(ifelse(month(CDate) < 10, "1905", "1904"),
                              "-", month(CDate), "-", day(CDate))))


#V. CONTINUOUS FIGURES--------------------
## WY Type Continuous Trends--------------
WY_plots_L <- df_cleanWY_L %>% split(.$Parameter) %>% 
  map(~ ggplot(., aes(x = CDate, y = Daily_avg, color = `Yr-type`)) + 
        geom_line() +
        theme_bw() +
        scale_colour_manual(values = c("#ED7D31","#A5A5A5","#4472C4")) +
        labs(y = .$Parameter,
             x = "Average Date",
             colour = "WY Type") +
        scale_x_date(labels = date_format("%b"),
                     breaks = breaks_pretty(15)))

WY_plots_L

##Export-------- 
####*before saving, UPDATE station in filenames---------
#ggsave(WY_plots$`Chlorophyll Fluorescence (µg/L)` , filename = "ORI_chlaPORtrends.tiff", device = "tiff", width = 6.5, height = 3)
#ggsave(WY_plots$`Depth (ft)` , filename = "ORI_depthPORtrends.tiff", device = "tiff", width = 6.5, height = 3)
#ggsave(WY_plots$`Dissolved Oxygen (mg/L)` , filename = "ORI_doPORtrends.tiff", device = "tiff", width = 6.5, height = 3)
#ggsave(WY_plots$`pH units` , filename = "ORI_phPORtrends.tiff", device = "tiff", width = 6.5, height = 3)
#ggsave(WY_plots$`Specific Conductance (µS/cm)` , filename = "ORI_spcndPORtrends.tiff", device = "tiff", width = 6.5, height = 3)
#ggsave(WY_plots$`Turbidity (FNU)` , filename = "ORI_turbPORtrends.tiff", device = "tiff", width = 6.5, height = 3)
#ggsave(WY_plots$`Water Temperature (°C)` , filename = "ORI_wtPORtrends.tiff", device = "tiff", width = 6.5, height = 3)

## POR Histograms------------------------
histo_plot <- df_cleanWY_L %>% ggplot(., aes(x = Daily_avg)) + 
  facet_wrap(~Parameter, scales = 'free_x') +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
  theme_bw() +
  labs(x = "Daily Average")

histo_plot

####*before saving, UPDATE station in filename---------
#ggsave(histo_plot, filename = "ORI_histogram.tiff", device = "tiff", width = 6.5, height = 8.5)

#VI. CONTINUOUS SUMMARY TABLES------------
#Create the dataframe
df_cleanSUM <- full_join(df_clean_L,SAC_WYIndex) %>% 
  group_by(StationCode, Parameter, Date) %>% 
  summarize(Daily_avg = mean(Value, na.rm=TRUE)) %>% 
  ungroup()

#Calculate the summary table
df_sum_table <- df_cleanSUM %>% group_by(StationCode, Parameter) %>% 
  summarize(min = min(Daily_avg, na.rm = TRUE),
            max = max(Daily_avg, na.rm = TRUE),
            count = n(),                                          #check that count is correct
            mean = mean(Daily_avg, na.rm=TRUE),
            #median = median(Daily_avg, na.rm = TRUE),
            sd = sd(Daily_avg, na.rm=TRUE)) 

#Find max and min value occurrence dates
df_max <- df_cleanSUM %>% group_by(StationCode, Parameter) %>% slice_max(Daily_avg) %>% select(StationCode, Parameter, Date) %>% rename("max_date" = Date)
df_min <- df_cleanSUM %>% group_by(StationCode, Parameter) %>% slice_min(Daily_avg) %>% select(StationCode, Parameter, Date) %>% rename("min_date" = Date)

#Join tables
POR_sum_table <- full_join(full_join(df_sum_table,df_max), df_min) %>% 
  select(StationCode,
         Parameter,
         min,
         min_date,
         max,
         max_date,
         count,
         mean,
         #median,
         sd)

## Export---------
#####*before saving -- UPDATE station in the filename----
write_excel_csv(POR_sum_table, file = "POR_LIS_continuous_sum_table.csv", na = "NA")

