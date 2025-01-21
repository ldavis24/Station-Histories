# Purpose: Code for staton history period of record discrete sample summary tables
# Created: 05/11/2023
# Author: Amanda Maguire amanda.maguire@water.ca.gov
# Revisions:

#Shortcut Key Codes
#Alt + 0176 = °   (degree symbol)
#Alt + 230 =  µ   (micro symbol)

#I. LIBRARIES---------
library(readxl)
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(scales)
setwd("Y:/Stations/Station Information/Station Histories/Station Update Code/Working Folder")

#II. DATA IMPORT------------------------
#R will look in the WORKING DIRECTORY to find files. 
df_raw_dis <- read_csv(
  file = "LIS_por_discrete.csv",  #######*add file name----
  col_names = TRUE) 

#III. DATA CLEAN ---------------------------
#remove rows of metadata at bottom of df
df_dis <- head(df_raw_dis, -2)  #######*check data trim----   

df_clean_dis <- df_dis %>% 
  rename(DateTime = `CollectionDate`,
         StationCode = 'ShortStationName') %>%
  mutate(DateTime = mdy_hm(DateTime)) %>% 
  mutate(Date= as.Date(floor_date(DateTime, "day"))) %>% 
  mutate(Year = year(DateTime)) %>%
  #separate <RL data into new RL and value columns.
  mutate(RL = case_when(grepl("<", Result) ~ "<",
                        TRUE ~ "=")) %>% 
  mutate(value = case_when(grepl("<", Result) ~ `RptLimit`,
                           TRUE ~ as.numeric(Result))) %>% 
  relocate(c(RL,value), .before = Result) %>%
  #keep data through 2024
  filter(!Year==2025) %>%
  #remove duplicate samples
  filter(is.na(Notes)|Notes!="Duplicate") %>% distinct()
  
#IV. DISCRETE SUMMARY TABLES------------
##1. Calculate the summary table---------
df_dis_sum_table <- df_clean_dis %>% 
  group_by(StationCode,Analyte) %>% 
  summarize(total_count = n(),   
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm=TRUE)) 

##2. Find MAXIMUM Results and occurrence date(s)---------
df_dis_max <- df_clean_dis %>% 
  group_by(StationCode,Analyte) %>% 
  slice_max(value) %>% 
  select(Analyte,Result,Date,Year) %>% 
  rename("max_Result" = Result, "max_date" = Date)  

#check results
df_dis_max 
#there are multiple days that a maximum sample value occurred (see VSS, rows 8-10). 
#summarize data for first and last date of occurrence and number of occurences
max_data <- df_dis_max %>% group_by(StationCode,Analyte,max_Result) %>% summarize(first_max = min(max_date),
                                                                       last_max = max(max_date),
                                                                       n_max= n())

##3. Find MINIMUM Results and occurrence date(s)-----------
df_dis_min <- df_clean_dis %>% 
  group_by(StationCode,Analyte) %>% 
  slice_min(value) %>% 
  select(Analyte,Result,Date,Year) %>% 
  rename("min_Result" = Result, "min_date" = Date) 

#check results
df_dis_min %>% print(n=30)
#If needed Remove the higher values (1) from the results.
#df_dis_min <- df_dis_min %>% filter(!min_Result=="1") %>% print(n=24)
#there are multiple days that a minimum sample value occurred (see chla, pheoa, TSS, and VSS). 
#summarize data for first and last date of occurrence and number of occurences
min_data <- df_dis_min %>% group_by(StationCode,Analyte,min_Result) %>% summarize(first_min = min(min_date),
                                                                       last_min = max(min_date),
                                                                       n_min= n())

##4. Join all three summary tables---------
dis_sum_table_all <- full_join(full_join(min_data,max_data), df_dis_sum_table)

##5. Export---------
#####*before saving -- UPDATE station in the file name----
write_excel_csv(dis_sum_table_all, file = "POR_LIS_discrete_sum_table.csv", na = "NA")


