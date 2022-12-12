# Libraries ---------------------------------------------------------------

library(tidyverse)   
library(data.table)
library(viridis)
library(forcats)  
library(zipcodeR)  #Zip State Map
library(numform)   #Padding 
library(ggforce)   #Facet Wrap Paginate
library(ggridges)  #Ridge plots
library(knitr)     #R Markdown
library(ggcorrplot)#Correlation Heatmaps
library(caret)     #Correlation Filtering
library(car)       #Multicollinarity
library(ggfortify) #Plotting Principal Components
library(lubridate) #For working with dates

#Discourage Scientific Notation
options(scipen=999)

#Set Working Directory
setwd("C:/Users/Eric_/Desktop/State Farm DS Exercise")



# Load in Data ------------------------------------------------------------

Claim_Data_raw <- read_csv("Claim_Data.csv", 
                           col_types = cols(clm_dates = col_date(format = "%m/%d/%Y")))

Predictor_Dataset_raw <- read_csv("Predictor_Dataset.csv")

Subsequent_Loss_Experience_raw <- read_csv("Subsequent_Loss_Experience.csv")



# 1. Prompt 1 - Aggregate Dataset 1 ---------------------------------------

Hist_Claim_Count <- Claim_Data_raw %>%
  
  #Filter to 5 years from 1/1/2017 (i.e. 1/1/2012 - 1/1/2017)
  filter(
    clm_dates >= '2012-01-01',
    clm_dates < '2017-01-01'
  ) %>%
  
  #Identify and Extract Accident Year (AY)
  mutate(Acc_Yr = year(clm_dates)) %>%
  
  #Rename at fault indicator to string (helpful for column titles later)
  mutate(At_Fault_Ind =ifelse (aft_ind == 1,"_f","_nf"),
         .keep = "unused") %>%
  
  #Group by Household ID, AY and At-Fault Indicator
  group_by(hhld_id,Acc_Yr,At_Fault_Ind) %>%
  
  #Count number of At Fault / Not At Fault claims
  summarise(Claim_Count = n()) %>%
  
  #Arrange by AY
  arrange(Acc_Yr) %>%
  
  #Pivot Wider such that each Year/Indicator combination is its own column
  pivot_wider(
    id_cols = hhld_id,
    names_from = c(Acc_Yr,At_Fault_Ind),
    values_from = Claim_Count,
  )  %>% 
  
  #Replace NAs with 0 (NAs generated since not all households have claims each year
  replace(is.na(.),0) %>%
  
  #Arrange by Household ID
  arrange(hhld_id) %>%
  
  #View results
  view("Prompt 1: Aggregated Dataset 1")


colnames(Hist_Claim_Count)
# 2. Prompt 2 - Merge Datasets 1 (aggregated),2, and 3 ------------------------------------


Combined_Data_raw <- 
  #1. Join Datasets 1(aggregated) and 2
  left_join(
    Predictor_Dataset_raw,
    Hist_Claim_Count,
    by = "hhld_id"
    
  ) %>%  
  
  #Cheat a little to remove NAs from Left Join:
  #1) Drop columns from Predictor Dataset with NAs
  #2) Remove NAs from entire dataframe 
      #(this will remove NAs from dataset 1, i.e. households without historical losses)
  #3) Re-join NA columns from Predictor Dataset
  select(-c(veh_lien_cnt,prior_bi,time_w_carr)) %>%
  
  replace(is.na(.),0) %>%
  
  left_join(
    Predictor_Dataset_raw %>% select(hhld_id,c(veh_lien_cnt,prior_bi,time_w_carr)),
    by = c("hhld_id")
    ) %>%
  
  #2. Join on Dataset 3 
  inner_join(
    Subsequent_Loss_Experience_raw,
    by = "hhld_id"
  ) %>%
  
  view("Prompt 2: Merge Datasets 1-3")