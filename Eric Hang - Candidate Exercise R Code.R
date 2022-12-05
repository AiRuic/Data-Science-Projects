# Libraries ---------------------------------------------------------------

library(tidyverse)   
library(data.table)
library(ggridges) 
library(ggforce)
library(viridis)
library(scales) 
library(knitr) 
library(numform) 
library(forecast)

#Discourage Scientific Notation
options(scipen=999)

#Increase memory
memory.limit(size = 10000000)

#Set Working Directory
setwd("C:/Users/i31294/OneDrive - Verisk Analytics/Desktop/A Temp Folder")



# Read in Data ------------------------------------------------------------

#Data 1 - Loss Run Containing Data for Policy 123
Data1_Raw <- read_csv("dataset1.csv",col_types = cols(claim_number = col_character())) %>%
  
  #Convert from String to DateTime Format
  mutate(
    loss_date = as.Date(loss_date,format= "%m/%d/%Y %H:%M"),
    evaluation_date = as.Date(evaluation_date,format= "%m/%d/%Y %H:%M"),
    
  ) 


#Data 2 - Loss Run Containing Data for Policy 456
Data2_Raw <- read_csv("dataset2.csv", col_types = cols(`Claim Number` = col_character(), 
                                                       `Loss Date` = col_date(format = "%m/%d/%Y"), 
                                                       `Evaluation Date` = col_date(format = "%m/%d/%Y")))


#Data 3 - Exposure Data
Data3_Raw <- read_csv("dataset3.csv", col_types = cols(c.distance_miles_p3 = col_double(), 
                                                       c.trip_fare = col_double())) %>%
  
  #Convert from String to DateTime Format
  mutate(
    a.begintrip_timestamp_local = as.POSIXct(a.begintrip_timestamp_local,format= "%m/%d/%Y %H:%M"),
    a.endtrip_timestamp_local = as.POSIXct(a.endtrip_timestamp_local,format= "%m/%d/%Y %H:%M"),
    
  ) 

# First Impressions of Data -----------------------------------------------

view(Data1_Raw, "Data 1 - Policy 123 Losses")
summary(Data1_Raw)

#Initial Thoughts:
## Transaction level data
## Is Claimant Name personally identifying information? It may be best to remove for legal/privacy purposes
## Reserve Loss refers to Case Reserves, or Total Loss Reserves (Case + IBNR) - Based on evaluation data, assume it is Total Loss Reserves
## Losses date from 4/2018 - 12/2021
## Eval date ranges from 4/2018 - 6/2022, and are typically end of month
## Regarding Loss/Eval Dates - Hr/Minute detail is not provided
## What is feature identifier? Some claims are only differentiated by feature identifier (loss = 0, and loss > 0)
## Should only cover period 3 losses (on trip)

## Other Questions:
## $1m Limit per Occ - it appears losses are censored from above (i.e. capped). Full Ground-Up Losses are presumably unknown?
## Deductible? 
## Assume not all claims are not fully developed


view(Data2_Raw, "Data 2 - Policy 456 Losses")
summary(Data2_Raw)

#Initial Thoughts:
## Not as many columns as Data 1 - perhaps the most important missing column is Coverage
## Feature Identifier is also missing - it may be needed as a Join Key to another data set
## Losses date from 5/2018 - 12/2021
## Eval date ranges from 5/2018 - 6/2022
## Loss significantly higher than Policy 123 (e.g. Mean Paid Loss $512,381 vs $53,363)
## (Makes sense, given the layer of coverage is $1M X $1M)


view(Data3_Raw, "Data 3 - Exposure Data")
summary(Data3_Raw)

#Initial Thoughts:
## Source Tables a,b,c not provided 
## Provides detailed trip level data (risk segment, status, time stamp, distances, fares)
## Trip dates between 6/2019 - 3/2022
## 16 NAs in timestamp (begin/end)
## 21 NAs in Distance/Trip_Fare



# Data1 - Exploratory Data Analysis ----------------------------------------------

#How many unique Claims?
unique(Data1_Raw$claim_number) 
Data1_Raw$claim_number %>% unique() %>% length()



#How many unique Claimants? Are there any multi-claimant losses?
Data1_Raw %>% group_by(claim_number) %>% 
  
  summarise(Num_Claimants = n_distinct(claimant_name)) %>% 
  
  arrange(desc(Num_Claimants))%>%
  
  view("# of Claimants per Claim")

#Most (63/84) Claims have multiple claimants



#Dive a little deeper into period/coverage of these claims
Data1_Raw %>% group_by(claim_number,period,coverage) %>% 
  
  summarise(Num_Claimants = n_distinct(claimant_name)) %>% 
  
  arrange(desc(Num_Claimants))%>%
  
  ggplot()+
  
  geom_histogram(aes(x=Num_Claimants,fill = coverage),position = "dodge")+
  
  scale_x_continuous(limits=c(1,7),breaks = seq(0,7,by=1))+
  
  scale_fill_viridis_d("Coverage")+
  
  labs(
    title = "Policy 123 - # of Claimants per Claim",
    x = "# of Claimants",
    y = "# of Claims"
  )+
  
  theme_bw()


#View in Tabular Form
Data1_Raw %>% group_by(claim_number,period,coverage) %>% 
  
  summarise(Num_Claimants = n_distinct(claimant_name)) %>% 
  
  arrange(desc(Num_Claimants))%>% 
  
  view("# of Claimants per Claim")

#BI + Transporting Rider appears to have more multi-claimant losses (Which makes sense, if driver + passengers are injured in an accident)
#Common for the same loss event to trigger both BI + PD coverages (again, makes sense)



#Understanding the different coverage types
Data1_Raw$coverage %>% unique()

#Coverage Definitions (From Research)
# BI - (Liability) Covers 3rd Party Medical Bills for injuries caused by the (Uber) driver
# PD - (Liability) Covers 3rd Party Property Damage cased by the (Uber) driver
# UIMBI - Underinsured Motorist Bodily Injury (liability) covers injuries caused by an UIM to the (Uber) driver and passengers
# UMBI - Uninsured Motorist Bodily Injury (liability) covers injuries caused by an UIM to the (Uber) driver and passengers 
# No Coverage - No Coverage Provided (e.g. (Uber) driver was not at fault)  -- Not Sure
# Pending Coverage - Coverage is still TBD  -- (Not sure)


#**Unclear whether BI covers injuries to rideshare passengers (as opposed to MedPay/PIP). 
#*Assuming that BI covers Uber passengers


#Claimant Name contains some "unknowns" and "??? ???", but it shouldn't really affect anything (for this analysis)

#Periods should only include P3 (On Trip)
Data1_Raw$period %>% unique()

# Cleaning Data 1 ---------------------------------------------------------

Maturity_levels = c("3m","6m","9m","1y","2y","3y","4y","4y+")

Data1_v1 <- Data1_Raw %>%
  
  group_by(claim_number,claimant_name,coverage,loss_date,period) %>%
  
  mutate(
    #Calculate Incurred Losses ($1M Limit)
    Inc_Loss = paid_loss + reserve_loss,
    
    #Calculate the portion of losses that would have been ceded (250k - 1.5M)
    Ceded_loss = pmin(pmax(0,Inc_Loss-250000),1250000),
    
    #Flag Losses which would trigger reinsurance (250k-1.5M layer)
    Reins_Flag = ifelse(Inc_Loss > 250000,1,0),
    
    #Calculate Maturity (in months)
    Maturity = round((evaluation_date - loss_date)/30,4),
    
    #Bucket Maturities into 3m, 6m, 9m, 12m/1yr, 2yr, 3yr, 4y+
    Maturity_Bucket = factor(
      case_when(
      Maturity <= 3 ~ "3m",
      Maturity <= 6 ~ "6m",
      Maturity <= 9 ~ "9m",
      Maturity <= 12 ~ "1y",
      Maturity <= 24 ~ "2y",
      Maturity <= 36 ~ "3y",
      Maturity <= 48 ~ "4y",
      TRUE ~ "4y+"),
      levels = Maturity_levels,
      ordered = TRUE),    #For Reference, PL BI is probably 95%+ developed at 5 years
    
    #Flag the latest evaluation date
    Most_Recent_Flag = ifelse(evaluation_date == max(evaluation_date),1,0),
    
    #Flag the first evaluation date
    First_Eval_Flag = ifelse(evaluation_date == min(evaluation_date),1,0),
    
    #Extract Accident Year
    Acc_Yr = year(loss_date)
    
    ) %>%
    
   view("Cleaned Data 1")

#Happy with this level of data quality
Data1_Clean <- Data1_v1

# Dataset 1 - Exploratory Data Analysis Continued ------------------------------------------------



#Distribution of Incurred Losses (as of latest Eval Date)
Data1_Clean %>%
  filter(Most_Recent_Flag ==1) %>% 
  
  group_by(claim_number,coverage,loss_date,feature_identifier,period) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=Inc_Loss,y=(..count..)/sum(..count..)),fill = "darkslateblue",binwidth=100000) +
  
  #Add Line for Reinsurance threshold
  geom_vline(xintercept=250000, linetype = "dashed",color="red")+
  
  labs(
    title = "Policy 123 - Distribution of Incurred Loss",
    subtitle = "As of Latest Evaluation Date, all Coverages and Periods",
    x = "Incurred Loss",
    y = "% of Claims"
  )+
  
  scale_y_continuous(labels=percent) +
  
  theme_bw()



#Distribution of Incurred Losses (as of latest Eval Date), by Coverage and Period
Data1_Clean %>%
  filter(Most_Recent_Flag ==1) %>% 
  
  group_by(claim_number,coverage,loss_date,feature_identifier,period) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=Inc_Loss,y=(..count..)/sum(..count..)),fill = "darkslateblue",binwidth=100000) +
  
  #Add Line for Reinsurance threshold
  geom_vline(xintercept=250000, linetype = "dashed",color="red")+
  
  facet_grid(coverage~period)+
  
  labs(
    title = "Policy 123 - Distribution of Incurred Loss",
    subtitle = "As of Latest Evaluation Date, by Coverage and Period",
    x = "Incurred Loss",
    y = "% of Claims"
  )+
  
  scale_y_continuous(labels=percent) +
  
  theme_bw()
# Dataset 1 - Clarifying Questions ----------------------------------------


#Why are there losses paid for non-Period 3 on Policy 123? (Only supposed to cover P3)
#Is there a special reason those losses were covered?
Data1_v1 %>%
  
  filter(period != "Transporting Rider") %>%
  
  #filter(Most_Recent_Flag ==1) %>% 
  
  filter(Inc_Loss > 0) %>%
  
  view("Policy 123 Non-Period 3 Losses at Latest Eval")


  
#What is Feature ID?
#Scenarios where all other fields are identical, except Feature ID
# I believe losses are different as well

Data1_v1 %>%  
  
  group_by(claim_number,claimant_name,coverage,period,loss_date) %>%
  
  select(feature_identifier) %>%
  
  summarise(n_feature_ID = n_distinct(feature_identifier)) %>% 
  
  filter(n_feature_ID  > 1) %>%
  
  view("Policy 123 - Number of Feature ID")

  
  
  
  
## $1m Limit per Occ - it appears losses are censored from above (i.e. capped). Full Ground-Up Losses are presumably unkown?
# Are these limited losses at $1M? i.e. is the data censored from above?


#Does the same $1M limit apply to all coverages?
#BI typically has a per person as well as a per accident/occurrence limit. Is there a per-person limit in addition to the $1M
#per accident limit?
  
#Losses are censored from above at limit of $1M. Full Ground-Up losses are unknown  
Data1_v1 %>%
  
  filter(Most_Recent_Flag ==1) %>%
  
  filter(Inc_Loss >=1000000) %>%
  
  view("Policy 123 $1M Capped Losses")




# Dataset 2 - Exploratory Data Analysis ---------------------------------------


Data2_Raw$Period %>% unique()


#How many unique Claims?
unique(Data2_Raw$`Claim Number`) 
Data2_Raw$`Claim Number` %>% unique() %>% length()



#again, why does it include P0 and P1?
unique(Data2_Raw$Period)

# Cleaning Data 2 --------------------------------------------------------

Data2_v1 <- Data2_Raw %>%
  
  #Rename to remove spaces in column titles, and for consistency with Data1
  rename(
    claim_number = `Claim Number`,
    loss_date = `Loss Date`,
    evaluation_date = `Evaluation Date`,
    paid_loss = `Paid Loss`,
    reserve_loss = `Reserve Loss`
  ) %>%
  
  group_by(claim_number,loss_date,Period) %>%
  
  mutate(
    #Calculate Incurred Losses ($1M XS $1M)
    Inc_Loss = paid_loss + reserve_loss,
    
    #Calculate the portion of losses that would have been ceded (250k - 1.5M)
    Ceded_Loss = pmin(500000,Inc_Loss),
    
    #Flag Losses which would trigger reinsurance (250k-1.5M layer)
    #Reins_Flag = ifelse(Inc_Loss > 250000,1,0), #Not needed - since all losses trigger
    
    #Calculate Maturity (in months)
    Maturity = round((evaluation_date - loss_date)/30,4),
    
    #Bucket Maturities into 3m, 6m, 9m, 12m/1yr, 2yr, 3yr, 4y+
    Maturity_Bucket = factor(
      case_when(
        Maturity <= 3 ~ "3m",
        Maturity <= 6 ~ "6m",
        Maturity <= 9 ~ "9m",
        Maturity <= 12 ~ "1y",
        Maturity <= 24 ~ "2y",
        Maturity <= 36 ~ "3y",
        Maturity <= 48 ~ "4y",
        TRUE ~ "4y+"),
      levels = Maturity_levels,
      ordered = TRUE ),    #For Reference, PL BI is 95%+ developed at 5 years
    
    #Flag the latest evaluation date
    Most_Recent_Flag = ifelse(evaluation_date == max(evaluation_date),1,0),
    
    #Flag the first evaluation date
    First_Eval_Flag = ifelse(evaluation_date == min(evaluation_date),1,0),
    
    #Extract Accident Year
    Acc_Yr = year(loss_date)
    
    )
  


#Seeing some negative maturities!!
#Either Loss Date or Evaluation Date entered incorrectly - probably incorrect loss date
#(since only 1 date), and then incorrect loss date carried forward at each subsequent evaluation
#Claim Number: 27193310 ... However, it is a large loss ($1M) - let's try to keep it in the data
Data2_v1 %>%
  
  filter(Maturity < 0) %>% 
  
  view("Negative Maturities")


#On average, how long from loss to first evaluation date?
Data2_v1 %>%
  
  filter(First_Eval_Flag==1) %>% 
  
  filter(Maturity >= 0) %>%
  
  group_by(Period) %>%
  
  summarise(Avg_Maturity = mean(Maturity),
            Count = n()) %>%
  
  view("Avg Time from Loss to First Evaluation")


#Losses are typically evaluated within 5 months of loss date for Period 2 losses
#Impute a loss date of first evaluation date - 5 months =  1/31/2020 - 5 months = 8/30/2019
Data2_v2 <- Data2_v1 %>%
  
  group_by(claim_number,loss_date,Period) %>%
  
  mutate(
    
    #Correct Loss Date of Claim Number: 27193310
    loss_date = replace(loss_date,claim_number == "27193310",as.POSIXct("2019-08-30",format = "%Y-%m-%d")),
  
    
    #### Recalculate Maturities and Flags ###
    
    #Calculate Maturity (in months)
    Maturity = round((evaluation_date - loss_date)/30,4),
    
    #Bucket Maturities into 3m, 6m, 9m, 12m/1yr, 2yr, 3yr, 4y+
    Maturity_Bucket = factor(
      case_when(
        Maturity <= 3 ~ "3m",
        Maturity <= 6 ~ "6m",
        Maturity <= 9 ~ "9m",
        Maturity <= 12 ~ "1y",
        Maturity <= 24 ~ "2y",
        Maturity <= 36 ~ "3y",
        Maturity <= 48 ~ "4y",
        TRUE ~ "4y+"),
      levels = Maturity_levels,
      ordered = TRUE ),    #For Reference, PL BI is 95%+ developed at 5 years
    
    #Flag the latest evaluation date
    Most_Recent_Flag = ifelse(evaluation_date == max(evaluation_date),1,0),
    
    #Flag the first evaluation date
    First_Eval_Flag = ifelse(evaluation_date == min(evaluation_date),1,0),
    
    #Extract Accident Year
    Acc_Yr = year(loss_date)
  ) %>% view("Data 2 - Updated for Claim # 27193310")

unique(Data2_v2$Period)

#Note that data contains Period 0 & 1 Losses, but Policy only covers Period 2 & 3 Losses
#Remove non-Period 2 & 3 Losses
Data2_v3 <- Data2_v2 %>%
  
  filter(Period == "Transporting Rider" | Period == "En Route To Pickup" )

unique(Data2_v3$Period)


#Happy with this level of data quality
Data2_Clean <- Data2_v3


# Dataset 2 - Exploratory Data Analysis Continued -------------------------


#Distribution of Incurred Losses (as of latest Eval Date), by Period
Data2_v3 %>%
  
  filter(Most_Recent_Flag ==1) %>% 
  
  group_by(claim_number,loss_date,Period) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=Inc_Loss,y=(..count..)/sum(..count..),fill = Period),
                 bins=10,position = "dodge") +
  
  #Add Line for Reinsurance Limit
  geom_vline(xintercept=500000, linetype = "dashed",color="blue")+
  
  scale_fill_viridis_d()+
  
  labs(
    title = "Policy 456 - Distribution of Incurred Loss ($1M XS $1M)",
    subtitle = "As of Latest Evaluation Date, by Period",
    x = "Incurred Loss",
    y = "% of Claims"
  )+
  
  scale_y_continuous(labels=percent) +
  
  theme_bw()

#By Accident Year
Data2_v3 %>%
  
  filter(Most_Recent_Flag ==1) %>% 
  
  group_by(claim_number,loss_date,Period) %>%
  
  ggplot()+
  
  geom_density_ridges(aes(x = Inc_Loss,y = as.factor(Acc_Yr),fill = as.factor(Acc_Yr)),
                      stat = "binline",scale=2)+
  
  #facet_wrap(~Period)+
  
  xlim(0,1000000)+
  
  scale_fill_viridis_d("Accident Year")+
  
  theme_bw()

# Dataset 3 - Exploratory Data Analysis-------------------------------------------------------------------

Data3_Raw$a.trip_status %>% unique()

Data3_Raw$b.risk_segment %>% unique()


Data3_Raw %>% filter(is.na(c.distance_miles_p3)) %>% view()

Data3_Raw %>%
  
  filter(b.risk_segment == "\\N") %>% view()



# Cleaning Data 3 ---------------------------------------------------------


Data3_v1 <- Data3_Raw %>%
  
  mutate(
    #Create a Key for simplicity
    Key = paste0(a.trip_identifier,a.driver_identifier,a.vehicle_identifier),
    
    .keep = "unused"
  )  %>%
  
  relocate(Key) %>%
  
  mutate(
    #Calculate Trip Time in minutes
    Trip_Time_Min = as.double((a.endtrip_timestamp_local - a.begintrip_timestamp_local)/60)
  ) %>% 
  
  #Remove 21 NA rows which are missing distance, trip fare, or start/end time
  #Primarily correspond to canceled rides, and 21 rows are a trivial percent of the total dataset (~5000 observations)
  filter(!is.na(c.distance_miles_p3)) %>%
  
  filter(a.trip_status != "canceled") %>%
  
  #Remove 9 rows corresponding to unknown "/N" Risk Segment (again, minor portion of overall data)
  filter(b.risk_segment != "\\N") %>% 
  
  #Rename for simplicity
  rename(start_time = a.begintrip_timestamp_local,
         end_time = a.endtrip_timestamp_local,
         trip_distance_mi = c.distance_miles_p3) %>%
  
  view("Dataset3")


#Assess trip time for reasonability
Data3_v1 %>%
  
  #filter(Trip_Time_Min == 0) %>% 
  
  #As a reference, use Avg Speed
  mutate(Avg_Speed_MPH = trip_distance_mi/((Trip_Time_Min)/60)) %>% 
  
  #Assume any Uber trips that average 80 MPH is unreasonable
  filter(Avg_Speed_MPH > 80) %>%
  
  arrange(desc(Avg_Speed_MPH)) %>%
  
  view("Trips w/ Avg MPH > 80")


#It seems unlikely that a trip >1 mile can be completed in 0 minutes (or within 1 minute) 
#However, since using Trip Distance as the exposure base (and those values look fine), keep the rows



#Happy with this level of data quality
Data3_Clean <- Data3_v1




# Dataset 3 Exploratory Data Analysis - Continued ----------------------------


#How many trips were made for each risk segment?
Data3_v1 %>%
  
  group_by(b.risk_segment) %>%
  
  summarise(Risk_Segment_Count = n()) %>% 
  
  ggplot() +
  
  geom_bar(aes(x=b.risk_segment,y=Risk_Segment_Count,fill=b.risk_segment),stat="identity")+
  
  geom_text(aes(x=b.risk_segment,y=Risk_Segment_Count,label=scales::comma(Risk_Segment_Count)),vjust=-0.5)+
  
  scale_fill_viridis_d("Risk Segment")+
  
  labs(
    title = "Exposure Dataset - Count of Trips by Risk Segment",
    x = "Risk Segment",
    y = "Count of Trips"
  ) +
  
  theme_bw()

#Data includes pandemic time period, where Eats deliveries were probably more common
#than P2P due to lockdowns - this aligns with expectations


#Take it a step further - (assuming Trip Distance as exposures), how much exposure for each risk segment?
Data3_v1 %>%
  
  group_by(b.risk_segment) %>%
  
  summarise(Total_Trip_Dist_mi = round(sum(trip_distance_mi),0)) %>%  
  
  ggplot()+
  
  geom_bar(aes(x=b.risk_segment,y=Total_Trip_Dist_mi,fill=b.risk_segment),stat="identity")+
  
  geom_text(aes(x=b.risk_segment,y=Total_Trip_Dist_mi,label=paste(scales::comma(Total_Trip_Dist_mi)," mi")),vjust=-0.5)+
  
  scale_fill_viridis_d("Risk Segment")+
  
  labs(
    title = "Exposure Dataset - Total Trip Miles by Risk Segment",
    x = "Risk Segment",
    y = "Total Trip Miles"
  ) +
  
  theme_bw()


#Distribution of Trip Distance - Longer P2P tail explains the more total miles for P2P compared to Eats, despite fewer trips
Data3_v1 %>%
  
  ggplot()+
  
  geom_boxplot((aes(y=trip_distance_mi,x=b.risk_segment,fill=b.risk_segment)))+
  
  scale_fill_viridis_d("Risk Segment")+
  
  labs(
    title = "Exposure Dataset - Distribution of Trip Distance (Miles) by Risk Segment",
    x = "Risk Segment",
    y = "Trip Distance (Mi)",
  )+
  
  theme_bw()  



#Distribution of Trip Time
Data3_v1 %>%
  
  ggplot()+
  
  #geom_density_ridges(aes(y=b.risk_segment,x=Trip_Time_Min,fill=b.risk_segment))
  
  geom_boxplot((aes(y=Trip_Time_Min,x=b.risk_segment,fill=b.risk_segment)))+
  
  scale_fill_viridis_d("Risk Segment")+
  
  labs(
    title = "Exposure Dataset - Distribution of Trip Time (mins) by Risk Segment",
    x = "Risk Segment",
    y = "Trip Time (min)",
  )+
  
  theme_bw()


#Distribution of Average Trip Speed
Data3_v1 %>%
  
  #Add 30s since seconds detail is not being capture (in theory, the difference should be 30s on average)
  mutate(Avg_Speed_MPH = trip_distance_mi/((Trip_Time_Min+0.5)/60)) %>% 
  
  ggplot()+
  
  geom_boxplot(aes(x=b.risk_segment,y=Avg_Speed_MPH,fill = b.risk_segment))+
  
  scale_fill_viridis_d("Risk Segment")+
  
  labs(
    title = "Exposure Dataset - Avg Trip Speed (MPH) by Risk Segment",
    x = "Risk Segment",
    y = "Trip Speed (MPH)",
    caption = "Long tail alludes to unreliable timestamps/trip times"
  )+
  
  scale_y_continuous(limits = c(0,100),breaks=seq(0,100,by=10))+
  
  theme_bw()

#Some crazy MPH, even after adjusting for seconds
#Trip time (i.e. timestamps) does not seem as reliable - more reason to use miles driven




#Distribution of Fares
Data3_v1 %>%
  
  ggplot()+
  
  geom_boxplot(aes(x=b.risk_segment,y=c.trip_fare,fill = b.risk_segment))+
  
  scale_fill_viridis_d("Risk Segment")+
  
  labs(
    title = "Exposure Dataset - Trip Fare by Risk Segment",
    x = "Risk Segment",
    y = "Trip Fare ($)",
  )+
  
  scale_y_continuous(limits = c(0,50),breaks=seq(0,50,by=10))+
  
  theme_bw()

# Calculate Loss Cost and Frequency ---------------------------------------

#Note - Interpreting prompt as determining the last diagonal of a loss triangle only
#Therefore only looking at latest evaluation dates

#Total Losses (Data1) at various maturities, by AY and coverage (Ideal way to approach this)
Data1_Total_Loss <- Data1_Clean %>%
  
  #Filter each loss to its latest evaluation
  filter(Most_Recent_Flag == 1) %>%
  
  #Group by AY, Coverage, and Maturities
  group_by(Acc_Yr,coverage,Maturity_Bucket) %>%
  
  #Calculate Total Incurred Losses, Total Ceded Losses, and Claim Counts
  summarise(Total_Inc_Loss = sum(Inc_Loss),
            Total_Ceded_Loss = sum(Ceded_loss),
            Claim_Count = n()) %>%
  
  arrange(Acc_Yr,coverage,Maturity_Bucket) %>%
  
  view("Data 1 - Losses by AY and Coverage")


#Since Dataset 2 does not contain coverages, sum over coverage details (not ideal)
Data1_Total_Loss_vF <- Data1_Clean %>%

  #Filter each loss to its latest evaluation
  filter(Most_Recent_Flag == 1) %>%
  
  #Group by AY, and Maturities
  group_by(Acc_Yr,Maturity_Bucket) %>%
  
  #Calculate Total Incurred Losses, Total Ceded Losses, and Ceded Claim Counts
  summarise(Total_Inc_Loss = sum(Inc_Loss),
            Total_Ceded_Loss = sum(Ceded_loss),
            Ceded_Claim_Count = sum(Reins_Flag)) %>%
  
  #Add Column to identify as Data1
  mutate(
    Data_Source = "Data1 - $1M Lim"
  ) %>%
  
  relocate(Data_Source) %>%
  
  arrange(Acc_Yr,Maturity_Bucket) %>%
  
  view("Data 1 - Losses by AY")


#Repeat exercise for Dataset 2
Data2_Total_Loss_vF <- Data2_Clean %>%
  
  #Filter each loss to its latest evaluation
  filter(Most_Recent_Flag == 1) %>%
  
  #Group by AY, and Maturities
  group_by(Acc_Yr,Maturity_Bucket) %>%
  
  #Calculate Total Incurred Losses, Total Ceded Losses, and Claim Counts
  summarise(Total_Inc_Loss = sum(Inc_Loss),
            Total_Ceded_Loss = sum(Ceded_Loss),
            Ceded_Claim_Count = n()) %>%  #Since $1M XS $1M, all claims are in reinsurance layer
  
  #Add Column to identify as Data2
  mutate(
    Data_Source = "Data2 - $1M XS $1M"
  ) %>%
  
  relocate(Data_Source) %>%
  
  arrange(Acc_Yr,Maturity_Bucket) %>%
  
  view("Data 2 - Losses by AY")


#Combine Loss Data from Dataset 1 and 2
Loss_Data <- bind_rows(Data1_Total_Loss_vF, 
                       Data2_Total_Loss_vF) %>%

  group_by(Acc_Yr,Maturity_Bucket) %>%
  
  summarise(Total_Ceded_Loss_vF = sum(Total_Ceded_Loss),
            Ceded_Claim_Count_vF = sum(Ceded_Claim_Count)) %>%
  
  #Filter out rows with no ceded losses
  filter(Ceded_Claim_Count_vF > 0) %>%
  
  view("Final Combined Loss Data")


#Note - Mismatch in years between Loss and Exposure Datasets:
#Exposure data begins 6/2019, however losses begin Q2 2018
#Last AY is 2021, but exposures go to 2022
Expo_Data <- Data3_Clean %>%
  
  #Extract Trip Year
  mutate(Trip_Year = year(start_time)) %>%
  
  group_by(Trip_Year) %>%
  
  summarise(Total_Miles = round(sum(trip_distance_mi),0)) %>%
  
  view("Final Exposure Data")


#Calculate Ceded Loss Costs and Frequency
Final_Result <- 
  
  left_join(
    Loss_Data,
    Expo_Data,
    by = c("Acc_Yr" = "Trip_Year")
  ) %>% 
  
  #Calculate Ceded LC and Freq
  mutate(
    Ceded_LC = round(Total_Ceded_Loss_vF/Total_Miles,2),
    Ceded_Freq = Ceded_Claim_Count_vF / Total_Miles
  ) %>%
  
  view("Final Result - Ceded LC and Freq")



#I'm not sure how valueable/informative this is
#Losses are at different evaluation dates
#Dividing by total exposures ...
# Not all losses are at same final maturity

#For example, looking at Data 1...2018 claims have maturities ranging from < 10 months, to > 50 months!
#The data 
Data1_Clean %>%
  
  #Filter to latest eval Date
  filter(Most_Recent_Flag == 1) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=Maturity),fill = "navy",bins = 20)+
  
  facet_wrap(~Acc_Yr)+
  
  labs(
    title = "Policy 123 - Distribution of Maturity at Latest Eval Date",
    subtitle = "by AY for all Coverages and Periods",
    x = "Maturity (Months)",
    y = "Claim Count",
    caption = "Unclear whether claims were closed as of their last individual evaluation date,
    and if the true evaluation date was when the data was pulled"
  )+
  
  theme_bw()






# Calculate Loss Cost and Frequency, Assuming True Eval Date = 6/30/2022 --------

#6/30/2022 determined based on maximum evaluation date for Data 1 and Data 2
#Would imply AY 2018 at 4 years of maturity, 2019 at 3 years, etc.. since
#we evaluate from the average accident date of each AY (6/30)

#Total Losses (Data1) at various maturities by AY 
Data1_Total_Loss_v2 <- Data1_Clean %>%
  
  #Filter each loss to its latest evaluation
  filter(Most_Recent_Flag == 1) %>%
  
  #Group by AY (Implied that all AY share the same maturity)
  group_by(Acc_Yr) %>% 
  
  #Calculate Total Incurred Losses, Total Ceded Losses, and Ceded Claim Counts
  summarise(Total_Inc_Loss = sum(Inc_Loss),
            Total_Ceded_Loss = sum(Ceded_loss),
            Ceded_Claim_Count = sum(Reins_Flag)) %>%
  
  mutate(
    #Add Column to identify as Data1
    Data_Source = "Data1 - $1M Lim",
    
    #Calculate maturity assuming a true 6/30/2022 evaluation date for all claims
    Maturity = 2022 - Acc_Yr
    
  ) %>%
  
  relocate(Data_Source) %>%
  
  relocate(Maturity, .after="Acc_Yr") %>%
  
  arrange(Acc_Yr) %>%
  
  view("Data 1 - Losses by AY")



#Repeat exercise for Dataset 2
Data2_Total_Loss_v2 <- Data2_Clean %>%
  
  #Filter each loss to its latest evaluation
  filter(Most_Recent_Flag == 1) %>%
  
  #Group by AY
  group_by(Acc_Yr) %>%
  
  #Calculate Total Incurred Losses, Total Ceded Losses, and Claim Counts
  summarise(Total_Inc_Loss = sum(Inc_Loss),
            Total_Ceded_Loss = sum(Ceded_Loss),
            Ceded_Claim_Count = n()) %>%  #Since $1M XS $1M, all claims are in reinsurance layer
  
  mutate(
    #Add Column to identify as Data2
    Data_Source = "Data2 - $1M XS $1M",
    
    #Calculate maturity assuming a true 6/30/2022 evaluation date for all claims
    Maturity = 2022 - Acc_Yr
  ) %>%
  
  relocate(Data_Source) %>%
  
  relocate(Maturity, .after="Acc_Yr") %>%
  
  arrange(Acc_Yr) %>%
  
  view("Data 2 - Losses by AY")


#Combine Loss Data from Dataset 1 and 2
Loss_Data <- bind_rows(Data1_Total_Loss_v2, 
                       Data2_Total_Loss_v2) %>%
  
  group_by(Acc_Yr,Maturity) %>%
  
  summarise(Total_Ceded_Loss_vF = sum(Total_Ceded_Loss),
            Ceded_Claim_Count_vF = sum(Ceded_Claim_Count)) %>%
  
  #Filter out rows with no ceded losses
  filter(Ceded_Claim_Count_vF > 0) %>%
  
  view("Final Combined Loss Data")


#Calculate Ceded Loss Costs and Frequency
Final_Result_v2 <- 
  
  left_join(
    Loss_Data,
    Expo_Data,
    by = c("Acc_Yr" = "Trip_Year")
  ) %>% 
  
  #Calculate Ceded LC and Freq
  mutate(
    Ceded_LC = round(Total_Ceded_Loss_vF/Total_Miles,2),
    Ceded_Freq = Ceded_Claim_Count_vF / Total_Miles
  ) %>%
  
  view("Final Result - Ceded LC and Freq")
