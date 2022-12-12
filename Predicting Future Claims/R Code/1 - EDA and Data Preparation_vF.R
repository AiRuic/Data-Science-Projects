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
library(gridExtra) #For arranging multiple plots


#Discourage Scientific Notation
options(scipen=999)

#Set Working Directory
setwd("C:/Users/Eric_/Desktop/State Farm DS Exercise")



# Load in Data ------------------------------------------------------------

#Claims Data - Dataset 1
Claim_Data_raw <- read_csv("Claim_Data.csv", 
                       col_types = cols(clm_dates = col_date(format = "%m/%d/%Y")))

#Predictor Dataset - Dataset 2
Predictor_Dataset_raw <- read_csv("Predictor_Dataset.csv")

#Subsequent Loss Experience - Dataset 3
Subsequent_Loss_Experience_raw <- read_csv("Subsequent_Loss_Experience.csv")



# Assumptions -------------------------------------------------------------

#Assume single, same coverage for all datasets
#If BI Liability, ignore differences between at-fault/no-fault states for simplicity

#Paid amount might not be reliable, because recent claims less mature = less paid loss development
#Therefore, assume all historical claims with paid amounts are CLOSED, with no further development

#Assume Paid Amounts and Loss Amounts have already been trended to the same level

#Assume premium is not available as a predictor variable
#"Premium Received" in metadata might indicate presence of unpaid premium i.e. might not reflect the total policy premium

# EDA - Claim Data --------------------------------------------------------

#Preview Data
Claim_Data_raw %>% view("Claim Data")

#Summary
Claim_Data_raw %>% summary()  

#Historical Claim data from 2010 - 2017
#*Although only 4 claims in 2017, so almost all data is pre-2017

#Assume Paid losses have been trended to the same level
#Note - Paid amount is not as useful without knowing deductibles/limits

#Data appears reasonable


#Evaluate if this is claim level data or occurrence
Claim_Data_raw %>% 
  
  group_by(hhld_id,clm_dates) %>%
  
  unique() %>% nrow() 

#Seems to be for claim-level data for one coverage, because
#Single claim date (occurrence) is not associated with multiple claims


#Monthly Claims Data as a Time Series
Monthly_Claim_TS <-Claim_Data_raw %>%
  
  group_by(clm_month = floor_date(clm_dates,'month'))%>%
  
  summarise(
    Count_Claims = n(),      #Claims incurred in accident month
    Avg_Paid_Loss = mean(pd_amt),      #Mean Paid per claim for  claims incurred in accident month
  ) %>%
  
  arrange(clm_month)


#Time Series Plot #1 showing Claims per Month
Paid_Claim_ts_plot <- Monthly_Claim_TS %>%
  
  ggplot()+
  
  geom_line(aes(x=clm_month,y=Count_Claims))+
  
  labs(
    title = "Claim Counts by Accident Month",
    x = "Accident Month",
    y = "Claim Counts"
  )+
  
  theme_bw()
  

#Time Series Plot #2showing average losses per month
Paid_Sev_ts_plot <- Monthly_Claim_TS %>%
  
  ggplot()+
  
  geom_line(aes(x=clm_month,y=Avg_Paid_Loss)) +
  
  labs(
    title = "Avg Paid Loss by Accident Month",
    x = "Accident Month",
    y = "Avg Paid Loss"
  )+
  
  theme_bw()  


#Plot both TS plots side-by-side
grid.arrange(Paid_Claim_ts_plot,Paid_Sev_ts_plot,
             ncol = 2, nrow = 1)

#Looks like 2017 losses are new/not closed

#Observe some potential cyclicality, with lower losses around Dec/Jan, and higher losses mid-year
#Perhaps caused different types of claims in Winter vs Summer


#Quick look at time series decomposition for Claim Count (Freq)
TS_Decomp_Freq <- Monthly_Claim_TS %>% 
  
  #Drop post 2017, since minimal data
  filter(clm_month < '2017-01-01') %>% 
  
  select(Count_Claims) %>% 

  ts(start = c(2010,1),end = c(2017,1),frequency = 12) %>%
  
  decompose(type = "multiplicative") %>% 
  
  autoplot() + ggtitle("Claim Count, TS Decomposition")

#Quick look at time series decomposition for Avg Paid Loss (Sev)
TS_Decomp_Sev <- Monthly_Claim_TS %>% 
  
  #Drop post 2017, since minimal data
  filter(clm_month < '2017-01-01') %>% 
  
  select(Avg_Paid_Loss) %>% 
  
  ts(start = c(2010,1),end = c(2017,1),frequency = 12) %>%
  
  decompose(type = "multiplicative") %>% 
  
  autoplot() + ggtitle("Avg Paid Loss, TS Decomposition")


#Plot TS decomposition side-by-side
grid.arrange(TS_Decomp_Freq,TS_Decomp_Sev,
             ncol = 2, nrow = 1)

#Potential decreasing trend in severity over time-period
#Potential seasonality in Claim Count/Avg Paid, but sizable residuals


# % at Fault, and Paid Losses
Claim_Data_raw %>%
    
  group_by(aft_ind) %>%
    
  summarise(
    Percent_of_HH = formattable::percent(n()/nrow(Claim_Data_raw)),
    Avg_Paid = mean(pd_amt),
    StDev_Paid = sd(pd_amt)
    ) 
  
  
#Paid Amount by fault
Claim_Data_raw %>%
  
  ggplot()+
  
  geom_histogram(aes(x=pd_amt,y=after_stat(count/sum(count)),fill=factor(aft_ind)))+
  
  scale_fill_viridis_d("At Fault Indicator")+
  
  scale_y_continuous(labels = scales::percent)+
  
  labs(
    title = "Distribution of Paid Amount by Fault",
    y = "% of Total Paid Claims",
    x = "Paid Amount"
  )+
  
  theme_bw()

  

# EDA - Subsequent Loss Experience (Dependent Variable) -------------------

#Preview Data
Subsequent_Loss_Experience_raw %>% view("Subsequent Loss Experience")

#Summary 
Subsequent_Loss_Experience_raw %>% summary()  #Looks reasonable

#Understand Premium Distribution
Subsequent_Loss_Experience_raw %>% 
  
  ggplot()+
  
  geom_histogram(aes(x=premium),fill = "darkorchid4",color = "white")+
  
  labs(
    title = "Premium Distribution",
    y = "Households",
    x = "Premium"
  )+
  
  theme_bw()

#Understand Loss Distribution (esp. ignoring 0 loss Households)
Subsequent_Loss_Experience_raw %>% 
  
  filter(loss_amount >0) %>% 
  
  ggplot()+
  
  geom_histogram(aes(x=loss_amount),
                 fill = "cornflowerblue",color="white")+
  
  labs(
    title = "Subsequent Loss Experience",
    subtitle = "Losses > 0",
    x = "Loss Amount ($)",
    y = "Claim Counts",
    caption = "Only ~5% of Households suffered Claims"
  )+
  
  theme_bw()  #Losses look extremely low - perhaps this is in $1000s?


#Confirm Loss Ratio = Loss / Premium
Subsequent_Loss_Experience_raw %>%
  
  mutate(
    Calc_LR = (loss_amount) / premium,
    Check_LR = Calc_LR - loss_ratio
  ) %>%
  
  filter(round(Check_LR,3) !=0)  #Confirmed (also confirmed losses are in $)


#Evaluate overall Loss Ratio of book of business
Subsequent_Loss_Experience_raw %>%
  
  summarise(
    Total_Loss = sum(loss_amount),
    Total_Premium = sum(premium)
  ) %>%
  
  mutate(Overall_LR = Total_Loss/Total_Premium)
#Overall loss ratio 29% is very good, but expect to deterioriate as losses develop
  

#Understand % of Households with claims
Subsequent_Loss_Experience_raw %>%
  
  group_by(future_clm_ind) %>%
  
  summarise(
    Count_HH_w_Claims = n(),
    Percent_HH_w_Claims = formattable::percent(n()/nrow(Subsequent_Loss_Experience_raw)),
    Avg_Loss = mean(loss_amount),  #Average Loss ~$1,100
    Avg_Premium = mean(premium)    #Average Premium ~$170
    
  ) #Only about 5% of claimants suffered losses

#Premium is slightly higher for HH that suffered losses, so premium does capture some of the risk


#Relationship between Premium and Loss
Subsequent_Loss_Experience_raw %>%
  
  #filter(loss_amount > 0) %>%
  
  select(premium,loss_amount) %>%
  
  ggplot()+
  
  geom_point(aes(x=premium,y=loss_amount))+
  
  labs(
    title = "Premium Charged vs Loss Amount Incurred",
    x = "Premium",
    y = "Loss Amount"
  )+
  
  theme_bw()  #Does not appear to demonstrate a strong relationship between Premium & Loss


#Check correlation between premium and loss amount
Subsequent_Loss_Experience_raw %>%
  
  select(premium,loss_amount) %>%
  
  cor()  #Surprisingly low correlation



# 2. EDA Predictor Dataset ---------------------------------------------------

#Preview Data
Predictor_Dataset_raw %>% view("Predictor Data")

#Summary
Predictor_Dataset_raw %>% summary()

#Notes on Summary
#No outliers/unreasonable values
#Maximum age (112!) high, but not necessarily unbelievable
#Several columns contain NAs


# 2a. EDA Predictor Dataset - Zip Code ----------------------------------------------------------------

#Pull in a State to Zip Code Mapping
Zip_Mapping <- zip_code_db %>%
  
  select(state,zipcode)

#Map each Zip Code to its state, and check results
left_join(Predictor_Dataset_raw %>% mutate(zipcode = f_pad_zero(zipcode,5)),
          Zip_Mapping,
          by = c("zipcode")
          ) %>%
  
  relocate(state,zipcode,hhld_id) %>%
  
  group_by(state) %>%
  
  summarise(
    Count_Zips = n(),
    Percent_Zips = formattable::percent(Count_Zips/nrow(Predictor_Dataset_raw),digits = 1) 
    )  %>%
  
  #Replace NA with "NA" label
  mutate(state = ifelse(is.na(state),"NA",state)) %>%
  
  arrange(desc(Count_Zips)) %>%
  
  ggplot(aes(x=reorder(state,-Percent_Zips),y=Percent_Zips))+
  
  geom_bar(aes(fill=state),stat = "identity")+
  
  geom_text(aes(label = Percent_Zips),
            nudge_y = 0.02)+
  
  scale_y_continuous(labels = scales::percent)+
  
  labs(
    title = "Distribution of Zip Code to State Mapping",
    subtitle = "Most Zip Codes map to 'NA'",
    x = "State",
    y = "Percent of Zip Codes"
  )+
  
  theme_bw()+
  
  theme(legend.position = "none") #NAs and subsequent analysis indicated zipcodes were censored/randomized


rm(Zip_Mapping)

#Can check if there's concentration of zip codes..
Predictor_Dataset_raw %>%
  
  mutate(zipcode = f_pad_zero(zipcode,5)) %>%
  
  group_by(zipcode) %>%
  
  summarise(count = n()) %>%
  
  arrange(desc(count)) %>%
  
  ggplot()+
  
  geom_bar(aes(x=reorder(zipcode,-count),y=count),
           stat = "identity",alpha = 0.5,fill ="midnightblue")+
  
  labs(
    title = "Households per Zip Code",
    x = "Zip Code",
    y = "Household Count"
  )+
  
  theme_bw()+
  
  theme(axis.text.x=element_blank())
  
#113 Unique Zip Codes, with 150 - 225 Households per Zip Code
#Relative to size of data, zip code has too many levels to reasonably include in model
#Remove Zip Code variable


# 2b. EDA Predictor Dataset - Exploring NAs -------------------------------


#Several columns have NAs - subset and identify those columns
Predictor_Dataset_NAs <- Predictor_Dataset_raw %>% 
  select(hhld_id,curnt_insurer,which(colSums(is.na(.))>0)) %>% 
  view("Predictor Dataset - NAs") %>% 
  
  view("NA Predictor Dataset")

#Initial Impressions
#Prior BI is NA when current insurer is missing - confirm?
#Time with Current Insurer is NA when current insurer is missing - confirm?

#Noticed credit score is not an integer (randomly generated?)



#Count of NAs 
Predictor_Dataset_NAs %>% summarise_all(~sum(is.na(.)))
  
#Percent of NAs
Predictor_Dataset_NAs %>% summarise_all(~formattable::percent(sum(is.na(.))/nrow(Predictor_Dataset_NAs)))


#Vehicle Lien Count - 99.9% NA
#Credit_score - 4.6% missing, perhaps impute missing with simple average
#Prior BI and Time with Current Insurer share same NA % --> likely linked to current insurer missing


# 2b.1 - NAs: Vehicle Lien Count -----------------------------------------------

#Vehicle Lien Count Data
Predictor_Dataset_NAs %>% filter(!is.na(veh_lien_cnt)) %>% view("Non-NA Vehicle Lien Count")
Predictor_Dataset_NAs %>% group_by(veh_lien_cnt) %>% summarise(Count = n())


#Check if Own + Lien + Lease = Vehicle Count (for non-NA Vehicle Lien Count)
Predictor_Dataset_raw %>%
  
  select(hhld_id,veh_own_cnt,veh_lien_cnt,veh_lease_cnt,veh_cnt) %>%
  
  #Filter to non-NA rows of Vehicle Lien Count
  filter(!is.na(veh_lien_cnt)) %>%
  
  mutate(
    veh_lien_cnt = replace_na(veh_lien_cnt,0),
    Calc_Total_Veh = veh_own_cnt + veh_lien_cnt + veh_lease_cnt,
    Check_Total = ifelse(veh_cnt - Calc_Total_Veh==0,TRUE,FALSE)
  ) %>%
  
  arrange(Check_Total) %>%
  
  view("Check if Vehicles Own + Lien + Lease = Total") #25/26 non-NA rows align

  

#Understand Distribution of Vehicle Count Differences (with NA Vehicle Lien Count = 0)
Predictor_Dataset_raw %>%
  
  select(hhld_id,veh_cnt,veh_own_cnt,veh_lien_cnt,veh_lease_cnt) %>%
  
  #Filter to NA rows of Vehicle Lien Count
  filter(is.na(veh_lien_cnt)) %>%
  
  mutate(
    OwnLease_cnt = veh_own_cnt + veh_lease_cnt,
    Implied_Lien_Cnt = veh_cnt - OwnLease_cnt
  ) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=Implied_Lien_Cnt),fill = "skyblue")+
  
  labs(
    title = "Implied # of Vehicles Liened per Household",
    subtitle = "Total Vehicle Count - (Owned + Leased Count)",
    y = "# of Households",
    x = "Implied Liened Vehicle Count"
  )+
  
  theme_bw() #Seems fairly reasonable
  

# 2b.2 - NAs: Prior BI and Time w Current Insurer ------------------------------

#Prior BI and Time with Current Insurer is NA when current insurer is missing

#Current Insurer and Prior BI together:
Predictor_Dataset_NAs %>% 
  
  select(-hhld_id) %>%
  
  group_by(curnt_insurer,prior_bi,time_w_carr) %>%
  
  summarise(Count = n()) %>% 
  
  arrange(desc(Count))%>%
  
  head(10) %>%
  
  view("Current Insurer Missing and NAs")


#Visualize to better understand this data:

#Prior BI Limits Distribution
# *Need to be cautious about this - may be affected by trend/inflation
Predictor_Dataset_NAs %>%
  
  ggplot()+
  
  geom_histogram(aes(x=prior_bi))+
  
  labs(
    title = "Distribution of Prior BI Limit",
    y = "Count of Households",
    x = "BI Limit"
  )+
  
  theme_bw() #$25k, $50k, $100k, and $250k limits


#Time with Carrier Distribution
Predictor_Dataset_NAs %>%
  
  ggplot()+
  
  geom_histogram(aes(x=time_w_carr))+
  
  labs(
    title = "Distribution of Time with Current Carrier",
    y = "Count of Households",
    x = "Time with Current Carrier"
  )+
  
  theme_bw()

#Data is bucketed into 0.5 increments, presumably [0, 0.5), [0.5,1), etc
#with possible capping at 5 years (i.e. 5 years = 5+ )


#Thoughts:
# 1. Assume missing to mean didn't have a prior insurer
# (e.g. brand new driver)
# and therefore assume 0 BI, and 0 prior time

# 2. Assume they had a prior insurer, and data is truly missing (at random)
# Replace Time with Current with average/mode
# Replace Prior BI Limit with average/mode



#Current Insurer "Missing" = Not Currently Insured = NAs for prior_bi and time_w_carr
Predictor_Dataset_raw %>%
  
  select(hhld_id,curnt_insurer,inforce_ind,prior_bi,time_w_carr) %>%
  
  group_by(curnt_insurer,inforce_ind,prior_bi,time_w_carr) %>%
  
  summarise(Count = n()) %>% 
  
  filter(inforce_ind ==0) %>%
  
  view("Current Insurer 'Missing' = Not Currently Insured")


#Therefore, proceed with option 1:
# 1. Assume missing to mean didn't have a prior insurer
# (e.g. brand new driver)
# and therefore assume 0 BI, and 0 prior time


# 2b.3 - NAs: Conclusions ---------------------------------------------------

#Conclusions regarding NAs:
# Vehicles Liened Count 99.9% NA
# Due to large number of NAs, one option is to remove the column entirely
# Alternatively, imputed lien count = Veh - Own - Lease Counts

# Prior BI Limit and Time with Current Insurer are both NA when Current Insurer = Missing = No Prior Insurer --> 35% of data
# Replace NAs with:
#0 Time with Current Insurer (because they had 0 time with insurer, literally)
#0 BI Limit - imperfect, but captures the effect of having no prior insurance

# Credit Score NAs affect ~5% of data. Credit Score has been historically predictive for insurance purposes
# Replace missing values with simple average 
# Also needs adjustments to integers


# 2b.4 - NAs: Cleaning NA Columns ----------------------------------------------

#Calculate Average Credit Score to substitute for NAs
Avg_Credit_Score = mean(Predictor_Dataset_raw$credit_score,na.rm = TRUE)
Avg_Credit_Score

#Implement the above conclusion to clean NAs in Predictor Dataset
Predictor_Dataset_NA_adj <- Predictor_Dataset_raw %>%
  
  mutate(
    #Impute Vehicle Lien Count as Veh Count - (Own + Lease Counts)
    veh_lien_cnt = ifelse(is.na(veh_lien_cnt),veh_cnt-(veh_own_cnt + veh_lease_cnt),veh_lien_cnt),
    
    #Let NA Time with Current Insurer = 0 (demonstrated to have no current insurer)
    time_w_carr = replace_na(time_w_carr,0),
    
    #Let NA Prior BI Limit = 0 (if no prior/current insurer, then no BI Limit)
    prior_bi = replace_na(prior_bi,0),
    
    #Replace NA Credit Scores with mean
    credit_score = replace_na(credit_score,Avg_Credit_Score),
    
    #Round Credit Scores to integer value
    credit_score = round(credit_score)
  )

#Summary to confirm no NAs, and re-assess distributions
Predictor_Dataset_NA_adj %>% 
  
  select(veh_lien_cnt,time_w_carr,prior_bi,credit_score)  %>%  
  
  summary()


# 2c. - EDA Predictor Dataset - Continuous Variables Overview ----------------------

#Facet Wrap

#Prepare Data by pivoting longer
Predictor_Dataset_long <- Predictor_Dataset_raw %>% 
  
  #Drop ZIP, and Current Insurer (not interested in Zip, and Current Insurer is non-numeric)
  select(-c(zipcode,curnt_insurer)) %>%
  
  pivot_longer(
    cols = -hhld_id,
  ) 

#Overview
#Loop over all pages/facets to plot boxplots for all predictors
# for(i in 1:10){
#   
#   Predictor_Boxplots <- Predictor_Dataset_long %>% 
#     ggplot() +
#     
#     geom_boxplot(aes(y=value,fill=name))+
#       
#     labs(
#       title = paste0("Graph #",i),
#       y= "",
#     )+
#     
#     facet_wrap_paginate(~name,nrow=2,ncol=2,scales = "free",page = i)+
#     
#     theme_bw() +
#     
#     theme(legend.position = "none")
#   
#   print(Predictor_Boxplots)
#   
# }

#A bit too overwhelming / not informative - use a more careful approach to evaluate similar variables together


# 2c.1 - EDA Predictor Dataset - Driver & Vehicle Count ------------------------------

#Calculate and look at vehicle to driver ratio
Predictor_Dataset_raw %>%
  
  select(hhld_id,drvr_cnt,veh_cnt) %>%
  
  mutate(
    Vehicle_per_Driver = veh_cnt / drvr_cnt
  ) %>% 
  
  ggplot()+
  
  geom_histogram(aes(x=Vehicle_per_Driver)) +
  
  labs(
    title = "Distribution of Vehicles per Driver",
    x = "Vehicles per Driver",
    y = "Count of Households",
    caption = "Most house holds have <= 1 Vehicler per driver"
  )+
  
  scale_fill_viridis()+
  
  theme_bw()


#Examine Types of Vehicles owned
Predictor_Dataset_long %>%
  
  filter(
    name %in% c("veh_cnt","cnt_auto","cnt_mtrcyc")
  ) %>%
  
  ggplot()+
  
  #geom_boxplot(aes(y=value,x=name,fill = name)) +
  
  geom_density_ridges(aes(y = name, x= value, fill = name),
                      stat = "binline",scale = 0.75)+
  
  scale_fill_viridis_d()+
  
  labs(
    title = "Distribution of Vehicle Type",
    x = "Vehicles per Household",
    y = "",
    caption = "The majority of Vehicles owned are Automobiles"
  ) +
  
  theme_bw()+
  
  theme(legend.position = "none")

  
#Calculate and look at % of Vehicle Count is Auto vs Motorcycle


#Check if Auto + Motor = Total Vehicle Count
Predictor_Dataset_raw %>%
  
  select(hhld_id,veh_cnt,cnt_auto,cnt_mtrcyc) %>%
  
  mutate(
    Auto_and_Motor = cnt_auto + cnt_mtrcyc,
    Diff = veh_cnt - Auto_and_Motor
    ) %>%
  
  filter(Diff != 0)  #Confirms Auto + Motor = Total Vehicle Count
  
#Will need adjustment for multi-collinearity


#Plot Automobile Percentage
Predictor_Dataset_raw %>%
  
  select(hhld_id,veh_cnt,cnt_auto,cnt_mtrcyc) %>%
  
  mutate(
    Perc_Auto = formattable::percent(cnt_auto / veh_cnt), 
    #Perc_Motor = formattable::percent(cnt_mtrcyc / veh_cnt),
    .keep = "unused"
  ) %>%  
  
  ggplot()+
  
  geom_histogram(aes(x=Perc_Auto,y=after_stat(count/sum(count))),
                 fill = "mistyrose3")+
  
  labs(
    title = "Automobiles as a % of Household Vehicles",
    y = "% of Households",
    x = "Automobiles as % of Household Vehicles"
  )+
  
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  
  theme_bw()  


#Ownership Type of Vehicles
Predictor_Dataset_NA_adj %>%
  
  select(hhld_id,veh_cnt,veh_lien_cnt,veh_lease_cnt,veh_own_cnt) %>%
  
  pivot_longer(
    cols = -hhld_id,
    names_to = "Ownership_Type",
    values_to = "Vehicles"
  ) %>%
  
  ggplot()+
  
  #geom_boxplot(aes(y=value,x=name,fill = name)) +
  
  geom_density_ridges(aes(y = Ownership_Type, x= Vehicles, fill = Ownership_Type),
                      stat = "binline",scale = 0.75)+
  
  labs(
    title = "Vehicle Ownership Type",
    x = "# of Vehicles",
    y = "Ownership Type",
    caption = "Note - Vehicle Lien's Distribution post NA-adjustment"
  )+
  
  theme_bw()+
    
  theme(legend.position = "none") #Vast majority of household vehicles are automobile





# 2c.2 - EDA Predictor Dataset - Age --------------------------------------

#Age Distributions
Predictor_Dataset_raw %>%
  
  select(hhld_id,contains("age")) %>%
  
  pivot_longer(
    cols= -hhld_id,
    names_pattern = "(...)",
    names_to = "Type",
    values_to = "Age"
  ) %>%
  
  mutate(Type = factor(Type, levels = c("min","avg","max","hoh"))) %>%
  
  ggplot()+
  
  geom_density_ridges(aes(y=Type,x=Age,fill=Type),stat = "binline")+
  
  labs(
    title = "Distribution of Age Summary Statistics",
  )+
  
  scale_fill_viridis_d()+
  
  scale_x_continuous(breaks = seq(10,100,by = 5),lim = c(10,100)) +
  
  theme_bw()  #Normally Distributed with point mass at 16

#Interesting to note that HOH lines up closely with maximum age


#Surprised by x-axis range above, take a closer look at Min/Max age to confirm reasonability

#Min Age:
min(Predictor_Dataset_raw$min_age) #16 - reasonable

#Avg Age
min(Predictor_Dataset_raw$avg_age) #16 - reasonable 

#Suspect some correlation between younger drivers and no prior insurance

#Maximum Age:
Predictor_Dataset_raw %>% 
  
  select(max_age) %>%
  
  arrange(desc(max_age)) %>% 
  
  head(500) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=max_age),fill = "aquamarine3",color = "white" )+
  
  labs(
    title = "Max Driver Age Distribution",
    subtitle = "Oldest 500 (2.5%) Households",
    x = "Maximum Driver Age"
  )+
  
  theme_bw()  #~300 drivers age 80+, ~30 drivers age 90+

#Given dataset size and Normal nature of age, do not consider these outliers


# 2c.3 - EDA Predictor Dataset - Months Licensed --------------------------------------

#Suspect that it will be analagous to age, e.g. offset by ~16 years
Predictor_Dataset_raw %>%
  
  select(hhld_id,contains("mon_lic")) %>%
  
  pivot_longer(
    cols= -hhld_id,
    names_pattern = "(...)",
    names_to = "Type",
    values_to = "Months_Licensed"
  ) %>%
  
  mutate(
    Type = factor(Type, levels = c("min","avg","max","hoh")),
    
    #Convert to years for visualization purposes
    Years_Licensed = Months_Licensed/12) %>%
  
  ggplot()+
  
  geom_density_ridges(aes(y=Type,x=Years_Licensed,fill=Type),stat="binline")+
  
  labs(
    title = "Distribution of Months Licensed (in Years)",
    x = "Years Licensed",
    caption = "Converted to Years for visualization"
  )+
  
  scale_fill_viridis_d()+
  
  scale_x_continuous(breaks = seq(0,100,by = 5)) +
  
  theme_bw() #Yes, looks exactly like age, shifted left + a larger point mass at 0

#Expect these variables to be highly correlated with age variables




# 2c.4 - EDA Predictor Dataset - Count of Drivers -------------------------

#Perform some reasonability checks:

# Total Drivers = Male + Female?
Predictor_Dataset_raw %>%
  
  select(hhld_id,cnt_female,cnt_male,drvr_cnt) %>%
  
  mutate(
    Calc_Drv_Cnt = cnt_male + cnt_female,
    Diff = drvr_cnt - Calc_Drv_Cnt ) %>%
  
  filter(Diff !=0) #Checks out, Total Drivers = M + F
  

# Total Drivers = Married + Single?
Predictor_Dataset_raw %>%
  
  select(hhld_id,cnt_married,cnt_single,drvr_cnt) %>%
  
  mutate(
    Calc_Drv_Cnt = cnt_married + cnt_single,
    Diff = drvr_cnt - Calc_Drv_Cnt ) %>%
  
  filter(Diff !=0) #Checks out, Total Drivers = Single + Married

#Evaluate distributions of drivers
Predictor_Dataset_raw %>%
  
  select(hhld_id,cnt_yth,cnt_female,cnt_male,cnt_married,cnt_single) %>%
  
  pivot_longer(
    cols= -hhld_id,
    names_pattern = "_(.*)",
    names_to = "Demographic",
    values_to = "Driver_Count"
  ) %>%
  
  mutate(
    Demographic = factor(Demographic, levels = c("yth","male","female","single","married")),
    ) %>%
  
  ggplot()+
  
  geom_density_ridges(aes(y=Demographic,x=Driver_Count,fill=Demographic),
                      stat = "binline",scale = 0.9)+
  
  labs(
    title = "Driver Demographics"
  )+
  
  scale_fill_viridis_d(option = 5)+
  
  theme_bw() 

#Observations:
#Interesting (and intuitive) to note that married demographic
#has a higher % of 0 or 2, and relatively small % of only 1 driver

#Male/Female split evenly (primarily 0 or 1)

#Usually 0 or 1 youth drivers per HH


# 2c.5 - EDA Predictor Dataset - Major and Minor Violations ---------------

#Distribution of household traffic violations
bind_rows(
  Predictor_Dataset_raw %>%
    select(hhld_id, cnt_lic_susp) %>%
    
    pivot_longer(
      cols = -hhld_id,
      names_pattern = "(susp)",
      names_to = "Violation_Type",
      values_to = "Violations"
    ),
  
  Predictor_Dataset_raw %>%
    
    select(hhld_id,contains("viol")) %>% 
    
    pivot_longer(
      cols= -hhld_id,
      names_pattern = "(.*)_viol",
      names_to = "Violation_Type",
      values_to = "Violations"
    )
  
  )%>% 
  
  mutate(Violation_Type = factor(Violation_Type, levels = c("cnt_minr","avg_minr","cnt_majr","avg_majr","susp"))) %>%

  ggplot()+

  geom_density_ridges(aes(y=Violation_Type,x=Violations,fill = Violation_Type),
                      scale = 0.5,alpha = 0.7,stat = "binline")+
  
  labs(
    title = "Distribution of Household Traffic Violations",
    y = "",
    x = "Number of Violations"
  )+
  
  scale_fill_viridis_d()+
  
  xlim(NA,3)+
  
  theme_bw() +
  
  theme(legend.position = "none") #Most do not have any major/minor violations - this may be quite predictive



# 2c.6 - EDA Predictor Dataset - Current Insurer -----------------------

#Distribution of Inforce applicants
Predictor_Dataset_raw %>%
  
  select(inforce_ind) %>%
  
  group_by(inforce_ind) %>% 
  
  summarise(Percent = formattable::percent(n()/nrow(Predictor_Dataset_raw))) %>%
  
  ggplot()+
  
  geom_bar(aes(x="",y=Percent,fill=factor(inforce_ind,levels = c(1,0))),stat = "identity")+
  
  coord_polar("y",start = 0) +
  
  geom_label(aes(x="",y=Percent,label = Percent),
             nudge_x = 0.1,nudge_y = -0.2)+
  
  labs(
    title = "Households Currently Insured (Inforce Indicator)"
  )+
  
  scale_fill_viridis_d("Inforce Indicator")+
  
  theme_void()  #65% of applicants currently have a policy inforce


#Current Insurer is the only true non-numeric column - deep dive
Current_Insurer <- Predictor_Dataset_raw %>% 
  
  group_by(curnt_insurer) %>%
  
  summarise(
    Count = n(),
    Percent = formattable::percent(Count / nrow(Predictor_Dataset_raw))) %>%
  
  arrange(desc(Count)) %>% 
  
  view("Count of Current Insurer") #35% Missing is the largest group, followed by "Other" at 18%


#Plot to visualize 
Current_Insurer %>% 
  ggplot() +
  
  geom_bar(aes(x=reorder(curnt_insurer,-Count),y = Count,fill = curnt_insurer), stat = "identity")+
  
  geom_label(aes(x=reorder(curnt_insurer,-Count),y = Count,label = Count),nudge_y = 350)+
  
  labs(
    title = "Household Count of Current Insurer",
    x = "Current Insurer",
    y = "Household Count"
  )+
  
  theme_bw()+
  
  theme(legend.position = "none") 


#Group into "major" carriers, "missing", and "other"
#Compare Time Insured
Predictor_Dataset_NA_adj %>% 
  
  select(hhld_id, curnt_insurer,time_w_carr) %>%
  
  mutate(
    curnt_insurer_grp = factor(
      case_when(
        grepl("Missing",curnt_insurer,ignore.case = TRUE) ~ "Missing",
        grepl("Other",curnt_insurer,ignore.case = TRUE) ~ "Other Carrier",
        TRUE ~"Large Carrier"
      ),
      levels = c("Missing","Other Carrier","Large Carrier")
    )
    
  ) %>%
  
  ggplot() +
  
  geom_density_ridges(aes(y=curnt_insurer_grp,x=time_w_carr,fill=curnt_insurer_grp),
                      scale = 0.9,stat = "binline")+
  
  scale_x_continuous(breaks = seq(0,5,by=0.5))+
  
  labs(
    title = "Time with Current Insurer",
    y = "",
    x = "Time (years)",
    caption = "Grouped Named Insurers into 'Large'"
  )+
  
  scale_fill_viridis_d()+
  
  theme_bw()+
  
  theme(legend.position = "none")

#Most households that are currently insured (large or other)
#have been with their current insurer for 5+ years
#The rest are uniformly divided by 0- 4.5 years



# 2c.7 - EDA Predictor Dataset - Home Ownership & HO Insurance ------------

Predictor_Dataset_raw %>%
  
  select(hhld_id,fire_ind,homeowner_ind) %>%
  
  group_by(fire_ind,homeowner_ind) %>%
  
  summarise(count_hh = formattable::percent(n()/nrow(Predictor_Dataset_raw),digits = 0))  %>% 
  
  mutate(
    fire_ind = factor(fire_ind,levels=c(0,1)),
    homeowner_ind = factor(homeowner_ind,levels = c(0,1))
  ) %>%
  
  ggplot()+
  
  geom_tile(aes(x=fire_ind,y=homeowner_ind,fill = count_hh))+
  
  geom_label(aes(x=fire_ind,y=homeowner_ind,label = count_hh ))+
  
  labs(
    title = "Homeownership vs HO Insurance",
    x= "HO Insurance Indicator",
    y = "Homeowner Indicator"
  )+
  
  scale_fill_viridis()+
  
  theme_bw()+
  
  theme(legend.position ="none") 

#HO is required for a mortgage, but not legally required
#60-40 split of HO Insured vs Uninsured
#~50-50 split for Homeownership


# 2c.8 - EDA Predictor Dataset - Vehicle Ownership Type ----------------------------------------------------------


#Calculate % of ownership type
Predictor_Dataset_NA_adj %>%
  
  select(hhld_id,veh_cnt,veh_own_cnt,veh_lien_cnt,veh_lease_cnt) %>%
  
  mutate(
    Own_Perc = formattable::percent(veh_own_cnt/veh_cnt),
    Lease_Perc = formattable::percent(veh_lease_cnt/veh_cnt),
    Lien_Perc = formattable::percent(veh_lien_cnt/veh_cnt),
    .keep = "unused"
  ) %>% 
  
  pivot_longer(
    cols = -hhld_id,
    names_to = "Ownership_Type",
    values_to = "Percentage"
  ) %>% 
  
  group_by(Ownership_Type)%>%
  
  ggplot()+
  
  geom_histogram(aes(x=Percentage,y=after_stat(count/sum(count))))+
  
  scale_fill_viridis_d()+
  
  labs(
    title = "Vehicle Ownership Breakdown",
    x = "% of Total Household Vehicles",
    y = "% of Households"
  )+
  
  facet_wrap(~Ownership_Type)+
  
  scale_y_continuous(labels = scales::percent)+
  
  theme_bw()


# 2c.9 - EDA Predictor Dataset - Auto Coverages ---------------------------

#Auto Coverage Percentages
Predictor_Dataset_raw %>%
  
  select(hhld_id,starts_with("veh_w"),veh_cnt) %>% 
  
  #Convert to % of Vehicles
  mutate(
    Perc_Coll = formattable::percent(veh_w_coll_cnt / veh_cnt),
    Perc_Comp = formattable::percent(veh_w_comp_cnt / veh_cnt),
    Perc_Ers = formattable::percent(veh_w_ers_cnt / veh_cnt),
    .keep = "unused"
  ) %>% 
  
  pivot_longer(
    cols = -hhld_id,
    names_pattern = "Perc_(.*)",
    names_to = "Covg",
    values_to = "Percent"
  ) %>%
  
  ggplot() +
  
  geom_density_ridges(aes(y=Covg,x=Percent,fill=Covg),
                      scale = 1,stat = "binline")+
  
  scale_fill_viridis_d(option = 6)+
  
  labs(
    title = "% of Household Vehicles Insured (by Coverage)",
    x = "Percent of Household Vehicles",
    y = "Coverage"
  )+
  
  scale_x_continuous(breaks = seq(0,1,by=0.1),
                     #lim=c(0,1),
                     labels = scales::percent)+
  
  theme_bw()
  

#These insurance coverages are typically legally required
#Expect high correlation between coverages, and with total # of Vehicles


# 2c.10 - EDA Predictor Dataset - BI Limits per Person vs per Occurrence --------------------------------


#Common for BI per Occurrence limit to be a multiple of per Person
Predictor_Dataset_raw %>%
  
  select(hhld_id,starts_with("curnt_bi")) %>% 
  
  #Common for BI per Occurrence limit to be a multiple of per Person
  mutate(
    Occ_Limit_Ratio = curnt_bi_upp/curnt_bi_low
  ) %>% 
  
  group_by(Occ_Limit_Ratio) %>%
  
  summarise(Count = n()) %>%
  
  view("Distribution of BI Limit per Person : per Occ")

#100% of households have BI Per Occurrence Limit = BI Per Person Limit, i.e. ratio = 1.0


#These 2  columns are identical - will need to remove one:
all.equal(Predictor_Dataset_raw$curnt_bi_low,Predictor_Dataset_raw$curnt_bi_upp)

#Show Summary of Current BI Liability Limits
Predictor_Dataset_raw %>%
  
  select(starts_with("curnt_bi")) %>% summary()

#Distribution of BI Limit 
Predictor_Dataset_raw %>%
  
  select(hhld_id,BI_Limit = curnt_bi_low) %>% 
  
  ggplot()+
  
  geom_histogram(aes(x=BI_Limit, y=after_stat(count/sum(count))),
                 position = "dodge",fill = "turquoise")+
  
  labs(
    title = "Dsitribution of Current BI Liability Limits",
    x = "BI Liability Limit (thousands $)",
    y = "% of Households",
    caption = "Per Person = Per Occurrence"
  )+
  
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = seq(0,250,25))+
    
  theme_bw()

#Anecdotally, I believe there is some  correlation with selection of higher limits
#and worse loss experience (e.g. customers who expect to need higher limits, choose higher limits)

# 2c.11 - EDA Predictor Datasets - BI Limit Prior vs Current --------------


#Prior BI Liability Limit Distribution
Predictor_Dataset_NA_adj %>%  
  
  select(prior_bi) %>%
  
  group_by(prior_bi) %>%
  
  summarise(Count = n()) %>%
  
  ggplot(aes(x=prior_bi,y=Count))+
  
  geom_bar(aes(fill = prior_bi),stat = "identity")+
  
  geom_label(aes(label = Count),nudge_y = 400)+
  
  labs(
    title = "Prior BI Liability Limit Distribution",
    x = "Prior BI Liability Limit",
    caption = "Prior BI Limit = 0 corresponds to no prior policy"
  )+
  
  scale_x_continuous(breaks = seq(0,250000,by=25000),
                     labels = scales::comma)+
  
  theme_bw()+
  
  theme(legend.position = "none")
         
         
#Change in BI Liability Limit per Person
Predictor_Dataset_NA_adj %>%  
  
  #note - prior BI is in $, but curnt BI is in 1000s of $
  select(prior_bi,curnt_bi_low) %>% 
  
  mutate(
    prior_bi = factor(prior_bi, levels = c(0,25000,50000,100000,250000)),
    current_bi = factor(1000*curnt_bi_low,levels = c(25000,50000,100000,250000)),
    .keep = "unused"
  ) %>%
  
  group_by(prior_bi,current_bi) %>%
  
  summarise(Percent = formattable::percent(n()/nrow(Predictor_Dataset_NA_adj),digit =1)) %>% 
  
  ggplot(aes(x=prior_bi,y=current_bi))+
  
  geom_tile(aes(fill = Percent))+
  
  geom_label(aes(label = Percent))+
  
  scale_fill_viridis()+
  
  labs(
    title = "Prior vs Current BI Liability Limits",
    x = "Prior BI Limits",
    y = "Current BI Limits"
  )+
  
  theme_bw() #Regardless of Prior BI Limit, most selected $25k current BI Limit (lowest BI Limit)
  


# 2c.11 - EDA Predictor Dataset - Credit Score ----------------------------

#Credit Score Distribution prior to NA Adjustment
Credit_Score_Before <- Predictor_Dataset_raw %>%
  
  select(hhld_id,credit_score) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=credit_score), 
                 fill = "cornflowerblue",color ="white")+
  
  labs(
    title = "Household Credit Score",
    subtitle = "Before NA Adjustment",
    y = "# of Households",
    x = "Credit Score"
  )+
  
  scale_x_continuous(breaks = seq(300,850,by = 50))+
  scale_y_continuous(breaks = seq(0,1500,by = 250))+
  
  theme_bw()  #Mostly normally distributed around ~650, with point mass at 850


#Summary
Predictor_Dataset_raw$credit_score %>% summary()


#After Adjustment for NAs
Credit_Score_After <- Predictor_Dataset_NA_adj %>%
  
  select(hhld_id,credit_score) %>%
  
  ggplot()+
  
  geom_histogram(aes(x=credit_score), 
                 fill = "midnightblue",color ="white")+
  
  labs(
    title = "Household Credit Score",
    subtitle = "After NA Adjustment",
    y = "# of Households",
    x = "Credit Score"
  )+
  
  scale_x_continuous(breaks = seq(300,850,by = 50))+
  scale_y_continuous(breaks = seq(0,2500,by = 250))+
  
  theme_bw()  #Mostly normally distributed around ~650, with point mass at 850


#Side-by-Side Comparison
grid.arrange(Credit_Score_Before,Credit_Score_After,
             ncol=2)


# 3a. Cleaning Data - Predictor Dataset --------------------------------------------------------

#Begin with NA Adjusted Data
Predictor_Dataset_clean <- Predictor_Dataset_NA_adj %>%
  
  #1. Remove zipcodes because they have been anonymized (therefore cannot be aggregated to larger geographic regions)
  #100+ unique zip code levels with only 20,000 total data points does not allow sufficient data per level
  select(-c(zipcode)) %>%
  
  #2. Since BI per Person = BI per Occ Limit, only need to keep one variable
  #Convert BI Liability limit from thousands of $, and rename
  mutate(
    BI_Limit = curnt_bi_low * 1000
  ) %>%
  
  #Drop original BI Limit  variables
  select(-c(curnt_bi_low,curnt_bi_upp)) %>%
  
  #3. Encode hoh_married from True/False to indicator
  mutate(
    hoh_married_ind = ifelse(hoh_married,1,0),
    .keep = "unused"
  ) %>%
  
  #4a. Reduce categories for Categorical Variable (curnt_insurer) 
  #Reduce levels improves interpretability
  #Furthermore, individual insurer's  UW practices frequently change overtime,
  #reducing reliability of an individual insurer for future use
  mutate(
    Prior_Insurer_Bucket = case_when(
      curnt_insurer == "MISSING" ~"Missing",
      curnt_insurer == "OTHER" ~"Minor_Insurer",
      TRUE ~"Major_Insurer"
    ),
    
    #Prior_Insurer_Bucket = factor(Prior_Insurer_Bucket,levels = c())
    
    .keep = "unused"
  ) %>%
  
  #4b. Encode to Dummy Variables for correlation analysis purposes
  #Can do this manually, since only 3 levels 
  mutate(
    Prior_Insurer_Missing = ifelse(Prior_Insurer_Bucket == "Missing",1,0),
    
    Prior_Insurer_Minor = ifelse(Prior_Insurer_Bucket == "Minor_Insurer",1,0),
    
    Prior_Insurer_Major = ifelse(Prior_Insurer_Bucket == "Major_Insurer",1,0),
    
    
    
  ) %>% view("Cleaned Predictor Dataset") #For modeling, use Major_Insurer as base level (most observations)
  


  

# 3b. Cleaning Data - Claim Data ------------------------------------------

#Convert to a more usable format for modeling
Claim_Data_clean <- Claim_Data_raw %>%
  
  #Filter out losses after 1/1/2017
  filter(clm_dates < '2017-01-01' ) %>%
  
  mutate(
    #Identify and Extract Accident Year (AY)
    Acc_Yr = year(clm_dates),
    
    #Convert to time since 2017
    Yrs_Ago = 2017 - Acc_Yr,
    
    #Bucket Years
    Yr_Buckets = case_when(
      Yrs_Ago <= 1 ~ "< 1 yr",
      Yrs_Ago > 1 & Yrs_Ago <= 3 ~"1-3 yrs",
      Yrs_Ago > 3 & Yrs_Ago <= 5 ~"3-5 yrs",
      Yrs_Ago > 5 ~"5+ yrs"
    ),
    
    #Convert to Factor
    Yr_Buckets = factor(Yr_Buckets,levels=c("< 1 yr","1-3 yrs","3-5 yrs","5+ yrs"))
  ) %>%
  
  #Rename at fault indicator to string (helpful for column titles later)
  mutate(At_Fault_Ind =ifelse (aft_ind == 1,"_f","_nf"),
         .keep = "unused") %>%
  
  group_by(hhld_id,Yr_Buckets, At_Fault_Ind)  %>% 
  
  #Summarise losses and claim counts
  summarise(
    Total_Paid = sum(pd_amt),
    #Total_Claim_Count = n(),
    #Avg_Paid = mean(pd_amt)
  ) %>%
  
  arrange(At_Fault_Ind,Yr_Buckets) %>% 
  
  #Pivot Wider
  pivot_wider(
    id_cols = c(hhld_id),
    names_from = c(Yr_Buckets,At_Fault_Ind),
    values_from = c(Total_Paid)
  ) %>% 
  
  #Replace NAs with 0 (NAs generated since not all households have claims each year
  replace(is.na(.),0) %>%
  
  #Sort by household ID
  arrange(hhld_id) %>% view("Cleaned Claim Dataset")
  
#Time allowing, can explore a model based on Total Claim Count, instead of Total Paid


# 3c. Create Final Cleaned Data -------------------------------------------

#Combined Predictor Dataset with prior Claims Data
Clean_Predictors <- left_join(
  Predictor_Dataset_clean,
  Claim_Data_clean,
  by = "hhld_id"
) %>% 

  #Replace NAs with 0 (NAs generated since not all households have claims each year
  replace(is.na(.),0) %>%
  
  arrange(hhld_id)  

# 4a. Correlation Analysis -- Round 1 ----------------------------------

Correl_Matrix <- Clean_Predictors %>%
  
  #Drop ID column, and non-numeric columns
  select(-c(hhld_id,Prior_Insurer_Bucket)) %>%
  
  cor() %>% view("Predictor Correlation Matrix") 

#Correlation Heat map
ggcorrplot(Correl_Matrix, type ="lower",title = "Predictor Dataset Correlation Heatmap",
           show.diag = TRUE, lab = TRUE,digits = 1,lab_size = 2)


#Identify the pairs of predictors that are most highly correlated (magnitude)
Correl_Matrix %>%
  
  rownames_to_column("A") %>%
  
  pivot_longer(
    cols = -A,
    names_to = "B",
    values_to = "Correlation") %>%
  
  #Filter out the self-pairs (Correlation = 1.0)
  filter(A != B) %>%
  
  #Filter out duplicates (e.g. correlation between A&B same as B&A)
  filter(!duplicated(paste0(pmax(A,B),pmin(A,B)))) %>% 
  
  #Focus on highly correlated
  filter(abs(Correlation) > 0.5) %>%

  arrange(desc(abs(Correlation))) %>% 
  
  view("Sorted Pairwise Correlation - v0")


#From visual inspection of initial correlation:

#Highly Positive Correlated:

#1. Related to Age and Months Licensed:
#Age variants &  Months Licensed variants ---> makes sense, older usually means licensed for longer
#Age variants amongst themselves (e.g. min,max, avg) ---> Makes sense, summary stats of same data
#Months Licensed variants amongst themselves (e.g. min,max, avg) ---> Makes sense, summary stats of same data

#2. Related to Count of Individuals:
#Count of Married & HOH Marital status  ---> Usually 1 marriage per household, and HOH is usually part of that marriage
#Driver count and marital count/status --> Married means more people usually means more drivers

#3. Others:
#Vehicle Count & Auto Count ---> makes sense, 90%+ owned vehicles are autos
#Coll & Comp covg ---> makes sense, according to prior analysis 



#Highly Negatively Correlated:

#1. Count of individuals
#Count of Married/HOH Married vs Single --> Makes sense
#Count of Men/Women ---> Makes sense





#Reccommended Variables to be removed based on Correlation Filtering Methods:
Clean_Predictors %>%
  
  #Drop ID column, and non-numeric columns
  select(-c(hhld_id,Prior_Insurer_Bucket)) %>%
  
  cor() %>% 
  
  findCorrelation(cutoff = 0.6,names=TRUE, verbose=TRUE,exact = TRUE)  

#Explore PCA for Age/Months Licensed, Driver Demographics, Violations, Veh Count



# 4b.1 PCA: Age and Months License -------------------------------------------------

#Correlation among Age and Months Licensed
Clean_Predictors %>%
  
  select(contains(c("age","mon_lic"))) %>%
  
  cor() %>% 
  
  ggcorrplot(type = "lower",title = "Age and Months Licensed Correlation Matrix",
             lab = TRUE, digits=2)


#Perform PCA
Age_Lic_PCA <- Clean_Predictors %>%
  
  select(contains(c("age","mon_lic"))) %>%
  
  prcomp(center = TRUE,scale. = TRUE)

#PCA
Age_Lic_PCA

#Elbow/Scree Plot
ggplot()+
  
  geom_line(aes(x=c(1:8),y=(Age_Lic_PCA$sdev^2)/sum(Age_Lic_PCA$sdev^2)))+
  
  geom_text(aes(x=c(1:8),y=(Age_Lic_PCA$sdev^2)/sum(Age_Lic_PCA$sdev^2)),
             label=formattable::percent((Age_Lic_PCA$sdev^2)/sum(Age_Lic_PCA$sdev^2),digits =0),
            nudge_x = 0.3,nudge_y = 0.03)+
  
  geom_vline(xintercept =3,color = "blue",linetype = "dashed")+
  
  labs(
    title = "Age and Months Licensed PCA Scree Plot",
    x = "",
    y = "% of Variance Explained"
  )+
  
  scale_y_continuous(labels = scales::percent)+
  
  scale_x_continuous(breaks = seq(0,8,by=1))+
  
  theme_bw()


summary(Age_Lic_PCA) #Use PC1 & PC2


#Extract selected Age and Months Licensed PC
Age_PC <- Age_Lic_PCA$x[,1:2] %>% 
  
  as.data.frame() %>%
  
  rename(
    Age_PC1 = PC1,
    Age_PC2 = PC2
  ) %>% tibble()


# 4b.2 PCA: Driver Demographics --------------------------------------------------

#Correlation among Driver Demographics
Clean_Predictors %>%
  
  select(drvr_cnt,cnt_yth,cnt_female,cnt_male,cnt_married,cnt_single,hoh_married_ind) %>%
  
  cor() %>% 
  
  ggcorrplot(type = "lower",title = "Driver Demographics Correlation Matrix",
             lab = TRUE, digits=2)


#Drop cnt_married as per correlation filtering due to high correl with hoh_married_ind
#No PCA necessary based on correlation alone
#However, high risk of multi-collinearity, and benefit of dimensionality reduction --> Run PCA


Driver_Demog_PCA <- Clean_Predictors %>%
  
  select(drvr_cnt,cnt_yth,cnt_female,cnt_male,cnt_married,cnt_single,hoh_married_ind) %>%
  
  prcomp(center = TRUE,scale. = TRUE)

Driver_Demog_PCA


#Elbow Plot
ggplot()+
  
  geom_line(aes(x=c(1:7),y=(Driver_Demog_PCA$sdev^2)/sum(Driver_Demog_PCA$sdev^2)))+
  
  geom_text(aes(x=c(1:7),y=(Driver_Demog_PCA$sdev^2)/sum(Driver_Demog_PCA$sdev^2)),
            label=formattable::percent((Driver_Demog_PCA$sdev^2)/sum(Driver_Demog_PCA$sdev^2),digits =0),
            nudge_x = 0.3,nudge_y = 0.03)+
  
  geom_vline(xintercept = 5,color = "blue",linetype = "dashed")+
  
  labs(
    title = "Driver Demographics PCA Scree Plot",
    x = "",
    y = "% of Variance Explained"
  )+
  
  scale_y_continuous(labels = scales::percent)+
  
  scale_x_continuous(breaks = seq(0,8,by=1))+
  
  theme_bw()


summary(Driver_Demog_PCA) #Use PC1 - PC4


#Extract selected Driver Demographics PC
Demog_PC <- Driver_Demog_PCA$x[,1:4] %>% 
  
  as.data.frame() %>%
  
  rename(
    Demog_PC1 = PC1,
    Demog_PC2 = PC2,
    Demog_PC3 = PC3,
    Demog_PC4 = PC4,
  ) %>% tibble()


# 4b.3 PCA: Violations ----------------------------------------------------------

#Correlation among violations
Clean_Predictors %>%
  
  select(contains("viol"),cnt_lic_susp) %>%
  
  cor() %>% 
  
  ggcorrplot(type = "lower",title = "Violations Correlation Matrix",
             lab = TRUE, digits=2)
  
#As per Correlation Filtering, remove count of major & avg minor violations
#No PCA necessary


# 4b.4 PCA Vehicle Count -------------------------------------------------------

#Correlation among Vehicle Count
Clean_Predictors %>%
  
  select(veh_cnt,  #Total
         cnt_auto,cnt_mtrcyc, #Auto vs Motorcycle
         veh_lien_cnt,veh_lease_cnt,veh_own_cnt, #Ownership type
         veh_w_coll_cnt,veh_w_comp_cnt,veh_w_ers_cnt #Coverage
         ) %>%
  
  cor() %>% 
  
  ggcorrplot(type = "lower",title = "Vehicle Count Correlation Matrix",
             lab = TRUE, digits=2)

#Strong correlation between vehicle count and most variables
#Strong correlation between coll and comp covg
#Potential for multi-collinearity among these variables
#Perform PCA to prevent multi-collinearity


Veh_Cnt_PCA <- Clean_Predictors %>%
  
  select(veh_cnt,  #Total
         cnt_auto,cnt_mtrcyc, #Auto vs Motorcycle
         veh_lien_cnt,veh_lease_cnt,veh_own_cnt, #Ownership type
         veh_w_coll_cnt,veh_w_comp_cnt,veh_w_ers_cnt #Coverage
  ) %>%
  
  prcomp(center = TRUE,scale. = TRUE)

Veh_Cnt_PCA

#Elbow Plot
ggplot()+
  
  geom_line(aes(x=c(1:9),y=(Veh_Cnt_PCA$sdev^2)/sum(Veh_Cnt_PCA$sdev^2)))+
  
  geom_text(aes(x=c(1:9),y=(Veh_Cnt_PCA$sdev^2)/sum(Veh_Cnt_PCA$sdev^2)),
            label=formattable::percent((Veh_Cnt_PCA$sdev^2)/sum(Veh_Cnt_PCA$sdev^2),digits =0),
            nudge_x = 0.3,nudge_y = 0.03)+
  
  geom_vline(xintercept = 7,color = "blue",linetype = "dashed")+
  
  labs(
    title = "Driver Demographics PCA Scree Plot",
    x = "",
    y = "% of Variance Explained"
  )+
  
  scale_y_continuous(labels = scales::percent)+
  
  scale_x_continuous(breaks = seq(0,9,by=1))+
  
  theme_bw()


summary(Veh_Cnt_PCA) #Use PC1 - PC6


#Extract selected Vehicle Count PC
Veh_Cnt_PC <- Veh_Cnt_PCA$x[,1:6] %>% 
  
  as.data.frame() %>%
  
  rename(
    Veh_Cnt_PC1 = PC1,
    Veh_Cnt_PC2 = PC2,
    Veh_Cnt_PC3 = PC3,
    Veh_Cnt_PC4 = PC4,
    Veh_Cnt_PC5 = PC5,
    Veh_Cnt_PC6 = PC6,
  ) %>% tibble()


# 4b.5 Correlation Adjustments -- Round 1 ------------------------------------

Clean_Predictors_Correladj_v1 <- Clean_Predictors %>%
  
  #1. Drop all variables related to Age and Months Licensed
  select(-contains(c("age","mon_lic"))) %>%
  
  #2. Drop all variables related to Driver Demographics
  select(-c(drvr_cnt,cnt_yth,cnt_female,cnt_male,cnt_married,cnt_single,hoh_married_ind)) %>%
  
  #2. Drop all variables related to Vehicle Count
  select(-c(veh_cnt,  #Total
         cnt_auto,cnt_mtrcyc, #Auto vs Motorcycle
         veh_lien_cnt,veh_lease_cnt,veh_own_cnt, #Ownership type
         veh_w_coll_cnt,veh_w_comp_cnt,veh_w_ers_cnt #Coverage
  )) %>%
  
  #Replace with their Principal Components
  bind_cols(Age_PC,Demog_PC,Veh_Cnt_PC)


#Re-evaluate these variables after PCA analysis
  #2. Drop Count Married
  #select(-cnt_married) %>%
  
  #3. Drop Count of Major and Minor Violations
  #select(-cnt_majr_viol,-cnt_minr_viol) %>%
  
  #4. Drop Vehicle Count
  #select(-veh_cnt) %>%

  #5. Drop Inforce Indicator
  #select(-inforce_ind)

  



# 4c.  Correlation Analysis -- Round 2 -------------------------------------

Correl_Matrix_Adj_v1 <- Clean_Predictors_Correladj_v1 %>%
  
  #Drop ID column, and non-numeric columns
  select(-c(hhld_id,Prior_Insurer_Bucket)) %>%
  
  cor() %>% view("Predictor Correlation Matrix - Adj v1")

#Correlation Heat map
ggcorrplot(Correl_Matrix_Adj_v1, type ="lower",title = "Predictor Dataset Correlation Heatmap - Adj v1",
           show.diag = TRUE, lab = TRUE,digits = 1,lab_size = 2)

#Overall, strong improvement and reduction in pairwise correlation
#Age PC2 shows some mild negative correlation with several variables
#Some mild positive correlation with variables related to current (prior) insurer e.g. time_w_carr,inforce_ind,prior_bi




#Identify the pairs of predictors that are most highly correlated (magnitude)
Correl_Matrix_Adj_v1 %>%
  
  rownames_to_column("A") %>%
  
  pivot_longer(
    cols = -A,
    names_to = "B",
    values_to = "Correlation") %>%
  
  #Filter out the self-pairs (Correlation = 1.0)
  filter(A != B) %>%
  
  #Filter out duplicates (e.g. correlation between A&B same as B&A)
  filter(!duplicated(paste0(pmax(A,B),pmin(A,B)))) %>% 
  
  #Focus on highly correlated
  filter(abs(Correlation) > 0.5) %>%
  
  arrange(desc(abs(Correlation))) %>% 
  
  view("Sorted Pairwise Correlation - Adj v1")


#Variables removed based on Correlation Filtering Methods:
Clean_Predictors_Correladj_v1 %>%
  
  #Drop ID column, and non-numeric columns
  select(-c(hhld_id,Prior_Insurer_Bucket)) %>%
  
  cor() %>% 
  
  findCorrelation(cutoff = 0.6,names=TRUE, verbose=TRUE,exact = TRUE)  

#Inforce Indciator, Count Minor, and Average Major to be removed


# 4c.1 Correlation Adjustment 2 -----------------------------------------------------

Clean_Predictors_Correladj_v2 <- Clean_Predictors_Correladj_v1 %>%
  

  #1. Drop Avg Major Violations and Count of Minor Violations
  select(-avg_majr_viol,-cnt_minr_viol) %>%

  #2. Drop Inforce Indicator
  select(-inforce_ind)
  
#3 Retain Prior Insurer Buckets as it would be inappropriate to remove 
#specific levels of the categorical varaible (e.g. missing level)

#4. Retain Age PC2 - do not want to lose explanatory power related to 
# Age, even if -0.67 correlation is above the threshold of 0.6


# 4d.  Correlation Analysis -- Round 3 -----------------------------------------

Correl_Matrix_Adj_v2 <- Clean_Predictors_Correladj_v2 %>%
  
  #Drop ID column, and non-numeric columns
  select(-c(hhld_id,Prior_Insurer_Bucket)) %>%
  
  cor() %>% view("Predictor Correlation Matrix - Adj v2")

#Correlation Heat map
ggcorrplot(Correl_Matrix_Adj_v2, type ="lower",title = "Predictor Dataset Correlation Heatmap - Adj v2",
           show.diag = TRUE, lab = TRUE,digits = 1,lab_size = 2)

#Much better - no longer any pair-wise |correlation| > 0.6 unaccounted for

#Identify the pairs of predictors that are most highly correlated (magnitude)
Correl_Matrix_Adj_v2 %>%
  
  rownames_to_column("A") %>%
  
  pivot_longer(
    cols = -A,
    names_to = "B",
    values_to = "Correlation") %>%
  
  #Filter out the self-pairs (Correlation = 1.0)
  filter(A != B) %>%
  
  #Filter out duplicates (e.g. correlation between A&B same as B&A)
  filter(!duplicated(paste0(pmax(A,B),pmin(A,B)))) %>% 
  
  #Focus on highly correlated
  filter(abs(Correlation) > 0.5) %>%
  
  arrange(desc(abs(Correlation))) %>% 
  
  view("Sorted Pairwise Correlation")


#Retain Prior Insurer Buckets as it would be inappropriate to remove 
#specific levels of the categorical varaible (e.g. missing level)

#Retain Age PC2 - do not want to lose explanatory power related to 
# Age, even if -0.67 correlation is above the threshold of 0.6



# Final Predictors Dataset ------------------------------------------------

#Make Final Adjustments
Clean_Predictors_Final <- Clean_Predictors_Correladj_v2  %>%
  
  #Drop dummy variables for  current insurer
  select(-c(Prior_Insurer_Major,Prior_Insurer_Minor,Prior_Insurer_Missing)) %>%
  
  #Convert Prior Insurer Bucket to factor, with "Major Insurer" as base (most observations)
  mutate(Prior_Insurer_Bucket = factor(Prior_Insurer_Bucket,
                                        levels = c("Major_Insurer","Minor_Insurer","Missing"))
    
  ) %>%
  
  #Export
  write_csv("Clean_Predictors_Final.csv")
