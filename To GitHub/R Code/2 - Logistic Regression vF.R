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
library(caret)     #Classification and Regression
library(car)       #Multicollinearity
library(ggfortify) #Plotting Principal Components
library(lubridate) #For working with dates
library(glmnet)    #For Lasso and Ridge Regression
library(gridExtra) #For arranging multiple plots
library(pROC)      #For ROC Curve
library(ROCR)      #Another package option for ROC Curve
library(brglm2)     #Firth Logistic Regression

#Discourage Scientific Notation
options(scipen=999)

#Set Working Directory
setwd("C:/Users/Eric_/Desktop/State Farm DS Exercise")



# Import Raw Data ---------------------------------------------------------

#Claims Data
Claim_Data_raw <- read_csv("Claim_Data.csv", 
                           col_types = cols(clm_dates = col_date(format = "%m/%d/%Y")))

#Predictor Dataset
Predictor_Dataset_raw <- read_csv("Predictor_Dataset.csv")

#Subsequent Loss Experience Data
Subsequent_Loss_Experience_raw <- read_csv("Subsequent_Loss_Experience.csv")

# Import Results from "1 - EDA and Data Preparation" ---------------------------------------------

Clean_Predictors_Final <- read_csv("Clean_Predictors_Final.csv")


# 0. Creating Functions ------------------------------------------

#Function to evaluate GLM performance
GLM_Evaluation <- function(GLM){

  #Print key model information
  GLM %>% print()
  GLM %>% summary() %>% print()
  
  
  #Create data frame for Variable Importance 
  #*for Linear Models, absolute value of t-stat is used
  Importance <- varImp(GLM)
  
  #1. Plot Variable importance
  Importance_Plot <- Importance %>%
    
    ggplot(aes(x=rownames(Importance),y=Overall))+
    
    geom_bar(stat = "identity",fill = "lightgreen")+
    
    labs(
      title = "Logistic Regression Feature Importance",
      #caption = "Based on Absolute value of t-stat"
    )+
    
    theme_bw()
  
  #show(Importance_Plot)
  
  
  #2. VIF Plot for Multi-Collinearity
  VIF_Plot <- GLM$finalModel %>% vif() %>% enframe() %>% arrange(value) %>%
    
    ggplot()+
    
    geom_bar(aes(y=name,x=value),stat="identity", fill = "skyblue")+
    
    geom_vline(xintercept = 5,linetype = "dashed",color = "red")+
    
    labs(
      title = "Test for Multi-Collinearity",
      subtitle = "VIF: Variance Inflation Factor",
      x = "VIF",
      y = "Feature",
      caption = "Assume VIF threshold = 5.0"
    )+
    
    scale_x_continuous(limits = c(0,10),breaks = seq(0,10,by=1))+
    
    theme_bw()
    
    #show(VIF_Plot)
    
  
  #3. Generate Confusion Matrix and Statistics (assumes 50% threshold)
  CM <- confusionMatrix(confusionMatrix(GLM,positive = "Yes")$table,
                    positive = "Yes") %>% print()
  
  
  #Confusion Matrix Plot
  CM_Plot <- CM$table %>% as.data.frame() %>%
    
    ggplot(aes(y=Prediction,x=Reference)) +
    
    geom_tile(aes(fill=Freq))+
    
    geom_label(aes(label = scales::percent(Freq/100)))+
    
    scale_fill_viridis(option = "D")+
    
    labs(
      title = "Confusion Matrix",
      subtitle = "50% Threshold"
    )+
    
    theme_bw()+
    
    theme(legend.position = "none")
  
  #show(CM_Plot)
  
  
  #4. Plot Confusion Matrix Distribution
  CM_Hist <- GLM$pred %>%
    
    #Rename for clarity
    mutate(
      pred = factor(ifelse(pred == "Yes","Pred - Yes","Pred - No"),levels = c("Pred - Yes","Pred - No")),
      obs = ifelse(obs == "Yes", "Actual - Yes","Actual - No")
    ) %>%
    
    ggplot()+
    
    geom_histogram(aes(x=Yes,y= after_stat(count/sum(count))),fill="deeppink")+
    
    geom_vline(xintercept = 0.5,linetype = "dashed",color = "blue")+
    
    facet_grid(pred~obs,scale= "free_y")+
    
    labs(
      title = "Confusion Matrix Distribution",
      subtitle = "50% Threshold",
      x = "Predicted Future Claim Probability",
      y = "% of Households",
      caption = "Note: Different y-Axis Scales"
    )+
    
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(labels = scales::percent)+
    
    theme_bw()
  
  #show(CM_Hist)
  
  #Plot together in 2x2
  grid.arrange(Importance_Plot,CM_Plot,VIF_Plot,CM_Hist,
               ncol = 2, nrow = 2)
  
  
  #Accuracy and Kappa Distribution
  show(resampleHist(GLM))
  
  return(GLM)
}

#Lasso Regression for Feature Selection
Lasso_Variables <- function(Modeling_Data){
  
  #Convert Train Dataset into Design Matrix for glmnet
  Lasso_Data <- model.matrix(future_clm_ind~.,select(Modeling_Data,-c(hhld_id,future_clm_ind_yn)))
  
  #Cross-Validation to determine optimal lambda parameter
  CV_Lambda <- cv.glmnet(x= Lasso_Data,
                         y= Modeling_Data$future_clm_ind,
                         #family = "binomial",
                         nfolds = 10)
  
  #Options for Lambda          
  CV_Lambda$lambda.min  #Lambda lowest CV error
  CV_Lambda$lambda.1se  #Simpler than best model (lambda.min), but within 1 std. error
  
  
  #Fit Lasso - prefer simpler (lambda.1se) model to prevent overfitting
  Lasso_Model <- glmnet(x= Lasso_Data,
                        y= Modeling_Data$future_clm_ind,
                        family = "binomial",
                        alpha = 1,
                        lambda = CV_Lambda$lambda.1se)
  
  Lasso_Model %>% print()
  Lasso_Model %>% summary() %>% print()
  
  #Lasso Coefficients
  Lasso_Coef <- Lasso_Model$beta %>% print()
  
  
  #Extract Variable Names (except Intercept)
  Lasso_Variables <- dimnames(Lasso_Coef)[[1]][which(Lasso_Coef != 0)]
  
  print("Lasso Variables:")
  return(Lasso_Variables)
}


# 1. Prepare Modeling Dataset (Independent + Response Variable) --------------


#Join cleaned predictor dataset with response variable dataset (subsequent loss exp)
Modeling_Dataset <- left_join(
  Clean_Predictors_Final,
  Subsequent_Loss_Experience_raw %>%
    select(hhld_id,future_clm_ind),
  
  by = "hhld_id"
) %>% 
  
  #Convert Response variable to factor for train function
  mutate(
    future_clm_ind_yn = ifelse(future_clm_ind == 0,"No","Yes")
    ) %>%
  
  #Relocate response variable to front
  relocate(hhld_id,future_clm_ind,future_clm_ind_yn) %>% 
  
  arrange(hhld_id)




# 2. Set-up 10-Fold Cross Validation  -------------------------------------------

#Set up 10-Folder Cross Validation specifications
CV_Details <- trainControl(method="cv",
                          number = 10,
                          savePredictions = "all",
                          classProbs = TRUE)


# 3. Fit Initial Logistic Regression Model --------------------------------------

#Fit initial GLM with all variables, using Cross-Validation
GLM_v0 <- train(future_clm_ind_yn ~ .,
             method = "glm",
             family = "binomial",
             data = select(Modeling_Dataset,-c(hhld_id,future_clm_ind)),
             trControl= CV_Details)

#Evaluate GLM
GLM_Evaluation(GLM_v0)

#Model performsl well, with high accuracy, sensitivity, specificity, etc.
#No VIF > 5 (although Credit Score is close)
#Significant variables are: Credit Score,pior_bi Limit, Prior Insurer - Missing, Age PC1, Age PC2



#Initial thoughts on Feature Selection:
#Prior Insurer Bucket, only "Missing" level is significant
#Consider replacing with inforce indictaor

#Combine all historical loss experience into a single indicator (May be more significant)
#Separate indicators clutter model, and these individual features are not useful
#*technically 2 indicators, one for at-fault, one for not at-fault


# 4. Adjust Historical Loss Experience Predictors ------------------------------------

#i.e. do not bucket by years

#Convert to a more usable format for modeling
Claim_Data_clean_v2 <- Claim_Data_raw %>%
  
  #Filter out losses after 1/1/2017
  filter(clm_dates < '2017-01-01' ) %>%
  
  #Rename at fault indicator to string (helpful for column titles later)
  mutate(At_Fault_Ind =ifelse (aft_ind == 1,"_f","_nf"),
         .keep = "unused") %>%
  
  #Summarise to Total Losses over historical period
  group_by(hhld_id,At_Fault_Ind) %>%
  
  summarise(Total_Paid_Amt = sum(pd_amt)) %>% 
  
  #Pivot Wider
  pivot_wider(
    id_cols = c(hhld_id),
    names_from = At_Fault_Ind,
    names_prefix = "Paid_Amt",
    values_from = Total_Paid_Amt
  ) %>%  
  
  #Replace NAs with 0
  replace(is.na(.),0) %>%
  
  #Sort by household ID
  arrange(hhld_id)


# 4b.Create New Modeling Dataset_v2 ------------------------------------------


Modeling_Dataset_v2 <- Modeling_Dataset %>%
  
  #Remove original paid loss experience buckets (8 columns)
  select(-contains(c("_f","_nf"))) %>%
  
  #Replace with new combined paid loss experience (2 columns)
  left_join(
    Claim_Data_clean_v2,
    by = "hhld_id"
  ) %>%
  
  #Replace NAs with 0 (NAs generated since not all households have paid losses)
  replace(is.na(.),0)




# 5. Re-Fit GLM with modified Historical Loss Features --------

#Re-Fit GLM with Cross-Validation
GLM_v2 <- train(future_clm_ind_yn ~ .,
                method = "glm",
                family = "binomial",
                data = select(Modeling_Dataset_v2,-c(hhld_id,future_clm_ind)),
                trControl= CV_Details)

GLM_Evaluation(GLM_v2)

#Historical Paid amounts (at fault  and not at fault) continue to be insignificant
#However, historical paid amount features are cleaner
#Model is otherwise comparable to _v1


# 6. Feature Selection with Lasso (L1 Regularization) -------------------------------------------------------

#Perform Lasso for Feature Selection
Lasso_Variables(Modeling_Dataset_v2)

#Refit GLM based on features remaining from penalized regression
GLM_v3 <- train(future_clm_ind_yn ~ prior_bi + credit_score + Prior_Insurer_Bucket + Age_PC1,
                method = "glm",
                family = "binomial",
                data = select(Modeling_Dataset_v2,-c(hhld_id,future_clm_ind)),
                trControl= CV_Details)

#Evaluate GLM performance
GLM_Evaluation(GLM_v3)


#Maintains previous models' strong performance despite removal of most (superfluous) features
#Minor Insurer level is insignificant


#As per before, Prior Insurer Bucket, only "Missing" level is significant
#Try replacing with inforce ind, which is equivalent to the opposite of Prior Insurer Missing



# 7. Replace Prior_Insurer_Bucket with inforce_ind ------------------------


Modeling_Dataset_v3 <- Modeling_Dataset_v2 %>%
  
  #Drop Prior_Insurer_Bucket
  select(-Prior_Insurer_Bucket) %>%
  
  #Join on Inforce Indicator
  inner_join(
    Predictor_Dataset_raw %>% select(hhld_id,inforce_ind),
    by = "hhld_id"
  ) 
  

  


# 8. Re-Fit GLM with Inforce Indicator ------------------------------------

#Re-Fit GLM with Cross-Validation
GLM_v4 <- train(future_clm_ind_yn ~ .,
                method = "glm",
                family = "binomial",
                data = select(Modeling_Dataset_v3,-c(hhld_id,future_clm_ind)),
                trControl= CV_Details)

#Evaluate GLM performance
GLM_Evaluation(GLM_v4)

#Results:
#Inforce Indicator is highly significant, replacing Prior_Insurer_Bucket - Missing
#Otherwise, same significant variables as before with similar strong performance




# 9. Feature Selection with Lasso (L1 Regularization) ----------------------

Lasso_Variables(Modeling_Dataset_v3) #Same features selected as before, but with inforce_ind replacing prior_insurer

#Refit GLM based on features remaining from penalized regression
GLM_v5 <- train(future_clm_ind_yn ~ prior_bi + credit_score + Age_PC1 + inforce_ind,
                method = "glm",
                family = "binomial",
                data = select(Modeling_Dataset_v3,-c(hhld_id,future_clm_ind)),
                trControl= CV_Details)

GLM_Evaluation(GLM_v5)

#Model performance remains strong
#Age PC1 least important variable



# 10. Analyze Age PC1 --------------------------------------

#Try to see if we can simplify model slightly, and improve interpretablity by
#Replacing Age PC1 with an Age-related variable

#Create a dataset consisting of Age PC1, and its underlying variables
Age_PC1_Data <- inner_join(
  #Extract Age PC1
  Modeling_Dataset %>% select(hhld_id,Age_PC1),
  
  #Extract Age and Months Licensed variables
  Predictor_Dataset_raw %>% select(hhld_id,contains(c("age","mon_lic"))),
  
  by = "hhld_id"
)

#Interpret Age PC1 by analyzing its correlation
Age_Correlation <- Age_PC1_Data %>% select(-hhld_id) %>% cor() %>% as.data.frame() %>% select(Age_PC1)
Age_Correlation

#Negatively correlated with all Age and Months Licensed Variables
#Interpret as capturing household drivers' youthfulness 
#Supported (directionally) by positive coefficient (mouth youthful, more likely for a claim)

#Most (negatively) correlated with Average Age and Average Months Licensed
#Proposal: Replace Age PC1 with Average Household Age
#1) Age variables are easy to interpret
#2) Age variables are easier to obtain than Months Licensed
#3) Avg Age has the highest (absolute) correlation with Age PC1

#Update Modeling Dataset to remove Age PCs and replace with Average Age (only)
Modeling_Dataset_v4 <- inner_join(
  Modeling_Dataset_v3 %>% select(-contains("Age_PC")),
  Predictor_Dataset_raw %>% select(hhld_id, avg_age),
  by = "hhld_id"
) 



# 11. Re-Fit GLM with Avg Age substituted for Age PC1 --------------------------------------------------------------

#Replace Age_PC1 variable with Average Age
GLM_v6 <- train(future_clm_ind_yn ~ prior_bi + credit_score + avg_age + inforce_ind,
                method = "glm",
                family = "binomial",
                data = select(Modeling_Dataset_v4,-c(hhld_id,future_clm_ind)),
                trControl= CV_Details)

#Evaluate GLM
GLM_Evaluation(GLM_v6)

#Model performance remains strong
#Select this as final Model
