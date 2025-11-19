#Ordered Logistic Regression models
#================
  
  #### Example exercise:  This data set is collected from Addis Ababa (Etiopia) Sub-city police departments 
# for master's research work. The data set has been prepared from manual records of road traffic accidents
# of the year 2017-2020.
  
#  **Your task**: Estimate a Ordered Discrete Choice model by examining the attributes that 
# influence the severity of accidents. 

# Source: https://www.kaggle.com/datasets/kanuriviveknag/road-accidents-severity-dataset?resource=download 

##### Import Libraries
library(ordinal)
library(rcompanion)
library(MASS)
library(VGAM)
library(tidyverse)
library(ggplot2)
library(gofcat)
library(rms)

##### Import dataset
data_original <- read.csv("Data/RTA Dataset.csv")
  
# Create database for manipulation
  data <- data.frame(data_original)

  str(data)
  
#### Reclassify variables 
  
  #Check the unique values of the Dependent variable
  unique(data$Accident_severity)
  # Dependent variable
  data$Accident_severity <- factor(data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
  data$Accident_severity <- as.ordered(data$Accident_severity)
  
  str(data$Accident_severity)

  levels(data$Accident_severity)

  str(data)
  
  # Check Missing data
  table(is.na(data))

  # Independent variables

  data$Sex_of_driver <- as.factor(data$Sex_of_driver)
  data$Educational_level <- as.factor(data$Educational_level)
  data$Weather_conditions <- as.factor(data$Weather_conditions)
  data$Type_of_collision <- as.factor(data$Type_of_collision)  
  
# Try doing the other variables
  
  # Check the structure of the data
  str(data)
  
  # Check the levels of the variables
  levels(data$Sex_of_driver)

# Models

  ##Determining the null model (baseline cummulative logit model).

  model_null <- clm(Accident_severity ~ 1,  # CLM -> Cumulative Link Model = Ordinal logistic model.
                    data = data,
                    link = "logit")
  
  summary(model_null)
  
  # Since you have no predictors, the model is simply estimating the cutpoints (thresholds) 
  # on the latent severity scale that separate the ordered categories.
  
  # A threshold of 1.70 means that the cutoff between
  # "Slight Injury" and "Serious or Fatal" lies at log-odds = 1.70.
  
  # Convert thresholds to probabilities: 
  
  plogis(model_null$coefficients[1])
  
  plogis(model_null$coefficients[2])
  
  #So if you have a value on the log-odds scale, 
  # plogis() applies the inverse logit funciton and converts it to a probability between 0 and 1.
  
  # Meaning: 
  # P(Slight Injury) ≈ 84.5%
  # P(Serious Injury) ≈ 98.7% – 84.5% ≈ 14.2%
  # P(Fatal Injury) ≈ 1.3 %
 
  
# Model with predictors
  
  model1 <- clm(Accident_severity ~ 
                  Sex_of_driver +
                  Number_of_vehicles_involved +
                  Weather_conditions +
                  Type_of_collision,
                  data = data,
                  link = "logit")
  
  summary(model1)

  # nobs: Number of observations used in the model
  # niter: Number of iterations
  
  # Note: Positive coefficient → increases odds of more severe injury.
  #       Negative coefficient → reduces odds of more severe injury.

##### TASK IN CLASS: Convert the threshold coefficients to probabilities. 
  
  #Calculate the Odds ratios: 
  
  exp(coef(model1))
  
  # OR of Number of vehicles involved is 0.64, meaning that
  # there is a ~35% lower odds of more severe injury per extra vehicle.
  
  # To interpret the OR of categorical variables, it is important to know the reference level. 
  
# Test proportional of odds assumption
  
  # Brant test
  brant.test(model1)
  
  # The warning message may indicate:
      # You have many factor levels (e.g., Weather, Collision type)
      # Some levels are rare or never occur with certain severity categories (fatal only has 1% of probability of all data). 
      # VERY large p-values (0.99, 1.00) are suspicious and typical when sparse tables occur.
  # The test is less reliable, but does not mean it does not hold the assumption of proportional odds. 
  
  # We should try to plot the It plots mean predicted probabilities of each category of the ordinal outcome 
  # across values of a numeric predictor, and check which variables are more unstable.
  
  plot.xmean.ordinaly(Accident_severity ~ 
                        # Sex_of_driver +
                        Number_of_vehicles_involved, 
                        # Weather_conditions +
                        # Type_of_collision, 
                        data = data)
  
  # Only reliable with continuous predictors.

# Check if model_1 is better than the null model 
anova(model_null, model1)  

# Note: the null hypothesis is: all the predictors have no effect on accident severity.

# Calculate pseudo R square
nagelkerke(fit = model1,
           null = model_null)
