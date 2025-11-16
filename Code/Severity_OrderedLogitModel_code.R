#Ordered Discrete Choice models
#================
  
  #### Example exercise:  This data set is collected from Addis Ababa Sub-city police departments 
# for master's research work. The data set has been prepared from manual records of road traffic accidents
# of the year 2017-2020.
  
#  **Your task**: Estimate a Ordered Discrete Choice model 
# Source: https://www.kaggle.com/datasets/kanuriviveknag/road-accidents-severity-dataset?resource=download 
#### Variables:

str(data)
#' *´Time´: 
# ´Day_of_week´:
# ´Age_band_of_driver´:
#

##### Import Libraries
library(ordinal)
library(rcompanion)
library(MASS)
library(VGAM)
library(tidyverse)
library(ggplot2)

##### Import dataset
# data_original <- read.csv("Data/Road_Accident_Severity_Clean.csv")
data_original <- read.csv("Data/RTA Dataset.csv")
  # Create database for manipulation
  data <- data_original

  str(data)
  
  
#### Reclassify variables 

  # Dependent variable
  # data$Accident_Severity <- factor(data$Accident_Severity, levels = c("Slight", "Serious", "Fatal"))
  # data$Accident_Severity <- as.ordered(data$Accident_Severity)

  data$Accident_severity <- factor(data$Accident_severity, levels = c("Slight Injury", "Serious Injury", "Fatal injury"))
  data$Accident_severity <- as.ordered(data$Accident_severity)
  
  str(data$Accident_severity)

  levels(data$Accident_severity)

  # Create ID

  # data$ID <- seq_len(nrow(data)) 
  
  # Relocate ID to first column
  # data <- data %>% select(ID, everything())

  str(data)
  
  # Check Missing data
  table(is.na(data))
  
  # Independent variables

  data$Day_of_week <- as.factor(data$Day_of_week)
  data$Educational_level <- as.factor(data$Educational_level)
  data$Driving_experience <- as.factor(data$Driving_experience)
  data$Weather_conditions <- as.factor(data$Weather_conditions)
  data$Type_of_collision <- as.factor(data$Type_of_collision)                               
  # Check the structure of the data
  str(data)
  
  # Check the levels of the variables
  levels(data$Light_Conditions)
  
  #Note: Missing data will influence the number of levels. Should we treat them? 

# Models

  ##Determining the null model, assuming that a parallel regression is possible

  model_null <- clm(Accident_severity ~ 1, 
                    data = data,
                    link = "logit")
  
  summary(model_null)
  
# Model with predictors
  
  model1 <- clm(Accident_severity ~ Day_of_week +
                  Educational_level +
                  Driving_experience +
                  Weather_conditions +
                  Number_of_vehicles_involved +
                  Type_of_collision +
                  Number_of_casualties,
                data = data,
                link = "logit")
  
  summary(model1)

  # Brant test: 
  library(gofcat)
  brant.test(model1)  
   
# Check if model_1 is better than the null model 
anova(model_null, model1)  
   
# Calculate pseudo R square
nagelkerke(fit = model1,
           null = model_null)

# Plot thresholds coeficients

# Threshold values from your model
tau1 <- model1$coefficients[1]  #Slight Injury|Serious Injury
tau2 <- model1$coefficients[2]  #Serious Injury|Fatal injury

# Create a fake latent severity range
latent <- seq(-2, 6, length.out = 500)
density <- dnorm(latent, mean = 0, sd = 1.5)  # just for illustration

# Make data frame for plotting
df <- data.frame(latent, density)

ggplot(df, aes(x = latent, y = density)) +
  geom_line(size = 1) +
  geom_vline(xintercept = tau1, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = tau2, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = -1, y = 0.18, label = "Slight Injury", size = 4, color = "darkgreen") +
  annotate("text", x = 2.7, y = 0.18, label = "Serious Injury", size = 4, color = "orange") +
  annotate("text", x = 5.0, y = 0.18, label = "Fatal Injury", size = 4, color = "red") +
  labs(
    title = "Latent Severity Scale in Ordered Logit Model",
    x = "Latent severity (Y*)",
    y = "Probability density"
  ) +
  theme_minimal(base_size = 14)

# Calculate the odd ratios
exp(coef(model1))










summary(model1)
coef(model1) #returns the intercepts (cut-off) and variables parameters only
coef(model1, matrix= TRUE) #returns the intercepts (cut-off) and variables parameters only in a matrix for each category
deviance(model1)

##2nd model (probit)
fit2 <- vglm(CHOICE_ORD ~ WINTER + PEAK1820  + NOBST + VALETA + NOCUP + ACDIR+ CAPOTA + VEIC1 + AGE32  + GENDER, family=cumulative(parallel = FALSE ~GENDER, reverse = TRUE), data = data_3lev)
summary(fit2)
coef(fit2)#returns the variables parameters only
coef(fit2, matrix= TRUE) #returns the intercepts (cut-off) and variables parameters only in a matrix for each category
deviance(fit2)

#Testing the parallelism assumption
##Formally, there are two common ways to test the parallelism assumption. 

###The first is by a likelihood ratio test
lrtest(fit1, fit2)  

#When compared by the likelihood ratio test, it appears that the PPO model
#with single coefficient for the GENDER variable is adequate 
# (the fit is significantly different).The small p-value here indicates that 
#a parallelism assumption is not reasonable.

###The second method is using the Wald test
(cfit <- coef(fit2))
index <- 4:5 # These coefficients need testing for equality (2 values in this case)
L.mat <- cbind(diag(npred(fit2) - 1), -1) # Matrix of contrasts
T.mat <- solve(L.mat %*% vcov(fit1)[index, index] %*% t(L.mat))
W.stat <- t(L.mat %*% cfit[index]) %*% T.mat %*% (L.mat %*% cfit[index])
W.stat
round(digits = 5, pchisq(W.stat, df = nrow(L.mat), lower.tail = FALSE))  # p-value

#If for the coefficients tested for equality, the Wald test is significant 
#(p-value less than 0.05), we are able to reject the null hypothesis.
#Then we would conclude that the parameters associated with these variables are 
#not equal, so that a parallelism assumption is not reasonable.


#Marginal Effects
nrow= nparam(fit2) - nparam(null) 
ncol=3
k <- nobs(fit2)
marginal<-round(digits = 4, margeff(fit2))
mean = matrix(1:(nrow*ncol),nrow,ncol)
sd = matrix(1:(nrow*ncol),nrow,ncol)
for (j in 1:ncol) {
  for (i in 1:nrow) {
    mean [i,j]<- round(digits = 4,mean(marginal[i,j,1:k]))
    sd [i,j]<- round(digits = 4,sd(marginal[i,j,1:k]))
  }
}

mean
sd


#Model fitting statistics
statistics <- function (model, model_null)
{
  adjusted_rho2 <- round(digits = 4, 1-((logLik(model)-nparam(model))/logLik(model_null)))
  pseudoR2<- round(digits = 4,1-(logLik(model)/logLik(model_null)))
  cat("adjusted_rho2: ", adjusted_rho2," pseudoR2: ", pseudoR2, "AIC: ", AIC(model), "BIC: ", BIC(model)) 
}

statistics(fit2,null)

#Predict the probabilities of each outcome category
fitted(fit2)
depvar(fit2)
predict(fit2)
     