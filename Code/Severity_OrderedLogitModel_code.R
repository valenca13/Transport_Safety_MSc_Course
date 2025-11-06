#Ordered Discrete Choice models
#================

#This script was developed by Carlos Roque (LNEC) with some "polishing" by Filipe Moura.
#(v.2022)
  
  #### Example exercise: Road Accident Records in Kensington and Chelsea (January 2021)
  
#  **Your task**: Estimate a Ordered Discrete Choice model that detects_
#the unforgiving roadside contributors to different severity levels of crashes.

#### Variables:

# ´Accident_Index´: A unique identifier for each accident record.
# ´Accident Date´: The date on which the accident occurred (format: DD/MM/YYYY).
# ´Day_of_Week´: The day of the week when the accident took place.
# ´Junction_Control´: Describes the type of junction control at the accident location (e.g., "Give way or uncontrolled").
# ´Junction_Detail´: Provides additional details about the junction where the accident occurred (e.g., "T or staggered junction").
# ´Accident_Severity´: Indicates the severity of the accident (e.g., "Serious").
# ´Latitude´: The geographic latitude of the accident location.
# ´Light_Conditions´: Describes the lighting conditions at the time of the accident (e.g., "Daylight").
# ´Local_Authority_(District)´: The local authority district where the accident occurred.
# ´Carriageway_Hazards´: Describes any hazards present on the carriageway at the time of the accident 
#  (e.g., "None").
# ´Longitude´: The geographic longitude of the accident location.
# ´Number_of_Casualties´: The total number of casualties involved in the accident.

Number_of_Vehicles: The total number of vehicles involved in the accident.

Police_Force: The police force that handled the accident.

Road_Surface_Conditions: Describes the surface conditions of the road at the time of the accident (e.g., "Dry").

Road_Type: Specifies the type of road where the accident occurred (e.g., "One way street").

Speed_limit: The speed limit applicable to the road where the accident occurred.

Time: The time of day when the accident happened (format: HH:MM).

Urban_or_Rural_Area: Indicates whether the accident occurred in an urban or rural area.

Weather_Conditions: Describes the weather conditions at the time of the accident (e.g., "Fine no high winds").

Vehicle_Type: Specifies the type of vehicle involved in the accident (e.g., "Car," "Taxi/Private hire car").

##### Import Libraries
library(readr)
library(ordinal)
library(VGAM)


##### Import dataset
data <- read.csv("Data/Road Accident Severity Data.csv")

str(data)

data_3lev <- read.delim("/Volumes/HD2/Documents_20200412/Aulas/TDM/Examples/ODCM_example/example_croque/Data_freeways_2009_2010_3levels_v1.txt")
head(data_3lev)
View(data_3lev)

attach(data_3lev)

####Reclassify variables 
#####lhs variable
CHOICE_ORD<- as.factor(CHOICE_ORD)
CHOICE_ORD <- as.ordered(CHOICE_ORD)

#####rhs variables
CAPOTA <- as.factor(CAPOTA)
WINTER <- as.factor(WINTER)
GENDER <- as.factor(GENDER)
ACDIR <- as.factor(ACDIR)
VEIC1 <- as.factor(VEIC1)
PEAK1820 <- as.factor(PEAK1820)
AGE32 <- as.factor(AGE32)
AGE21 <- as.factor(AGE21)
AGE23 <- as.factor(AGE23)
AGE26 <- as.factor(AGE26)
VALETA <- as.factor(VALETA)
AGE <- as.integer(AGE)
NOCUP <- as.integer(NOCUP)
LVEL <- as.numeric(LVEL)

#models

##Determining the null model, assuming that a parallel regression is possible
null <- vglm(CHOICE_ORD ~ 1, family=cumulative(parallel=TRUE, reverse = TRUE), data = data_3lev)
summary(null)

##1st model (logit)
fit1 <- vglm(CHOICE_ORD ~ NOCUP + GENDER + WINTER + ACDIR+ VEIC1 + NOBST + CAPOTA + VALETA + AGE32 + PEAK1820, family=cumulative(parallel=TRUE, reverse = TRUE), data = data_3lev)
summary(fit1)
coef(fit1) #returns the intercepts (cut-off) and variables parameters only
coef(fit1, matrix= TRUE) #returns the intercepts (cut-off) and variables parameters only in a mtrix for each category
deviance(fit1)

##2nd model (probit)
fit2 <- vglm(CHOICE_ORD ~ WINTER + PEAK1820  + NOBST + VALETA + NOCUP + ACDIR+ CAPOTA + VEIC1 + AGE32  + GENDER, family=cumulative(parallel = FALSE ~GENDER, reverse = TRUE), data = data_3lev)
summary(fit2)
coef(fit2)#returns the variables parameters only
coef(fit2, matrix= TRUE) #returns the intercepts (cut-off) and variables parameters only in a mtrix for each category
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
     