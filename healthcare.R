# # Part1
## Find the age category with max hospitalization.
## store the age category in "max_age_category"
# >> Import the dataset of healthcare
data <- read.csv("HospitalCosts.csv")
# Histogram visualization
hist(data$AGE,col = "Sky blue",xlab = "Patient Age",main = "Historgram for AGE")
# 
max_age_category <- summarise(group_by(data,AGE),mean(TOTCHG))
max_age_category
write.csv(max_age_category,"max_age_category.csv")
View(max_age_category)
# *************************************************************************
 
# Part2
## save the diagnosis related group with maximum hospitalization and expenditureas 
## as "max_diag_grp"
# >> install dplyr package and then library(dplyr)
# install.packages("dplyr")
library(dplyr)
max_diag_grp <-  summarise(group_by(data,Diagnosis_Related_Group=APRDRG), max_hospitalize = n(),
                          ExpenditureCost = max(TOTCHG))
max_diag_grp <- as.data.frame(max_diag_grp)
max_diag_grp <- arrange(max_diag_grp,desc(max_diag_grp$ExpenditureCost))
write.csv(max_diag_grp,"max_diag_grp.csv")
View(max_diag_grp)
#******************************************************************
# Part3
## Does hospitalization cost vary accross Race.
## store your Anova model as "anova_model"
library(dplyr)
# Dependent variable >> TOTCHG and Independent Variable >> RACE
# H0 in anova when there is no difference in means
# H1 in anova when there means are not similar to each other
data <- read.csv("HospitalCosts.csv")
str(data)
# Removing the NA's values from the dataset
data <- na.omit(data)
any(is.na(data))
# RACE is independent variable which is int we need to make categorical 
data$RACE <- as.factor(data$RACE)

str(data)
boxplot(data$TOTCHG~data$RACE,ylab="Hospital discharge costs",
         xlab="Race of the patient ",main = "Boxplot ",col = rainbow(5))
# Anova test
anova_test <- aov(data$TOTCHG~data$RACE)

summary(anova_test)
anova_test <- as.list(anova_test)
# output of the anova model
capture.output(summary(anova_test),file="anova_model.csv")
class(anova_test)
# By performing the anova test we conclude that the group means of RACE related to Total hospital cost is difference
# H0 hypothesis is accepted and H1 is rejected 

#*******************************************************************
#Part4
## how does Age and Gender affect the hospitalization costs.
## store your model values in "reg_model"
# Building the linear regression model 
# Data understanding and Exploration

data$RACE <- as.integer(data$RACE)
str(data)
# >> 	499 obs. of  6 variables
any(is.na(data))
data <- na.omit(data)
# Remove all NA's values from the dataset 
any(is.na(data))
colnames(data)
head(data)
tail(data)
class(data)
summary(data)
# Dependent variable >> Hospitalization costs Independent variable >> Age and Gender
# install.packages caTools for splitting into 70:30
library(caTools)
# setting the seed sample to 2
set.seed(2)
# split the dataset into 70:30
sample <- sample.split(data$TOTCHG,SplitRatio = 0.70)
summary(sample)
# 70 % of the data goes for training
train_data <- subset(data,sample==TRUE)
str(train_data)
# 30% of the data goes for testing
test_data <- subset(data,sample==FALSE)
str(test_data)
# Linear model
model <- lm(TOTCHG~., data = train_data)
summary(model)
model <- lm(TOTCHG~AGE + FEMALE , data = train_data)
summary(model)
# Age and female is significance independent value P <0.05 
# but age has +ve relationship while Female has -ve relationship
# The model has the accuracy of 74% 
# Predict test on the sample 
predtest <- predict(model,test_data)
View(predtest)

# Combine the prediction value with test data
reg_model<- cbind(test_data,predtest)
class(reg_model)
write.csv(reg_model,"reg_model.csv")
#******************************************************************
#Part5
## Model to predict length of stay(LOS) from Age Gender and Race
## Save your model as "reg_model_los"
# Data understanding and Exploration
data <- read.csv("HospitalCosts.csv")
str(data)
# >> 	499 obs. of  6 variables
any(is.na(data))
data <- na.omit(data)
# Remove all NA's values from the dataset 
any(is.na(data))
colnames(data)
head(data)
tail(data)
class(data)
summary(data)

# Dependent variable >> Length of stay 
# Independent variable >> Age,Gender and Race
# install.packages caTools for splitting into 70:30
library(caTools)
# setting the seed sample to 2
set.seed(2)
# split the dataset into 70:30
sample <- sample.split(data$LOS,SplitRatio = 0.70)
summary(sample)
# 70 % of the data goes for training
train_data <- subset(data,sample==TRUE)
str(train_data)
# 30% of the data goes for testing
test_data1 <- subset(data,sample==FALSE)
str(test_data1)
# Building Linear Regression model
model2 <- lm(LOS~., data = train_data)
summary(model2)
str(train_data)
# The adjusted R-square is 46% 
data$LOS <- as.integer(data$LOS)
model2 <- lm(LOS~ AGE,data = train_data)
summary(model2)
# Predicting the values on test data
predtest1 <- predict(model2,test_data1)
View(predtest1)
# Final outcome
result <- cbind(test_data1,predtest1)
View(result)
write.csv(result,"reg_model_los.csv")

#****************************************************************

#Part6
## save the variable affecting the hospitalization cost as "max_hosp_cost"
# Building the linear regression model 
# Data understanding and Exploration
data <- read.csv("HospitalCosts.csv")
str(data)
# >> 	499 obs. of  6 variables
any(is.na(data))
data <- na.omit(data)
# Remove all NA's values from the dataset 

str(data)
any(is.na(data))
colnames(data)
head(data)
tail(data)
class(data)
summary(data)
# Dependent variable >> Hospitalization costs Independent variable >> Age and Gender
# install.packages caTools for splitting into 70:30
library(caTools)
# setting the seed sample to 2
set.seed(2)
# split the dataset into 70:30
sample <- sample.split(data$TOTCHG,SplitRatio = 0.70)
summary(sample)
# 70 % of the data goes for training
train_data <- subset(data,sample==TRUE)
str(train_data)
# 30% of the data goes for testing
test_data3 <- subset(data,sample==FALSE)
str(test_data3)
# Linear model
model3 <- lm(TOTCHG~., data = train_data)
summary(model3)
capture.output(summary(model3),file="final_model.xlsx")

# Linear model with significance independent variables AGE+FEMALE+LOS+APRDRG
model3 <- lm(TOTCHG~AGE+LOS+APRDRG, data = train_data)
summary(model3)
# The adjusted R-squared is 81% which is greater than 70% so it is considered as a good model
predtest2 <- predict(model3,test_data3)
View(predtest2)
# Combine the predict value with test data
result <- cbind(test_data3,predtest2)
View(result)
write.csv(result,"max_hosp_cost.csv")
# It means that 74% of the total hospital cost is explained by AGE , APRDRG and LOS


#*************************************End****************************************************
