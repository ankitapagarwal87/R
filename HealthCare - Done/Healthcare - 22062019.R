rm(list=ls())

hops  <- read.csv('HospitalCosts.csv') 
head(hops)

# Attribute	      Description
# Age 	          Age of the patient discharged
# Female 	        A binary variable that indicates if the patient is female
# Los	            Length of stay in days
# Race 	          Race of the patient (specified numerically)
# Totchg	        Hospital discharge costs
# Aprdrg	        All Patient Refined Diagnosis Related Groups

nrow(hops)
summary(hops)

str(hops)

# 'data.frame':	500 obs. of  6 variables:
# $ AGE   : int  17 17 17 17 17 17 17 16 16 17 ...
# $ FEMALE: int  1 0 1 1 1 0 1 1 1 1 ...
# $ LOS   : int  2 2 7 1 1 0 4 2 1 2 ...
# $ RACE  : int  1 1 1 1 1 1 1 1 1 1 ...
# $ TOTCHG: int  2660 1689 20060 736 1194 3305 2205 1167 532 1363 ...
# $ APRDRG: int  560 753 930 758 754 347 754 754 753 758 ...

hops$RACE <- as.factor(hops$RACE)
hops$FEMALE <- as.factor(hops$FEMALE)

unique(hops$APRDRG)

hops$APRDRG_Factor <- as.factor(hops$APRDRG)

str(hops)

hops$age_bins <- ifelse((hops$AGE  < 1), "infant",
                        ifelse(hops$AGE < 3, 'toddler',
                               ifelse(hops$AGE < 11, 'child',
                                      'adolescent')))

hops$age_bins <- as.factor(hops$age_bins)

str(hops)

# 'data.frame':	500 obs. of  8 variables:
# $ AGE          : int  17 17 17 17 17 17 17 16 16 17 ...
# $ FEMALE       : Factor w/ 2 levels "0","1": 2 1 2 2 2 1 2 2 2 2 ...
# $ LOS          : int  2 2 7 1 1 0 4 2 1 2 ...
# $ RACE         : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ TOTCHG       : int  2660 1689 20060 736 1194 3305 2205 1167 532 1363 ...
# $ APRDRG       : int  560 753 930 758 754 347 754 754 753 758 ...
# $ APRDRG_Factor: Factor w/ 63 levels "21","23","49",..: 32 51 62 55 52 28 52 52 51 55 ...
# $ age_bins     : Factor w/ 4 levels "adolescent","child",..: 1 1 1 1 1 1 1 1 1 1 ...

head(hops)
View(hops)

# 1. To record the patient statistics, the agency wants to find the age 
# category of people who frequents the hospital and has the maximum expenditure.  

# a. To find the category that has the highest frequency of hospital visit, 

# We can use graphical analysis. A histogram would display the number of 
# occurrences of each age category.  The as.factor() is called to make sure 
# that the categories are not treated as numbers. Outlier treatments: None  

# Code: 

hist(hops$AGE)

summary(as.factor(hops$AGE))

# Age:        0     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# Frequency:  307  10   1   3   2   2   2   3   2   2   4   8  15  18  25  29  29  38

# Result: From the graph that is displayed, we can see that infants AGE = 0) have the maximum 
# frequency of hospital visit, going above 300. The summary of AGE attribute gives 
# the numerical output (after converting the age from numeric to factor) - and we 
# can see that there are 307 entries for those in the range of 0-1 year.

# b. To find the age category with the maximum expenditure, 

# we need to add the expenditure for each age, and find the maximum value 
# from the sum. We will use the aggregate function to add the values of 
# total expenditure according to the values of age.

# With AGE

aggregate(TOTCHG ~ AGE, FUN = sum, data = hops)
max(aggregate(TOTCHG ~ AGE, FUN = sum, data = hops))

df <- aggregate(TOTCHG ~ AGE, FUN = sum, data = hops)
df
df[(df$TOTCHG == max(df$TOTCHG)),]

#     AGE  TOTCHG
# 1    0   678118

# age_bins

df <- aggregate(TOTCHG ~ age_bins, FUN = sum, data = hops)
df

max(aggregate(TOTCHG ~ age_bins, FUN = sum, data = hops)$TOTCHG)
df[(df$TOTCHG == max(df$TOTCHG)),]

#    age_bins   TOTCHG
# 3    infant   678118
 
# Result: From the result we can see that the infant category (AGE = 0) has maximum hospital costs 
# as well (in accordance with the number or frequency of visit). Following the infants, 
# 15 and 17 year old individuals have high hospitalization costs.


# 2. In order of severity of the diagnosis and treatments and to find out the expensive 
# treatments, the agency wants to find the diagnosis related group that has maximum 
# hospitalization and expenditure.


# Similar to the previous analysis, we can find the diagnosis 
# related group with maximum hospitalization and expenditure. For this, we will use the aggregate 
# and the histogram functions. The which.max function can be used to get the index of the data frame 
# with the maximum value. The as.factor() is called to make sure that the categories are not treated as numbers.  

#######################################################################################

head(hops)

summary(hops$APRDRG_Factor)

which.max(summary(hops$APRDRG_Factor))

df <- aggregate(TOTCHG ~ APRDRG_Factor, FUN = sum, data = hops)
df

df[which.max(summary(hops$APRDRG_Factor)),]

#      APRDRG   TOTCHG
# 44      640   436822

# df <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = hops)
# df
# 
# df[(df$TOTCHG == max(df$TOTCHG)),]

#      APRDRG   TOTCHG
# 44      640   436822

# Result: From the results we can see that the category 640 has the 
# maximum entries of hospitalization, by a huge contrast (266 of 500 entries), 
# and also has the highest total hospitalization cost (437978)

#######################################################################################

# 3. To make sure that there is no malpractice, the agency needs to analyze if the 
# race of the patient is related to the hospitalization costs.

# If there is any effect of RACE on TOTCHG

# To analyze, first convert the Race variable to factors and perform a summary of the variable. This 
# will help you to find how many patients belonging to the different groups were admitted. 
# Then, to verify if the races made an impact on the costs, perform an ANOVA with the 
# following variables:  

# Defining Hypothesis

# Ho: The races had no an impact on the costs
# H1: The races had an impact on the costs

# ANOVA dependent variable: TOTCHG 
# Categorical/grouping variable: RACE Missing values: 1 NA value, use na.omit to remove the NA value   
# 
# Code:  

colSums(is.na(hops))

hops <- na.omit(hops)

colSums(is.na(hops))

model <- aov(TOTCHG ~ RACE, data = hops)  # continuous ~ categorical varibale

# dependent variable ~ independent variable

summary(model)

alpha = 0.05

pvalue = 0.943

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Result:  The p-value is very high specifying that there is no relation between 
# the race of patient and the hospital cost.

summary(hops$RACE) 

# Race        1   2   3   4   5   6 
# Frequecy  484   6   1   3   3   2 

# From the summary we can also see that:
# the data has 484 patients of Race 1 out of the 500 entries. This will affect the 
# results of ANOVA as well, since the number of observations is very much skewed. 
# In conclusion, there is not enough data to verify if the race of patient is related 
# to the hospitalization cost.

# 4. To properly utilize the costs, the agency has to analyze the severity of the 
# hospital costs by age and gender for proper allocation of resources.  

# Ho: There is no effect of age_bins on TOTCHG
# H1: There is effect of age_bins on TOTCHG

# Ho: There is no effect of Gender on TOTCHG
# H1: There is effect of Gender on TOTCHG

# 
# TOTCHG   - continuous
# age_bins - Categorical
# FEMALE   - Categorical

# 2 categorical variable having effect on one continuous variable

# Code:  
colnames(hops)

model1 <- aov(TOTCHG ~ age_bins + FEMALE, data = hops)
summary(model1)

alpha = 0.05

pvalue_age_bins = 6.15e-08

pvalue_Female = 0.213

pvalue_age_bins < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

pvalue_Female < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis


# Ho: There is no effect of AGE and Gender on TOTCHG
# H1: There is effect of AGE and Gender on TOTCHG
# 
# TOTCHG - continuous
# AGE    - continuous
# FEMALE - Categorical

model2 <- lm(TOTCHG ~ AGE + FEMALE, data = hops)
summary(model2)

# Result: Age is a very important factor in the hospital costs as seen by the 
# significance levels and p-values. The gender also seems to have an impact. 
# When we see a summary function, we find that there is an equal number of male 
# and female patients. The negative coefficient shows that on an average, females 
# incur lesser cost than males.  

# TOTCHG = 2718.63 + (86.28*AGE) + (-748.19*Female)

# Female = 1 if the patient if female
#        = 0                    male

# PAtient Age 0

# Male
TOTCHG_M = 2718.63 + (86.28*0) + (-748.19*0)
TOTCHG_M

#Female
TOTCHG_F = 2718.63 + (86.28*0) + (-748.19*1)
TOTCHG_F

# 5. Since, the length of stay is the crucial factor for inpatients, the agency 
# wants to find if the length of stay can be predicted from age, gender, and race.

# Since the length of stay is a continuous variable, we use linear regression to 
# predict the variable.  Dependent variable: LOS Independent variables: AGE, FEMALE, 
# RACE. Note that RACE and FEMALE should be converted into factors, whereas AGE is a 
# numerical variable.  

Model3 <- lm(LOS ~ AGE + FEMALE + RACE, data = hops)
summary(Model3)

# Result: The significance codes are almost null for all the variables, except for 
# the intercept. The very high p-value signifies that there is no linear relationship 
# between the given variables. That is, with just the age, gender, and race, it is not 
# possible to predict the length of stay of a patient.   


# 6. To perform a complete analysis, the agency wants to find the variable that 
# mainly affects the hospital costs. To find the variables that mainly affect the 
# total costs, construct a linear model with all the variables as the causing variables.  
# Dependent variable: TOTCHG Independent variables: All other variables  

str(hops)

model5 <- lm(TOTCHG ~ . - APRDRG_Factor - age_bins, data = hops)
summary(model5)

# Result: We can see that age and length of stay affect the total hospital cost. Length 
# of stay positively affects the cost. That is, with an increase of 1 day, there is an 
# addition of a value of 742 to the total cost.
