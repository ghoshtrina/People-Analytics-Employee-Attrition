#
# IST 707 - Homework 01 - Association Rules Mining
# Author: Trina Ghosh


file.choose()
dataSet <- "/Users/trinaghosh/Desktop/Trina/IST 707/HW 1/employee_attrition.csv"
DF <- read.csv(dataSet, header = T, stringsAsFactors = T)
summary(DF)
library(arules)
library(arulesViz)
library(outliers)


###############################################################
# DATA CLEANING -> Removing NA values

sum(is.na.data.frame(DF))
# 9

complete.cases(DF)
emp_attr <- DF[complete.cases(DF) == T, ]
nrow(DF[complete.cases(DF) == F,])
rownames(emp_attr) <- NULL


summary(emp_attr)
# Gender and OverTime columns have 1 NULL value each. 

# Gender
table(emp_attr$Gender)
which(emp_attr$Gender == "")
# 1053
emp_attr <- emp_attr[-1053,]
row.names(emp_attr) <- NULL

table(emp_attr$Gender)
as.character(emp_attr$Gender)
as.factor(emp_attr$Gender)
table(emp_attr$Gender)
emp_attr$Gender <- droplevels.factor(emp_attr$Gender)
table(emp_attr$Gender)

# OverTime
table(emp_attr$OverTime)
which(emp_attr$OverTime == "")
# 1004
emp_attr <- emp_attr[-1004,]
rownames(emp_attr) <- NULL

table(emp_attr$OverTime)
emp_attr$OverTime <- droplevels(emp_attr$OverTime)
table(emp_attr$OverTime)





#####################################################

# DATA PREPROCESSING -> FACTORING

str(emp_attr)
summary(emp_attr)

#-----------------------------------
for (i in 1:dim(emp_attr)[2])
{
  if (class(emp_attr[,i]) == "integer" && length(unique(emp_attr[,i])) < 10)
  {
    print(colnames(emp_attr[i]))
    print(table(emp_attr[,i]))
  }
}
#------------------------------------------


# Variables to change from nominal to ordinal. 
#  Education
#  EnvironmentSatisfaction
#  JobInvolvement
#  JobLevel
#  JobSatisfaction
#  PerformanceRating
#  RelationshipSatisfaction
#  StockOptionLevel
#  WorkLifeBalance

emp_attr$Education <- as.factor(emp_attr$Education)
emp_attr$EnvironmentSatisfaction <- as.factor(emp_attr$EnvironmentSatisfaction)
emp_attr$JobInvolvement <- as.factor(emp_attr$JobInvolvement)
emp_attr$JobLevel <- as.factor(emp_attr$JobLevel)
emp_attr$JobSatisfaction <- as.factor(emp_attr$JobSatisfaction)
emp_attr$PerformanceRating <- as.factor(emp_attr$PerformanceRating)
emp_attr$RelationshipSatisfaction <- as.factor(emp_attr$RelationshipSatisfaction)
emp_attr$StockOptionLevel <- as.factor(emp_attr$StockOptionLevel)
emp_attr$WorkLifeBalance <- as.factor(emp_attr$WorkLifeBalance)

str(emp_attr)


#####################################################
# DATA CLEANING -> OUTLIER DETECTION & REMOVAL

str(emp_attr)
summary(emp_attr)

for (i in 1:dim(emp_attr)[2])
{
  if (class(emp_attr[,i]) == "integer")
  {
    print(colnames(emp_attr[i]))
    print(range(abs(scores(emp_attr[,i], type = "z"))))
  }
}

#"Age" : 0.007286022 2.542727027
#"DailyRate" : 0.002202084 1.727855114
#"DistanceFromHome" : 0.04760456 20.86917247
#"EmployeeCount" : NaN NaN
#"EmployeeNumber" : 0.0009617816 1.7274598646
#"HourlyRate" : 0.0137744 1.7583062
#"MonthlyIncome" : 0.001113298 2.867817086
#"MonthlyRate" : 0.0005826989 1.7581005615
#"NumCompaniesWorked" : 0.1257517 2.5674305
#"PercentSalaryHike" : 0.08227161 2.62525463
#"StandardHours" : NaN NaN
#"TotalWorkingYears" : 0.04154975 12.33006770
#"TrainingTimesLastYear" : 0.1455313 2.4366655
#"YearsAtCompany" : 0.02106581 5.45646822
#"YearsInCurrentRole" : 0.03599774 3.98539391
#"YearsSinceLastPromotion" : 0.03505689 4.08073060
#"YearsWithCurrManager" : 0.03098807 29.70747134

summary(emp_attr)

for (i in 1:dim(emp_attr)[2])
{
  if (class(emp_attr[,i]) == "integer" && max(scores(emp_attr[,i], type = "z")) > 3 && sum(is.nan(scores(emp_attr[,i], type = "z"))) == 0)
  {
    print(colnames(emp_attr[i]))
    print(sum(abs(scores(emp_attr[,i], type = "z")) > 3))
  }
}

#[1] "DistanceFromHome" : 1
#[1] "TotalWorkingYears" : 8
#[1] "YearsAtCompany" : 21
#[1] "YearsInCurrentRole" : 13
#[1] "YearsSinceLastPromotion" : 34
#[1] "YearsWithCurrManager" : 1

#Removing outlier from DistanceFromHome
which.max(abs(scores(emp_attr$DistanceFromHome, type = "z")))
#[1] 120
emp_attr <- emp_attr[-120,]
rownames(emp_attr) <- NULL

#Removing outliers from TotalWorkingYears
emp_attr$Outlier_TotalWorkingYears <- abs(scores(emp_attr$TotalWorkingYears, type = "z"))
emp_attr <- subset(emp_attr, emp_attr$Outlier_TotalWorkingYears <= 3)
rownames(emp_attr) <- NULL

#Removing outliers from YearsAtCompany
emp_attr$Outlier_YearsAtCompany <- abs(scores(emp_attr$YearsAtCompany, type = "z"))
emp_attr <- subset(emp_attr, emp_attr$Outlier_YearsAtCompany <= 3)
rownames(emp_attr) <- NULL

#Removing outliers from YearsinCurrentRole
emp_attr$Outlier_YearsInCurrentRole <- abs(scores(emp_attr$YearsInCurrentRole, type = "z"))
emp_attr <- subset(emp_attr, emp_attr$Outlier_YearsInCurrentRole <= 3)
rownames(emp_attr) <- NULL

#Removing outliers from YearsSinceLastPromotion
emp_attr$Outlier_YearsPromotion <- abs(scores(emp_attr$YearsSinceLastPromotion, type = "z"))
emp_attr <- subset(emp_attr, emp_attr$Outlier_YearsPromotion <= 3)
rownames(emp_attr) <- NULL

#Removing outliers from YearsWithCurrManager
emp_attr$Outlier_YearsManager <- abs(scores(emp_attr$YearsWithCurrManager, type = "z"))
emp_attr <- subset(emp_attr, emp_attr$Outlier_YearsManager <= 3)
rownames(emp_attr) <- NULL

dim(emp_attr)
#[1] 1092   40

emp_attr <- emp_attr[,-36:-40]
dim(emp_attr)
#[1] 1092   35


#############################################################################
# DATA CLEANING -> REMOVING UNNECESSARY COLUMNS

summary(emp_attr)

# Columns EmployeeCount, Over18 and StandardHours have only 1 value each. 

colnames(emp_attr)
emp_attr <- emp_attr[,-9]

colnames(emp_attr)
emp_attr <- emp_attr[,-21]

colnames(emp_attr)
emp_attr <- emp_attr[,-25]

summary(emp_attr)
str(emp_attr)


#############################################################################
# DATA PREPROCESSING -> DISCRETIZATION

str(emp_attr)

for (i in 1:dim(emp_attr)[2])
{
  if (class(emp_attr[,i]) == "integer")
  {
    print(colnames(emp_attr[i]))
  }
}

#Discretizing "Age"
sort(unique(emp_attr$Age))
Age_Disc <- discretize(emp_attr$Age, method = "fixed"
                       , breaks = c(18,30,35,42,60)
                       , labels = c("Young","Thirties","Middle-Aged","Seniors"))
table(Age_Disc)

#[1] "DailyRate"
sort(unique(emp_attr$DailyRate))
DailyRate_Disc <- discretize(emp_attr$DailyRate, method = "fixed"
                             , breaks = c(102,450,800,1150,1498)
                             , labels = c("Low","Moderate","High","Very High"))
table(DailyRate_Disc)

#[1] "DistanceFromHome"
sort(unique(emp_attr$DistanceFromHome))
DistFromHome_Disc <- discretize(emp_attr$DistanceFromHome, method = "fixed"
                             , breaks = c(1,3,8,15,29)
                             , labels = c("Very Near","Moderately Near","Far","Very Far"))
table(DistFromHome_Disc)

table(emp_attr$Education)
emp_attr$Education <- as.numeric(emp_attr$Education)
Education_disc <- discretize(emp_attr$Education, method = "fixed"
                             , breaks = c(1,2,3,4,5)
                             , labels = c("1","2","3","4-5"))
table(Education_disc)

#[1] "EmployeeNumber"
sort(unique(emp_attr$EmployeeNumber))
range(emp_attr$EmployeeNumber)
# EmployeeNumber column has all unique values, like an ID number.
# So, omitting the column as necessary. 
colnames(emp_attr)
emp_attr <- emp_attr[,-9]

#[1] "HourlyRate"
sort(unique(emp_attr$HourlyRate))
HourlyRate_Disc <- discretize(emp_attr$HourlyRate, method = "fixed"
                             , breaks = c(30,48,65,83,100)
                             , labels = c("Low","Moderate","High","Very High"))
table(HourlyRate_Disc)

#JobInvolvement
table(emp_attr$JobInvolvement)
emp_attr$JobInvolvement <- as.numeric(emp_attr$JobInvolvement)
JobInvolvement_Disc <- discretize(emp_attr$JobInvolvement, method = "fixed"
                                  , breaks = c(1,2,3,4)
                                  , labels = c("1","2","3-4"))
table(JobInvolvement_Disc)

#JobLevel
table(emp_attr$JobLevel)
emp_attr$JobLevel <- as.numeric(emp_attr$JobLevel)
JobLevel_Disc <- discretize(emp_attr$JobLevel, method = "fixed"
                            , breaks = c(1,2,3,4,5)
                            , labels = c("1","2","3","4-5"))
table(JobLevel_Disc)

#[1] "MonthlyIncome"
sort(unique(emp_attr$MonthlyIncome))
range(emp_attr$MonthlyIncome)
MonthInc_Disc <- discretize(emp_attr$MonthlyIncome, method = "fixed"
                            , breaks = c(1009,2600,4100,5600,9000,19943)
                            , labels = c("Very Low","Moderately Low","Medium",
                                         "Moderately High","Very High"))
table(MonthInc_Disc)

#[1] "MonthlyRate"
sort(unique(emp_attr$MonthlyRate))
range(emp_attr$MonthlyRate)
MonthRate_Disc <- discretize(emp_attr$MonthlyRate, method = "fixed"
                             , breaks = c(2094,5200,8200,11000,18000,26999)
                             , labels = c("Very Low","Moderately Low","Medium",
                                         "Moderately High","Very High"))
table(MonthRate_Disc)

#[1] "NumCompaniesWorked"
sort(unique(emp_attr$NumCompaniesWorked))
NumCompanies_Disc <- discretize(emp_attr$NumCompaniesWorked, method = "fixed"
                                , breaks = c(0,1,2,4,9)
                                , labels = c("Low","Moderate","High","Very High"))
table(NumCompanies_Disc)
                             
#[1] "PercentSalaryHike"
sort(unique(emp_attr$PercentSalaryHike))
SalaryHike_Disc <- discretize(emp_attr$PercentSalaryHike, method = "fixed"
                                , breaks = c(11,13,15,19,25)
                                , labels = c("Low","Moderate","High","Very High"))
table(SalaryHike_Disc)

#PerformanceRating
table(emp_attr$PerformanceRating)
emp_attr$PerformanceRating <- as.numeric(emp_attr$PerformanceRating)
PerformanceRating_Disc <- discretize(emp_attr$PerformanceRating, method = "fixed"
                                     , breaks = c(1,2)
                                     , labels = c("3 or 4")
)
table(PerformanceRating_Disc)

#StockOptionLevel
table(emp_attr$StockOptionLevel)
emp_attr$StockOptionLevel <- as.numeric(emp_attr$StockOptionLevel)
StockOptionLevel_Disc <- discretize(emp_attr$StockOptionLevel, method = "fixed"
                                    , breaks = c(0,1,2,3)
                                    , labels = c("None","0","1,2,3"))
table(StockOptionLevel_Disc)


#[1] "TotalWorkingYears"
sort(unique(emp_attr$TotalWorkingYears))
WorkYears_Disc <- discretize(emp_attr$TotalWorkingYears, method = "fixed"
                              , breaks = c(0,5,8,10,12,18,36)
                              , labels = c("Very Low","Moderately Low","Medium"
                                           ,"Moderately High","High","Very High"))
table(WorkYears_Disc)

#[1] "TrainingTimesLastYear"
sort(unique(emp_attr$TrainingTimesLastYear))
Training_Disc <- discretize(emp_attr$TrainingTimesLastYear, method = "fixed"
                             , breaks = c(0,2,3,4,6)
                             , labels = c("Low","Moderate","High","Very High"))
table(Training_Disc)

#WorkLifeBalance
table(emp_attr$WorkLifeBalance)
emp_attr$WorkLifeBalance <- as.numeric(emp_attr$WorkLifeBalance)
WorkLifeBalance_disc <- discretize(emp_attr$WorkLifeBalance, method = "fixed"
                                   , breaks = c(1,3,4)
                                   , labels = c("1-2","3-4"))
table(WorkLifeBalance_disc)

#[1] "YearsAtCompany"
sort(unique(emp_attr$YearsAtCompany))
YearsAtCo_Disc <- discretize(emp_attr$YearsAtCompany, method = "fixed"
                            , breaks = c(0,3,6,10,24)
                            , labels = c("Low","Moderate","High","Very High"))
table(YearsAtCo_Disc)

#[1] "YearsInCurrentRole"
sort(unique(emp_attr$YearsInCurrentRole))
CurrRole_Disc <- discretize(emp_attr$YearsInCurrentRole, method = "fixed"
                             , breaks = c(0,2,3,5,8,14)
                             , labels = c("Very Low","Moderately Low","Medium",
                                          "Moderately High","Very High"))
table(CurrRole_Disc)

#[1] "YearsSinceLastPromotion"
sort(unique(emp_attr$YearsSinceLastPromotion))
LastPro_Disc <- discretize(emp_attr$YearsSinceLastPromotion, method = "fixed"
                            , breaks = c(0,1,3,5,10)
                            , labels = c("Low","Moderate","High","Very High"))
table(LastPro_Disc)

#[1] "YearsWithCurrManager"
sort(unique(emp_attr$YearsWithCurrManager))
CurrMangr_Disc <- discretize(emp_attr$YearsWithCurrManager, method = "fixed"
                           , breaks = c(0,1,3,5,8,17)
                           , labels = c("Very Low","Moderately Low","Medium",
                                        "Moderately High","Very High"))
table(CurrMangr_Disc)

##########################################################################
# DATA PREPROCESSING -> CATEGORICAL DATA FRAME

str(emp_attr)

colnames(emp_attr)
#Columns/Variables:-
#"Age","Attrition","BusinessTravel","DailyRate","Department",DistanceFromHome",
#"Education","EducationField","EnvironmentSatisfaction","Gender"
#"HourlyRate","JobInvolvement","JobLevel","JobRole",
#"JobSatisfaction","MaritalStatus","MonthlyIncome","MonthlyRate",
#"NumCompaniesWorked","OverTime","PercentSalaryHike","PerformanceRating",
#"RelationshipSatisfaction","StockOptionLevel","TotalWorkingYears",
#"TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany",
#"YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager"

EMP <- data.frame(Age_Disc,emp_attr$Attrition,emp_attr$BusinessTravel
                  ,DailyRate_Disc,emp_attr$Department,DistFromHome_Disc
                  ,Education_disc,emp_attr$EducationField,emp_attr$EnvironmentSatisfaction
                  ,emp_attr$Gender,HourlyRate_Disc,JobInvolvement_Disc,JobLevel_Disc
                  ,emp_attr$JobRole,emp_attr$JobSatisfaction,emp_attr$MaritalStatus,MonthInc_Disc
                  ,MonthRate_Disc,NumCompanies_Disc,emp_attr$OverTime,SalaryHike_Disc
                  ,PerformanceRating_Disc,emp_attr$RelationshipSatisfaction,StockOptionLevel_Disc
                  ,WorkYears_Disc,Training_Disc,WorkLifeBalance_disc,YearsAtCo_Disc,CurrRole_Disc
                  ,LastPro_Disc,CurrMangr_Disc)
EMP
str(EMP)
summary(EMP)

colnames(EMP) <- c("Age","Attrition","BusinessTravel","DailyRate","Department","DistanceFromHome",
                   "Education","EducationField","EnvironmentSatisfaction","Gender",
                   "HourlyRate","JobInvolvement","JobLevel","JobRole",
                   "JobSatisfaction","MaritalStatus","MonthlyIncome","MonthlyRate",
                   "NumCompaniesWorked","OverTime","PercentSalaryHike","PerformanceRating",
                   "RelationshipSatisfaction","StockOptionLevel","TotalWorkingYears",
                   "TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany",
                   "YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")
summary(EMP)

sum(colnames(EMP) != colnames(emp_attr))

?apriori
rules <- apriori(EMP, parameter = list(support=0.05,confidence=0.3)
                 ,appearance = list(rhs="Attrition=Yes", default = "lhs"))
inspect(rules)


rules <- apriori(EMP, parameter = list(support=0.5,confidence=0.5)
                 ,appearance = list(rhs="Attrition=No", default = "lhs"))
inspect(rules)


plot(rules)


file.choose()
setwd("/Users/trinaghosh/Desktop/Trina/IST 707/HW 1/")
write.csv(EMP, "employee.csv")
