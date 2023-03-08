library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(car)
library(caTools)


homeloan.dt <- fread("/Users/phyllisyen/Desktop/BC2406 ANALYTICS 1/AY22 BC2406 CBA/homeloan2.csv")
View(homeloan.dt)
summary(homeloan.dt)

# DATA CLEANING

# check data types
str(homeloan.dt)

# factoring nominal/logical variables
homeloan.dt$Gender <- factor(homeloan.dt$Gender)
homeloan.dt$Married <- factor(homeloan.dt$Married)
homeloan.dt$Dependents <- factor(homeloan.dt$Dependents)
homeloan.dt$Self_Employed <- factor(homeloan.dt$Self_Employed)
homeloan.dt$Property_Area <- factor(homeloan.dt$Property_Area)
homeloan.dt$Loan_Status <- factor(homeloan.dt$Loan_Status)

# factoring ordinal variables
homeloan.dt$Education <- factor(homeloan.dt$Education, ordered =T, levels= c("Not Graduate", "Graduate"))
homeloan.dt$Credit_Score <- factor(homeloan.dt$Credit_Score, ordered =T, levels= c(0,1))


# Cleaning NAs
anyNA(homeloan.dt)
summary(homeloan.dt)
# Since the use-case of this dataset is to provide financial information of borrowers to the bank, we try to avoid deleting information since it can be critical for the bank’s decision-making when large sums of money are transacted. Deleting rows could warp data analysis since other columns which contain highly valuable information are omitted.


ggplot(homeloan.dt, aes(y=Loan_Amount_Term)) + geom_boxplot()
ggplot(homeloan.dt, aes(x = Credit_Score, fill = Loan_Status)) + geom_bar()

# Dependents: replace missing values with "to declare"
homeloan.dt[! (Dependents == ""), Dependents1:=Dependents]
homeloan.dt[is.na(Dependents1), Dependents1 := "to declare"]
homeloan.dt$Dependents1 <- factor(homeloan.dt$Dependents1, levels = c("0","1","2","3+", "to declare"))
# summary check on new column
summary(homeloan.dt$Dependents)
summary(homeloan.dt$Dependents1)
str(homeloan.dt$Dependents1)

       
# Loan amount term: assign median
homeloan.dt[!(is.na(Loan_Amount_Term)), Loan_Amount_Term1 :=Loan_Amount_Term]
homeloan.dt[is.na(Loan_Amount_Term), Loan_Amount_Term1:=360]
summary(homeloan.dt$Loan_Amount_Term)
summary(homeloan.dt$Loan_Amount_Term1)


# Credit Score: implement condition using Loan_Status
homeloan.dt[Loan_Status == "Y" & is.na(Credit_Score), Credit_Score1 := "1"]
homeloan.dt[Loan_Status == "N" & is.na(Credit_Score), Credit_Score1 := "0"]
homeloan.dt[!(is.na(Credit_Score)), Credit_Score1 := Credit_Score]
homeloan.dt$Credit_Score1 <- factor(homeloan.dt$Credit_Score1, ordered = T, levels = c(0,1))
summary(homeloan.dt$Credit_Score)
summary(homeloan.dt$Credit_Score1)



# Married, Gender and Self_Employed: replace missing values with "missing"
homeloan.dt[Gender == "Male", Gender1:="Male"]
homeloan.dt[Gender == "Female", Gender1:='Female']
homeloan.dt[Gender == "", Gender1:='missing']
homeloan.dt$Gender1 <- factor(homeloan.dt$Gender1, levels = c("Male", "Female", "missing"))
summary(homeloan.dt$Gender)
summary(homeloan.dt$Gender1)

homeloan.dt[Married == "", Married1:="missing"]
homeloan.dt[Married == "Yes", Married1:='Yes']
homeloan.dt[Married == "No", Married1:='No']
homeloan.dt$Married1 <- factor(homeloan.dt$Married1, levels = c("Yes", "No", "missing"))
summary(homeloan.dt$Married)
summary(homeloan.dt$Married1)


homeloan.dt[Self_Employed == "", Self_Employed1:="missing"]
homeloan.dt[Self_Employed == "Yes", Self_Employed1:='Yes']
homeloan.dt[Self_Employed == "No", Self_Employed1:='No']
homeloan.dt$Self_Employed1 <- factor(homeloan.dt$Self_Employed1, levels = c("Yes", "No", "missing"))
summary(homeloan.dt$Self_Employed)
summary(homeloan.dt$Self_Employed1)


# remove original columns
homeloan.dt <- homeloan.dt[, ! c("Dependents", "Loan_Amount_Term", "Credit_Score", "Gender", "Married", "Self_Employed")]
summary(homeloan.dt)
anyNA(homeloan.dt)

# final number of cases in cleaned dataset
homeloan.dt[, .N]


# DATA EXPLORATION
# applicant income 
summary(homeloan.dt$ApplicantIncome)
ggplot(homeloan.dt, aes(x=ApplicantIncome)) + geom_histogram() + labs(title ="Applicant Incomes", y="Number of Applicants") 
median(homeloan.dt$ApplicantIncome)
# identify outliers
box_plot <- boxplot(homeloan.dt$ApplicantIncome)$out
outlier_row <- which(homeloan.dt$ApplicantIncome %in% c(box_plot))
homeloan.dt[outlier_row,]
# remove outlier
outliers <- boxplot(homeloan.dt$ApplicantIncome, plot = F)$out
homeloan_removed <- homeloan.dt[-which(homeloan.dt$ApplicantIncome %in% outliers),]
ggplot(homeloan_removed, aes(x=ApplicantIncome)) + geom_histogram() + labs(title ="Applicant Incomes (w/o Outliers)", y="Number of Applicants") 
median(homeloan_removed$ApplicantIncome)

# coapplicant income
summary(homeloan.dt$CoapplicantIncome)
subgroup <- homeloan.dt[Self_Employed1 == "No" & Credit_Score1 == 1 & Married1 == "Yes"]
percentile <- ecdf(subgroup$CoapplicantIncome)
percentile(1612)
homeloan.dt$CoapplicantIncome[which(homeloan.dt$Loan_ID == "LP002369")] <- 1612
View(homeloan.dt)

# loan amount
homeloan.dt$LoanAmount <- homeloan.dt$LoanAmount*1000
options(scipen = 10)

ggplot(homeloan.dt, aes(x=LoanAmount, y=Loan_Status)) + geom_boxplot() + labs(title = "Relationship between Loan Amount and Loan Status") 

ggplot(homeloan.dt, aes(x=LoanAmount, y=Property_Area, fill = Loan_Status)) + geom_boxplot() + labs(title = "Property Area and Loan Amount") 

# credit score
ggplot(homeloan.dt, aes(x=Credit_Score1, y=Loan_Status)) + geom_jitter() + labs(title = "Credit Score and Loan Status") 
ggplot(homeloan.dt, aes(x=Loan_Status, fill=Credit_Score1)) + geom_bar(position="fill") + labs(title = "Stacked Bar Chart for Credit Score and Loan Status")

# dependents 
ggplot(homeloan.dt, aes(y=Loan_Status, x=Dependents1, colour = Credit_Score1)) + geom_jitter() + labs(title = "Dependents and Loan Status") 
homeloan.dt[Dependents1 == 0, .N]
homeloan.dt[Dependents1 != 0, .N]
ggplot(homeloan.dt, aes(x=Dependents1, fill=Loan_Status)) + geom_bar(position="fill") + labs(title = "Stacked Bar Chart for Dependents and Loan Status") 

# self employment
ggplot(homeloan.dt, aes(x=Self_Employed1, fill=Loan_Status)) + geom_bar(position="fill") + labs(title = "Stacked Bar Chart for  Self Employment Status and Loan Status with Credit Score")+ facet_grid(~ Credit_Score1)

# property area
ggplot(homeloan.dt, aes(y=Loan_Status, x=Property_Area, colour = Credit_Score1)) + geom_jitter() + labs(title = "Self Employment and Loan Status") 

# marital status
ggplot(homeloan.dt, aes(x=Married1, fill=Loan_Status)) + geom_bar(position="fill") + labs(title = "Stacked Bar Chart for Marital Status and Loan Status")

ggplot(homeloan.dt, aes(x=Married1, y=LoanAmount)) + geom_boxplot() + labs(title = "Boxplot for Marital Status and Loan Amount")


# DATA MODELING
# CART 
set.seed(8)
train <- sample.split(Y = homeloan.dt$Loan_Status, SplitRatio = 0.7)
trainset <- subset(homeloan.dt, train == T)
testset <- subset(homeloan.dt, train == F)
trainset[,.N]
testset[,.N]


# plots the maximal tree and results.
m1 <- rpart(Loan_Status ~ Gender1 + Married1 + Self_Employed1 + LoanAmount + Loan_Amount_Term1 + Credit_Score1+ Education + Property_Area + ApplicantIncome + CoapplicantIncome + Dependents1, data = trainset, method = 'class', control = rpart.control(minsplit = 2, cp= 0.0))
rpart.plot(m1, nn= T, main = "Maximal Tree in homeloan.csv")
print(m1)
printcp(m1)

plotcp(m1, main = "Subtrees in homeloan.csv")

cp1 <- 0.076

m2 <- prune(m1, cp = cp1)
printcp(m2)
rpart.plot(m2, nn= T, main = "Pruned Tree with cp = 0.076")
print(m2)

# test set
cart.predict <- predict(m2, newdata = testset, type = "class")
results <- data.frame(testset, cart.predict)
table(testset$Loan_Status, cart.predict)
mean(testset$Loan_Status == cart.predict)


# LOGSITIC REGRESSION

fitsall <- glm(Loan_Status ~ . -Loan_ID, data = homeloan.dt, family = "binomial")

fitsallStep <- step(fitsall)
vif(fitsallStep)

m3 <- glm(Loan_Status ~ Married1 + Property_Area + Credit_Score1, family = "binomial", data = homeloan.dt)
summary(m3)

train.m1 <- glm(Loan_Status ~ Married1 + Property_Area + Credit_Score1, family = "binomial", data = trainset)
summary(train.m1)
prob.train <- predict(train.m1, type = "response")
train.m1.predict <- ifelse(prob.train > 0.5, "Yes", "No")
cm <- table(trainset$Loan_Status, train.m1.predict, deparse.level = 2) 
accuracy.train <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision.train <- cm[4] / sum(cm[4], cm[2])

test.m1 <- glm(Loan_Status ~ Married1 + Property_Area + Credit_Score1, family = binomial, data = testset)
summary(test.m1)
prob.test <- predict(test.m1, type = "response")
test.m1.predict <- ifelse(prob.test > 0.5, "Yes", "No")
cm1 <- table(test.m1.predict, testset$Loan_Status, deparse.level = 2)
accuracy.test <- sum(cm1[1], cm1[4]) / sum(cm1[1:4])
precision.test <- cm1[4] / sum(cm1[4], cm1[2])


# FURTHER ANALYSIS 
# This section is used to examine if there is gender discrimination in the loan processing procedure
# drop missing rows
gender.homeloan <- homeloan.dt[Gender1 != "missing", ! "Loan_ID"]

# Gender and Loan_Status:
ggplot(gender.homeloan, aes(x= Gender1, y=Loan_Status)) + geom_jitter() + labs(title = "Gender and Loan Status")
# more males have approved loans, however this disparity could be due to the difference in number of applicants from both genders
# to test this assumption, I checked the number of males and females in the dataset
gender.homeloan[Gender1 == "Male", .N] #470
gender.homeloan[Gender1 == "Female", .N] #109
# assumption is true -> significantly more males than females
# use stacked bar chart to view results based on percentage instead of nominal number
ggplot(gender.homeloan, aes(x = Gender1, fill=Loan_Status)) + geom_bar(position="fill")  + labs(title = "Stacked Chart Gender and Loan Status")
gender.homeloan[Gender1 == "Female" & Loan_Status == "Y", .N]
gender.homeloan[Gender1 == "Male" & Loan_Status == "Y", .N]
# About 70% of Males and 67% of Females have their loans approved. Since this is a negligible difference, it debunks the claim that there is gender discrimination in the loan approval process.

# Gender, Credit_Score & Loan_Status:
ggplot(gender.homeloan, aes(x = Gender1, fill=Loan_Status)) + geom_bar(position="fill")  + labs(title = "Gender, Credit Score and Loan Status") + facet_grid(~ Credit_Score1)
# Furthermore, applicants from both genders with the same credit score are as likely to have their loans approved or denied, with Females being only a few percentage points behind.

# Hence, my overall findings corroborates the view that the loan approval process is not discriminatory towards Females or Males. 

