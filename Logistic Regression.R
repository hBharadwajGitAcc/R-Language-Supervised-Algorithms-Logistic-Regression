############################ Questions on Simple logistic regression ############################ 

# 1.	Build a simple logistic regression model on the 'customer_churn' dataframe, where the dependent variable is 'Churn' & the independent variable is 'TechSupport'. Store the result in 'log_mod_1': a.Have a glance at the summary of the model built.
library(readr)
churn = read.csv("C:\\Users\\user\\Downloads\\Module-01_Introduction to Data Science _ R Programming_Module_1\\Data-Set\\customer_churn.csv")

log_mod_1 = glm(Churn~TechSupport, data= churn, family="binomial") 
summary(log_mod_1)


# b.Predict the result when the value of 'TechSupport' is 'Yes'
levels(churn$TechSupport)
predict(log_mod_1,data.frame(TechSupport="Yes"),type="response")


# c.Predict the result when the value of 'TechSupport' is 'No'
predict(log_mod_1,data.frame(TechSupport="No"),type="response")


# d.Predict the result when the value of 'TechSupport' is 'No internet service'
predict(log_mod_1,data.frame(TechSupport="No internet service"),type="response")


# 2.Build a simple logistic regression model on the 'customer_churn' dataframe, where the dependent variable is 'Dependents' & the independent variable is 'tenure'. Store the result in 'log_mod_2': a.Have a glance at the summary of the model built.
log_mod_2 = glm(Dependents~tenure, data= churn, family="binomial")
summary(log_mod_2)


# b.Predict the result when the value of 'tenure' is 10
predict(log_mod_2,data.frame(tenure=10),type="response")


# c.Predict the result when the value of 'tenure' is 50
predict(log_mod_2,data.frame(tenure=50),type="response")


# d.Predict the result when the value of 'tenure' is 70
predict(log_mod_2,data.frame(tenure=70),type="response")




############################ Questions on Multiple Logistic regression ############################ 
  
# 1.Build a multiple logistic regression model: a.Start off by dividing the data-set into 'train' & 'test' sets in 65:35 ratio, with the split-criteria being determined by 'gender' column
library(caTools)
split = sample.split(churn$gender, SplitRatio = 0.65) 
train = subset(churn, split==T)
test = subset(churn, split==F)

nrow(train)
nrow(test)


# b.Build a logistic regression model on the train set where the dependent variable is 'gender' & the independent variables are 'Dependents', 'InternetService' & 'Contract' & store the result in 'log_mod_multi'
log_mod_multi = glm(gender~Dependents+InternetService+Contract, data=train, family = "binomial")


# c.Predict the values on top of the test set & store the result in 'result_log_multi'
result_log_multi = predict(log_mod_multi, newdata=test, type="response")
head(result_log_multi)


# d.Have a look at the range of 'result_log_multi' & build a confusion matrix where the threshold of predicted values is greater than '0.49'
range(result_log_multi)
table(test$gender, result_log_multi > 0.49)
con_mat = table(result_log_multi > 0.49, test$gender)
con_mat


# e.Calculate the accuracy of the model from the confusion matrix
?performance
library(ROCR)
predict_log = prediction(result_log_multi,test$gender)
acc = performance(predict_log, "acc")
acc

accuracy = sum(diag(con_mat)/sum(con_mat))
accuracy # 0.4896552

error = 1 - accuracy
error # 0.5103448


# 2.Build second logistic regression model on the same 'train' & 'test' sets: a.In this case dependent variable is 'gender' & the independent variables are 'tenure', 'MonthlyCharges' & 'PaymentMethod
log_mod_multi2 = glm(gender~tenure+MonthlyCharges+PaymentMethod, data=train, family = "binomial")


# b.Predict the values on top of the test set & store the result in 'result_log_multi2'
result_log_multi2 = predict(log_mod_multi2, newdata=test, type="response")
head(result_log_multi2)


# c.Have a look at the range of 'result_log_multi2' & build a confusion matrix where the threshold of predicted values is greater than 0.49
range(result_log_multi2)
table(test$gender, result_log_multi2 > 0.49)
con_mat2 = table(result_log_multi2 > 0.49, test$gender)
con_mat2


# d.Calculate the accuracy of the model from the confusion matrix
predict_log2 = prediction(result_log_multi2,test$gender)
acc2 = performance(predict_log2, "acc")
acc2

accuracy2 = sum(diag(con_mat2)/sum(con_mat2))
accuracy2 # 0.5014199

error2 = 1 - accuracy2
error2 # 0.4985801




############################ Questions on ROCR package ############################ 

# 1.Build a logistic regression model: a.Start off by dividing the data-set into 'train' & 'test' sets in 80:20 ratio, with the split-criteria being determined by 'Churn' column
library(caTools)
split2 = sample.split(churn$Churn, SplitRatio = 0.80) 
train2 = subset(churn, split2==T)
test2 = subset(churn, split2==F)

nrow(train2)
nrow(test2)


# b.Build a logistic regression model on the train set where the dependent variable is 'Churn' & the independent variables are 'MonthlyCharges', 'tenure' & 'TechSupport' & store the result in 'log_mod_roc'
log_mod_roc = glm(Churn~MonthlyCharges+tenure+TechSupport, data=train2, family = "binomial")


# c.Predict the values on top of the test set & store the result in 'result_log_roc'
result_log_roc = predict(log_mod_roc,newdata=test2,type="response")
head(result_log_roc)


# d.Use the performance() function from the ROCR package & build the 'Accuracy vs cut-off' plot
library(ROCR)
#table(test$Churn, result_log_roc > 0.0)
predict_log3 = prediction(result_log_roc,test2$Churn)
acc3 = performance(predict_log3, "acc")
plot(acc3)


# e.Plot the 'ROC' curve
table(test2$Churn, result_log_roc > 0.41)
roc_curve = performance(predict_log3,"tpr","fpr") 
plot(roc_curve)
plot(roc_curve, colorize=T)


# f.Find out the "area under the curve"
table(test2$Churn,result_log_roc > 0.28)

auc = performance(predict_log3, "auc")
auc

