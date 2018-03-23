library(ggplot2)
library(leaps)
library(glmnet)

set.seed(1)
inp_data=read.csv("BA_Project_Data.csv")
summary(inp_data)
head(inp_data)
inp_data$outcome=ifelse(inp_data$y=='yes',1,0)
train=sample(1:nrow(inp_data),0.75*nrow(inp_data))
train_data=inp_data[train,]
test_data=inp_data[-train,]

base_model_accuracy=sum(inp_data$outcome)/(nrow(inp_data))*100
base_model_accuracy=(100-base_model_accuracy)
base_model_accuracy

#Some basic graphs to get a sense of the data
boxplot(duration ~ outcome, data = inp_data,xlab = "outcome", ylab = "Call Duration",main = "Call duration ")

boxplot(inp_data$age, main="Age Box plot", xlab="Age", horizontal=TRUE,col=terrain.colors(3))

min = min(inp_data$age)
max = max(inp_data$age)

Groups = cut(x = inp_data$age, breaks = seq(from = min, to = max, by = 10))
Groups = data.matrix(Groups, rownames.force = NA)

Bygroup = tapply(inp_data$outcome, Groups, sum)

barplot(height = Bygroup, xlab = "age", ylab = "outcomes")

min = min(inp_data$euribor3m)
max = max(inp_data$euribor3m)

Groups = cut(x = inp_data$euribor3m, breaks = seq(from = min, to = max, by = 0.25))
Groups = data.matrix(Groups, rownames.force = NA)

Bygroup = tapply(inp_data$outcome, Groups, sum)

barplot(height = Bygroup, xlab = "euribor", ylab = "outcomes")

y1=xtabs(outcome~marital, inp_data)
barplot(y1, col='skyblue',xlab = "marital status", ylab = "Total Yes", main=" Marital analysis")

y2=xtabs(outcome~education, inp_data)
barplot(y2, col='skyblue',xlab = "education", ylab = "Total Yes", main=" Education analysis")

y3=xtabs(outcome~default,inp_data)
barplot(y3, col='skyblue',xlab = "credit default", ylab = "Total Yes", main=" Credit default analysis")

y4=xtabs(outcome~housing,inp_data)
barplot(y4, col='skyblue',xlab = "housing loan", ylab = "Total Yes", main=" Housing loan analysis")

y5=xtabs(outcome~loan,inp_data)
barplot(y5, col='skyblue',xlab = "personal loan", ylab = "Total Yes", main=" Person loan analysis")

y6=xtabs(outcome~month,inp_data)
barplot(y6, col='skyblue',xlab = "month", ylab = "Total Yes", main=" Monthly analysis")

y7=xtabs(outcome~job,inp_data)
barplot(y7, col='skyblue',xlab = "job", ylab = "Total Yes", main=" Job analysis")

y8=xtabs(outcome~pdays,inp_data)
barplot(y8, col='skyblue',xlab = "pdays", ylab = "Total Yes", main=" Pdays analysis")

y9=xtabs(outcome~previous,inp_data)
barplot(y9, col='skyblue',xlab = "previous contacts", ylab = "Total Yes", main=" Previous contact analysis")

y10=xtabs(outcome~poutcome,inp_data)
barplot(y10, col='skyblue',xlab = "poutcome", ylab = "Total Yes", main=" Previous campaign outcome analysis")

#Applying Chi square tests to check dependance 
temp = table(inp_data$job,inp_data$outcome) 
chisq.test(temp) 
#significant

temp = table(inp_data$loan,inp_data$outcome) 
chisq.test(temp) 
#insignificant

temp = table(inp_data$marital,inp_data$outcome) 
chisq.test(temp) 
#significant

temp = table(inp_data$education,inp_data$outcome) 
chisq.test(temp) 
#significant

temp = table(inp_data$default,inp_data$outcome) 
chisq.test(temp) 
#significant

temp = table(inp_data$housing,inp_data$outcome) 
chisq.test(temp) 
#insignificant

cor_data=inp_data[,c(16,17,18,19,20)]
cor_table=cor(cor_data)
cor_table

#Trying a logistic regression model to predict the outcome 
log_model=glm(outcome~.-y-duration-cons.price.idx-nr.employed-emp.var.rate-cons.conf.idx-loan-housing,data=train_data,family=binomial)
summary(log_model)

library(ROCR)

pred = predict(log_model, type = 'response')
ROCRpred = prediction(pred, train_data$outcome)
ROCRperf = performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(ROCRperf, ROCRpred))

#Trying to achieve best threshold by max accuracy

log_predict=predict(log_model,train_data,type='response')
l1=min(log_predict)
l2=max(log_predict)
log_threshold=seq(l1,l2,0.0001)
min_error=nrow(train_data)
min_error_thresh=1
for (i in 1:length(log_threshold)){
  tempf=ifelse(log_predict>=log_threshold[i],1,0)
  err=sum(tempf!=train_data$outcome)
  if(err<min_error){
    min_error=err
    min_error_thresh=log_threshold[i]
  }
}
min_error
min_error_thresh
log_classification_error=(min_error/nrow(train_data))*100
tempf=ifelse(log_predict>=min_error_thresh,1,0)
classficationTable=table(truth=train_data$outcome,predict=tempf)
classficationTable
train_data_accuracy=(1-min_error/nrow(train_data))*100
train_data_accuracy

#Test data error for the above approach
log_predict=predict(log_model,test_data,type='response')
tempf=ifelse(log_predict>=min_error_thresh,1,0)
test_data_accuracy=(sum(tempf==test_data$outcome)/nrow(test_data))*100
test_data_accuracy
classficationTable=table(truth=test_data$outcome,predict=tempf)
classficationTable

#Error and classification table when using ROC curve for selecting the threshold
log_predict=predict(log_model,train_data,type='response')
tempf=ifelse(log_predict>=0.10138562,1,0)
classficationTable=table(truth=train_data$outcome,predict=tempf)
classficationTable
train_data_accuracy_roc=(sum(tempf==train_data$outcome)/(nrow(train_data)))*100
train_data_accuracy_roc
#test data error for the ROC approach
log_predict=predict(log_model,test_data,type='response')
tempf=ifelse(log_predict>=0.10138562,1,0)
classficationTable=table(truth=test_data$outcome,predict=tempf)
classficationTable
test_data_accuracy_roc=(sum(tempf==test_data$outcome)/(nrow(test_data)))*100
test_data_accuracy_roc


#fitting a decision tree model with pruning on train data
library(tree)
d_tree=tree(y~.-outcome-duration-cons.price.idx-nr.employed-emp.var.rate-cons.conf.idx-loan-housing,data=train_data)
summary(d_tree)
plot(d_tree)
text(d_tree,pretty = 0)
new_tree = cv.tree(d_tree, FUN=prune.misclass)
names(new_tree)
par(mfrow=c(1,2))
plot(new_tree$size,new_tree$dev,type="b")
plot(new_tree$k,new_tree$dev,type="b")
pruned_tree = prune.misclass(d_tree,best=3)
plot(pruned_tree)
text(pruned_tree,pretty=0)
tree_predictions=predict(pruned_tree,train_data,type = "class")
train_data_accuracy_tree=sum(tree_predictions==train_data$y)/nrow(train_data)
train_data_accuracy_tree
classficationTable=table(truth=train_data$y,predict=tree_predictions)
classficationTable
#Tree accuracy on test data
tree_predictions=predict(pruned_tree,test_data,type = "class")
test_data_accuracy_tree=sum(tree_predictions==test_data$y)/nrow(test_data)
test_data_accuracy_tree
classficationTable=table(truth=test_data$y,predict=tree_predictions)
classficationTable


# trying to predict the call duration 

grid=c(0.001,0.01,0.1,0,1,10,100,1000)
x=model.matrix(duration~. -y-outcome-cons.price.idx-nr.employed-emp.var.rate-cons.conf.idx-loan-housing,data=train_data)[,-1]
y=train_data$duration
cv.out=cv.glmnet(x,y,alpha=1,lambda=grid,nfolds=10) 
bestlam=cv.out$lambda.min
bestlam
#Making the model on the entire dataset with the lambda obtained
x=model.matrix(duration~. -y-outcome-cons.price.idx-nr.employed-emp.var.rate-cons.conf.idx-loan-housing,data=inp_data)[,-1]
y=inp_data$duration
lasso_final_model=glmnet(x,y,alpha=1,lambda=bestlam)
coefficients(lasso_final_model)

xtrain=model.matrix(duration~. -y-outcome-cons.price.idx-nr.employed-emp.var.rate-cons.conf.idx-loan-housing ,data=inp_data)[,-1]
lasso_predict=predict(lasso_final_model, xtrain)
rmse=sqrt(sum((lasso_predict-inp_data$duration)^2)/nrow(inp_data))
rmse

#KNN
inp_data=read.csv("BA_Project_N.csv")
summary(inp_data)
head(inp_data)
num.vars <- sapply(inp_data, is.numeric)
myvars <- c("age","job","education","default","housing","loan","contact","month","day_of_week","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")
inp_data.subset <- inp_data[myvars]
summary(inp_data.subset)
set.seed(1)
test<- sample(1:nrow(inp_data),0.25*nrow(inp_data))
train.inp_data <- inp_data.subset[-test,]
test.inp_data <- inp_data.subset[test,]
train.def <- inp_data$y[-test]
test.def <- inp_data$y[test]

library(class)

knn.1 <- knn(train.inp_data,test.inp_data,train.def,k=1)
knn.2 <- knn(train.inp_data,test.inp_data,train.def,k=2)
knn.3 <- knn(train.inp_data,test.inp_data,train.def,k=3)
knn.4 <- knn(train.inp_data,test.inp_data,train.def,k=4)
knn.5 <- knn(train.inp_data,test.inp_data,train.def,k=5)
knn.6 <- knn(train.inp_data,test.inp_data,train.def,k=6)
knn.7 <- knn(train.inp_data,test.inp_data,train.def,k=7)
knn.8 <- knn(train.inp_data,test.inp_data,train.def,k=8)
knn.9 <- knn(train.inp_data,test.inp_data,train.def,k=9)
knn.10 <- knn(train.inp_data,test.inp_data,train.def,k=10)
knn.11 <- knn(train.inp_data,test.inp_data,train.def,k=11)
knn.12 <- knn(train.inp_data,test.inp_data,train.def,k=12)
knn.13 <- knn(train.inp_data,test.inp_data,train.def,k=13)
knn.14 <- knn(train.inp_data,test.inp_data,train.def,k=14)
knn.15 <- knn(train.inp_data,test.inp_data,train.def,k=15)
knn.16 <- knn(train.inp_data,test.inp_data,train.def,k=16)
knn.17 <- knn(train.inp_data,test.inp_data,train.def,k=17)
knn.18 <- knn(train.inp_data,test.inp_data,train.def,k=18)
knn.19 <- knn(train.inp_data,test.inp_data,train.def,k=19)
knn.20 <- knn(train.inp_data,test.inp_data,train.def,k=20)
knn.21 <- knn(train.inp_data,test.inp_data,train.def,k=21)
knn.22 <- knn(train.inp_data,test.inp_data,train.def,k=22)
knn.23 <- knn(train.inp_data,test.inp_data,train.def,k=23)
knn.24 <- knn(train.inp_data,test.inp_data,train.def,k=24)
knn.25 <- knn(train.inp_data,test.inp_data,train.def,k=25)
knn.26 <- knn(train.inp_data,test.inp_data,train.def,k=26)
knn.27 <- knn(train.inp_data,test.inp_data,train.def,k=27)
knn.28 <- knn(train.inp_data,test.inp_data,train.def,k=28)
knn.29 <- knn(train.inp_data,test.inp_data,train.def,k=29)
knn.30 <- knn(train.inp_data,test.inp_data,train.def,k=30)

100 * sum(test.def == knn.1)/10297
100 * sum(test.def == knn.2)/10297
100 * sum(test.def == knn.3)/10297
100 * sum(test.def == knn.4)/10297
100 * sum(test.def == knn.5)/10297
100 * sum(test.def == knn.6)/10297
100 * sum(test.def == knn.7)/10297
100 * sum(test.def == knn.8)/10297
100 * sum(test.def == knn.9)/10297
100 * sum(test.def == knn.10)/10297
100 * sum(test.def == knn.11)/10297
100 * sum(test.def == knn.12)/10297
100 * sum(test.def == knn.13)/10297
100 * sum(test.def == knn.14)/10297
100 * sum(test.def == knn.15)/10297
100 * sum(test.def == knn.16)/10297
100 * sum(test.def == knn.17)/10297
100 * sum(test.def == knn.18)/10297
100 * sum(test.def == knn.19)/10297
100 * sum(test.def == knn.20)/10297
100 * sum(test.def == knn.21)/10297
100 * sum(test.def == knn.22)/10297
100 * sum(test.def == knn.23)/10297
100 * sum(test.def == knn.24)/10297
100 * sum(test.def == knn.25)/10297
100 * sum(test.def == knn.26)/10297
100 * sum(test.def == knn.27)/10297
100 * sum(test.def == knn.28)/10297
100 * sum(test.def == knn.29)/10297
100 * sum(test.def == knn.30)/10297

table(knn.1,test.def)
table(knn.5,test.def)
table(knn.10,test.def)
table(knn.15,test.def)
table(knn.20,test.def)
table(knn.23,test.def)
table(knn.25,test.def)
table(knn.28,test.def)
table(knn.30,test.def)

