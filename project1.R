rm(list = ls())

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


df = read.csv('Absenteeism_at_work_Project.csv')
str(df)

missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)

names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100

missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

#remove commas and convert variable to numeric
df$Work.load.Average.day = gsub(',','',df$Work.load.Average.day)
df$Work.load.Average.day=as.numeric(df$Work.load.Average.day)

for (i in c(df$ID[is.na(df$Transportation.expense)])){
  df$Transportation.expense[is.na(df$Transportation.expense) & df$ID==i] = head(df$Transportation.expense[df$ID==i & !(is.na(df$Transportation.expense)) ],1)
}

for (i in c(df$ID[is.na(df$Distance.from.Residence.to.Work)])){
  df$Distance.from.Residence.to.Work[is.na(df$Distance.from.Residence.to.Work) & df$ID==i] = head(df$Distance.from.Residence.to.Work[df$ID==i & !(is.na(df$Distance.from.Residence.to.Work)) ],1)
}

for (i in c(df$ID[is.na(df$Service.time)])){
  df$Service.time[is.na(df$Service.time) & df$ID==i] = head(df$Service.time[df$ID==i & !(is.na(df$Service.time)) ],1)
}

for (i in c(df$ID[is.na(df$Age)])){
  df$Age[is.na(df$Age) & df$ID==i] = head(df$Age[df$ID==i & !(is.na(df$Age)) ],1)
}

for (i in c(df$ID[is.na(df$Education)])){
  df$Education[is.na(df$Education) & df$ID==i] = head(df$Education[df$ID==i & !(is.na(df$Education)) ],1)
}

for (i in c(df$ID[is.na(df$Son)])){
  df$Son[is.na(df$Son) & df$ID==i] = head(df$Son[df$ID==i & !(is.na(df$Son)) ],1)
}

for (i in c(df$ID[is.na(df$Social.drinker)])){
  df$Social.drinker[is.na(df$Social.drinker) & df$ID==i] = head(df$Social.drinker[df$ID==i & !(is.na(df$Social.drinker)) ],1)
}

for (i in c(df$ID[is.na(df$Social.smoker)])){
  df$Social.smoker[is.na(df$Social.smoker) & df$ID==i] = head(df$Social.smoker[df$ID==i & !(is.na(df$Social.smoker)) ],1)
}

for (i in c(df$ID[is.na(df$Pet)])){
  df$Pet[is.na(df$Pet) & df$ID==i] = head(df$Pet[df$ID==i & !(is.na(df$Pet)) ],1)
}

for (i in c(df$ID[is.na(df$Weight)])){
  df$Weight[is.na(df$Weight) & df$ID==i] = head(df$Weight[df$ID==i & !(is.na(df$Weight)) ],1)
}

for (i in c(df$ID[is.na(df$Height)])){
  df$Height[is.na(df$Height) & df$ID==i] = head(df$Height[df$ID==i & !(is.na(df$Height)) ],1)
}

for (i in c(df$ID[is.na(df$Body.mass.index)])){
  df$Body.mass.index[is.na(df$Body.mass.index) & df$ID==i] = head(df$Body.mass.index[df$ID==i & !(is.na(df$Body.mass.index)) ],1)
}


for (i in c(df$Month.of.absence[is.na(df$Work.load.Average.day)])){
  df$Work.load.Average.day[is.na(df$Work.load.Average.day) & df$Month.of.absence==i] = 
    head(df$Work.load.Average.day[df$Month.of.absence==i & !(is.na(df$Work.load.Average.day)) & !(is.na(df$Month.of.absence)) & !(is.na(df$Hit.target)) & 
      df$Hit.target==head(df$Hit.target[df$Month.of.absence==i & is.na(df$Work.load.Average.day) ],1)],
         length(df$Work.load.Average.day[is.na(df$Work.load.Average.day) & df$Month.of.absence==i]))}


for (i in c(df$Month.of.absence[is.na(df$Hit.target)])){
  df$Hit.target[is.na(df$Hit.target) & df$Month.of.absence==i & df$Work.load.Average.day
                ==head(df$Work.load.Average.day[df$Month.of.absence==i & is.na(df$Hit.target)],1)] = head(df$Hit.target[df$Month.of.absence==i & !(is.na(df$Work.load.Average.day)) & !(is.na(df$Month.of.absence)) & !(is.na(df$Hit.target)) & 
 df$Work.load.Average.day ==head(df$Work.load.Average.day[df$Month.of.absence==i & is.na(df$Hit.target)],1)],
                     length(df$Hit.target[is.na(df$Hit.target) & df$Month.of.absence==i & df$Work.load.Average.day
                                          ==head(df$Work.load.Average.day[df$Month.of.absence==i & is.na(df$Hit.target)],1)]))
}


for (i in c(df$Work.load.Average.day[is.na(df$Month.of.absence)])){
  df$Month.of.absence[is.na(df$Month.of.absence)] = head(df$Month.of.absence[df$Work.load.Average.day==i & !(is.na(df$Month.of.absence)) ],1)
}

#mean - 20.49
#KNN - 24.07
#Median - 23
#Actual value - 23

for (i in c(df$Absenteeism.time.in.hours[is.na(df$Reason.for.absence)])){
  df$Reason.for.absence[is.na(df$Reason.for.absence) ] = median(df$Reason.for.absence[df$Absenteeism.time.in.hours==i & !(is.na(df$Reason.for.absence)) ],na.rm = TRUE)
}


for(i in 1:nrow(df)){
  if(df$Reason.for.absence[i] == 0){
    df$Disciplinary.failure[i] = 1
  } else {df$Disciplinary.failure[i] = 0}
}



#mean - 11.6
#KNN - 20
#Median - 8
#Actual value - 8

for (i in c(df$Reason.for.absence[is.na(df$Absenteeism.time.in.hours)])){
  df$Absenteeism.time.in.hours[is.na(df$Absenteeism.time.in.hours) ] = median(df$Absenteeism.time.in.hours[df$Reason.for.absence==i & !(is.na(df$Absenteeism.time.in.hours)) ],na.rm = TRUE)
}

#-------------------------------Outlier analysis-------------------------

continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Transportation.expense',
                    'Hit.target', 'Height','Weight', 'Work.load.Average.day',
                    'Body.mass.index', 'Absenteeism.time.in.hours')

catagorical_vars = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
                     'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')

#boxplot for Transportation.expense, Distance.from.Residence.to.Work, Service.time, Age, Hit.target



boxplot(df[,c('Transportation.expense','Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target')])

#boxplot for Weight,Height,Body.mass.index,Absenteeism.time.in.hours

boxplot(df[,c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours')])

#boxplot for Work.load.Average.day 

boxplot(df[,"Work.load.Average.day"])

#for(i in continuous_vars){
 #    df[,i][df[,i] %in% boxplot.stats(df[,i])$out] = NA
  # }

#df = knnImputation(df,k=3)
#imputing by KNN gives us improper results and floating point values

for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  q = quantile(df[,i],c(0.25,0.75))
  iqr = q[2]-q[1]
  min = q[1]-1.5*iqr
  max = q[2]+1.5*iqr
  df[,i][df[,i]<min] = min
  df[,i][df[,i]>max] = max
}

#-------------------------------Feature selection-------------------------

corrgram(df[,continuous_vars], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
# Remove Weight


for (i in catagorical_vars){
  df[,i] = as.factor(df[,i])
}

factor_index = sapply(df,is.factor)
factor_data = df[,factor_index]

for (i in 1:11)
{
  print(names(factor_data)[i])
  print(chisq.test(table(df$Absenteeism.time.in.hours,factor_data[,i])))
}

#Remove social smoker and education

df = subset(df, select = -c(Weight,Social.smoker,Education))

continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Transportation.expense','Hit.target', 'Height', 'Work.load.Average.day',
                    'Body.mass.index', 'Absenteeism.time.in.hours')

#----------------------------Scaling--------------------

hist(df$Absenteeism.time.in.hours,main="Histogram of Absenteeism ")
hist(df$Distance.from.Residence.to.Work,main="Histogram of 
     distance between work and residence")
hist(df$Transportation.expense,main="Histogram of Transportation Expense")
hist(df$Work.load.Average.day,main="Histogram of Work load")

for(i in continuous_vars){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i] - min(df[,i])))
}

#-----------------Model development--------

rmExcept("df")

train.index = createDataPartition(df$Absenteeism.time.in.hours, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]

library(rpart)    #Library for regression model
DT_model= rpart(Absenteeism.time.in.hours~.,train,method="anova")
DT_model

#Prediction for train data-
DT_test=predict(DT_model,test[-18])

rmse= function(y,y1){
  sqrt(mean(abs(y-y1)^2))
}


#RMSE calculation for test data-
rmse(test[,18],DT_test)
#0.1811702

#r-square calculation-
#function for r-square-
rsquare=function(y,y1){
  cor(y,y1)^2
}

#r-square calculation for test data-
rsquare(test[,18],DT_test)
#0.2446165


#-----------------------_Random forest---------------------------

library(randomForest)
RF_model = randomForest(Absenteeism.time.in.hours ~ ., train, importance = TRUE, ntree = 500)

#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-18])

rmse(test[,18],RF_Predictions)
#RMSE_test=  0.1913823

#r-square calculation for test data-
rsquare(test[,18],RF_Predictions)
#r-square= 0.4537444


#-------------------Linear regression-----------------

numeric_index1= c('Distance.from.Residence.to.Work', 'Service.time', 'Age', 'Transportation.expense','Hit.target', 'Height', 'Work.load.Average.day',
                  'Body.mass.index', 'Absenteeism.time.in.hours')
numeric_data1= df[,numeric_index1]
cnames1= colnames(numeric_data1)
cnames1

library(usdm)  #Library for VIF(Variance Infleation factor)
vif(numeric_data1)
vifcor(numeric_data1,th=0.7) #VIF calculation for numeric variables

lr_model= lm(Absenteeism.time.in.hours~.,train[-c(1,2)])
summary(lr_model)


#check model performance on test data-
lr_test= predict(lr_model,test[-c(1,2,18)])

#RMSE calculation for test data-
rmse(test[,18],lr_test)
#RMSE_test=0.241316

regr.eval(test[,18],lr_test,stats = c('mae','mape'))

#r-square calculation for test data-
rsquare(test[,18],lr_test)
#r-square_test=0.1394887



