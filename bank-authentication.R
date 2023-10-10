df<-read.csv('bank_note_data.csv')
head(df)
str(df)


#Splitting the dataset into training and testing set
library(caTools)
set.seed(101)
split<-sample.split(df$Class,SplitRatio = 0.7)
library(dplyr)
train<-subset(df,split==T)
test<-subset(df,split==F)

str(df)

#Building the neural net
library(neuralnet)
help(neuralnet)

#We are training the neural net by passing in the features and 
# we are setting linear.output as False as we are doing classifaction not regression
#Also we want 10 neurons in our hidden layer
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)

#Prediciting the test values and here we are forcing it to use compute function from neuralnet package only and not from any other package

predicted.nn.values <-neuralnet::compute(nn, test[,1:4])
predicted.nn.values
head(predicted.nn.values$net.result)

#Rounding the predicted result so that we get 0 and 1 only and not any decimal value
predictions<-sapply(predicted.nn.values$net.result,round)
head(predictions)
#Visualizing the performance using confusion matrix
table(predictions,test$Class)

#Neuralnet performed great


#Now lets check how well randomforest performs here
library(randomForest)

#Now we have to convert Class datatype which is int to factor as randomforest needs it to be factor unlike neural net

df$Class<-factor(df$Class)

#Doing train-test split

library(caTools)
set.seed(101)

split<-sample.split(df$Class,SplitRatio = 0.7)
train<-subset(df,split==T)
test<-subset(df,split==F)


#Building the model and training it
model<-randomForest(Class ~ . ,data=train)

rf.predict<-predict(model,test)

#Make table to get the confusion matrix
table(rf.predict,test$Class)

#Even our randomforest model performed great