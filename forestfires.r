

forestfires<-read.csv(choose.files())
forestfires


####splitting the data into train and test data####
install.packages('caTools') 
library(caTools) 
set.seed(123) 
split = sample.split(forestfires$size_category, SplitRatio = 0.70) 
split


training_set = subset(forestfires, split == TRUE) 
training_set
test_set = subset(forestfires, split == FALSE) 
test_set

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(size_category~.,data = training_set,kernel = "vanilladot")
model1


# kernel = rfdot 
model_rfdot<-ksvm(size_category ~.,data = training_set,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_set)
mean(pred_rfdot==test_set$size_category)  #78%

# kernel = vanilladot
model_vanilla<-ksvm(size_category ~.,data = training_set,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test_set)
mean(pred_vanilla==test_set$size_category) #85%


# kernal = besseldot
model_besseldot<-ksvm(size_category ~.,data = training_set,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test_set)
mean(pred_bessel==test_set$size_category)  #62%

# kernel = polydot

model_poly<-ksvm(size_category ~.,data = training_set,kernel = "polydot")
pred_poly<-predict(model_poly,newdata =test_set)
mean(pred_poly==test_set$size_category)  #85%













