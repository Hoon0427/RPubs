install.packages("DT")
install.packages("ggvis")

library(kernlab)
library(nnet)
library(rpart)
library(randomForest)

library(dplyr)
library(caret)

library(DT)
library(class)
library(ggvis)

plot(iris)

iris %>% ggvis(~Petal.Length, ~Petal.Width, fill= ~factor(Species)) %>% layer_points()

str(iris)
summary(iris)

sum(is.na(iris))

df<-iris

set.seed(919)
training_sampling<-sort(sample(1:nrow(df), nrow(df)*0.7))
test_sampling<-setdiff(1: nrow(df),training_sampling)

training_set<-df[training_sampling,]
test_set<-df[test_sampling,]

multi_logit_m<-multinom(Species ~Petal.Length + Petal.Width, data = training_set)

multi_logit_p<-predict(multi_logit_m, newdata= test_set, type = "class")

rpart_m<-rpart(Species ~ Petal.Length + Petal.Width, data = training_set)
rpart_p<-predict(rpart_m, newdata = test_set, type = "class")

rf_m <- randomForest(Species ~ Petal.Length + Petal.Width, data=training_set)
rf_p <- predict(rf_m, newdata= test_set, type="class")

