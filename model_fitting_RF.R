#Surya 
library(dplyr);library(caret); library(randomForest);library(doMC)
registerDoMC(16)

train.DF= readRDS("training_set.RDS")%>% 
  select(-business_id, -user_id, -review_id) 
train.DF = apply(train.DF, FUN=as.numeric, 2)%>% 
  mutate(y.stars=as.factor(y.stars))

test.DF = readRDS("testing_set.RDS")
pred.rnames = test.DF$business_id

test.DF = test.DF %>% 
  select(-business_id, -user_id, -review_id) 
test.DF = apply(test.DF, FUN=as.numeric, 2)%>% 
  as.data.frame 

y.train = train.DF[,1] %>% 
  as.factor
x.train = train.DF[,-1] %>% 
  as.matrix

x.test = test.DF %>% 
  as.matrix 

rf.models = train(y=y.train, x=x.train, method="rf", 
                 tuneLength=5, 
                 trControl= trainControl(method="cv"))

best.param_rf = rf.models$bestTune

rf.model = rf.models$finalModel

rf.prediction = predict(rf.model, newdata=x.test)
rf.predict = data.frame(business_id = pred.rnames, stars=rf.prediction)

test.DF=NULL;train.DF=NULL; save.image("~/model_models/RF.RData")