#Sekhmet
library(dplyr);library(caret);library(doMC)
registerDoMC(4)

train.DF= readRDS("training_set.RDS")%>% 
  select(-business_id, -user_id, -review_id) 
train.DF = apply(train.DF, FUN=as.numeric, 2)%>% 
  as.data.frame 

test.DF = readRDS("testing_set.RDS")
pred.rnames = test.DF$business_id

test.DF = test.DF %>% 
  select(-business_id, -user_id, -review_id) 
test.DF = apply(test.DF, FUN=as.numeric, 2)%>% 
  as.data.frame 


my.grid = expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
nnet.models =  train(y.stars ~ ., data = train.DF, 
                      method = "nnet", maxit = 1000, 
                   tuneGrid = my.grid, trace = F, linout = 1)  
best.param_nnet = nnet.models$bestTune

nnet.model = nnet.models$finalModel

nnet.prediction = predict(nnet.model, newdata=test.DF)
nnet.predict = data.frame(business_id = pred.rnames, stars=nnet.prediction)

test.DF=NULL;train.DF=NULL; save.image("~/model_models/NNET.RData")