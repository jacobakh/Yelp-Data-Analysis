#APOLLO
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

ols.model = lm(y.stars~., data = train.DF)
ols.prediction = predict(ols.model, newdata=test.DF)

ols.predict = data.frame(business_id = pred.rnames, stars=ols.prediction)


test.DF=NULL;train.DF=NULL; save.image("~/model_models/OLS.RData")

# ridge ---------------------------------------------------------------------


train.DF= readRDS("training_set.RDS")%>% 
  select(-business_id, -user.id, -review_id) 
train.DF = apply(train.DF, FUN=as.numeric, 2)%>% 
  as.data.frame 

test.DF= readRDS("testing_set.RDS")%>% 
  select(-business_id, -user.id, -review_id) 
test.DF = apply(test.DF, FUN=as.numeric, 2)%>% 
  as.data.frame 


##alpha=0 ridge alpha parameter 
ridge.models = train(y.stars ~ ., method="glmnet", 
                     data = train.DF, 
                     trControl= trainControl(method="cv"),
                     preProcess = c("center", "scale"),
                     tuneGrid = expand.grid(alpha = 0, 
                                            lambda = seq(0.05, 0.5, by = 0.05)))
best.param_ridge = ridge.models$bestTune

ridge.model = train(y.stars ~ ., method="glmnet", 
                    data = train.DF, preProcess = c("center", "scale"),
                    trControl= trainControl(method="cv"),
                    tuneGrid = expand.grid(alpha = best.param_ridge$alpha, 
                                           lambda = best.param_ridge$lambda))

ridge.prediction = predict(ridge.model, newdata=test.DF)
ridge.predict = data.frame(business_id = pred.rnames, stars=ridge.prediction)

test.DF=NULL;train.DF=NULL; save.image("~/model_models/Ridge.RData")
