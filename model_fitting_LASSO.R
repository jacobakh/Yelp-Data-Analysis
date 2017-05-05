#Helios
library(dplyr);library(caret)
# Load Data --------------------------------------------------------
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

##alpha=1 is LASSO
lasso.models = train(y.stars ~ ., method="glmnet",
                     data = train.DF, preProcess = c("center", "scale"),
                     trControl= trainControl(method="cv"),
                     tuneGrid = expand.grid(alpha = c(1),
                                            lambda = seq(0.05, 0.5, by = 0.05)))
best.param_lasso = lasso.models$bestTune


lasso.model = train(y.stars ~ ., method="glmnet",
                    data = train.DF, preProcess = c("center", "scale"),
                    trControl= trainControl(method="cv"),
                    tuneGrid = expand.grid(alpha = best.param_lasso$alpha, 
                                           lambda = best.param_lasso$lambda))

lasso.prediction = predict(lasso.model, newdata=test.DF)
lasso.predict = data.frame(business_id = pred.rnames, stars=lasso.prediction)
test.DF=NULL;train.DF=NULL; save.image("~/model_models/LASSO.RData")