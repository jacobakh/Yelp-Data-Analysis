
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
pca.models = train(y.stars~., method="pcr", 
                   data = train.DF, 
                   preProcess = c("center", "scale"), 
                   trControl= trainControl(method="cv"))

best.param_pca = pca.models$bestTune
  
pca.model = train(y.stars ~ ., method="pcr", 
                    data = train.DF, 
                    preProcess = c("center", "scale"),
                    trControl= trainControl(method="cv"),
                    tuneGrid = expand.grid(ncomp=))

pca.prediction = predict(pca.model, newdata=test.DF)
pca.predict = data.frame(business_id = pred.rnames, stars=pca.prediction)