library(randomForest)
library(dplyr)
train_data <- read.csv('/Users/kefeng.shi/Downloads/santander-customer-transaction-prediction/train.csv') %>% 
  select(-ID_code)
train_data$target[train_data$target == 0] <- "N"
train_data$target[train_data$target == 1] <- "P"
train_data$target <- as.factor(train_data$target)

test_data <- read.csv('/Users/kefeng.shi/Downloads/santander-customer-transaction-prediction/test.csv')
train_data <- train_data %>% mutate(group = sample(1:4,nrow(train_data),replace = T, prob = NULL))
unique(train_data$group)

train_output <- data.frame()
test_output <- data.frame()
for(i in unique(train_data$group)){
  train1 <- train_data %>% filter(group != i) %>% select(-group)
  test1 <- train_data %>% filter(group == i) %>% select(-group)
  set.seed(3)
  RF_model <- randomForest(target ~ ., data=train1, nodesize=10, na.action=na.omit, ntree=100)
  varImpPlot(RF_model, type=2)
  train_prediction <- predict(RF_model,train1,type="prob") 
  train_p <- cbind(train1 ,as.data.frame(train_prediction) %>% select(P))
  train_output <- rbind(train_output,train_p)
  
  
  test_prediction <- predict(RF_model,test1,type="prob")
  test_p <- cbind(test1 ,as.data.frame(test_prediction) %>% select(P))
  test_output <- rbind(test_output,test_p)
}

##train error
train_output$pred[train_output$P > 0.5] <- 'P'
train_output$pred[train_output$P <= 0.5] <- 'N'
train_error <- nrow(train_output%>% filter(pred != target))/nrow(train_output)
train_acc <- 1- train_error
##test error
test_output$pred[test_output$P > 0.5] <- 'P'
test_output$pred[test_output$P <= 0.5] <- 'N'
test_error <- nrow(test_output%>% filter(pred != target))/nrow(test_output)
test_acc <- 1 - test_error



###get AUC
tru_value <- test_output %>% mutate(my_true_value = ifelse(target == 'P',1,0)) %>% select(my_true_value)
pre_value <- test_output %>% select(P)
pred <- prediction(pre_value, tru_value)
perf1 <- performance(pred,'auc')
AUC <- perf1@y.values[[1]]   
perf <- performance(pred,"tpr","fpr")
myAUC <- round(AUC, digits =3)
plot(perf,ylim=c(0,1),xlim=c(0,1),yaxs='i', xaxs='i',col="darkblue",lwd=5)
abline(0,1,col="gray",lty=1)

### live prediction
train_data <- train_data %>% select(-group)
RF_model <- randomForest(target ~ ., data=train_data, nodesize=10, na.action=na.omit, ntree=100)
live_prediction <- predict(RF_model,test_data,type="prob")
live_prediction_p <- cbind(test_data, as.data.frame(live_prediction) %>% select(P) ) %>% mutate(pred = ifelse(P>0.5,1,0))



















