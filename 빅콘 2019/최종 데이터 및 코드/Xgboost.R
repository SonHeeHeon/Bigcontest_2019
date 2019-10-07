library(xgboost)
library(ggplot2)
library(reshape2)
# install.packages("Ecdat")
library(Ecdat)

# = parameters = #
# = eta candidates = #
eta=c(0.05,0.1,0.2,0.5,1)
# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)
# = max_depth candidates = #
md=c(2,4,6,10)
# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

# = standard model is the second value  of each vector above = #
standard=c(2,2,3,2)

install.packages("Epi")
library(Epi)

ROC(test = p, stat = test$label, plot = "ROC", AUC = T, main = "SVM")


##################### 돌리고 감 

{
  start <- Sys.time(); print(start);
  train<-train_D[[i]]
  test<-test_D[[i]]
  pdt <- p_delay_time_function(train,test)
  train$p_delay_time <- pdt[[1]]
  test$p_delay_time <- pdt[[2]]
  train$delay_time <- NULL
  test$delay_time <- NULL
  train <-list(data= as(as.matrix(train[,-1]),"dgCMatrix"),label=train$dly)
  test <-list(data= as(as.matrix(test[,-1]),"dgCMatrix"),label=test$dly)
}

#####그리드 서치 

grid = expand.grid(eta=seq(0.1,0.4,0.05), gamma = seq(0,5,1))
train_rmse_last <- NULL
test_rmse_last <- NULL
for ( i in 1:nrow(grid) ){
  start <- Sys.time(); print(start);
  params=list(eta = grid[i,1], gamma = grid[i,2])
  model = xgb.cv(data = train$data, label = train$label,
                 nfold = 5, nrounds = 200, early_stopping_rounds = 150,
                 objective = "binary:logistic",verbose = F, prediction = T,
                 params = params)
  train_rmse_last <- c(train_rmse_last,unlist(model$evaluation_log[,2]) %>% last)
  test_rmse_last <- c(test_rmse_last,unlist(model$evaluation_log[,4]) %>% last)
  print(Sys.time()-start)
  print(i)
}
temp <- data.frame(grid,train_rmse_last,test_rmse_last)

grid[which.min(temp$test_rmse_last),]
#  eta gamma
# 0.15     1


####### 파라메터 적용 
params=list(eta = 0.1, gamma = 2)
model = xgboost(data=train$data, label= train$label,
                nrounds = 200, early_stopping_rounds = 150,
                objective = "binary:logistic",verbose = F,
                params = params)

imp = xgb.importance(model = model)

data.frame(variable = rep(imp$Feature,3),
           value = c(imp$Gain,imp$Cover,imp$Frequency),
           Type = c(rep('Gain',nrow(imp)),rep('Cover',nrow(imp)),rep('Frequency',nrow(imp)))
) %>% ggplot(aes(variable,value,fill = variable))+
  geom_bar(stat = 'identity')+
  facet_grid(~Type)+
  theme_bw()+
  ggtitle('XGBoost : Customized Importance Plot',
          subtitle = "힘내자")

p <- predict(model, newdata=test$data, type="prob")
pr <- prediction(p, test$label)
auc <- performance(pr, measure = "auc")
aucscores <- NULL
aucscores[i]<-auc@y.values[[1]]





##### 
grid_search = foreach(i = 1:nrow(grid),.combine = rbind,.packages = c('dplyr','xgboost')) %dopar% {
  model = xgb.cv(data = train$data, label = train$label,
  nfold = 5, nrounds = 200, early_stopping_rounds = 150,
  objective = "binary:logistic",verbose = F, prediction = T,
  params = grid[i,]
  )
  data.frame(train_rmse_last = unlist(model$evaluation_log[,2]) %>% last,
             test_rmse_last = unlist(model$evaluation_log[,4]) %>% last)
  }
print(Sys.time()-start)
stopCluster(cl)

grid_search[which.min(grid_search$test_rmse_last),]

grid[which.min(grid_search$test_rmse_last),]

# eta  gamma
# 0.4 0 

####
model = xgboost(data=train$data, label= train$label,
                nrounds = 200, early_stopping_rounds = 150,
                objective = "binary:logistic",verbose = F,
                params = params)

imp = xgb.importance(model = model)

data.frame(variable = rep(imp$Feature,3),
           value = c(imp$Gain,imp$Cover,imp$Frequency),
           Type = c(rep('Gain',nrow(imp)),rep('Cover',nrow(imp)),rep('Frequency',nrow(imp)))
) %>% ggplot(aes(variable,value,fill = variable))+
  geom_bar(stat = 'identity')+
  facet_grid(~Type)+
  theme_bw()+
  ggtitle('XGBoost : Customized Importance Plot',
          subtitle = "힘내자")

p <- predict(model, newdata=test$data, type="prob")
pr <- prediction(p, test$label)
auc <- performance(pr, measure = "auc")
aucscores <- NULL
aucscores[i]<-auc@y.values[[1]]




### 0908 17:55
# 하이퍼 파라메터 찾기 
####################
train<-train_D[[i]]
test<-test_D[[i]]
pdt <- p_delay_time_function(train,test)
train$p_delay_time <- pdt[[1]]
test$p_delay_time <- pdt[[2]]

train$delay_time <- NULL
test$delay_time <- NULL

train <-list(data= as(as.matrix(train[,-1]),"dgCMatrix"),label=train$dly)
test <-list(data= as(as.matrix(test[,-1]),"dgCMatrix"),label=test$dly)


#####
library(foreach)
library(doParallel)
cl= makeCluster(detectCores()-1)
registerDoParallel(cl)
grid = expand.grid(eta=seq(0.1,0.4,0.05), gamma = seq(0,5,1))


start <- Sys.time(); print(start);
# 12시 38분

grid_search = foreach(i = 1:nrow(grid),.combine = rbind,.packages = c('dplyr','xgboost')) %dopar% {
  model = xgb.cv(data = train$data, label = train$label,
                 nfold = 5, nrounds = 200, early_stopping_rounds = 150,
                 objective = "binary:logistic",verbose = F, prediction = T,
                 params = grid[i,]
  )
  data.frame(train_rmse_last = unlist(model$evaluation_log[,2]) %>% last,
             test_rmse_last = unlist(model$evaluation_log[,4]) %>% last)
}
print(Sys.time()-start)
stopCluster(cl)

grid_search[which.min(grid_search$test_rmse_last),]

grid[which.min(grid_search$test_rmse_last),]