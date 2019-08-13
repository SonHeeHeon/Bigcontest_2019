library(caret)
library(MLmetrics)

k=5
aucscores<-vector(length=k)
accuracy<-vector(length=k)
for( i in 1:k){
  train<-read.csv(paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/dep_train_",i,".csv",sep=""),header=T)
  test<-read.csv(paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/dep_test_",i,".csv",sep=""),header=T)
  train<-train[,c(12,17,18,19,20,21,22,23,24,25,26)]
  test<-test[,c(12,17,18,19,20,21,22,23,24,25,26)]
  model<-glm(지연여부~.,family='binomial',data=train)
  # step은 생략
  p <- predict(model, newdata=test, type="response")
  pr <- prediction(p, test$지연여부)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  optid<-(1:length(prf@y.values[[1]][-1]))[((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-11])^2)
                                           ==min((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-1])^2)]
  optcut<-prf@alpha.values[[1]][-1][optid]
  glm.pred=rep("N",nrow(test))
  glm.pred[p>=optcut]="Y"
  glm_cfm<-confusionMatrix(as.factor(glm.pred),test$지연여부)
  pr_1 <- prediction(as.numeric(as.factor(glm.pred)),as.numeric(test$지연여부))   # 이부분이 가장 관건 => prediction에 대한 정확한 개념인지가 필요하다 
  prf_1 <- performance(pr_1, measure = "tpr", x.measure = "fpr")
  accuracy[i]<-glm_cfm$overall[[1]]
  auc <- performance(pr_1, measure = "auc")
  aucscores[i]<-auc@y.values[[1]]
}

aucscores
accuracy
mean(aucscores)
mean(accuracy)