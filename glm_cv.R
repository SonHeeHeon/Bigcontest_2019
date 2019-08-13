library(caret)
library(MLmetrics)

k=5
aucscores<-vector(length=k)
accuracy<-vector(length=k)
for( i in 1:k){
  train<-read.csv(paste("C:/Desktop/Son/������/���� 2019/������_����/dep_train_",i,".csv",sep=""),header=T)
  test<-read.csv(paste("C:/Desktop/Son/������/���� 2019/������_����/dep_test_",i,".csv",sep=""),header=T)
  train<-train[,c(12,17,18,19,20,21,22,23,24,25,26)]
  test<-test[,c(12,17,18,19,20,21,22,23,24,25,26)]
  model<-glm(��������~.,family='binomial',data=train)
  # step�� ����
  p <- predict(model, newdata=test, type="response")
  pr <- prediction(p, test$��������)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  optid<-(1:length(prf@y.values[[1]][-1]))[((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-11])^2)
                                           ==min((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-1])^2)]
  optcut<-prf@alpha.values[[1]][-1][optid]
  glm.pred=rep("N",nrow(test))
  glm.pred[p>=optcut]="Y"
  glm_cfm<-confusionMatrix(as.factor(glm.pred),test$��������)
  pr_1 <- prediction(as.numeric(as.factor(glm.pred)),as.numeric(test$��������))   # �̺κ��� ���� ���� => prediction�� ���� ��Ȯ�� ���������� �ʿ��ϴ� 
  prf_1 <- performance(pr_1, measure = "tpr", x.measure = "fpr")
  accuracy[i]<-glm_cfm$overall[[1]]
  auc <- performance(pr_1, measure = "auc")
  aucscores[i]<-auc@y.values[[1]]
}

aucscores
accuracy
mean(aucscores)
mean(accuracy)