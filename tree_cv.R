library(e1071)
library(tree)
aucscores<-vector(length=10)
accuracy<-vector(length=10)
i=1
for(i in 1:10){
  train_1<-read.csv(paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/dep_train_",i,".csv",sep=""),header=T)
  test_1<-read.csv(paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/dep_test_",i,".csv",sep=""),header=T)
  train_1<-train_1[,c(12,17,18,19,20,21,22,23,24,25,26)]
  test_1<-test_1[,c(12,17,18,19,20,21,22,23,24,25,26)]
  tree_1<-tree(지연여부~.,train_1)
  tree_1_pred<-predict(tree_1,test_1,type="class")
  tree_1_pred1<-predict(tree_1,test_1,type="vector")
  tree_x<-rep(0,length(test_1$match))
  tree_x[tree_1_pred1[,2]>.25]<-1
  tree_cfm<-confusionMatrix(as.factor(tree_x),test_1$match)
  accuracy[i]<-tree_cfm$overall[[1]] # tree 정확ㄷ
  error_1[i]<-tree_cfm$table[1,2]/(tree_cfm$table[1,2]+tree_cfm$table[2,2])
  pr <- prediction(as.numeric(tree_x),as.numeric(test_1$match)-1)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  tree_auc <- performance(pr, measure = "auc") 
  tree_auc <- tree_auc@y.values[[1]]
  aucscores[i]<-tree_auc
  f1scores[i]<-F1_Score(test_1$match,tree_x,positive=1)
}
aucscores
f1scores
accuracy
error_1
mean(aucscores)
mean(f1scores)
mean(accuracy)
mean(erro_1)