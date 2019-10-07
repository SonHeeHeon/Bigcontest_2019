

dnn_aucscores<-vector(length=5)

{
  set.seed(1)                       ## seed 추가 
  x<-createFolds(D$dly,k=5)
  test_D<-list()
  train_D<-list()
  for(i in 1:5){
    test_D[[i]]<-D[x[[i]],]
    train_D[[i]]<-D[-x[[i]],]
  }
}
{
  set.seed(1)                       ## seed 추가 
  y<-createFolds(A$dly,k=5)
  test_A<-list()
  train_A<-list()
  for(i in 1:5){
    test_A[[i]]<-A[y[[i]],]
    train_A[[i]]<-A[-y[[i]],]
  }
}


for(i in 1:5){
  test<-test_D[[i]]
  x<-read.csv(paste0("C:/Users/higi4/PycharmProjects/untitled9/valid_",i,".csv"))
  pred<-x$X1
  pr<-prediction(pred,test$dly)
  auc<-performance(pr,measure = "auc")
  dnn_roc <- performance(pr, "tpr", "fpr")  
  par(new=T)
  plot(dnn_roc, col=rainbow(5)[i])
  dnn_aucscores[i]<-auc@y.values[[1]]
}
legend("bottomright", legend=c("K=1", "K=2", "K=3", "K=4", "K=5"), col=rainbow(5), lty=1, bg = "white", cex = 1)
dnn_aucscores
RMSE(pred,test$dly)

for(i in 1:5){
  test<-test_A[[i]]
  x<-read.csv(paste0("C:/Users/higi4/PycharmProjects/untitled9/valid_",i,".csv"))
  pred<-x$X1
  pr<-prediction(pred,test$dly)
  auc<-performance(pr,measure = "auc")
  dnn_roc <- performance(pr, "tpr", "fpr")  
  par(new=T)
  plot(dnn_roc, col=rainbow(5)[i])
  dnn_aucscores[i]<-auc@y.values[[1]]
}
legend("bottomright", legend=c("K=1", "K=2", "K=3", "K=4", "K=5"), col=rainbow(5), lty=1, bg = "white", cex = 1)
dnn_aucscores
RMSE(pred,test$dly)


