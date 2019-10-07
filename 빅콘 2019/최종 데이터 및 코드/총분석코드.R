{
  if(!require(caret)) {
    install.packages("caret")
  }
  library(caret)
  if(!require(MLmetrics)) {
    install.packages("MLmetrics")
  }
  library(MLmetrics)
  if(!require(e1071)) {
    install.packages("e1071")
  }
  library(e1071)
  if(!require(kknn)) {
    install.packages("(kknn")
  }
  library(kknn)
  if(!require(dplyr)) {
    install.packages("dplyr")
  }
  library(dplyr)
  if(!require(ROCR)) {
    install.packages("ROCR")
  }
  library(ROCR)
  if(!require(randomForest)) {
    install.packages("randomForest")
  }
  library(randomForest)
  if(!require(doParallel)) {
    install.packages("doParallel")
  }
  library(doParallel)
  if(!require(parallel)) {
    install.packages("parallel")
  }
  library(parallel)
  if(!require(doSNOW)) {
    install.packages("doSNOW")
  }
  library(doSNOW)
  if(!require(kernlab)) {
    install.packages("kernlab")
  }
  library(kernlab)
  if(!require(naivebayes)) {
    install.packages("naivebayes")
  }
  library(naivebayes)
}

detectCores()     # cpu 갯수 확인 
numCores <- detectCores() - 3   #  병렬처리 
myCluster <- makeCluster( numCores )
registerDoSNOW(myCluster)


k=5   # 각 기법들의 auc / accuracy 를 입력할 공간 마련
{
  glm_aucscores<-vector(length=k)
  glm_accuracy<-vector(length=k)
  knn_aucscores<-vector(length=k)
  knn_accuracy<-vector(length=k)
  bag_aucscores<-vector(length=k)
  bag_accuracy<-vector(length=k)
  ranf_aucscores<-vector(length=k)
  ranf_accuracy<-vector(length=k)
  gb_aucscores<-vector(length=k) # 그래디언트 부스팅
  gb_accuracy<-vector(length=k)
  xgb_aucscores<-vector(length=k)
  xgb_accuracy<-vector(length=k)
  svml_aucscores<-vector(length=k)
  svml_accuracy<-vector(length=k)
  svmr_aucscores<-vector(length=k)
  svmr_accuracy<-vector(length=k)
  naive_aucscores<-vector(length=k)
  naive_accuracy<-vector(length=k)
  naivek_aucscores<-vector(length=k)
  naivek_accuracy<-vector(length=k)
}

dep_1<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/depart_zisu_final.csv")
arr_1<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/arrive_zisu_final.csv")
str(dep_1)

dep_1$dly<-as.factor(dep_1$dly)
dep_1$연<-as.factor(dep_1$연)
dep_1$월<-as.factor(dep_1$월)
dep_2<-dep_1[,c(18,20,21,22,23,24,25,26)]
arr_1$dly<-as.factor(arr_1$dly)
arr_1$연<-as.factor(arr_1$연)
arr_1$월<-as.factor(arr_1$월)
arr_2<-arr_1[,c(18,20,21,22,23,24,25,26)]  # 출도착,dly(지연여부),지수만 데이터 추출 (머신러닝 분석에 간편성을 위해)
str(arr_2)
install.packages('xfun')
library(xfun)
#dat1_3<-dat1_3 %>% 
#  mutate(dly = factor(dly, labels = make.names(levels(dly))))

dep_glm<-dep_1      # glm 출발데이터 
arr_glm<-arr_1      # glm 도착데이터

# kfold로 데이터를 train / test로 나누는 작업

{
  set.seed(1)                       ## seed 추가 
  x<-createFolds(dep_2$dly,k=5)
  test_d<-list()
  train_d<-list()
  for(i in 1:5){
    test_d[[i]]<-dep_2[x[[i]],]
    train_d[[i]]<-dep_2[-x[[i]],]
  }
  y<-createFolds(arr_1$dly,k=5)
  test_a<-list()
  train_a<-list()
  for(i in 1:5){
    test_a[[i]]<-arr_2[y[[i]],]
    train_a[[i]]<-arr_2[-y[[i]],]
  }
  
  test_d_glm<-list()
  train_d_glm<-list()
  for(i in 1:5){
    test_d_glm[[i]]<-dep_glm[x[[i]],]
    train_d_glm[[i]]<-dep_glm[-x[[i]],]
  }
  test_a_glm<-list()
  train_a_glm<-list()
  for(i in 1:5){
    test_a_glm[[i]]<-arr_glm[y[[i]],]
    train_a_glm[[i]]<-arr_glm[-y[[i]],]
  }
}

str(train_d_1)
str(train_d_glm)
i=1

for( i in 1:k){
  train_d_1<-train_d[[i]]
  test_d_1<-test_d[[i]]
  train_d_glm_1<-train_d_glm[[i]]
  test_d_glm_1<-test_d_glm[[i]]
  # 1. glm
  model_glm<-glm(dly~연+월+요일+공항+상대공항+항공사+계획시간지수+노선지수,family='binomial',data=train_d_glm_1) # 연지수/노선/요일지수/항공사 지수 => glm 결과에 na 뜸 (거의 똑같은 작동이 되는 지수가 있는경우 na 가 된다고 함(즉 상관성이 높은 다른 변수가 있다는것))
  # 연지수,요일지수,항공사지수로 이용했을때보다 더 좋은 결과 (auc 0.05정도)
  summary(model_glm)
  # step은 생략
  p <- predict(model_glm, newdata=test_d_glm_1, type="response")
  pr <- prediction(p, test_d_glm_1$dly)
  auc <- performance(pr, measure = "auc")
  glm_aucscores[i]<-auc@y.values[[1]]
  # 2. knn
  set.seed(5)
  model_knn<-train.kknn(dly~.,train_d_1,ks=seq(3,50,by=1),scale=TRUE,kcv=5)  # scale 해야되는가?? => knn 다시 알아보기 train 으로 좋은 k 를 설정 하는 기준을 auc로 하면 더좋아질수 잇더 .
  best_k<-model_knn$best.parameters$k
  knn_pred<-kknn(dly~.,train_d_1,test_d_1,k=best_k,scale=TRUE)
  p_1<-knn_pred$prob[,2]
  pr_1 <- prediction(p_1, test_d_1$dly)
  auc_1 <- performance(pr_1, measure = "auc")
  knn_aucscores[i]<-auc_1@y.values[[1]]
  # 3. bagging
  model_bag<-randomForest(dly~.,data=train_d_1,mtry=(length(train_d_1)-1),importance=TRUE,ntree=400) 
  p_2<-predict(model_bag,test_d_1,type="prob")    
  pr_2<-prediction(p_2[,2],test_d_1$dly)
  auc_2<-performance(pr_2,measure="auc")
  bag_aucscores[i]<-auc_2@y.values[[1]]
  # 4. randomforest
  customGrid <- expand.grid(mtry = 1:(length(train_d_1)-1))    
  fitControl <- trainControl(method = "cv", number = 5,classProbs=TRUE, summaryFunction=twoClassSummary)
  set.seed(10)
  model_ranf<-train(dly~.,data = train_d_1, method = "rf", trControl = fitControl, tuneGrid = customGrid,metric="ROC", verbose = F,ntree=200)
  p_3<-predict(model_ranf,test_d_1,type="prob")
  pr_3<-prediction(p_3[,2],test_d_1$dly)
  auc_3<-performance(pr_3,measure = "auc")
  ranf_aucscores[i]<-auc_3@y.values[[1]]
  # 5. svm
  ## 5-1 linear svm
  grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
  set.seed(15)
  model_svml <- train(dly ~., data = train_d_1, method = "svmLinear",trControl=fitControl,preProcess = c("center", "scale"),tuneGrid = grid,metric="ROC")
  model_svml
  plot(model_svml)
  p_4<-predict(model_svml,test_d_1,type="prob")
  pr_4<-prediction(p_4,test_d_1$dly)
  auc_4<-performance(pr_4,measure = "auc")
  svml_aucscores[i]<-auc_4@y.values[[1]]
  ## 5-2 Radial svm
  grid_radial <- expand.grid(sigma = c(0.01,0.03,0.05, 0.07, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),C = c(0.01, 0.1, 0.25, 0.5,1, 1.5, 2,5))
  set.seed(20)
  model_svmr <- train(dly ~., data = train_d_1, method = "svmRadial",trControl=fitControl,preProcess = c("center", "scale"),tuneGrid = grid_radial)
  model_svmr
  plot(model_svmr)
  p_5<-predict(model_svmr,test_d_1,type="prob")
  pr_5<-prediction(p_5,test_d_1$dly)
  auc_5<-performance(pr_5,measure = "auc")
  svmr_aucscores[i]<-auc_5@y.values[[1]]
  # 6. naive bayes
  ## 6-1. no kernel
  model_naive=naiveBayes(dly~., data=train_d_1)
  print(model_naive)
  p_6<-predict(model_naive,test_d_1,type="raw")
  pr_6<-prediction(p_6[,2],test_d_1$dly)
  auc_6<-performance(pr_6,measure="auc")
  naive_aucscores[i]<-auc_6@y.values[[1]]
  ## 6-2. kernel
  model_naivek=naive_bayes(dly~.,usekernel=T, data=train_d_1)
  print(model_naivek)
  p_7<-predict(model_naivek,test_d_1[,-1],type="prob")
  pr_7<-prediction(p_7[,2],test_d_1$dly)
  auc_7<-performance(pr_7,measure="auc")
  naivek_aucscores[i]<-auc_7@y.values[[1]]
  
  print(paste(i,"번째 model 완료"))
}
str(train_d_1)
unique(train_d_1$노선지수)
sum(train_d_1$dly=="X1")/length(train_d_1$dly) 
# 지금  auc가 너무 낮음 => 해결방안이 필요    
# 편명 지수 어디?

# roc 커브를 각각 겹치게 그려서 image로 뽑아내기
# 과적합인지 보는것도 만들기

mean_glm<-mean(glm_aucscores)
mean_bag<-mean(bag_aucscores)
mean_knn<-mean(knn_aucscores)
mean_ranf<-mean(rnaf_aucscores)
mean_gb<-mean(gb_aucscores)
mean_xgb<-mean(xgb_aucscores)
mean_svm<-mean(svm_aucscores)
mean_naive<-mean(naive_aucscores)
data.frame()

'''

timer <- proc.time()
glmSerial <- foreach(i = 1:3) %dopar% {
  glm(dly~연+월+요일+공항+상대공항+항공사+계획시간지수+노선지수,family='binomial',data=train_d_glm[[i]])
}
proc.time() - timer  

timer <- proc.time()
for( i in 1:3){
  glm(dly~연+월+요일+공항+상대공항+항공사+계획시간지수+노선지수,family='binomial',data=train_d_glm[[i]]) # 연지수/노선/요일지수/항공사 지수 => glm 결과에 na 뜸 (거의 똑같은 작동이 되는 지수가 있는경우 na 가 된다고 함(즉 상관성이 높은 다른 변수가 있다는것))
}
proc.time() - timer 

model_glm<-glm(dly~연지수+월지수+요일지수+공항지수+상대공항지수+항공사지수+계획시간지수+노선지수,family='binomial',data=train_d_glm_1) # 연지수/노선/요일지수/항공사 지수 => glm 결과에 na 뜸 (거의 똑같은 작동이 되는 지수가 있는경우 na 가 된다고 함(즉 상관성이 높은 다른 변수가 있다는것))
summary(model_glm)
str(train_d_glm_1)
varImp(model_bag)

str(dep_glm)
dep_glm_1<-dep_glm[,-(19:27)]
str(dep_glm_1)

temp1<-as.data.frame(model.matrix(~공항,data=dat1_2),row.names=F)
names(temp1)<-levels(dat1_2$공항)
temp2<-as.data.frame(model.matrix(~항공사,data=dat1_2),row.names=F)
names(temp2)<-levels(dat1_2$항공사)
temp3<-as.data.frame(model.matrix(~상대공항,data=dat1_2),row.names=F)
names(temp3)<-paste(levels(dat1_2$상대공항),"_1",sep="")
temp4<-as.data.frame(model.matrix(~연,data=dat1_2),row.names=F)
names(temp4)<-levels(dat1_2$연)
temp5<-as.data.frame(model.matrix(~월,data=dat1_2),row.names=F)
names(temp5)<-levels(dat1_2$월)
temp6<-as.data.frame(model.matrix(~요일,data=dat1_2),row.names=F)
names(temp6)<-levels(dat1_2$요일)

dat1_x<-cbind(temp1,temp2,temp3,temp4,temp5,temp6,"계획시간지수"=dat1_2$계획시간지수,"노선지수"=dat1_2$노선지수,"dly"=dat1_2$dly,"출도착"=dat1_2$출도착)
str(dat1_x)
dat1_x_d<-filter(dat1_x,출도착=="D")[,-61]

x_1<-createFolds(dat1_x_d$dly,k=5)
test_x_d<-list()
train_x_d<-list()
for(i in 1:5){
  test_x_d[[i]]<-dat1_x_d[x_1[[i]],]
  train_x_d[[i]]<-dat1_x_d[-x_1[[i]],]
}
str(train_x_d[[1]])

model_naive_x=naiveBayes(dly~., data=train_x_d[[1]])
print(model_naive_x)
p_6<-predict(model_naive_x,test_x_d[[1]],type="raw")
pr_6<-prediction(p_6[,2],test_x_d[[1]]$dly)
auc_6<-performance(pr_6,measure="auc")
naive_aucscores[i]<-auc_6@y.values[[1]]
'''

