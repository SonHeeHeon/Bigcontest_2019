library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)
{
  if(!require(caret)) {install.packages("caret")};  library(caret);
  if(!require(MLmetrics)) {install.packages("MLmetrics")};  library(MLmetrics);
  if(!require(e1071)) {install.packages("e1071")};  library(e1071);
  if(!require(kknn)) {install.packages("kknn")};  library(kknn);
  if(!require(dplyr)) {install.packages("dplyr")};  library(dplyr);
  if(!require(ROCR)) {install.packages("ROCR")};  library(ROCR);
  if(!require(randomForest)) {install.packages("randomForest")};  library(randomForest);
  if(!require(doParallel)) {install.packages("doParallel")};  library(doParallel);
  if(!require(parallel)) {install.packages("parallel")};  library(parallel);
  if(!require(doSNOW)) {install.packages("doSNOW")};  library(doSNOW);
  if(!require(kernlab)) {install.packages("kernlab")};  library(kernlab);
  if(!require(naivebayes)) {install.packages("naivebayes")};  library(naivebayes);
  if(!require(xgboost)) {install.packages("xgboost")};  library(xgboost);
}

AFSNT <- read.csv("C:/Desktop/Son/공모전/빅콘 2019/최종 데이터 및 코드/AFSNT.csv")
AFSNT_DLY <- read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT_DLY.csv")
str(AFSNT)
## 제출 데이터에 없는 편명은 지우기 

{
  AFSNT$dly <- as.numeric(AFSNT$DLY=="Y")
  AFSNT$STT_hour <- sapply(strsplit(as.character(AFSNT$STT),":"),"[",1)
  AFSNT$STT_minute <- sapply(strsplit(as.character(AFSNT$STT),":"),"[",2)
  AFSNT$ATT_hour <- sapply(strsplit(as.character(AFSNT$ATT),":"),"[",1)
  AFSNT$ATT_minute <- sapply(strsplit(as.character(AFSNT$ATT),":"),"[",2)
  AFSNT$STT2 <- as.numeric(AFSNT$STT_hour) + as.numeric(AFSNT$STT_minute)/60
  AFSNT$ATT2 <- as.numeric(AFSNT$ATT_hour) + as.numeric(AFSNT$ATT_minute)/60
  # 계획 시간을 반올림 한 변수 
  AFSNT$STT3 <- as.factor(round(AFSNT$STT2))
  AFSNT$date <- ymd(paste(AFSNT$SDT_YY,AFSNT$SDT_MM,AFSNT$SDT_DD))
  
  # 해당 날짜의 공항에 시간당 운항 개수를 담은 파생 변수 
  AFSNT %>% group_by(date,ARP,STT3) %>% summarise(n=n()) -> temp1
  AFSNT$temp <- paste(AFSNT$date,AFSNT$ARP,AFSNT$STT3)
  temp1$temp <- paste(temp1$date,temp1$ARP,temp1$STT3)
  temp2 <- unique(temp1$n)
  for(i in temp2){
    tempinfo <- temp1$temp[temp1$n==i]
    AFSNT$ARP_STT3_n[AFSNT$temp %in% tempinfo] <- i
    # print(i)
  }
  AFSNT$temp <- NULL
  rm(temp1,temp2,tempinfo)
  # 실제 지연 시간 (음수로 나타난 경우는 나중에 전처리 과정에서 다 삭제 됨.)
  AFSNT$delay_time <- AFSNT$ATT2 - AFSNT$STT2
}

# 987,709 rows -> 859,930 rows (127,779 rows deleted)
AFSNT %>% filter(FLT %in% unique(AFSNT_DLY$FLT)) -> AFSNT

## 부정 기편 제거 

# 859,930 rows -> 857,372 rows (2,558 rows deleted)
AFSNT %>% filter(IRR == "N") -> AFSNT

## 제출 데이터에 계획시간 외에는 제거 

# 857,372 rows -> 857,354 rows (18 rows deleted)
AFSNT$hm_STT <- hm(AFSNT$STT)
AFSNT %>% filter(hm_STT >= hm("06:00") & hm_STT <= hm("23:00")) -> AFSNT

## 결항편 제거

# 857,354 rows -> 850,609 rows (6,745 rows deleted)
AFSNT %>% filter(CNL == "N") -> AFSNT

## 이상치 제거 

# 제주도 활주로 비행기 사고 
# 849,151 rows -> 848,270 rows ( 881 rows deleted )
AFSNT %>% filter(date != ymd("20170929")) -> AFSNT


# 지연 시간 이상치 ( 850,609 rows -> 850,588 rows )
AFSNT %>% filter(!delay_time %in% sort(AFSNT$delay_time)[1:21]) -> AFSNT

###

# 850,588 rows -> 849,151 rows
# 지연 시간이 3시간 이상인 것들은 이상치로 판단
AFSNT %>% filter(delay_time <3) -> AFSNT


# 출발 또는 도착만 존재하는 데이터 지우기  (849,151 rows ->  rows)
AFSNT %>%
  group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>%
  summarise(n=n()) %>% 
  filter(n==1 | n==4) %>%
  as.data.frame()-> temp

index <- NULL
for( i in 1:nrow(temp) ){
  index <- c(index,which((AFSNT$SDT_YY==temp$SDT_YY[i]) & (AFSNT$SDT_MM==temp$SDT_MM[i]) & (AFSNT$SDT_DD==temp$SDT_DD[i]) & (AFSNT$FLT==temp$FLT[i])))
  # print(i)
}
AFSNT <- AFSNT[-index,]




weather=read.csv("C:/Desktop/Son/공모전/빅콘 2019/최종 데이터 및 코드/weather.csv",header=T) #날씨데이터
weather$date <- paste0(sprintf("%04d",weather$yyyy),sprintf("%02d",weather$mm),sprintf("%02d",weather$dd))

AFSNT$date0 <- paste0(sprintf("%04d",AFSNT$SDT_YY),sprintf("%02d",AFSNT$SDT_MM),sprintf("%02d",AFSNT$SDT_DD))

AFSNT %>% mutate(wea = ifelse(date0 %in% weather$date, 1, 0)) %>% as.data.frame -> AFSNT


View(AFSNT)
# A (426,485 rows)
AFSNT %>% filter(AOD=="A") -> A
# D (426,569 rows)
AFSNT %>% filter(AOD=="D") -> D
```

variable_scoring = function(x) {
  # stt 
  x %>% group_by(STT3) %>% summarise(STT_score=round(mean(dly)*10,1)) %>% as.data.frame() -> STT_score
  
  for(i in 1:nrow(STT_score)){
    x$STT_score[x$STT3==STT_score$STT3[i]] <-  STT_score$STT_score[i]
  }
  
  # flo
  x %>% group_by(FLO) %>% summarise(FLO_score=round(mean(dly)*10,1)) %>% as.data.frame() -> FLO_score
  
  for(i in 1:nrow(FLO_score)){
    x$FLO_score[x$FLO==FLO_score$FLO[i]] <-  FLO_score$FLO_score[i]
  }
  
  # flt
  x %>% group_by(FLT,SDT_DY) %>% summarise(FLT_score=round(mean(dly)*10,1),n=n()) %>% as.data.frame() -> temp_FLT_score
  
  temp_FLT_score %>% filter(n >= 50) -> temp1
  temp_FLT_score %>% filter(n < 50) -> temp2
  index <- quantile(temp1$FLT_score, probs=c(0,0.2,0.4,0.6,0.8,1))
  
  temp1 %>% mutate(FLT_rank=ifelse(FLT_score >= index[5],"E",
                                   ifelse(FLT_score >= index[4],"D",
                                          ifelse(FLT_score >= index[3],"C",
                                                 ifelse(FLT_score >= index[2],"B","A"))))) -> temp1
  
  qt <- quantile(temp2$FLT_score, probs=c(0,0.33,0.66,1))
  temp2 %>% mutate(FLT_rank= ifelse(FLT_score >= qt[3],"D",
                                    ifelse(FLT_score >= qt[2],"C","B"))) -> temp2
  FLT_score <- rbind(temp1,temp2)
  
  
  temp_paste1 <- paste(FLT_score$FLT,FLT_score$SDT_DY)
  temp_paste2 <- paste(x$FLT,x$SDT_DY)
  
  temp_unique <- unique(FLT_score$FLT_rank)
  
  for(i in temp_unique){
    tempinfo <- temp_paste1[FLT_score$FLT_rank==i]
    x$FLT_rank[temp_paste2 %in% tempinfo] <-  i
    # print(i)
  }
  
  ### 원 핫 인코딩 
  temp3 <- model.matrix(~FLT_rank,data=x)[,-1]
  x$FLT_rankA <- as.numeric(x$FLT_rank=="A")
  x <- data.frame(x,temp3)
  
  
  # yy
  x %>% group_by(SDT_YY) %>% summarise(YY_score=round(mean(dly)*10,1)) %>% as.data.frame() -> YY_score   
  for(i in 1:nrow(YY_score)){
    x$YY_score[x$SDT_YY==YY_score$SDT_YY[i]] <-  YY_score$YY_score[i]
  }
  
  ## mm
  x %>% group_by(SDT_MM) %>% summarise(MM_score=round(mean(dly)*10,1)) %>% as.data.frame() -> MM_score   
  for(i in 1:nrow(MM_score)){
    x$MM_score[x$SDT_MM==MM_score$SDT_MM[i]] <-  MM_score$MM_score[i]
  }
  
  ## dy
  x %>% group_by(SDT_DY) %>% summarise(DY_score=round(mean(dly)*10,1)) %>% as.data.frame() -> DY_score   
  for(i in 1:nrow(DY_score)){
    x$DY_score[x$SDT_DY==DY_score$SDT_DY[i]] <-  DY_score$DY_score[i]
  }
  
  ## arp
  x %>% group_by(ARP) %>% summarise(ARP_score=round(mean(dly)*10,1)) %>% as.data.frame() -> ARP_score   
  for(i in 1:nrow(ARP_score)){
    x$ARP_score[x$ARP==ARP_score$ARP[i]] <-  ARP_score$ARP_score[i]
  }
  
  ## odp
  x %>% group_by(ODP) %>% summarise(ODP_score=round(mean(dly)*10,1)) %>% as.data.frame() -> ODP_score   
  for(i in 1:nrow(ODP_score)){
    x$ODP_score[x$ODP==ODP_score$ODP[i]] <-  ODP_score$ODP_score[i]
  }
  
  ## arp_odp
  (x %>% mutate(ARP_ODP = paste(ARP,ODP)) -> x) %>% 
    group_by(ARP_ODP) %>% summarise(ARP_ODP_score=round(mean(dly)*10,1)) %>% as.data.frame() -> ARP_ODP_score
  for(i in ARP_ODP_score$ARP_ODP_score){
    tempinfo <- ARP_ODP_score$ARP_ODP[ARP_ODP_score$ARP_ODP_score==i]
    x$ARP_ODP_score[x$ARP_ODP %in% tempinfo] <-  i
  }
  
  ## flo_reg
  x %>% group_by(FLO) %>% summarise(flo_reg_n = length(unique(REG))) -> flo_reg_n
  
  for (i in 1:nrow(flo_reg_n)){
    x$flo_reg_n[x$FLO == flo_reg_n$FLO[i]] <- flo_reg_n$flo_reg_n[i]
  }
  
  ## Season
  x %>% mutate(Season = ifelse(SDT_MM<=2,"Winter",
                               ifelse(SDT_MM <= 5,"Spring",
                                      ifelse(SDT_MM <= 8,"Summer",
                                             ifelse(SDT_MM <= 11,"fall","Winter"))))) -> x
  x %>% group_by(Season) %>% summarise(Season_score=round(mean(dly)*10,2)) -> Season_score
  for(i in 1:nrow(Season_score)){
    x$Season_score[x$Season==Season_score$Season[i]] <-  Season_score$Season_score[i]
  }
  
  ## AM_PM
  x %>% mutate(AM_PM = ifelse(as.numeric(STT2)<=12,0,1)) -> x
  
  return(x)
}

D <- variable_scoring(D)
A <- variable_scoring(A)
str(D)
str(A)


## p_delay_time
p_delay_time_function = function(train,test=NA){
  
  train_x_num <- train %>% select(-delay_time,-dly) %>% data.matrix
  train_y_num <- train$delay_time
  
  cv_model <- xgboost(data=train_x_num,label=train_y_num,
                      nfold = 5,nrounds = 200,early_stopping_rounds = 150,
                      objective='reg:linear', verbose = F,prediction = T)
  
  p_delay_time <- list()
  
  p_delay_time[[1]] <- predict(cv_model,train_x_num)
  
  if(sum(is.na(test))!=1){
    test_x_num <- test %>% select(-delay_time,-dly) %>% data.matrix
    test_y_num <- test$delay_time
    p_delay_time[[2]] <- predict(cv_model,test_x_num)
  }
  return(p_delay_time)
}

p_delay_time_A_function = function(train,test=NA){
  temp01 <- paste(train$date,train$FLT)
  temp02 <- paste(D$date,D$FLT)
  
  D_train <- D[temp02 %in% temp01,]
  D_model_data1 <- D_train[,c("dly","ARP_STT3_n","delay_time","wea","STT_score","FLO_score","FLT_rankA","FLT_rankB","FLT_rankC","FLT_rankD","FLT_rankE","YY_score","MM_score","DY_score","ARP_score","ODP_score","ARP_ODP_score","flo_reg_n","Season_score","AM_PM")]
  
  D_train_x_num <- D_model_data1 %>% select(-delay_time,-dly) %>% data.matrix
  D_train_y_num <- D_model_data1$delay_time
  
  cv_model <- xgboost(data=D_train_x_num,label=D_train_y_num,
                      nfold = 5,nrounds = 200,early_stopping_rounds = 150,
                      objective='reg:linear', verbose = F,prediction = T)
  
  p_delay_time <- list()
  
  p_delay_time[[1]] <- round(predict(cv_model,D_train_x_num),2)
  p_delay_time[[1]] <- data.frame(temp=paste(D_train$date,D_train$FLT),p_delay_time=p_delay_time[[1]])
  
  if(sum(is.na(test))!=1){
    D_test <- D[!temp02 %in% temp01,]
    D_model_data2 <- D_test[,c("dly","ARP_STT3_n","delay_time","wea","STT_score","FLO_score","FLT_rankA","FLT_rankB","FLT_rankC","FLT_rankD","FLT_rankE","YY_score","MM_score","DY_score","ARP_score","ODP_score","ARP_ODP_score","flo_reg_n","Season_score","AM_PM")]
    test_x_num <- D_model_data2 %>% select(-delay_time,-dly) %>% data.matrix
    test_y_num <- D_model_data2$delay_time
    p_delay_time[[2]] <- round(predict(cv_model,test_x_num),2)
    p_delay_time[[2]] <- data.frame(temp=paste(D_test$date,D_test$FLT),p_delay_time=p_delay_time[[2]])
    
  }
  return(p_delay_time)
}


'''
AFSNT %>% group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>% summarise(driving=round(abs(diff(STT2)),3)) %>% as.data.frame() -> dfcv_1
dfcv_1[,c(1,5)] -> dfcv_2

unique(dfcv_2) -> uniq_dfcv_2

dfcv_1$date <- ymd(paste(dfcv_1$SDT_YY,dfcv_1$SDT_MM,dfcv_1$SDT_DD))
AFSNT$date <- ymd(paste(AFSNT$SDT_YY,AFSNT$SDT_MM,AFSNT$SDT_DD))
temp_list <- list()

for(i in 1:nrow(uniq_dfcv_2)){
  temp_list[[i]] <- which(dfcv_1$FLT==uniq_dfcv_2$FLT[i] & dfcv_1$driving==uniq_dfcv_2$driving[i])
  print(i)
}
temp_list[[i]]

temp <- as.numeric(nrow(AFSNT))
for(i in 1:nrow(uniq_dfcv_2)){
  tempdate <- dfcv_1$date[temp_list[[i]]]
  AFSNT$temp[(AFSNT$date %in% tempdate) & AFSNT$FLT==uniq_dfcv_2$FLT[i]] <- uniq_dfcv_2$driving[i]
  print(i)
}

AFSNT$temp2 <- as.factor(round(AFSNT$temp,3))
ggplot(AFSNT,aes(x=temp2,fill=DLY)) + geom_bar(position="fill")
ggplot(AFSNT,aes(x=temp2,fill=DLY)) + geom_bar()
'''

colnames(D)
D_model_data <- D[,c("dly","ARP_STT3_n","delay_time","wea","STT_score","FLO_score","FLT_rankA","FLT_rankB","FLT_rankC","FLT_rankD","FLT_rankE","YY_score","MM_score","DY_score","ARP_score","ODP_score","ARP_ODP_score","flo_reg_n","Season_score","AM_PM")]

A_model_data <- A[,c("dly","ARP_STT3_n","delay_time","wea","STT_score","FLO_score","FLT_rankA","FLT_rankB","FLT_rankC","FLT_rankD","FLT_rankE","YY_score","MM_score","DY_score","ARP_score","ODP_score","ARP_ODP_score","flo_reg_n","Season_score","AM_PM")]



# dnn 을 위한 train / validation 나누기


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

A

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



i=1
str(train)
# 출발
for( i in 1:5 ){
  train<-train_D[[i]]
  test<-test_D[[i]]
  train<-train[,-(1:17)]
  train<-train[,-c(2,3,4,5,6,7,8,9,12,13,17)]
  train<-train[,-c(17,20)]
  test<-test[,-(1:17)]
  test<-test[,-c(2,3,4,5,6,7,8,9,12,13,17)]
  test<-test[,-c(17,20)]
  pdt <- p_delay_time_function(train,test)
  train$p_delay_time <- pdt[[1]]
  test$p_delay_time <- pdt[[2]]
  write.csv(train,paste0("C:/Desktop/Son/공모전/빅콘 2019/최종 데이터 및 코드/train_",i,".csv"),row.names=FALSE)
  write.csv(test,paste0("C:/Desktop/Son/공모전/빅콘 2019/최종 데이터 및 코드/valid_",i,".csv"),row.names=FALSE)
}

str(train)
# 도착
for( i in 1:5 ){
  train<-train_A[[i]]
  test<-test_A[[i]]
  pdt <- p_delay_time_A_function(train,test)
  pdt_rbind <- rbind(pdt[[1]],pdt[[2]])
  for ( j in unique(pdt_rbind$p_delay_time)){
    tempinfo <- pdt_rbind$temp[pdt_rbind$p_delay_time == j]
    train$p_delay_time[paste(train$date,train$FLT) %in% tempinfo] <- j
    test$p_delay_time[paste(test$date,test$FLT) %in% tempinfo] <- j
  }
  train<-train[,-(1:17)]
  train<-train[,-c(2,3,4,5,6,7,8,9,12,13,17)]
  train<-train[,-c(17,20)]
  test<-test[,-(1:17)]
  test<-test[,-c(2,3,4,5,6,7,8,9,12,13,17)]
  test<-test[,-c(17,20)]
  
  write.csv(train,paste0("C:/Desktop/Son/공모전/빅콘 2019/최종 데이터 및 코드/train_a",i,".csv"),row.names=FALSE)
  write.csv(test,paste0("C:/Desktop/Son/공모전/빅콘 2019/최종 데이터 및 코드/valid_a",i,".csv"),row.names=FALSE)
}


####### TEST 데이터에 파생변수 만든거 넣기

str(AFSNT_DLY)

{
  AFSNT_DLY$STT_hour <- sapply(strsplit(as.character(AFSNT_DLY$STT),":"),"[",1)
  AFSNT_DLY$STT_minute <- sapply(strsplit(as.character(AFSNT_DLY$STT),":"),"[",2)
  AFSNT_DLY$STT2 <- as.numeric(AFSNT_DLY$STT_hour) + as.numeric(AFSNT_DLY$STT_minute)/60
  # 계획 시간을 반올림 한 변수 
  AFSNT_DLY$STT3 <- as.factor(round(AFSNT_DLY$STT2))
  AFSNT_DLY$date <- ymd(paste(AFSNT_DLY$SDT_YY,AFSNT_DLY$SDT_MM,AFSNT_DLY$SDT_DD))
}

AFSNT_DLY %>% group_by(date,ARP,STT3) %>% summarise(n=n()) -> temp1
AFSNT_DLY$temp <- paste(AFSNT_DLY$date,AFSNT_DLY$ARP,AFSNT_DLY$STT3)
temp1$temp <- paste(temp1$date,temp1$ARP,temp1$STT3)
temp2 <- unique(temp1$n)
for(i in temp2){
  tempinfo <- temp1$temp[temp1$n==i]
  AFSNT_DLY$ARP_STT3_n[AFSNT_DLY$temp %in% tempinfo] <- i
  # print(i)
}
AFSNT_DLY$temp <- NULL
rm(temp1,temp2,tempinfo)

# 출도착 나눠준 후 파생변수 출도착 나눠진 데이터에서 넣기

AFSNT_DLY %>% filter(AOD=="D") -> D_DLY
AFSNT_DLY %>% filter(AOD=="A") -> A_DLY

# 파생변수 넣어주기

# 1. D

## YY_score
D_DLY$YY_score<-0
for(i in unique(D_DLY$SDT_YY)){
  D_DLY[D_DLY$SDT_YY==i,]$YY_score<-unique(D[D$SDT_YY==i,"YY_score"])
}
## MM_score
D_DLY$MM_score<-0
for(i in unique(D_DLY$SDT_MM)){
  D_DLY[D_DLY$SDT_MM==i,]$MM_score<-unique(D[D$SDT_MM==i,"MM_score"])
}
## DY_score
D_DLY$DY_score<-0
for(i in unique(D_DLY$SDT_DY)){
  D_DLY[D_DLY$SDT_DY==i,]$DY_score<-unique(D[D$SDT_DY==i,"DY_score"])
}
## ARP_score
# ARP 10 이랑 예측 데이터(ARSNT_DLY)의 ROW 수(운행횟수?)가 가장 비슷한 7,11,12,13,14
D_DLY$ARP_score<-0
for(i in unique(D_DLY$ARP)){
  if(i=="ARP10")
    D_DLY[D_DLY$ARP==i,]$ARP_score<-round(mean(unique(D[((D$ARP=="ARP7")|(D$ARP=="ARP11")|(D$ARP=="ARP12")|(D$ARP=="ARP13")|(D$ARP=="ARP14")),"ARP_score"])),2)
  else
    D_DLY[D_DLY$ARP==i,]$ARP_score<-unique(D[D$ARP==i,"ARP_score"])
}
## ODP_score
D_DLY$ODP_score<-0
for(i in unique(D_DLY$ODP)){
  if(i=="ARP10")
    D_DLY[D_DLY$ODP==i,]$ODP_score<-round(mean(unique(D[((D$ODP=="ARP7")|(D$ODP=="ARP11")|(D$ODP=="ARP12")|(D$ODP=="ARP13")|(D$ODP=="ARP14")),"ODP_score"])),2)
  else
    D_DLY[D_DLY$ODP==i,]$ODP_score<-unique(D[D$ODP==i,"ODP_score"])
}
## FLO_score
# 항공사 M 은 F,H,I,L 항공사가 운행횟수가 적어서 이거의 지연율 평균으로
D_DLY$FLO_score<-0
for(i in unique(D_DLY$FLO)){
  if(i=="M")
    D_DLY[D_DLY$FLO==i,]$FLO_score<-round(mean(unique(D[((D$FLO=="F")|(D$FLO=="H")|(D$FLO=="I")|(D$FLO=="L")),"FLO_score"])),2)
  else
    D_DLY[D_DLY$FLO==i,]$FLO_score<-unique(D[D$FLO==i,"FLO_score"])
}
## flo_reg_n
# M 항공사가 가지고 있는 항공기 갯수는 3개
D_DLY$flo_reg_n<-0
for(i in unique(D_DLY$FLO)){
  if(i=="M")
    D_DLY[D_DLY$FLO==i,]$flo_reg_n<-3
  else
    D_DLY[D_DLY$FLO==i,]$flo_reg_n<-unique(D[D$FLO==i,"flo_reg_n"])
}
## FLT_rank

# M1361 => 9:30~10:00 제주 가는거
# M1363 => 16:50~16:55 제주가는거
# M1351 => 13:40~14:00 부산(김해)가는거
# M1352 => 15:00~16:30 부산(김해) 에서 양양으로 돌아오는거

#### 요일 까진 안나눔
# M1361과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => C rank
D %>% filter((STT3==10)&(ODP=="ARP3")&(AOD=="D")|((STT3==11)|(STT3==12))&(ARP=="ARP3")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n())->
# M1363과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => c rank
D %>% filter((STT3==17)&(ODP=="ARP3")&(AOD=="D")|(STT3==19)&(ARP=="ARP3")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n()) 
# M1351과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => E rank
D %>% filter((STT3==14)&(ODP=="ARP2")&(AOD=="D")|((STT3==15)|(STT3==16))&(ARP=="ARP2")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n()) 
# M1352과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => E rank
D %>% filter((STT3==15)|(STT3==16)&(ARP=="ARP2")&(AOD=="D")|((STT3==17)|(STT3==18))&(ODP=="ARP2")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n()) 
# 편명 랭크 넣기~~~
D_DLY$FLT_rank<-0
for(i in unique(D_DLY$FLT)){
  if(substr(i,1,4)=="M136")
    D_DLY[D_DLY$FLT==i,]$FLT_rank<-"C"
  else if(substr(i,1,4)=="M135")
    D_DLY[D_DLY$FLT==i,]$FLT_rank<-"E"
  else{
    for(j in unique(D_DLY[D_DLY$FLT==i,"SDT_DY"])){
      if(length(unique(D[((D$FLT==i)&(D$SDT_DY==j)),"FLT_rank"]))==0)
        print(i)
      else
        D_DLY[((D_DLY$FLT==i)&(D_DLY$SDT_DY==j)),"FLT_rank"]<-unique(D[((D$FLT==i)&(D$SDT_DY==j)),"FLT_rank"])
    }
  }
}
# J1809 편명이 D_DLY 에는 있지만 D엔 없으므로 그와 비슷한 값들로 RANK를 넣어줌
D_DLY[(D_DLY$FLT=="J1809"),]
D[((D$FLO=="J")&(D$ARP=="ARP3")&(D$ODP=="ARP4")&(D$STT3==13)),] %>% group_by(FLT_rank) %>% summarise(n=n()) # => D rank
D_DLY[(D_DLY$FLT=="J1809"),"FLT_rank"]<-"D"

# ARP_ODP_score
## 양양으로 왓다갓다가 적으니 노선의 복잡도로 인한 지연은 거의 없을거니 m 항공사로 인한 추가 노선들은 가장 낮은 노선스코어를 넣어줌
D_DLY$ARP_ODP_score<-0
D_DLY$ARP_ODP<-paste(D_DLY$ARP,D_DLY$ODP)
for(i in unique(D_DLY$ARP_ODP)){
  if((i=="ARP3 ARP10")|(i=="ARP10 ARP3")|(i=="ARP2 ARP10")|(i=="ARP10 ARP2"))
    D_DLY[D_DLY$ARP_ODP==i,]$ARP_ODP_score<-min(D$ARP_ODP_score)
  else
    D_DLY[D_DLY$ARP_ODP==i,]$ARP_ODP_score<-unique(D[D$ARP_ODP==i,]$ARP_ODP_score)
}

# STT_score
D_DLY$STT_score<-0
for(i in unique(D_DLY$STT3)){
  D_DLY[D_DLY$STT3==i,]$STT_score<-unique(D[D$STT3==i,]$STT_score)
}

# wea = 0
## 재해가 일어날만한 일이 없기 때문에
D_DLY$wea=0
# Season_score
D_DLY %>% mutate(Season = ifelse(SDT_MM<=2,"Winter",
                                 ifelse(SDT_MM <= 5,"Spring",
                                        ifelse(SDT_MM <= 8,"Summer",
                                               ifelse(SDT_MM <= 11,"fall","Winter"))))) -> D_DLY
D_DLY$Season_score<-0
for(i in unique(D_DLY$Season)){
  D_DLY[D_DLY$Season==i,]$Season_score<-unique(A[A$Season==i,]$Season_score)
}

# AM_PM
D_DLY %>% mutate(AM_PM = ifelse(as.numeric(STT2)<=12,0,1)) -> D_DLY
str(D_DLY)

# 

temp3 <- model.matrix(~FLT_rank,data=D_DLY)[,-1]
D_DLY$FLT_rankA<-as.numeric(D_DLY$FLT_rank=="A")
D_DLY <- data.frame(D_DLY,temp3)

# 2. A

## YY_score
A_DLY$YY_score<-0
for(i in unique(A_DLY$SDT_YY)){
  A_DLY[A_DLY$SDT_YY==i,]$YY_score<-unique(A[A$SDT_YY==i,"YY_score"])
}
## MM_score
A_DLY$MM_score<-0
for(i in unique(A_DLY$SDT_MM)){
  A_DLY[A_DLY$SDT_MM==i,]$MM_score<-unique(A[A$SDT_MM==i,"MM_score"])
}
## DY_score
A_DLY$DY_score<-0
for(i in unique(A_DLY$SDT_DY)){
  A_DLY[A_DLY$SDT_DY==i,]$DY_score<-unique(A[A$SDT_DY==i,"DY_score"])
}
## ARP_score
# ARP 10 이랑 예측 데이터(ARSNT_DLY)의 ROW 수(운행횟수?)가 가장 비슷한 7,11,12,13,14
A_DLY$ARP_score<-0
for(i in unique(A_DLY$ARP)){
  if(i=="ARP10")
    A_DLY[A_DLY$ARP==i,]$ARP_score<-round(mean(unique(A[((A$ARP=="ARP7")|(A$ARP=="ARP11")|(A$ARP=="ARP12")|(A$ARP=="ARP13")|(A$ARP=="ARP14")),"ARP_score"])),2)
  else
    A_DLY[A_DLY$ARP==i,]$ARP_score<-unique(A[A$ARP==i,"ARP_score"])
}
## ODP_score
A_DLY$ODP_score<-0
for(i in unique(A_DLY$ODP)){
  if(i=="ARP10")
    A_DLY[A_DLY$ODP==i,]$ODP_score<-round(mean(unique(A[((A$ODP=="ARP7")|(A$ODP=="ARP11")|(A$ODP=="ARP12")|(A$ODP=="ARP13")|(A$ODP=="ARP14")),"ODP_score"])),2)
  else
    A_DLY[A_DLY$ODP==i,]$ODP_score<-unique(A[A$ODP==i,"ODP_score"])
}
## FLO_score
A_DLY$FLO_score<-0
for(i in unique(A_DLY$FLO)){
  if(i=="M")
    A_DLY[A_DLY$FLO==i,]$FLO_score<-round(mean(unique(A[((A$FLO=="F")|(A$FLO=="H")|(A$FLO=="I")|(A$FLO=="L")),"FLO_score"])),2)
  else
    A_DLY[A_DLY$FLO==i,]$FLO_score<-unique(A[A$FLO==i,"FLO_score"])
}
## flo_reg_n
# M 항공사가 가지고 있는 항공기 갯수는 3개
A_DLY$flo_reg_n<-0
for(i in unique(A_DLY$FLO)){
  if(i=="M")
    A_DLY[A_DLY$FLO==i,]$flo_reg_n<-3
  else
    A_DLY[A_DLY$FLO==i,]$flo_reg_n<-unique(A[A$FLO==i,"flo_reg_n"])
}
## FLT_rank

# M1361 => 9:30~10:00 제주 가는거
# M1363 => 16:50~16:55 제주가는거
# M1351 => 13:40~14:00 부산(김해)가는거
# M1352 => 15:00~16:30 부산(김해) 에서 양양으로 돌아오는거

#### 요일 까진 안나눔
# M1361과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => D rank
A %>% filter((STT3==10)&(ODP=="ARP3")&(AOD=="D")|((STT3==11)|(STT3==12))&(ARP=="ARP3")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n())
# M1363과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => E rank
A %>% filter((STT3==17)&(ODP=="ARP3")&(AOD=="D")|(STT3==19)&(ARP=="ARP3")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n()) 
# M1351과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => E rank
A %>% filter((STT3==14)&(ODP=="ARP2")&(AOD=="D")|((STT3==15)|(STT3==16))&(ARP=="ARP2")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n()) 
# M1352과 비슷한 편명 찾기(시간과 도착지가 같은걸 찾아서 그것들의 FLT_rank 구도를 보고 가장 많이 나온 rank 이용) => D rank
A %>% filter((STT3==15)|(STT3==16)&(ARP=="ARP2")&(AOD=="D")|((STT3==17)|(STT3==18))&(ODP=="ARP2")&(AOD=="A")) %>% group_by(FLT_rank) %>% summarise(n=n()) 
# 편명 랭크 넣기~~~
A_DLY$FLT_rank<-0
for(i in unique(A_DLY$FLT)){
  if(i=="M1361")
    A_DLY[A_DLY$FLT==i,]$FLT_rank<-"D"
  else if(i=="M1363")
    A_DLY[A_DLY$FLT==i,]$FLT_rank<-"E"
  else if(i=="M1351")
    A_DLY[A_DLY$FLT==i,]$FLT_rank<-"E"
  else if(i=="M1352")
    A_DLY[A_DLY$FLT==i,]$FLT_rank<-"D"
  else{
    for(j in unique(A_DLY[A_DLY$FLT==i,"SDT_DY"])){
      if(length(unique(A[((A$FLT==i)&(A$SDT_DY==j)),"FLT_rank"]))==0)
        print(i)
      else
        A_DLY[((A_DLY$FLT==i)&(A_DLY$SDT_DY==j)),"FLT_rank"]<-unique(A[((A$FLT==i)&(A$SDT_DY==j)),"FLT_rank"])
    }
  }
}

# J1809 편명이 D_DLY 에는 있지만 D엔 없으므로 그와 비슷한 값들로 RANK를 넣어줌
A_DLY[(A_DLY$FLT=="J1809"),]
A[((A$FLO=="J")&(A$ARP=="ARP4")&(A$ODP=="ARP3")&(A$STT3==14)),] %>% group_by(FLT_rank) %>% summarise(n=n()) # => D rank
A_DLY[(A_DLY$FLT=="J1809"),"FLT_rank"]<-"C"

# ARP_ODP_score
A_DLY$ARP_ODP_score<-0
A_DLY$ARP_ODP<-paste(A_DLY$ARP,A_DLY$ODP)
for(i in unique(A_DLY$ARP_ODP)){
  if((i=="ARP3 ARP10")|(i=="ARP10 ARP3")|(i=="ARP2 ARP10")|(i=="ARP10 ARP2"))
    A_DLY[A_DLY$ARP_ODP==i,]$ARP_ODP_score<-min(A$ARP_ODP_score)
  else
    A_DLY[A_DLY$ARP_ODP==i,]$ARP_ODP_score<-unique(A[A$ARP_ODP==i,]$ARP_ODP_score)
}

# STT_score
A_DLY$STT_score<-0
for(i in unique(A_DLY$STT3)){
  A_DLY[A_DLY$STT3==i,]$STT_score<-unique(A[A$STT3==i,]$STT_score)
}

# wea = 0
## 재해가 일어날만한 일이 없기 때문에
A_DLY$wea=0
# Season_score
A_DLY %>% mutate(Season = ifelse(SDT_MM<=2,"Winter",
                                 ifelse(SDT_MM <= 5,"Spring",
                                        ifelse(SDT_MM <= 8,"Summer",
                                               ifelse(SDT_MM <= 11,"fall","Winter"))))) -> A_DLY
A_DLY$Season_score<-0
for(i in unique(A_DLY$Season)){
  A_DLY[A_DLY$Season==i,]$Season_score<-unique(A[A$Season==i,]$Season_score)
}

# AM_PM
A_DLY %>% mutate(AM_PM = ifelse(as.numeric(STT2)<=12,0,1)) -> A_DLY

#

temp3 <- model.matrix(~FLT_rank,data=A_DLY)[,-1]
A_DLY$FLT_rankA<-as.numeric(A_DLY$FLT_rank=="A")
A_DLY <- data.frame(A_DLY,temp3)


D %>% group_by(SDT_MM) %>% summarise(d=mean(dly)) -> D_1
A %>% group_by(SDT_MM) %>% summarise(d=mean(dly)) -> A_1
bar_d_dly<-barplot(D_1$d,names.arg=D_1$SDT_MM,main="출발 월별 지연율",col=rainbow(12))
barplot(D_1$d,names.arg=D_1$SDT_MM,main="출발 월별 지연율",col=rainbow(12))

barplot(x_dly_2$연,names.arg=x_dly_2$지연사유,main=paste(i,"월 지연사유",sep=""),col=rainbow(length(x_dly_2$연)),cex.main = 3)
text(x=bar_x_dly,y=x_dly_2$연,labels=x_dly_2$연,pos=3,col="black",cex=2)

barplot()



