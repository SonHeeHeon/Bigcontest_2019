AFSNT_DLY

{
  AFSNT_DLY$STT_hour <- sapply(strsplit(as.character(AFSNT_DLY$STT),":"),"[",1)
  AFSNT_DLY$STT_minute <- sapply(strsplit(as.character(AFSNT_DLY$STT),":"),"[",2)
  AFSNT_DLY$STT2 <- as.numeric(AFSNT_DLY$STT_hour) + as.numeric(AFSNT_DLY$STT_minute)/60
  # 계획 시간을 반올림 한 변수 
  AFSNT_DLY$STT3 <- as.factor(round(AFSNT_DLY$STT2))
  AFSNT_DLY$date <- ymd(paste(AFSNT_DLY$SDT_YY,AFSNT_DLY$SDT_MM,AFSNT_DLY$SDT_DD))
}

AFSNT_DLY

# ARP_STT3_n
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

AFSNT_DLY

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

D_DLY

## MM_score
D_DLY$MM_score<-0
for(i in unique(D_DLY$SDT_MM)){
  D_DLY[D_DLY$SDT_MM==i,]$MM_score<-unique(D[D$SDT_MM==i,"MM_score"])
}
D_DLY

## DY_score
D_DLY$DY_score<-0
for(i in unique(D_DLY$SDT_DY)){
  D_DLY[D_DLY$SDT_DY==i,]$DY_score<-unique(D[D$SDT_DY==i,"DY_score"])
}
D_DLY

## ARP_score
# ARP 10 이랑 예측 데이터(ARSNT_DLY)의 ROW 수(운행횟수?)가 가장 비슷한 7,11,12,13,14
D_DLY$ARP_score<-0
for(i in unique(D_DLY$ARP)){
  if(i=="ARP10")
    D_DLY[D_DLY$ARP==i,]$ARP_score<-round(mean(unique(D[((D$ARP=="ARP7")|(D$ARP=="ARP11")|(D$ARP=="ARP12")|(D$ARP=="ARP13")|(D$ARP=="ARP14")),"ARP_score"])),2)
  else
    D_DLY[D_DLY$ARP==i,]$ARP_score<-unique(D[D$ARP==i,"ARP_score"])
}

D_DLY$ARP_score
sum(is.na(D_DLY$ARP_score))

## ODP_score
D_DLY$ODP_score<-0
for(i in unique(D_DLY$ODP)){
  if(i=="ARP10")
    D_DLY[D_DLY$ODP==i,]$ODP_score<-round(mean(unique(D[((D$ODP=="ARP7")|(D$ODP=="ARP11")|(D$ODP=="ARP12")|(D$ODP=="ARP13")|(D$ODP=="ARP14")),"ODP_score"])),2)
  else
    D_DLY[D_DLY$ODP==i,]$ODP_score<-unique(D[D$ODP==i,"ODP_score"])
}
D_DLY$ODP_score
sum(is.na(D_DLY$ODP_score))

## FLO_score
# 항공사 M 은 F,H,I,L 항공사가 운행횟수가 적어서 이거의 지연율 평균으로
D_DLY$FLO_score<-0
for(i in unique(D_DLY$FLO)){
  if(i=="M")
    D_DLY[D_DLY$FLO==i,]$FLO_score<-round(mean(unique(D[((D$FLO=="F")|(D$FLO=="H")|(D$FLO=="I")|(D$FLO=="L")),"FLO_score"])),2)
  else
    D_DLY[D_DLY$FLO==i,]$FLO_score<-unique(D[D$FLO==i,"FLO_score"])
}
D_DLY$FLO_score
sum(is.na(D_DLY$FLO_score))


## flo_reg_n
# M 항공사가 가지고 있는 항공기 갯수는 3개
D_DLY$flo_reg_n<-0
for(i in unique(D_DLY$FLO)){
  if(i=="M")
    D_DLY[D_DLY$FLO==i,]$flo_reg_n<-3
  else
    D_DLY[D_DLY$FLO==i,]$flo_reg_n<-unique(D[D$FLO==i,"flo_reg_n"])
}
D_DLY$flo_reg_n
sum(is.na(D_DLY$flo_reg_n))



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

D_DLY$FLT_rank
unique(D_DLY$FLT_rank)


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
D_DLY$ARP_ODP_score
unique(D_DLY$ARP_ODP_score)


# STT_score
D_DLY$STT_score<-0
for(i in unique(D_DLY$STT3)){
  D_DLY[D_DLY$STT3==i,]$STT_score<-unique(D[D$STT3==i,]$STT_score)
}
D_DLY$STT_score
unique(D_DLY$STT_score)

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
  D_DLY[D_DLY$Season==i,]$Season_score<-unique(D[D$Season==i,]$Season_score)
}


# AM_PM
D_DLY %>% mutate(AM_PM = ifelse(as.numeric(STT2)<=12,0,1)) -> D_DLY
str(D_DLY)

# 

temp3 <- model.matrix(~0+FLT_rank,data=D_DLY)
D_DLY <- data.frame(D_DLY,temp3)


# D 데이터로 XGboost 학습하기 

D_model_data <- D[,c("dly","ARP_STT3_n","delay_time","wea",
                     "STT_score","FLO_score","FLT_rankA","FLT_rankB",
                     "FLT_rankC","FLT_rankD","FLT_rankE","YY_score",
                     "MM_score","DY_score","ARP_score","ODP_score",
                     "ARP_ODP_score","flo_reg_n","Season_score",
                     "AM_PM")]

{
  set.seed(1)                       ## seed 추가 
  x<-createFolds(D_model_data$dly,k=5)
  test_D<-list()
  train_D<-list()
  for(i in 1:5){
    test_D[[i]]<-D_model_data[x[[i]],]
    train_D[[i]]<-D_model_data[-x[[i]],]
  }
}


i = 3
train<-train_D[[i]]

##

train_x_num <- train %>% select(-delay_time,-dly) %>% data.matrix
train_y_num <- train$delay_time

cv_model <- xgboost(data=train_x_num,label=train_y_num,
                    nfold = 5,nrounds = 200,early_stopping_rounds = 150,
                    objective='reg:linear', verbose = F,prediction = T)


train$p_delay_time <- predict(cv_model,train_x_num)

##

train$delay_time <- NULL
train <-list(data= as(as.matrix(train[,-1]),"dgCMatrix"),label=train$dly)


params=list(eta = 0.1, gamma = 2)

model = xgboost(data=train$data, label= train$label,
                nrounds = 200, early_stopping_rounds = 150,
                objective = "binary:logistic",verbose = F,
                params = params)


D_DLY_model_data <- D_DLY[,c("ARP_STT3_n","wea",
                     "STT_score","FLO_score","FLT_rankA","FLT_rankB",
                     "FLT_rankC","FLT_rankD","FLT_rankE","YY_score",
                     "MM_score","DY_score","ARP_score","ODP_score",
                     "ARP_ODP_score","flo_reg_n","Season_score",
                     "AM_PM")]

D_DLY_x_num <- D_DLY_model_data %>%  data.matrix

D_DLY_model_data$p_delay_time <-  predict(cv_model,D_DLY_x_num)


D_DLY_model_data <-list(data= as(as.matrix(D_DLY_model_data),"dgCMatrix"))


D_DLY$DLY_RATE <- predict(model, newdata=D_DLY_model_data$data, type="prob")


write.csv(D_DLY,"F:/빅콘 코드/D_DLY.csv")



# 2. A

## YY_score
A_DLY$YY_score<-0
for(i in unique(A_DLY$SDT_YY)){
  A_DLY[A_DLY$SDT_YY==i,]$YY_score<-unique(A[A$SDT_YY==i,"YY_score"])
}
A_DLY$YY_score

## MM_score
A_DLY$MM_score<-0
for(i in unique(A_DLY$SDT_MM)){
  A_DLY[A_DLY$SDT_MM==i,]$MM_score<-unique(A[A$SDT_MM==i,"MM_score"])
}
A_DLY$MM_score

## DY_score
A_DLY$DY_score<-0
for(i in unique(A_DLY$SDT_DY)){
  A_DLY[A_DLY$SDT_DY==i,]$DY_score<-unique(A[A$SDT_DY==i,"DY_score"])
}
A_DLY$DY_score

## ARP_score
# ARP 10 이랑 예측 데이터(ARSNT_DLY)의 ROW 수(운행횟수?)가 가장 비슷한 7,11,12,13,14
A_DLY$ARP_score<-0
for(i in unique(A_DLY$ARP)){
  if(i=="ARP10")
    A_DLY[A_DLY$ARP==i,]$ARP_score<-round(mean(unique(A[((A$ARP=="ARP7")|(A$ARP=="ARP11")|(A$ARP=="ARP12")|(A$ARP=="ARP13")|(A$ARP=="ARP14")),"ARP_score"])),2)
  else
    A_DLY[A_DLY$ARP==i,]$ARP_score<-unique(A[A$ARP==i,"ARP_score"])
}
unique(A_DLY$ARP_score)

## ODP_score
A_DLY$ODP_score<-0
for(i in unique(A_DLY$ODP)){
  if(i=="ARP10")
    A_DLY[A_DLY$ODP==i,]$ODP_score<-round(mean(unique(A[((A$ODP=="ARP7")|(A$ODP=="ARP11")|(A$ODP=="ARP12")|(A$ODP=="ARP13")|(A$ODP=="ARP14")),"ODP_score"])),2)
  else
    A_DLY[A_DLY$ODP==i,]$ODP_score<-unique(A[A$ODP==i,"ODP_score"])
}
unique(A_DLY$ODP_score)

## FLO_score
A_DLY$FLO_score<-0
for(i in unique(A_DLY$FLO)){
  if(i=="M")
    A_DLY[A_DLY$FLO==i,]$FLO_score<-round(mean(unique(A[((A$FLO=="F")|(A$FLO=="H")|(A$FLO=="I")|(A$FLO=="L")),"FLO_score"])),2)
  else
    A_DLY[A_DLY$FLO==i,]$FLO_score<-unique(A[A$FLO==i,"FLO_score"])
}
unique(A_DLY$FLO_score)


## flo_reg_n
# M 항공사가 가지고 있는 항공기 갯수는 3개
A_DLY$flo_reg_n<-0
for(i in unique(A_DLY$FLO)){
  if(i=="M")
    A_DLY[A_DLY$FLO==i,]$flo_reg_n<-3
  else
    A_DLY[A_DLY$FLO==i,]$flo_reg_n<-unique(A[A$FLO==i,"flo_reg_n"])
}
unique(A_DLY$flo_reg_n)


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

unique(A_DLY$FLT_rank)

# ARP_ODP_score
A_DLY$ARP_ODP_score<-0
A_DLY$ARP_ODP<-paste(A_DLY$ARP,A_DLY$ODP)
for(i in unique(A_DLY$ARP_ODP)){
  if((i=="ARP3 ARP10")|(i=="ARP10 ARP3")|(i=="ARP2 ARP10")|(i=="ARP10 ARP2"))
    A_DLY[A_DLY$ARP_ODP==i,]$ARP_ODP_score<-min(A$ARP_ODP_score)
  else
    A_DLY[A_DLY$ARP_ODP==i,]$ARP_ODP_score<-unique(A[A$ARP_ODP==i,]$ARP_ODP_score)
}
A_DLY$ARP_ODP[A_DLY$ARP_ODP_score==0]
unique(A_DLY$ARP_ODP_score)


# STT_score
A_DLY$STT_score<-0
for(i in unique(A_DLY$STT3)){
  A_DLY[A_DLY$STT3==i,]$STT_score<-unique(A[A$STT3==i,]$STT_score)
}
unique(A_DLY$STT_score)


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

temp3 <- model.matrix(~0+FLT_rank,data=A_DLY)
A_DLY <- data.frame(A_DLY,temp3)


## A 데이터로 학습 시키기 

A_data1  <- A[,c("dly","date","FLT","ARP_STT3_n","delay_time",
                     "wea","STT_score","FLO_score","FLT_rankA",
                     "FLT_rankB","FLT_rankC","FLT_rankD","FLT_rankE",
                     "YY_score","MM_score","DY_score","ARP_score",
                     "ODP_score","ARP_ODP_score","flo_reg_n",
                     "Season_score","AM_PM")]


i=3
# xgboost로 학습시키기 

# parameter
params=list(eta = 0.15, gamma = 4)


# D를 이용하여 모델을 학습 시킨뒤 D_DLY의 p_delay_time을 예측하고 , 
# A_DLY의 편명, 날짜와 매칭하여 넣어준다 . 

D_data1 <- D[,c("dly","ARP_STT3_n","delay_time",
                              "wea","STT_score","FLO_score","FLT_rankA",
                              "FLT_rankB","FLT_rankC","FLT_rankD",
                              "FLT_rankE","YY_score","MM_score",
                              "DY_score","ARP_score","ODP_score",
                              "ARP_ODP_score","flo_reg_n","Season_score",
                              "AM_PM")]




  
D_train_x_num <- D_data1 %>% select(-delay_time,-dly) %>% data.matrix
D_train_y_num <- D_data1$delay_time
  
cv_model <- xgboost(data=D_train_x_num,label=D_train_y_num,
                      nfold = 5,nrounds = 200,early_stopping_rounds = 150,
                      objective='reg:linear', verbose = F,prediction = T)

D_data2 <- D_DLY[,c("ARP_STT3_n",
                        "wea","STT_score","FLO_score","FLT_rankA",
                        "FLT_rankB","FLT_rankC","FLT_rankD",
                        "FLT_rankE","YY_score","MM_score",
                        "DY_score","ARP_score","ODP_score",
                        "ARP_ODP_score","flo_reg_n","Season_score",
                        "AM_PM")]


D_data3 <- D_data2  %>% data.matrix

D_data2$p_delay_time <- round(predict(cv_model,D_data3),2)

tempdf <- data.frame(temp=paste(D_DLY$date,D_DLY$FLT),p_delay_time= D_DLY_for_A$p_delay_time)

for ( j in unique(tempdf$p_delay_time)){
    tempinfo <- tempdf$temp[D_DLY_for_A$p_delay_time == j]
    A_DLY$p_delay_time[paste(A_DLY$date,A_DLY$FLT) %in% tempinfo] <- j
  }

sum(is.na(A_DLY$p_delay_time))



A_data1  <- A[,c("dly","ARP_STT3_n","delay_time",
                 "wea","STT_score","FLO_score","FLT_rankA",
                 "FLT_rankB","FLT_rankC","FLT_rankD","FLT_rankE",
                 "YY_score","MM_score","DY_score","ARP_score",
                 "ODP_score","ARP_ODP_score","flo_reg_n",
                 "Season_score","AM_PM")]

train_x_num <- A_data1 %>% select(-delay_time,-dly) %>% data.matrix
train_y_num <- A_data1$delay_time

cv_model2 <- xgboost(data=train_x_num,label=train_y_num,
                    nfold = 5,nrounds = 200,early_stopping_rounds = 150,
                    objective='reg:linear', verbose = F,prediction = T)


A_data1$p_delay_time <- predict(cv_model,train_x_num)

A_data1$date <- A_data1$FLT <- A_data1$delay_time <- NULL 

A_data1 <-list(data= as(as.matrix(A_data1[,-1]),"dgCMatrix"),label=A_data1$dly)

params=list(eta = 0.15, gamma = 4)
model = xgboost(data=A_data1$data, label= A_data1$label,
                nrounds = 200, early_stopping_rounds = 150,
                objective = "binary:logistic",verbose = F,
                params = params)
colnames(A_data1$data)

A_DLY_model_data <- A_DLY[,c("ARP_STT3_n","wea",
                             "STT_score","FLO_score","FLT_rankA","FLT_rankB",
                             "FLT_rankC","FLT_rankD","FLT_rankE","YY_score",
                             "MM_score","DY_score","ARP_score","ODP_score",
                             "ARP_ODP_score","flo_reg_n","Season_score",
                             "AM_PM","p_delay_time")]


A_DLY_x_num <- A_DLY_model_data %>%  data.matrix



A_DLY_model_data <-list(data= as(as.matrix(A_DLY_model_data),"dgCMatrix"))


A_DLY$DLY_RATE <- predict(model, newdata=A_DLY_model_data$data, type="prob")

write.csv(A_DLY,"F:/빅콘 코드/A_DLY.csv")

p <- predict(model, newdata=test$data, type="prob")
pr <- prediction(p, test$label)
auc <- performance(pr, measure = "auc")
xgb_aucscores[i]<-auc@y.values[[1]]
print(Sys.time()-start)
  

