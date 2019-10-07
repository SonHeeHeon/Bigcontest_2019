# 출발 데이터
dep_d<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/departure.csv")
str(dep_d)
dep_d$노선<-paste(dep_d$공항,dep_d$상대공항,sep="")
dep_d$노선<-as.factor(dep_d$노선)
levels(dep_d$항공사)
# 시즌을 구분짓기 위한 코드
tk<-dep_d %>% 
  group_by(월) %>% 
  summarize(sumed=sum(지연여부=="Y")/NROW(연))
tk_1<-barplot(tk$sumed,names.arg = tk$월,main="월별 지연율",col=rainbow(length(tk$월)))
text(x=tk_1,y=tk$sumed,labels = round(tk$sumed,2),pos=1,col='black')
barplot(x_dly_2$연,names.arg=x_dly_2$지연사유,main=paste(i,"월 지연사유",sep=""),col=rainbow(length(x_dly_2$연)),cex.main = 3)
## 운행횟수가 차이가 나서 시즌을 만들려했는데 지연율이 시즌을 따르진 않아서 시즌은 안하는게 좋을듯 => 월로 대체가 될듯

library(dplyr)
c("연","월","공항","상대공항","편명","노선","등록기호","계획시각","요일","항공사")
# 항공사 숫자
# 계획시각 

{
  for( i in unique(dep_d$연)){
    dep_d_1<-filter(dep_d,연==i)
    dep_d[dep_d$연==i,"연_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  # QQQQQQQQQQQQQQQQQQQ 연평균/ 월평균 / 시즌 평균 등을 집어넣었을때 TEST에선 모든데이터에 같은값만 들어가는데 이는 상관없는가?
  
  for( i in unique(dep_d$월)){
    dep_d_1<-filter(dep_d,월==i)
    dep_d[dep_d$월==i,"월_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  
  for( i in unique(dep_d$공항)){
    dep_d_1<-filter(dep_d,공항==i)
    if(NROW(dep_d_1)<600){
      dep_d[dep_d$공항==i,"공항_dly"] <- NULL
    }
    else
      dep_d[dep_d$공항==i,"공항_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  dep_d_2<-dep_d[is.null(dep_d$공항_dly),]
  dep_d[is.null(dep_d$공항_dly),"공항_dly"]<-NROW(dep_d_2[dep_d_2$지연여부=="Y",])/NROW(dep_d_2)
  
  for( i in unique(dep_d$상대공항)){
    dep_d_1<-filter(dep_d,상대공항==i)
    if(NROW(dep_d_1)<600){
      dep_d[dep_d$상대공항==i,"상대공항_dly"] <- NULL
    }
    else
      dep_d[dep_d$상대공항==i,"상대공항_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  dep_d_2<-dep_d[is.null(dep_d$상대공항_dly),]
  dep_d[is.null(dep_d$상대공항_dly),"상대공항_dly"]<-NROW(dep_d_2[dep_d_2$지연여부=="Y",])/NROW(dep_d_2)
  
  for( i in unique(dep_d$편명)){
    dep_d_1<-filter(dep_d,편명==i)
    dep_d[dep_d$편명==i,"편명_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  for( i in unique(dep_d$노선)){
    dep_d_1<-filter(dep_d,노선==i)
    dep_d[dep_d$노선==i,"노선_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  for( i in unique(dep_d$등록기호)){
    dep_d_1<-filter(dep_d,등록기호==i)
    dep_d[dep_d$등록기호==i,"등록기호_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  for( i in unique(dep_d$계획시각)){
    dep_d_1<-filter(dep_d,계획시각==i)
    dep_d[dep_d$계획시각==i,"계획시각_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  for( i in unique(dep_d$요일)){
    dep_d_1<-filter(dep_d,요일==i)
    dep_d[dep_d$요일==i,"요일_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
  for( i in unique(dep_d$항공사)){
    dep_d_1<-filter(dep_d,항공사==i)
    dep_d[dep_d$항공사==i,"항공사_dly"] <- NROW(dep_d_1[dep_d_1$지연여부=="Y",])/NROW(dep_d_1)
  }
}

## 이과정은 수정이 좀 필요함 => 등록기호나 노선,편명이라던지 계획시간 등 각각의 요소에 적은 데이터들은 하나로 뭉친다던지 해야될듯
# 운행횟수가 600 이하인 것들은 모아서 전체 평균으로 계산하기
hist(dep_d$항공사_dly)
hist(dep_d$요일_dly)
hist(dep_d$계획시각_dly)
hist(dep_d$등록기호_dly)
hist(dep_d$노선_dly)
hist(dep_d$편명_dly)
hist(dep_d$공항_dly)
hist(dep_d$상대공항_dly)

install.packages("car")
library(car)
str(dep_d)
model<-glm(지연여부~연_dly+월_dly+공항_dly+상대공항_dly+편명_dly+노선_dly+등록기호_dly+계획시각_dly+요일_dly+항공사_dly,family='binomial',data=dep_d)
summary(model)  ## 전부 유의하게 작용함
vif(model)      ## 

## 데이터 나누는 것

library(caret)
set.seed(1)
x<-createFolds(dep_d$지연여부,k=5)
x[[1]]
test<-list()
train<-list()

for(i in 1:5){
  test[[i]]<-dep_d[x[[i]],]
  train[[i]]<-dep_d[-x[[i]],]
}
for(i in 1:5){
  write.csv(train[[i]],file=paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/","dep_train_",i,".csv",sep=""),row.names=F)
}
for(i in 1:5){
  write.csv(test[[i]],file=paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/","dep_test_",i,".csv",sep=""),row.names=F)
}

## QQQQQQQQQQQQQQQQQQ 이상치나 이상한 데이터들에 대해선 어떻게 하지?

# glm

library(ROCR)
i=1
train<-read.csv(paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/dep_train_",i,".csv",sep=""),header=T)
test<-read.csv(paste("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/dep_test_",i,".csv",sep=""),header=T)
train<-train[,c(12,17,18,19,20,21,22,23,24,25,26)]
test<-test[,c(12,17,18,19,20,21,22,23,24,25,26)]
model<-glm(지연여부~.,family='binomial',data=train)
# step 은 생략
summary(model)
qchisq(0.95,df=386424)   # 적합도 검정이라는데 뭔지 잘모르겠다 
p <- predict(model, newdata=test, type="response")
length(p)
str(test)
sum(p>=0.5)/length(p)  # 확률이 0.5 이상인 것 2% => threshold를 조절한 필요 있음 

pr <- prediction(p, test$지연여부)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main='ROC of Test Data')
auc <- performance(pr, measure = "auc")
auc@y.values[[1]]   ## 0.72 / 이건 사실 필요없음 => 예측에선 threshold 를 fix 하고 해야하기때문 

glm.pred=rep("N",nrow(test))
glm.pred[p>0.5]="Y"
glm_cfm<-confusionMatrix(as.factor(glm.pred),test$지연여부)
glm_cfm     ## 특이도가 바닥 / QQQQQQQQQ . 특이도 민감도가 바꼈는데 이거 어떻게 해야하지?

points(prf@x.values[[1]][-1][optid],prf@y.values[[1]][-1][optid], col='red', pch=15)
optid<-(1:length(prf@y.values[[1]][-1]))[((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-11])^2)
                                         ==min((prf@x.values[[1]][-1])^2 + (1-prf@y.values[[1]][-1])^2)]

optcut<-prf@alpha.values[[1]][-1][optid]
glm.pred=rep("N",nrow(test))
glm.pred[p>optcut]="Y"
glm_cfm_1<-confusionMatrix(as.factor(glm.pred),test$지연여부)
glm_cfm_1

## 이 과정부터는 불확실성 투성이 => 수정 필요
pr_1 <- prediction(as.numeric(as.factor(glm.pred)),as.numeric(test$지연여부))  # 이부분이 가장 관건 => prediction에 대한 정확한 개념인지가 필요하다 
prf_1 <- performance(pr_1, measure = "tpr", x.measure = "fpr")
plot(prf_1, main='ROC of Test Data') 
glm_cfm_1$overall[[1]] 
glm_cfm_1$table[1,2]/(glm_cfm_1$table[1,2]+glm_cfm_1$table[2,2])  ## y인걸 y로 못맞추는 오류율
auc_1 <- performance(pr_1, measure = "auc")
auc_1@y.values[[1]] 





## 등록기호 누적평균 

library(ggplot2)
library(lubridate)
temp_FLT <- unique(dat1$등록기호)
dat1$DLY2<-ifelse(dat1$지연여부=="N",0,1)
# 편명 요약 작업 
{
  temp1 <- NULL  # 등록기호 담을
  temp2 <- NULL  # 평균 지연율 담을 
  temp3 <- NULL  # 운행횟수 담을 
  temp4 <- NULL  # 출발 도착 담을 
  temp5 <- NULL  # 항공사 담을 
  
  for ( i in 1:length(temp_FLT) ) {
    temp <- filter(dat1, 등록기호==temp_FLT[i]) # 필터
    temp$DATE<-paste(temp$연,temp$월,temp$일,sep="-")
    temp_tot = NULL # 누적 평균 담을 변수 
    for(j in 1:(nrow(temp))){
      temp_tot = c(temp_tot,mean(temp[1:j,"DLY2"]))
    }
    m_val <- temp_tot[length(temp_tot)] # 평균 
    
    # ggplot 그리기
    
    temp_tot <- data.frame("date"=temp$DATE,temp_tot)
    temp_xlab <- paste0("#", temp[1,]$DATE ," ~ ", temp[nrow(temp),]$DATE )
    temptext1 <- unique(paste(temp$계획시각,temp$출도착,sep="-"))
    temptext2 <- paste0("#총 ",nrow(temp),"회 운항")
    ggplot(temp_tot,aes(x=ymd(date),y=temp_tot)) + geom_point() + geom_line() +
    geom_hline(aes(yintercept=m_val),color="red") + ggtitle(temp_FLT[i]) + ylab("누적 평균") +
    xlab(temp_xlab) + annotate("text", x=ymd("20190630") ,  y = m_val+0.02, label = m_val, color = "red" ) +
    annotate("text", x=ymd("20190101") ,  y = 0.02, label = temptext1[1], color = "red" ) +
    annotate("text", x=ymd("20190101") ,  y = 0.011, label = temptext1[2], color = "blue" ) +
    annotate("text", x=ymd("20171101") ,  y = 0.011, label = temptext2, color = "red"  ) +
    theme(plot.title=element_text( size=20, vjust=2, color="blue"))
    plotname <- paste0("C:/Desktop/Son/공모전/빅콘 2019/등록기호별 누적평균/",temp_FLT[i],".jpg") 
    ggsave(file=plotname, width=20, height=15, units=c("cm"))
    # 담기
    temp1 <- c(temp1, as.character(temp_FLT[i]))
    temp2 <- c(temp2, as.numeric(m_val))
    temp3 <- c(temp3, as.numeric(nrow(temp)))
    temp4 <- c(temp4, as.character(paste(temp$공항[1],temp$상대공항[1],sep=" - ")))
    temp5 <- c(temp5, as.character(unique(temp$항공사)[1]))
    
    print(i/length(temp_FLT)) # 진행상황 
  }
  temp1
  jm_summary_1 <- data.frame("등록기호"=temp1,"MEAN_DLY"=temp2, "FLIGHT N" = temp3,
                              "D - A"=temp4 , "FLO" = temp5)
}




## 계획시간 누적평균 

library(ggplot2)
library(lubridate)
str(dat1)
temp_FLT <- unique(dat1$계획시각)
dat1$DLY2<-ifelse(dat1$지연여부=="N",0,1)
# 편명 요약 작업 
{
  temp1 <- NULL  # 계획시각 담을
  temp2 <- NULL  # 평균 지연율 담을 
  temp3 <- NULL  # 운행횟수 담을 
  temp4 <- NULL  # 출발 도착 담을 
  temp5 <- NULL  # 항공사 담을 
  
  for ( i in 1:length(temp_FLT) ) {
    temp <- filter(dat1, 계획시각==temp_FLT[i]) # 필터
    temp$DATE<-paste(temp$연,temp$월,temp$일,sep="-")
    temp_tot = NULL # 누적 평균 담을 변수 
    for(j in 1:(nrow(temp))){
      temp_tot = c(temp_tot,mean(temp[1:j,"DLY2"]))
    }
    m_val <- temp_tot[length(temp_tot)] # 평균 
    
    # ggplot 그리기
    
    temp_tot <- data.frame("date"=temp$DATE,temp_tot)
    temp_xlab <- paste0("#", temp[1,]$DATE ," ~ ", temp[nrow(temp),]$DATE )
    temptext1 <- unique(paste(temp$계획시각,temp$출도착,sep="-"))
    temptext2 <- paste0("#총 ",nrow(temp),"회 운항")
    ggplot(temp_tot,aes(x=ymd(date),y=temp_tot)) + geom_point() + geom_line() +
      geom_hline(aes(yintercept=m_val),color="red") + ggtitle(temp_FLT[i]) + ylab("누적 평균") +
      xlab(temp_xlab) + annotate("text", x=ymd("20190630") ,  y = m_val+0.02, label = m_val, color = "red" ) +
      annotate("text", x=ymd("20190101") ,  y = 0.02, label = temptext1[1], color = "red" ) +
      annotate("text", x=ymd("20190101") ,  y = 0.011, label = temptext1[2], color = "blue" ) +
      annotate("text", x=ymd("20171101") ,  y = 0.011, label = temptext2, color = "red"  ) +
      theme(plot.title=element_text( size=20, vjust=2, color="blue"))
    plotname <- paste0("C:/Desktop/Son/공모전/빅콘 2019/계획시각별 누적평균/",strsplit(as.character(temp_FLT[i]),":")[[1]][1],"_",strsplit(as.character(temp_FLT[i]),":")[[1]][2],".jpg") 
    ggsave(file=plotname, width=20, height=15, units=c("cm"))
    # 담기
    temp1 <- c(temp1, as.character(temp_FLT[i]))
    temp2 <- c(temp2, as.numeric(m_val))
    temp3 <- c(temp3, as.numeric(nrow(temp)))
    temp4 <- c(temp4, as.character(paste(temp$공항[1],temp$상대공항[1],sep=" - ")))
    temp5 <- c(temp5, as.character(unique(temp$항공사)[1]))
    
    print(i/length(temp_FLT)) # 진행상황 
  }
  temp1
  jm_summary_1 <- data.frame("계획시각"=temp1,"MEAN_DLY"=temp2, "FLIGHT N" = temp3,
                             "D - A"=temp4 , "FLO" = temp5)
}


## 노선 누적평균 
dat1$노선<-paste(dat1$공항,dat1$상대공항,sep="")
dat1$노선<-as.factor(dat1$노선)
library(dplyr)
library(ggplot2)
library(lubridate)
temp_FLT <- unique(dat1$노선)
dat1$DLY2<-ifelse(dat1$지연여부=="N",0,1)
# 편명 요약 작업 
{
  temp1 <- NULL  # 노선 담을
  temp2 <- NULL  # 평균 지연율 담을 
  temp3 <- NULL  # 운행횟수 담을 
  temp4 <- NULL  # 출발 도착 담을 
  temp5 <- NULL  # 항공사 담을 
  
  for ( i in 1:length(temp_FLT) ) {
    temp <- filter(dat1, 노선==temp_FLT[i]) # 필터
    temp$DATE<-paste(temp$연,temp$월,temp$일,sep="-")
    temp_tot = NULL # 누적 평균 담을 변수 
    for(j in 1:(nrow(temp))){
      temp_tot = c(temp_tot,mean(temp[1:j,"DLY2"]))
    }
    m_val <- temp_tot[length(temp_tot)] # 평균 
    
    # ggplot 그리기
    
    temp_tot <- data.frame("date"=temp$DATE,temp_tot)
    temp_xlab <- paste0("#", temp[1,]$DATE ," ~ ", temp[nrow(temp),]$DATE )
    temptext1 <- unique(paste(temp$노선,temp$출도착,sep="-"))
    temptext2 <- paste0("#총 ",nrow(temp),"회 운항")
    ggplot(temp_tot,aes(x=ymd(date),y=temp_tot)) + geom_point() + geom_line() +
      geom_hline(aes(yintercept=m_val),color="red") + ggtitle(temp_FLT[i]) + ylab("누적 평균") +
      xlab(temp_xlab) + annotate("text", x=ymd("20190630") ,  y = m_val+0.02, label = m_val, color = "red" ) +
      annotate("text", x=ymd("20190101") ,  y = 0.02, label = temptext1[1], color = "red" ) +
      annotate("text", x=ymd("20190101") ,  y = 0.011, label = temptext1[2], color = "blue" ) +
      annotate("text", x=ymd("20171101") ,  y = 0.011, label = temptext2, color = "red"  ) +
      theme(plot.title=element_text( size=20, vjust=2, color="blue"))
    plotname <- paste0("C:/Desktop/Son/공모전/빅콘 2019/노선별 누적평균/",temp_FLT[i],".jpg") 
    ggsave(file=plotname, width=20, height=15, units=c("cm"))
    # 담기
    temp1 <- c(temp1, as.character(temp_FLT[i]))
    temp2 <- c(temp2, as.numeric(m_val))
    temp3 <- c(temp3, as.numeric(nrow(temp)))
    temp4 <- c(temp4, as.character(paste(temp$공항[1],temp$상대공항[1],sep=" - ")))
    temp5 <- c(temp5, as.character(unique(temp$항공사)[1]))
    
    print(i/length(temp_FLT)) # 진행상황 
  }
  temp1
  jm_summary_1 <- data.frame("노선"=temp1,"MEAN_DLY"=temp2, "FLIGHT N" = temp3,
                             "D - A"=temp4 , "FLO" = temp5)
}


