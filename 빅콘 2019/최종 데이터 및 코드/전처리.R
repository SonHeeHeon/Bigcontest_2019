library(dplyr)
dat1<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT_korea.csv",header=T)
codename<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/지연사유 코드명.csv",header=T)

dat1 %>%
  group_by(부정기편) %>% 
  summarize(sumed=mean(dly))   # 부정기편 지연율

str(dat1)
head(dat1)
View(dat1)
apply(is.na(dat1), 2, sum)
## na의 갯수를 측정할수 없다. => 빈칸을 na로 보지 않음
nrow(filter(dat1,지연여부=="Y"))               # 지연한 횟수 118937 (약 12만)
nrow(filter(dat1,결항여부=="Y"))               # 결항한 횟수 8259
nrow(filter(dat1,지연여부=="Y"))/nrow(dat1)    # 지연여부 yes 인게 전체 데이터의 12%를 차지()
nrow(filter(dat1,결항여부=="Y"))/nrow(dat1)    # 결항여부 yes 인게 전체 데이터의 0.8%를 차지
nrow(filter(dat1,결항여부=="Y" & 월==9))/nrow(filter(dat1,월==9))  # 9월 데이터중 0.3%가 결항 
'''
sum(filter(dat1,지연여부=="Y")$지연사유=="")   # 지연인데 지연사유가 없는 경우 => 7개(결측치)
sum(filter(dat1,결항여부=="Y")$결항사유=="")   # 결항인데 결항사유가 없는 경우 => 5278개(결측치)
'''
dat2<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT_korea.csv",header=T,na.strings='')
apply(is.na(dat2), 2, sum)
dat3<-select(dat2,연:실제시각)
data.frame('na'=apply(is.na(dat3), 2, sum))
na_note<-data.frame('등록기호'=filter(data.frame('na'=apply(is.na(dat3), 2, sum)),na!=0)[1,],'실제시각'=filter(data.frame('na'=apply(is.na(dat3), 2, sum)),na!=0)[2,],
                    '지연사유'=sum(filter(dat1,지연여부=="Y")$지연사유==""),'결항사유'=sum(filter(dat1,결항여부=="Y")$결항사유==""))
t(as.matrix(na_note))                          # 결측치들 정리(등록기호)
'''
### 결항이라 실제시각이 결측치인것들이 대부분 but 아닌것들도 있음
dat2 %>% 
  filter(is.na(실제시각)) %>% 
  group_by(결항여부) %>% 
  summarize(summed=NROW(연))                   # 이중에 2개는 지연이다.
### 이 결과를 빗댄 결측값을 다시 정리해보면 
na_note[2]=na_note[2]-8233
t(as.matrix(na_note))                          # 결측치들 정리(등록기호/실제시각/지연사유/결항사유)-수정
## 등록기호 결측값들은 전부 실제시각이 없는 데이터임 (그렇다고 다 결항은 아니다) 
dat2 %>% 
  filter(is.na(실제시각)) %>% 
  summarize(summed=sum(is.na(등록기호)))       # 458개 => 등록기호 결측값 총개수
dat2 %>% 
  filter(is.na(실제시각)) %>% 
  group_by(결항여부) %>% 
  summarize(summed=sum(is.na(등록기호)))       # n : 4개는 실제시각은 없는데 결항은 아닌 데이터에 등록기호가 없는 데이터 
'''
dat2 %>% 
  group_by(결항여부) %>% 
  summarize(summed=sum(is.na(등록기호)))       # 결항이 아닌데 등록기호가 없는 4개의 뱅기가 있음

View(filter(dat2[is.na(dat2$등록기호),],결항여부=='N'))
# 4개를 하나하나 들여다 보자 
View(dat2 %>% 
       filter(연==2018 & 월==1 & 편명=="J1124T"))
View(dat2 %>% 
       filter(연==2018 & 월==1 & 편명=="J1125T"))
View(dat2 %>% 
       filter(연==2018 & 월==1 & 편명=="J1126T"))
View(dat2 %>% 
       filter(연==2018 & 월==1 & 편명=="J1127T"))  # 전부 4일/11일이 이상함 + 그외 등등 

# 출도착 쌍이 안맞는거



# 출발도착 둘다 지연인거 아닌거




# 결항을 잘 해결하는게 중요할듯하다 

# 결항

x=aggregate(연~결항사유,dat1,NROW)[-1,]
x                                              # 결항사유에 따른 결항횟수
k<-barplot(x[,2],space=1,names.arg=x[,1],col=rainbow(length(aggregate(연~결항사유,dat1,NROW)[-1,1])))
text(x=k,y=x[,2],labels = x[,2],col='black',cex=1)
str(codename)
rs<-filter(codename,코드 %in% as.character(filter(x,연>100)[,1]))[order(-filter(x,연>100)[,2]),] # 주요 결항 사유(순위별)
cbind(rs,'횟수'=filter(x[order(-x$연),],연>=100)[,2])
## 김포/대구/김해엔 curfew time 이 존재 (7_4째주 공모전정리 노트 참고)
## 강풍/안개/강설/운고 등은 중기예보에서 확인 불가능 => 예측불가
## but 태풍, a/c 접속, a/c 정비 , curfew 등은 가능할듯  

# 지연
x1=aggregate(연~지연사유,dat1,NROW)[-1,]
x1                                              # 지연사유에 따른 지연횟수
x1[order(-x1$연),]
filter(x1[order(-x1$연),],연>=100)
k1<-barplot(x1[,2],space=1,names.arg=x1[,1],col=rainbow(length(aggregate(연~지연사유,dat1,NROW)[-1,1])))
text(x=k1,y=x1[,2],labels = x1[,2],col='black',cex=0.8)
rs1<-filter(codename,코드 %in% as.character(filter(x1,연>100)[,1]))[order(-filter(x1,연>100)[,2]),] # 주요 결항 사유(순위별)
cbind(rs1,'횟수'=filter(x1[order(-x1$연),],연>=100)[,2])
## 결항사유랑은 어느정도 차이가 있다 

# 월별 지연율


tx<-list()
for(i in 1:12){
  xxx=filter(dat1,월==i)
  tx[[i]]<-(aggregate(월~연,filter(xxx,지연여부=="Y"),NROW)/aggregate(월~연,xxx,NROW))$월
  if(i<=6){
    bar_x<-barplot(tx[[i]],names.arg=c(2017,2018,2019),main=paste(i,"월 지연율"))
    text(x=bar_x,y=tx[[i]],labels=tx[[i]],pos=1,col='black')
  }
  else{
    barplot(tx[[i]],names.arg=c(2017,2018),main=paste(i,"월 지연율"))
    text(x=bar_x,y=tx[[i]],labels=tx[[i]],pos=1,col='black')
  }
}
png("월별 년도 지연율.png",width=1000,height=800)
par(mfrow=c(2,1))
xz1<-NULL
k=1
for(i in 1:6){
  for(j in 1:3){
    xz1[k]<-tx[[i]][j]
    k=k+1
  }
}
barplot(matrix(xz1,nrow=3),names.arg = c("1월","2월","3월","4월","5월","6월"),main="월별 년도 지연율",col=c("lightblue","lightgreen","lightyellow"),beside=T,xlab="2017 ~ 2019 년도")
legend("topright",legend=c("2017","2018","2019"),col=c("lightblue","lightgreen","lightyellow"),pch=15,bty="n",y.intersp=0.5)
xy1<-NULL
kt=1
for(i in 7:12){
  for(j in 1:2){
    xy1[kt]<-tx[[i]][j]
    kt=kt+1
  }
}
xy1
barplot(matrix(xy1,nrow=2),names.arg = c("7월","8월","9월","10월","11월","12월"),main="월별 년도 지연율",col=c("lightblue","lightgreen"),beside=T,xlab="2017 ~ 2018 년도")
legend("topright",legend=c("2017","2018"),col=c("lightblue","lightgreen"),pch=15,bty="n",y.intersp=0.5)
dev.off()

# 연도별 지연율
setwd("C:/Desktop/Son/공모전/빅콘 2019")
y_dy<-(aggregate(월~연,filter(dat1,지연여부=="Y"),NROW)/aggregate(월~연,dat1,NROW))$월
png("연도별 지연율.png",width=1000,height=800)
par(mfrow=c(1,1))
bar_y<-barplot(y_dy,names.arg = c(2017,2018,2019),main="연도별 지연율",col=c("lightblue","lightgreen","lightyellow"),xlab="2017 ~ 2019 년도",cex.main=2.5)
text(x=bar_y,y=y_dy,labels=y_dy,pos=1,col="black",cex=2)
dev.off()

# 월별 지연사유
str(codename)
j=0
for(i in 1:12){
  xxx=filter(dat1,월==i)
  x_dly=aggregate(연~지연사유,xxx,NROW)[-1,]
  x_dly_1<-filter(x_dly,연>30)         
  x_dly_2<-x_dly_1[order(-x_dly_1$연),]
  if(i%%4==1){
    j=j+1
    png(paste(i,"-",i+3,"월 지연사유.png",sep=""),width = 1500,height = 1000)
    par(mfrow=c(2,2))
  }
  bar_x_dly<-barplot(x_dly_2$연,names.arg=x_dly_2$지연사유,main=paste(i,"월 지연사유",sep=""),col=rainbow(length(x_dly_2$연)),cex.main = 3)
  text(x=bar_x_dly,y=x_dly_2$연,labels=x_dly_2$연,pos=3,col="black",cex=2)
  legend("topright",legend=filter(codename,코드 %in% x_dly_2$지연사유)[order(-x_dly_1$연),]$코드명,pch=15,bty="n",col=rainbow(length(x_dly_2$연)),cex=2)
  if(i%%4==0){
    dev.off()
  }
}

### 9월엔 날씨로 인한 영향 거의 없음 

# 2017,2018 - 9월 지연사유
for(i in c(2017,2018)){
  xxx=filter(dat1,월==9 & 연==i)
  x_dly=aggregate(연~지연사유,xxx,NROW)[-1,]
  x_dly_1<-filter(x_dly,연>20)         
  x_dly_2<-x_dly_1[order(-x_dly_1$연),]
  if(i%%2017==0){
    png("2017-2018 9월 지연사유.png",width = 1500,height = 1000)
    par(mfrow=c(2,1))
  }
  bar_x_dly<-barplot(x_dly_2$연,names.arg=x_dly_2$지연사유,main=paste(i,"년 9월 지연사유",sep=""),col=rainbow(length(x_dly_2$연)),cex.main = 3)
  text(x=bar_x_dly,y=x_dly_2$연,labels=x_dly_2$연,pos=3,col="black",cex=2)
  legend("topright",legend=filter(codename,코드 %in% x_dly_2$지연사유)[order(-x_dly_1$연),]$코드명,pch=15,bty="n",col=rainbow(length(x_dly_2$연)),cex=2)
  text(x=3,y=2500,labels=paste("총 지연횟수 : ",sum(x_dly_2$연)),cex=3)
  if(i%%2018==0){
    dev.off()
  }
}

## 다른 달들과 달리 9월에 특별히 더 원인이 되는 이유가 있을가? => 9월 지연율 및 지연사유 
dat1_9<-filter(dat1,월==9)
str(dat1_9)
aggregate(월~연,filter(dat1_9,지연여부=="Y"),NROW)/aggregate(월~연,dat1_9,NROW)               # 2017~2018년 지연율 => 평소보다 낮다 

x1=aggregate(연~지연사유,dat1_9,NROW)[-1,]
x1                                              # 지연사유에 따른 지연횟수
x1[order(-x1$연),]
filter(x1[order(-x1$연),],연>=100)
k1<-barplot(x1[,2],space=1,names.arg=x1[,1],col=rainbow(length(aggregate(연~지연사유,dat1,NROW)[-1,1])))
text(x=k1,y=x1[,2],labels = x1[,2],col='black',cex=0.8)

rs2<-filter(codename,코드 %in% as.character(filter(x1,연>100)[,1]))[order(-filter(x1,연>100)[,2]),] # 주요 결항 사유(순위별)
cbind(rs2,'횟수'=filter(x1[order(-x1$연),],연>=100)[,2])

# 출발 도착 간의 시간 차이
for(i in levels())


temp1<-model.matrix(~공항,data=dat1)[,-1]
temp2<-model.matrix(~항공사,data=dat1)[,-1]
temp3<-model.matrix(~출도착,data=dat1)[,-1]
temp4<-model.matrix(~상대공항,data=dat1)[,-1]
temp5<-model.matrix(~지연여부,data=dat1)[,-1]

tempdf<-data.frame(temp2,temp5)
str(tempdf)
tempdf$X.Intercept.<-as.factor(tempdf$X.Intercept.)
tempdf$항공사B<-as.factor(tempdf$항공사B)
tempdf$항공사C<-as.factor(tempdf$항공사C)
tempdf$항공사D<-as.factor(tempdf$항공사D)
tempdf$항공사E<-as.factor(tempdf$항공사E)
tempdf$항공사F<-as.factor(tempdf$항공사F)
tempdf$항공사G<-as.factor(tempdf$항공사G)
tempdf$항공사H<-as.factor(tempdf$항공사H)
tempdf$항공사I<-as.factor(tempdf$항공사I)
tempdf$항공사J<-as.factor(tempdf$항공사J)
tempdf$항공사K<-as.factor(tempdf$항공사K)
tempdf$항공사L<-as.factor(tempdf$항공사L)
tempdf$temp5<-as.factor(tempdf$temp5)
library(tree)
str(dat1)


tree_1<-tree(temp5~.,tempdf)
summary(tree_1)


summary(tree_1) # 훈련오차율 0.15 / 이탈도 굉장히 높음
par(mfrow=c(1,1))
plot(tree_1)
text(tree_1,pretty = 0)
# like , like_o 의 영향이 너무 큼
tree_1 # 분할기준, 그가지의 총 관측치 수 , 이탈도 , 그 가지에 대한 전체 예측값(0,1) , 0~1 관측치 비율 
tree_1_pred<-predict(tree_1,test_1,type="class")
tree_cfm<-confusionMatrix(tree_1_pred,test_1$match)
tree_cfm

tree_er<-1-tree_cfm$overall[[1]] # tree 오분류율
tree_er_1<-55/67 # tree 1이 되는 오분류율
pr <- prediction(as.numeric(tree_1_pred),as.numeric(test_1$match)-1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main='ROC of Test Data') # tree roc plot
tree_auc <- performance(pr, measure = "auc") 
tree_auc <- tree_auc@y.values[[1]]t
tree_auc # tree auc


str(dat1)
temp


model<-glm(temp5~.,family='binomial',data=tempdf)
summary(model)

# Test 데이터

test_1<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT_DLY.csv")

## test 데이터의 편명에 train 데이터의 편명이 없는거 

length(unique(test_1$편명))
sum(unique(test_1$편명) %in% unique(dat1$편명))

unique(test_1$편명)[!(unique(test_1$편명) %in% unique(dat1$편명))]
unique(test_1[test_1$항공사=="M",]$편명)
uniq_1<-unique(test_1$편명)[!(unique(test_1$편명) %in% unique(dat1$편명))]
View(test_1[test_1$편명 %in% uniq_1,])
str(test_1[test_1$편명 %in% uniq_1,])       ### test 데이터의 편명에 train 데이터 편명이 없는거 갯수 => 100개 (m: 96 / j : 4)
### m 항공사 총 96개 있음 / j 항공사중 train 에 없는 편명 4개 데이터 있음











