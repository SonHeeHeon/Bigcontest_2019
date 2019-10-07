library(dplyr)
library(lubridate)
afsnt<-read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT.csv")
colnames(afsnt)<-c("연","월","일","요일","공항","상대공항","항공사","편명","등록기호","출도착","부정기편","계획시각","실제시각","지연여부","지연사유","결항여부","결항사유") # 이름 한글로
str(afsnt)

# 불필요 데이터 삭제

afsnt_1<-afsnt[afsnt$부정기편=="N",] # 부정기편 삭제한 이유?
unique(afsnt_1$항공사)
str(afsnt_1)
afsnt_2<-afsnt_1[afsnt_1$결항여부=="N",]  # 결항 데이터 삭제한 이유?
str(afsnt_2)
afsnt_3<-subset(afsnt_2,select=-c(부정기편,결항여부,결항사유))
str(afsnt_3)

afsnt_3$dly<-ifelse(afsnt_3$지연여부=="N",0,1)

afsnt_4<-afsnt_3[!(hour(hm(afsnt_3$계획시각))==0|hour(hm(afsnt_3$계획시각))==1|hour(hm(afsnt_3$계획시각))==23),] # 23~1시 까지는 테스트 데이터 없으므로 삭제 
afsnt_4$계획시간_1<-round(hour(hm(afsnt_4$계획시각))+minute(hm(afsnt_4$계획시각))/60) # 계획시간 : minute은 반올림해서 시간대로 만든 변수 

afsnt_4[afsnt_4$계획시간_1==23,]$계획시간_1 <- 22 # 23시로 반올림 된거 전부 22시로 
dat2_2<-filter(afsnt_4,계획시간_1>=6 & 계획시간_1<=22 )





# 공항지수

{
  A_list <- c("ARP1", "ARP2", "ARP3", "ARP4", "ARP5", "ARP6", "ARP7", "ARP8", "ARP9", "ARP11", "ARP12", "ARP13", "ARP14", "ARP15")
  D_Airport <- round(c(0.490492789,0.059043815,1.901457807,0.234447455,-1.061457822,0.499884787,-0.933675944,0.415792726,-0.778829403,-1.446426786,-0.640284693,1.61445175,-0.843064871,0.48816839),2)
  D_OpAirpot <- round(c(0.895091838,0.659510205,0.433906684,0.617554106,-1.135163783,1.352211494,0.061344167,0.716742095,-0.627710186,-1.627199831,-0.989911886,1.185230948,-0.083459445,-1.458146405),2)
  A_Airport <- round(c(0.568224252,0.646387812,1.203231595,0.830013976,-1.133301587,1.420993399,0.136366327,0.564280006,-0.618207163,-1.508724426,-0.810378922,0.81333406,-0.685555301,-1.426664028),2)
  A_OpAirpot <- round(c(0.675790145,-0.036620667,1.060890968,0.257967196,-1.160094566,1.01390101,-0.82333905,0.520653181,-0.663092847,-1.562263219,-0.627900317,2.097925584,-0.560101089,-0.193716328),2)
  
  dat2_2$공항지수<-0
  dat2_2$상대공항지수<-0
  dat2_2$공항 <- as.character(dat2_2$공항)
  for(i in 1:length(A_list)){
    dat2_2[dat2_2$공항==A_list[i] & dat2_2$출도착=="D",]$공항지수 <- D_Airport[i]
  }
  dat2_2$상대공항 <- as.character(dat2_2$상대공항)
  for(i in 1:length(A_list)){
    dat2_2[dat2_2$상대공항 == A_list[i] & dat2_2$출도착 == "D",]$상대공항지수 <- D_OpAirpot[i]
  }
  for(i in 1:length(A_list)){
    dat2_2[dat2_2$공항 == A_list[i] & dat2_2$출도착 == "A",]$공항지수 <- A_Airport[i]
  }
  for(i in 1:length(A_list)){
    dat2_2[dat2_2$상대공항 == A_list[i] & dat2_2$출도착 == "A",]$상대공항지수 <- A_OpAirpot[i]
  }
  
  dat2_2$상대공항지수 <- as.numeric(dat2_2$상대공항지수)
  dat2_2$공항지수 <- as.numeric(dat2_2$공항지수)
}

# 출도착 데이터 나누기

dat2_3_d<-filter(dat2_2,출도착=="D")
dat2_3_a<-filter(dat2_2,출도착=="A")
str(dat2_3_d)
################ 출발 데이터 지수 

# 노선지수
{
  line=paste(dat2_3_d$공항,dat2_3_d$상대공항,sep="_")
  linedelay=as.data.frame(line)
  linedelay=cbind(linedelay,DLY=dat2_3_d$지연여부)
  delaysum=linedelay %>% group_by(line) %>% summarise(delay=length(DLY))
  sort(delaysum$delay,decreasing=T)
  plot(sort(delaysum$delay,decreasing=T))
  abline(h=c(10000,25000,100000))
  dat2_3_d[,"line"]=paste(dat2_3_d$공항,dat2_3_d$상대공항,sep="_")
  dat2_3_d=left_join(dat2_3_d,delaysum,by="line")
  dat2_3_d$노선지수=ifelse(dat2_3_d$delay<10000,0,ifelse(dat2_3_d$delay<25000,0.5,ifelse(dat2_3_d$delay<100000,1,1.5)))
}
# 연지수
{
  dat2_3_d$연지수<-0
  dat2_3_d[dat2_3_d$연==2019,]$연지수<-0.9
  dat2_3_d[dat2_3_d$연==2018,]$연지수<-1.1
  dat2_3_d[dat2_3_d$연==2017,]$연지수<-1
}
# 월지수
{
  dat2_3_d$월지수<-0
  dat2_3_d[dat2_3_d$월==12,]$월지수<-0.9
  dat2_3_d[dat2_3_d$월==3,]$월지수<-0.6
  dat2_3_d[(dat2_3_d$월==1 | dat2_3_d$월==10),]$월지수<-1.2
  dat2_3_d[(dat2_3_d$월==5 | dat2_3_d$월==6),]$월지수<-0.8
  dat2_3_d[(dat2_3_d$월==2 | dat2_3_d$월==4 | dat2_3_d$월==8),]$월지수<-1.3
  dat2_3_d[(dat2_3_d$월==7 | dat2_3_d$월==9 | dat2_3_d$월==11),]$월지수<-1
}
# 계획시간별 지수
{
  z_1<-dat2_3_d %>%                        ## 계획시간별 지연율  
    group_by(계획시간_1) %>% 
    summarize(dly_mean=mean(dly)) 
  z_1_1<-barplot(z_1$dly_mean,names.arg = z_1$계획시간_1) 
  text(x=z_1_1,y=z_1$dly_mean,labels=round(z_1$dly_mean,2),pos=1)
  z_2<-dat2_3_d %>%                        ## 계획시간별 운행횟수 
    group_by(계획시간_1) %>% 
    summarize(drive=NROW(dly)) 
  barplot(z_2$drive,names.arg = z_2$계획시간_1) 
  
  dat2_3_d$계획시간지수<-0
  dat2_3_d[dat2_3_d$계획시간_1==6,]$계획시간지수<-round(4/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==7,]$계획시간지수<-round(5/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==8,]$계획시간지수<-round(8/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==9,]$계획시간지수<-round(10/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==10,]$계획시간지수<-round(10/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==11,]$계획시간지수<-round(12/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==12,]$계획시간지수<-round(12/13,2)
  dat2_3_d[dat2_3_d$계획시간_1==13,]$계획시간지수<-1
  dat2_3_d[(dat2_3_d$계획시간_1>13 & dat2_3_d$계획시간_1<=18),]$계획시간지수<-1.5
  dat2_3_d[dat2_3_d$계획시간_1==19,]$계획시간지수<-1
  dat2_3_d[dat2_3_d$계획시간_1==20,]$계획시간지수<-1.2
  dat2_3_d[dat2_3_d$계획시간_1==21,]$계획시간지수<-1.5
  dat2_3_d[dat2_3_d$계획시간_1==22,]$계획시간지수<-round(9/13,2)
}

dat2_3_d<-subset(dat2_3_d,select=-c(delay,line))
str(dat2_3_d)

# 항공사 지수

{
  z_1<-dat2_3_d %>%                        ## 항공사별 지연율  
    group_by(항공사) %>% 
    summarize(dly_mean=mean(dly)) 
  z_1_1<-barplot(z_1$dly_mean,names.arg = z_1$항공사) 
  text(x=z_1_1,y=z_1$dly_mean,labels=round(z_1$dly_mean,2),pos=1)
  z_2<-dat2_3_d %>%                        ## 항공사별 운행횟수 
    group_by(항공사) %>% 
    summarize(drive=NROW(dly)) 
  barplot(z_2$drive,names.arg = z_2$항공사) 
  
  dat2_3_d$항공사지수<-0
  dat2_3_d[dat2_3_d$항공사=="I",]$항공사지수<-1.5
  dat2_3_d[(dat2_3_d$항공사=="F" | dat2_3_d$항공사=="H" | dat2_3_d$항공사=="L"),]$항공사지수<-1
  dat2_3_d[(dat2_3_d$항공사=="A" | dat2_3_d$항공사=="B"),]$항공사지수<-0.5
  dat2_3_d[dat2_3_d$항공사=="J",]$항공사지수<-0
}

################ 도착 데이터 지수 

# 노선지수
{
  line=paste(dat2_3_a$공항,dat2_3_a$상대공항,sep="_")
  linedelay=as.data.frame(line)
  linedelay=cbind(linedelay,DLY=dat2_3_a$지연여부)
  delaysum=linedelay %>% group_by(line) %>% summarise(delay=length(DLY))
  sort(delaysum$delay,decreasing=T)
  plot(sort(delaysum$delay,decreasing=T))
  abline(h=c(10000,25000,100000))
  dat2_3_a[,"line"]=paste(dat2_3_a$공항,dat2_3_a$상대공항,sep="_")
  dat2_3_a=left_join(dat2_3_a,delaysum,by="line")
  dat2_3_a$노선지수=ifelse(dat2_3_a$delay<10000,0,ifelse(dat2_3_a$delay<25000,0.5,ifelse(dat2_3_a$delay<100000,1,1.5)))
}
# 연지수
{
  dat2_3_a$연지수<-0
  dat2_3_a[dat2_3_a$연==2019,]$연지수<-0.9
  dat2_3_a[dat2_3_a$연==2018,]$연지수<-1.1
  dat2_3_a[dat2_3_a$연==2017,]$연지수<-1
}
# 월지수
{
  dat2_3_a$월지수<-0
  dat2_3_a[dat2_3_a$월==11,]$월지수<-1
  dat2_3_a[dat2_3_a$월==12,]$월지수<-0.9
  dat2_3_a[dat2_3_a$월==3,]$월지수<-0.5
  dat2_3_a[dat2_3_a$월==7,]$월지수<-0.8
  dat2_3_a[dat2_3_a$월==10,]$월지수<-1.2
  dat2_3_a[(dat2_3_a$월==1 | dat2_3_a$월==2),]$월지수<-1.5
  dat2_3_a[(dat2_3_a$월==5 | dat2_3_a$월==6 | dat2_3_a$월==9),]$월지수<-0.7
  dat2_3_a[(dat2_3_a$월==4 | dat2_3_a$월==8),]$월지수<-1.3
}

# 계획시간별 지수
{
  z_1<-dat2_3_a %>%                        ## 계획시간별 지연율  
    group_by(계획시간_1) %>% 
    summarize(dly_mean=mean(dly)) 
  z_1_1<-barplot(z_1$dly_mean,names.arg = z_1$계획시간_1) 
  text(x=z_1_1,y=z_1$dly_mean,labels=round(z_1$dly_mean,2),pos=1)
  z_2<-dat2_3_a %>%                        ## 계획시간별 운행횟수 
    group_by(계획시간_1) %>% 
    summarize(drive=NROW(dly)) 
  barplot(z_2$drive,names.arg = z_2$계획시간_1) 
  
  dat2_3_a$계획시간지수<-0
  dat2_3_a[dat2_3_a$계획시간_1==7,]$계획시간지수<-0.15
  dat2_3_a[dat2_3_a$계획시간_1==8,]$계획시간지수<-round(3/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==9,]$계획시간지수<-round(4/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==10,]$계획시간지수<-round(5/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==11,]$계획시간지수<-round(6/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==12,]$계획시간지수<-round(6/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==13,]$계획시간지수<-round(7/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==14,]$계획시간지수<-round(8/9,2)
  dat2_3_a[dat2_3_a$계획시간_1==15,]$계획시간지수<-1
  dat2_3_a[dat2_3_a$계획시간_1==16,]$계획시간지수<-1.2
  dat2_3_a[(dat2_3_a$계획시간_1>=17 & dat2_3_a$계획시간_1<=21),]$계획시간지수<-1
  dat2_3_a[dat2_3_a$계획시간_1==22,]$계획시간지수<-round(7/9,2)
}

# 항공사 지수

{
  z_1<-dat2_3_a %>%                        ## 항공사별 지연율  
    group_by(항공사) %>% 
    summarize(dly_mean=mean(dly)) 
  z_1_1<-barplot(z_1$dly_mean,names.arg = z_1$항공사) 
  text(x=z_1_1,y=z_1$dly_mean,labels=round(z_1$dly_mean,2),pos=1)
  z_2<-dat2_3_a %>%                        ## 항공사별 운행횟수 
    group_by(항공사) %>% 
    summarize(drive=NROW(dly)) 
  barplot(z_2$drive,names.arg = z_2$항공사) 
  
  dat2_3_a$항공사지수<-0
  dat2_3_a[(dat2_3_a$항공사=="F" | dat2_3_a$항공사=="I" | dat2_3_a$항공사=="L"),]$항공사지수<-1
  dat2_3_a[(dat2_3_a$항공사=="A" | dat2_3_a$항공사=="B"| dat2_3_a$항공사=="H"),]$항공사지수<-0.5
  dat2_3_a[dat2_3_a$항공사=="J",]$항공사지수<-0
}


str(dat2_3_a)
dat2_3_a<-subset(dat2_3_a,select=-c(delay,line))
str(dat2_3_a)

write.csv(dat2_3_d,file="C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/depart_zisu_final.csv",row.names=FALSE)
write.csv(dat2_3_a,file="C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/arrive_zisu_final.csv",row.names=FALSE)

afsnt_4$계획시간_2<-round(hour(hm(afsnt_4$계획시각))+minute(hm(afsnt_4$계획시각))/60,2) # 계획시간  


z_2<-afsnt_4 %>%                        ## 계획시간별 운행횟수 
  group_by(계획시간_2) %>% 
  summarize(drive=NROW(dly)) 
barplot(z_2$drive,names.arg = z_2$계획시간_2)


# 종수형 코드

library(ggplot2)
library(dplyr)
library(lubridate)

AFSNT <- read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT.csv")
AFSNT_DLY <- read.csv("C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT_DLY.csv")
str(AFSNT)
## 제출 데이터에 없는 편명은 지우기 

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

# 파생 변수 및 이상치 제거 
{
  AFSNT$STT_hour <- sapply(strsplit(as.character(AFSNT$STT),":"),"[",1)
  AFSNT$STT_minute <- sapply(strsplit(as.character(AFSNT$STT),":"),"[",2)
  AFSNT$ATT_hour <- sapply(strsplit(as.character(AFSNT$ATT),":"),"[",1)
  AFSNT$ATT_minute <- sapply(strsplit(as.character(AFSNT$ATT),":"),"[",2)
  AFSNT$STT2 <- as.numeric(AFSNT$STT_hour) + as.numeric(AFSNT$STT_minute)/60
  AFSNT$ATT2 <- as.numeric(AFSNT$ATT_hour) + as.numeric(AFSNT$ATT_minute)/60
  AFSNT$delay_time <- AFSNT$ATT2 - AFSNT$STT2
}

# 지연 시간 이상치 ( 850,609 rows -> 850,588 rows )
AFSNT %>% filter(!delay_time %in% sort(AFSNT$delay_time)[1:21]) -> AFSNT

###

## 출발 또는 도착만 존재하는 데이터 지우기  (850,588 rows -> 850,524 rows)

AFSNT %>% group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>% summarise(n=n()) -> AFSNT_2
AFSNT_2 %>% filter(n==1) %>% as.data.frame()-> AFSNT_3
for ( i in 1:nrow(AFSNT_3)){
  AFSNT<-AFSNT[!((AFSNT$SDT_YY==AFSNT_3$SDT_YY[i]) & (AFSNT$SDT_MM==AFSNT_3$SDT_MM[i]) & (AFSNT$SDT_DD==AFSNT_3$SDT_DD[i]) & (AFSNT$FLT==AFSNT_3$FLT[i])),] 
}
AFSNT %>% group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>% summarise(n=n()) %>% filter(n==4) %>% as.data.frame() -> AFSNT_5
for ( i in 1:nrow(AFSNT_5)){
  AFSNT<-AFSNT[!((AFSNT$SDT_YY==AFSNT_5$SDT_YY[i]) & (AFSNT$SDT_MM==AFSNT_5$SDT_MM[i]) & (AFSNT$SDT_DD==AFSNT_5$SDT_DD[i]) & (AFSNT$FLT==AFSNT_5$FLT[i])),] 
}
##

write.csv(AFSNT,"C:/Desktop/Son/공모전/빅콘 2019/데이터_수정/AFSNT_3.csv")

AFSNT %>% group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>% summarise(driving=abs(diff(STT2))) %>% as.data.frame() -> dfcv_1
dfcv_1[,c(1,5)]->dfcv_2
unique(dfcv_2) -> uniq_dfcv_2

for ( i in 1:nrow(uniq_dfcv_2)){
  dfcv_1$FLT==uniq_dfcv_2$FLT[i]
}
unique(dfcv_1)->dfcv_3
df

str(dfcv_3)
view(dfcv_3)

dfcv_3$date <- ymd(paste(dfcv_3$SDT_YY,dfcv_3$SDT_MM,dfcv_3$SDT_DD))
dfcv_3

AFSNT %>% group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>% summarise(n=n()) %>% filter(n==4) %>% as.data.frame() -> AFSNT_5
str(AFSNT_5)

for ( i in 1:nrow(AFSNT_5)){
  AFSNT<-AFSNT[!((AFSNT$SDT_YY==AFSNT_5$SDT_YY[i]) & (AFSNT$SDT_MM==AFSNT_5$SDT_MM[i]) & (AFSNT$SDT_DD==AFSNT_5$SDT_DD[i]) & (AFSNT$FLT==AFSNT_5$FLT[i])),] 
}

unique(AFSNT_5$n)
head(AFSNT_5)
AFSNT$driving_time<-0
for ( i in 1:nrow(AFSNT_5)){
  AFSNT$driving_time<-AFSNT[((AFSNT$SDT_YY==AFSNT_5$SDT_YY[i]) & (AFSNT$SDT_MM==AFSNT_5$SDT_MM[i]) & (AFSNT$SDT_DD==AFSNT_5$SDT_DD[i]) & (AFSNT$FLT==AFSNT_5$FLT[i])) & (AFSNT$AOD=="D"),]$STT2-AFSNT[((AFSNT$SDT_YY==AFSNT_5$SDT_YY[i]) & (AFSNT$SDT_MM==AFSNT_5$SDT_MM[i]) & (AFSNT$SDT_DD==AFSNT_5$SDT_DD[i]) & (AFSNT$FLT==AFSNT_5$FLT[i])) & (AFSNT$AOD=="A"),]$STT2
  
  }
AFSNT_6 %>% group_by(FLT,SDT_YY,SDT_MM,SDT_DD) %>% summarise(driving_time = diff(STT2)) %>% as.data.frame() -> AFSNT_7 
AFSNT_6$driving_time<-0
for ( i in 1:nrow(AFSNT_7)){
  AFSNT_6[((AFSNT_6$SDT_YY==AFSNT_7$SDT_YY[i]) & (AFSNT_6$SDT_MM==AFSNT_7$SDT_MM[i]) & (AFSNT_6$SDT_DD==AFSNT_7$SDT_DD[i]) & (AFSNT_6$FLT==AFSNT_7$FLT[i])),]$driving_time <- AFSNT_7$driving_time[i]
}

for ( i in 1:nrow(AFSNT_7)){
  AFSNT_6[((AFSNT_6$SDT_YY==AFSNT_7$SDT_YY[i]) & (AFSNT_6$SDT_MM==AFSNT_7$SDT_MM[i]) & (AFSNT_6$SDT_DD==AFSNT_7$SDT_DD[i]) & (AFSNT_6$FLT==AFSNT_7$FLT[i])),]$driving_time <- AFSNT_7$driving_time[i]
}

merge(AFSNT,AFSNT_6,all = )




AFSNT[((AFSNT$FLT %in% AFSNT_3$FLT) & (AFSNT$SDT_YY %in% AFSNT_3$SDT_YY) & (AFSNT$SDT_MM %in% AFSNT_3$SDT_MM) & (AFSNT$SDT_DD %in% AFSNT_3$SDT_DD)),] -> AFSNT_4

view(AFSNT_4)
sum((AFSNT$FLT %in% AFSNT_3$FLT)&(AFSNT$SDT_YY %in% AFSNT_3$SDT_YY)&(AFSNT$SDT_MM %in% AFSNT_3$SDT_MM)&(AFSNT$SDT_DD %in% AFSNT_3$SDT_DD)&(AFSNT$REG %in% AFSNT_3$REG))
str(AFSNT_4)
unique(AFSNT_2$n)

AFSNT %>% filter(FLT=="A1001") -> AFSNT_1
view(AFSNT_1)
#3 출 도착 나누기 

# A (426,485 rows)
AFSNT %>% filter(AOD=="A") -> A
# D (426,569 rows)
AFSNT %>% filter(AOD=="D") -> D

## 휴일 / 출발 시간이랑 도착시간이랑 빼서 




