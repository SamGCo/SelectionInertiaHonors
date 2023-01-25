library(naniar)
County15<-read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\demo\\2015_OEP_County-Level_Public_Use_File.xlsx",7)
County16<-read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\demo\\2016_OEP_County-Level_Public_Use_File.xlsx",7)
demo17<-read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\demo\\2017_OEP_State-Level_Public_Use_File_082018.xlsx",10)
demo18<-read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\demo\\OE2018_State_PUF_20180808.xlsx",8)
demo19<-read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\2019 OEP State-Level Public Use File.xlsx",8)
demo20<-read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\demo\\2020 OEP State-Level Public Use File.csv",8)
demo21<-read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\demo\\2021 OEP State-Level Public Use File.csv",8)
County15$Year<-2015
County16$Year<-2016
demo17$Year<-2017
demo18$Year<-2018
demo19$Year<-2019
demo20$Year<-2020
demo21$Year<-2021
common_col_names <- intersect(names(demo20), names(demo21))
demo20s<-rbind(select(demo20,all_of(common_col_names)),select(demo21,all_of(common_col_names)))
demo20s<-select(demo20s,contains("Age"),-contains("BHP"),-contains("Dntl"),Cnsmr,State_Abrvtn,Male,Female,AIAN,ASN,NHPI,WHT,BLACK,Mlt_Race,Unk_Race,Year)
col_order<-c("State_Abrvtn","Cnsmr","Age_0_17","Age_18_25","Age_26_34","Age_35_44","Age_45_54","Age_55_64","Age_GE65","Male","Female","ASN","AIAN","BLACK","WHT","NHPI","Mlt_Race","Unk_Race","Year" )
demo20s<-demo20s[,col_order]

colnames(County15)[1]<-'FIPS'
fips15<-as.numeric(County15$FIPS)
state15<-unique(County15$State)
state15<-state15[!is.na(state15)]
County15[County15=='*']<-NA


colnames(County16)[1]<-'FIPS'
fips16<-as.numeric(County15$FIPS)
state16<-unique(County15$State)
state16<-state16[!is.na(state15)]
County16[County16=='*']<-NA
Democount<-1
demo16<-data.frame(matrix(ncol=19,nrow=0))
colnames(demo16)<-c('State','TotalEnrollees',"Age18","Age1825", "Age2634","Age3544","Age4554", "Age5564" ,"Age65",'Male','Female','Asian','AIAN','AA','White','NHPI','Multi','unknown','Year')

for(St in state16)
{
  currentDemoState<-subset(County16,State==St)
 

  demo16[Democount,]<-c(St,sum(currentDemoState$`Total Number of Consumers Who Have Selected a Marketplace Plan`,na.rm=TRUE),sum(as.numeric(currentDemoState$`Age < 18`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 18-25`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 26-34`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 35-44`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 45-54`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 55-64`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 65+`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age Unknown`),na.rm=TRUE),sum(as.numeric(currentDemoState$`American Indian/ Alaska Native`),na.rm=TRUE),sum(as.numeric(currentDemoState$Asian),na.rm=TRUE),sum(as.numeric(currentDemoState$`African-American`),na.rm=TRUE),sum(as.numeric(currentDemoState$Latino),na.rm=TRUE),sum(as.numeric(currentDemoState$`Native Hawaiian/ Pacific Islander`),na.rm=TRUE),sum(as.numeric(currentDemoState$Multiracial),na.rm=TRUE),sum(as.numeric(currentDemoState$White),na.rm=TRUE),sum(as.numeric(currentDemoState$`Unknown Race/ Ethnicity`),na.rm=TRUE),2016)
  
  
  Democount<-Democount+1
}


colnames(County15)[1]<-'FIPS'
fips15<-as.numeric(County15$FIPS)
state15<-unique(County15$State)
state15<-state15[!is.na(state15)]
County15[County15=='*']<-NA

Democount<-1
demo15<-data.frame(matrix(ncol=19,nrow=0))
colnames(demo15)<-c('State','TotalEnrollees',"Age18","Age1825", "Age2634","Age3544","Age4554", "Age5564" ,"Age65",'Male','Female','Asian','AIAN','AA','White','NHPI','Multi','unknown','Year')

for(St in state15)
{
  currentDemoState<-subset(County15,State==St)
  
  
  demo15[Democount,]<-c(St,sum(currentDemoState$`Total Number of Consumers Who Have Selected a Marketplace Plan`,na.rm=TRUE),sum(as.numeric(currentDemoState$`Age < 18`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 18-25`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 26-34`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 35-44`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 45-54`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 55-64`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age 65+`),na.rm=TRUE),sum(as.numeric(currentDemoState$`Age Unknown`),na.rm=TRUE),sum(as.numeric(currentDemoState$`American Indian/ Alaska Native`),na.rm=TRUE),sum(as.numeric(currentDemoState$Asian),na.rm=TRUE),sum(as.numeric(currentDemoState$`African-American`),na.rm=TRUE),sum(as.numeric(currentDemoState$Latino),na.rm=TRUE),sum(as.numeric(currentDemoState$`Native Hawaiian/ Pacific Islander`),na.rm=TRUE),sum(as.numeric(currentDemoState$Multiracial),na.rm=TRUE),sum(as.numeric(currentDemoState$White),na.rm=TRUE),sum(as.numeric(currentDemoState$`Unknown Race/ Ethnicity`),na.rm=TRUE),2015)
  
  
  Democount<-Democount+1
}


totalDemo<-rbind(demo15,demo16)
demo17<-demo17[-c(1,3,21:24)]
colnames(demo17)<-colnames(totalDemo)
totalDemo<-rbind(totalDemo,demo17)
demo18<-demo18[-c(1,3,17,22:26)]
colnames(demo18)<-colnames(totalDemo)
totalDemo<-rbind(totalDemo,demo18)
demo19<-demo19[-c(1,3,17,22:26)]
colnames(demo19)<-colnames(totalDemo)
colnames(demo20s)<-colnames(demoRA)
totalDemo<-rbind(totalDemo,demo19)
totalDemo<-rbind(totalDemo,demo20s)
totalDemo<-subset(totalDemo,State!='Total')
totalDemo<-replace_with_na_all(totalDemo,~.x %in% c("NR", "+","*"))
totalDemo[] <- lapply(totalDemo, function(x) gsub(",", "", x))

totalDemo[,2:19]<-sapply(totalDemo[,2:19],as.numeric)
#totalDemo$TotalEnrollees<-as.numeric(totalDemo$TotalEnrollees)
