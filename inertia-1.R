library(readxl)
inertia2018<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\OE2018_County_PUF_20180404.xlsx',sheet=4)
inertia2019<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\2019 OEP County-Level Public Use File.xlsx',sheet=4)
inertiaDemo2018<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\OE2018_County_PUF_20180404.xlsx',sheet=7)
inertiaDemo2019<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\2019 OEP County-Level Public Use File.xlsx',sheet=7)
names(inertiaDemo2018)[names(inertiaDemo2018)=="Age ≥65"]<-"Age ≥ 65"
inertiaDemo2018$Year<-2018
inertiaDemo2019$Year<-2019
inertiaDemoTotal<-rbind(inertiaDemo2018,inertiaDemo2019)
inertia2018$Year<-2018
inertia2019$Year<-2019
inertiaTotal<-rbind(inertia2018,inertia2019)
inertiaTotal<-inertiaTotal[!is.na(as.numeric(inertiaTotal$`Active Re-enrollees who Remained in the Same Plan or a Crosswalked Plan`)), ]
inertiaTotal$`Active Re-enrollees`<-as.numeric(inertiaTotal$`Active Re-enrollees`)
inertiaTotal$`Active Re-enrollees who Remained in the Same Plan or a Crosswalked Plan`<-as.numeric(inertiaTotal$`Active Re-enrollees who Remained in the Same Plan or a Crosswalked Plan`)
inertiaTotal$inertiaShare<-inertiaTotal$`Active Re-enrollees who Remained in the Same Plan or a Crosswalked Plan`/inertiaTotal$`Active Re-enrollees`
inertiaTotal<-left_join(inertiaTotal,countyCrosswalk)
totalPlansforInertia<-totalPlans%>%
  group_by(YEAR,AREA)%>%
  mutate(planNum=n())%>%
  mutate(planNum2=planNum^2)
inertiaWithPlans<-left_join(inertiaTotal,totalPlansforInertia, by=c("State"="ST","Year"="YEAR","AREA"="AREA"))
inertiaWithPlans<-left_join(inertiaWithPlans,inertiaDemoTotal)
inertiaWithPlans[inertiaWithPlans=="*"]<-'0'


inertiaFinal<-inertiaWithPlans%>%
  distinct(AREA,Year,State,.keep_all = TRUE)
inertiaFinal[,19:38]<-lapply(inertiaFinal[,19:38],as.numeric)
inertiaTest<-feols(inertiaShare~planNum+planNum2+`Age < 18`+`Age 18-25`+`Age 26-34`+`Age 35-44`+`Age 55-64`+`Age ≥ 65`+Male+ `American Indian / Alaska Native`+Asian+White +`Native Hawaiian / Pacific Islander`+`Other Race`+Multiracial+`Unknown Race`|State+Year,weights=inertiaFinal$`Active Re-enrollees`,data=inertiaFinal)
summary(inertiaTest)
