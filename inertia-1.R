library(readxl)
inertia2018<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\OE2018_County_PUF_20180404.xlsx',sheet=4)
inertia2019<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\2019 OEP County-Level Public Use File.xlsx',sheet=4)
inertiaDemo2018<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\OE2018_County_PUF_20180404.xlsx',sheet=7)
inertiaDemo2019<-read_xlsx('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\2019 OEP County-Level Public Use File.xlsx',sheet=7)
inertia2020<-read.csv('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\2020 OEP County-Level Public Use File.csv')
inertia2021<-read.csv('C:\\Users\\squam\\OneDrive\\Documents\\Honors Thesis\\weights\\2021 OEP County-Level Public Use File.csv')
names(inertiaDemo2018)[names(inertiaDemo2018)=="Age ≥65"]<-"Age ≥ 65"
inertia2020<-inertia2020[,-16]
names(inertia2021)[c(2,35:36,59)]<-c("Cnty_FIPS_Cd","Male","Female","FPL_OTHR")
inertia2020$Year<-2020
inertia2021$Year<-2021
inertia20s<-rbind(inertia2020,inertia2021)

inertiaDemo2018$Year<-2018
inertiaDemo2019$Year<-2019
inertiaDemoTotal<-rbind(inertiaDemo2018,inertiaDemo2019)
names(inertiaDemoTotal)[names(inertiaDemoTotal)=="Age ≥ 65"]<-"Age > 65"
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
inertia20s$inertiaShare<-as.numeric(inertia20s$Actv_Renrl_Nsw)/as.numeric(inertia20s$Actv_Renrl)
names(inertia20s)[2]<-"County FIPS Code"
inertia20s<-left_join(inertia20s,countyCrosswalk)
inertia20s$`County Name`<-NA
inertia20s<-inertia20s[,-c(4:5,7,10:27,48:67)]
inertia20sOrdered<-inertia20s%>%select(2,1,30,4,5,6,27,28,29,3,7:26)
colnames(inertia20sOrdered)<-colnames(inertiaBefore20s)

inertiaBefore20s<-left_join(inertiaTotal,inertiaDemoTotal)
inertiaWithPlans<-rbind(inertiaBefore20s,inertia20sOrdered)
inertiaWithPlans<-left_join(inertiaWithPlans,totalPlansforInertia, by=c("State"="ST","Year"="YEAR","AREA"="AREA"))
inertiaWithPlans[inertiaWithPlans=="*"]<-'0'


inertiaFinal<-inertiaWithPlans%>%
  distinct(AREA,Year,State,.keep_all = TRUE)
inertiaFinal[,c(4:6,11:39)]<-lapply(inertiaFinal[,c(4:6,11:39)],as.numeric)
noInertiaFE<- feols(fml=inertiaShare~planNum+planNum2+`Age < 18`+`Age 18-25`+`Age 26-34`+`Age 35-44`+`Age 55-64`+`Age > 65`+Male+ `American Indian / Alaska Native`+Asian+White +`Native Hawaiian / Pacific Islander`+`Other Race`+Multiracial+`Unknown Race`,weights=inertiaFinal$`Active Re-enrollees`,data=inertiaFinal)
stateInertiaFE<- feols(fml=inertiaShare~planNum+planNum2+`Age < 18`+`Age 18-25`+`Age 26-34`+`Age 35-44`+`Age 55-64`+`Age > 65`+Male+ `American Indian / Alaska Native`+Asian+White +`Native Hawaiian / Pacific Islander`+`Other Race`+Multiracial+`Unknown Race`|State,weights=inertiaFinal$`Active Re-enrollees`,data=inertiaFinal)
yearInertiaFE<- feols(fml=inertiaShare~planNum+planNum2+`Age < 18`+`Age 18-25`+`Age 26-34`+`Age 35-44`+`Age 55-64`+`Age > 65`+Male+ `American Indian / Alaska Native`+Asian+White +`Native Hawaiian / Pacific Islander`+`Other Race`+Multiracial+`Unknown Race`|Year,weights=inertiaFinal$`Active Re-enrollees`,data=inertiaFinal)
inertiaTest<-feols(inertiaShare~planNum+planNum2+`Age < 18`+`Age 18-25`+`Age 26-34`+`Age 35-44`+`Age 55-64`+`Age > 65`+Male+ `American Indian / Alaska Native`+Asian+White +`Native Hawaiian / Pacific Islander`+`Other Race`+Multiracial+`Unknown Race`|State+Year,weights=inertiaFinal$`Active Re-enrollees`,data=inertiaFinal)
inertiaNoWeight<-feols(inertiaShare~planNum+planNum2+`Age < 18`+`Age 18-25`+`Age 26-34`+`Age 35-44`+`Age 55-64`+`Age > 65`+Male+ `American Indian / Alaska Native`+Asian+White +`Native Hawaiian / Pacific Islander`+`Other Race`+Multiracial+`Unknown Race`|State+Year,data=inertiaFinal)

summary(inertiaTest)
numericalInertia <- select_if(inertiaFinal[,c(-7,-8,-16,-18)], is.numeric,na.rm=TRUE)
inertiaCor<- cor(numericalInertia[, -3], numericalInertia[, 3], use = "pairwise.complete.obs")
# corrplot(inertiaCor, method = "color",
#          tl.col = "black", tl.srt = 45, 
#          title = "Correlation Chart of the Number of Plans",addCoef.col = "black", addCoefasPercent = TRUE,tl.cex=.5)
save.image("workspace.RData")
