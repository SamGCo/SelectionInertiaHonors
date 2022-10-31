filingData2019 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\2019FilingsData.csv")
#incorporating demographic data into the RA data
demoRA<-read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\2019 OEP State-Level Public Use File.xlsx",8)
statesFiling<-unique(filingData2019$STATE)
filingData2019<-subset(filingData2019,MARKET!="Small Group")
plansByStates<- data.frame(matrix(ncol=11,nrow=0))
colnames(plansByStates)<-c('State','PlanNum',"Age18","Age1825", "Age2634","Age3544","Age4554", "Age5564" ,"Age65",'Male','Female'  )

#dataframe of number of markets per state

Markets<-c(13,3,7,7,19,9,8,1,1,67,16,1,6,13,17,7,7,8,8,4,4,7,16,9,6,10,4,4,4,1,6,5,8,16,4,17,5,7,9,1,46,4,8,27,6,1,12,9,11,16,3)
mktStates<-(c("AL","AK", "AZ","AR","CA","CO" ,"CT","DE", "DC" , "FL","GA", "HI","ID", "IL","IN", "IA", "KS", "KY", "LA", "ME","MD","MA", "MI", "MN" ,"MS","MO","MT","NE","NV","NH", "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))
mktDf<-data.frame(mktStates,Markets)

forTally<-1
for(s in statesFiling)
{
  if(s=='MA'||s=='WY'||s=='DE'||s=='NE'||s=='AK'){} #dropping MA and VT because there is no Risk Adjustment Data on them
  else if(s=='VT'){}
  else{
  currentState<-subset(filingData2019,STATE==s)
  currentMkt<-subset(mktDf,mktStates==s)
  currentDemo<-subset(demoRA,demoRA$'State Abbr.'==s)
  count=0
  plans<-currentState$PLAN_ID
  for(p in plans)
  {
    count=count+1
  }
  
  plansByStates[forTally,]<-c(s,count/currentMkt$Markets,as.numeric(currentDemo$`Age < 18`),as.numeric(currentDemo$`Age 18-25`),as.numeric(currentDemo$`Age 26-34`),as.numeric(currentDemo$`Age 35-44`),as.numeric(currentDemo$`Age 45-54`),as.numeric(currentDemo$`Age 55-64`),as.numeric(currentDemo$`Age ≥65`),currentDemo$Male,currentDemo$Female)
  forTally<-forTally+1
  }
}
plansByStates$PlanNum<-as.numeric(plansByStates$PlanNum)
plansByStates$Age18<-as.numeric(plansByStates$Age18)
plansByStates$Age1825<-as.numeric(plansByStates$Age1825)
plansByStates$Age2634<-as.numeric(plansByStates$Age2634)
plansByStates$Age3544<-as.numeric(plansByStates$Age3544)
plansByStates$Age4554<-as.numeric(plansByStates$Age4554)
plansByStates$Age5564 <-as.numeric(plansByStates$Age5564)
plansByStates$Age65 <-as.numeric(plansByStates$Age65)
plansByStates$Male <-as.numeric(plansByStates$Male)
plansByStates$Female <-as.numeric(plansByStates$Female)


RAwithPlans<-merge(RAsum,plansByStates, by='State')
RAwithPlans$comp<-as.numeric(RAwithPlans$comp)
comp.RA.lm<- lm(formula=RiskAdjustmentSum ~ PlanNum+Age1825+ Age2634+Age3544+Age4554+Age5564+Age65+Male+comp,data=RAwithPlans)
summary(comp.RA.lm)  

