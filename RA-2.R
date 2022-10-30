filingData2019 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\2019FilingsData.csv")
statesFiling<-unique(filingData2019$STATE)
filingData2019<-subset(filingData2019,MARKET!="Small Group")
plansByStates<- data.frame(matrix(ncol=2,nrow=0))
colnames(plansByStates)<-c('State','PlanNum')

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
  count=0
  plans<-currentState$PLAN_ID
  for(p in plans)
  {
    count=count+1
  }
  
  plansByStates[forTally,]<-c(s,count/currentMkt$Markets)
  forTally<-forTally+1
  }
}
plansByStates$PlanNum<-as.numeric(plansByStates$PlanNum)

RAwithPlans<-merge(RAsum,plansByStates, by='State')
comp.RA.lm<- lm(formula=RiskAdjustmentSum ~ PlanNum,data=RAwithPlans)
summary(comp.RA.lm)  

