library(fixest)
filingData <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\acaData.csv")
#incorporating demographic data into the RA data
demoRA<-totalDemo
demoRA$Year<-as.integer(demoRA$Year)
statesFiling<-unique(totalPlans$ST)

plansByStates<- data.frame(matrix(ncol=21,nrow=0))
colnames(plansByStates)<-c('State','PlanNum','TotalEnrollees',"Age18","Age1825", "Age2634","Age3544","Age4554", "Age5564" ,"Age65",'Male','Female','Asian','AIAN','AA','White','NHPI','Multi','unknown','Year','EnrolleesPerMkt')

#creating dataframe of number of markets per state

Markets<-c(13,3,7,7,19,9,8,1,1,67,16,1,6,13,17,7,7,8,8,4,4,7,16,9,6,10,4,4,4,1,6,5,8,16,4,17,5,7,9,1,46,4,8,27,6,1,12,9,11,16,3)
mktStates<-(c("AL","AK", "AZ","AR","CA","CO" ,"CT","DE", "DC" , "FL","GA", "HI","ID", "IL","IN", "IA", "KS", "KY", "LA", "ME","MD","MA", "MI", "MN" ,"MS","MO","MT","NE","NV","NH", "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))
mktDf<-data.frame(mktStates,Markets)

popRA<-data.frame(matrix(ncol=5,nrow=0))
colnames(popRA)<-c('State','RiskAdjustmentSum','RAVar','comp','Year')
Years<-c(2015,2016,2017,2018,2019,2020,2021)
forTally<-1


for(y in Years)
{
for(s in statesFiling)
{
  areaThing<-subset(totalPlans,ST==s)
  
  if(s=='MA'||s=='VT'){} #dropping MA and VT because there is no Risk Adjustment Data on them, dropping the rest because there is no demographic data for them
  
  else{
  currentState<-subset(totalPlans,ST==s & YEAR==y)
  currentMkt<-subset(mktDf,mktStates==s)
  currentDemo<-subset(demoRA,demoRA$State==s& Year==y)
  forRA <-subset(RAsum,State==s & year==y)
  if ((dim(forRA)[1] == 0)||(dim(currentDemo)[1] == 0))
  {
    
  }
  else{
  popRA[forTally,]<-c(forRA$State,forRA$RiskAdjustmentSum/as.numeric(currentDemo$TotalEnrollees),forRA$RAVar, forRA$comp,forRA$year)
  
  count=0
  }
  
  for(a in areaThing$AREA)
  {
    plans<-subset(currentState, AREA==a)$PLANID
  
  for(p in plans)
  {
    #I need code somewhere to calculate the weighted average of plans per state from the population of rating areas
    count=count+1 #creating a count of number of plans per state 
    
    
  }
    count<-count*plans$pop
  }
  if ((dim(currentDemo)[1] == 0))
  {}
  else{
  plansByStates[forTally,]<-c(s,count/currentMkt$Markets,as.numeric(currentDemo$TotalEnrollees),as.numeric(currentDemo$Age18),as.numeric(currentDemo$Age1825),as.numeric(currentDemo$Age2634),as.numeric(currentDemo$Age3544),as.numeric(currentDemo$Age4554),as.numeric(currentDemo$Age5564),as.numeric(currentDemo$Age65),currentDemo$Male,currentDemo$Female,as.numeric(currentDemo$Asian),as.numeric(currentDemo$AIAN),as.numeric(currentDemo$AA),as.numeric(currentDemo$White),as.numeric(currentDemo$NHPI),as.numeric(currentDemo$Multi),as.numeric(currentDemo$unknown),y,currentDemo$TotalEnrollees/currentMkt$Markets)
  forTally<-forTally+1

  }
  
  }
  }

}
#changing each variable into a numeric from a character
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

plansByStates$Asian <-as.numeric(plansByStates$Asian)
plansByStates$AIAN <-as.numeric(plansByStates$AIAN)
plansByStates$AA <-as.numeric(plansByStates$AA)
plansByStates$White <-as.numeric(plansByStates$White)
plansByStates$NHPI <-as.numeric(plansByStates$NHPI)
plansByStates$Multi <-as.numeric(plansByStates$Multi)
plansByStates$unknown <-as.numeric(plansByStates$unknown)

RAwithPlans<-inner_join(popRA,plansByStates,by=c("State","Year"))
RAwithPlans$comp<-as.numeric(RAwithPlans$comp)
RAwithPlans$RiskAdjustmentSum<-as.numeric(RAwithPlans$RiskAdjustmentSum)
RAwithPlans$PlanNum2<-as.numeric(RAwithPlans$PlanNum)^2
RAwithPlans$comp2<-as.numeric(RAwithPlans$comp)^2
RAwithPlans<-subset(RAwithPlans,PlanNum!=0)
RAwithPlans$TotalEnrollees<-as.numeric(RAwithPlans$TotalEnrollees)
noFE<- feols(fml=RiskAdjustmentSum ~ PlanNum+PlanNum2+Age18+Age1825+ Age2634+Age3544+Age4554+Age65+Female+AIAN+White+NHPI+Multi+Asian+unknown+comp,data=RAwithPlans)
stateFE<- feols(fml=RiskAdjustmentSum ~ PlanNum+PlanNum2+Age18+Age1825+ Age2634+Age3544+Age4554+Age65+Female+AIAN+White+NHPI+Multi+Asian+unknown+comp | State,data=RAwithPlans)
yearFE<- feols(fml=RiskAdjustmentSum ~ PlanNum+PlanNum2+Age18+Age1825+ Age2634+Age3544+Age4554+Age65+Female+AIAN+White+NHPI+Multi+Asian+unknown+comp | Year,data=RAwithPlans)

plans.RA.lm<- feols(fml=RiskAdjustmentSum ~ PlanNum+PlanNum2+Age18+Age1825+ Age2634+Age3544+Age4554+Age65+Female+AIAN+White+NHPI+Multi+Asian+unknown+comp | State+Year,data=RAwithPlans)
print(summary(plans.RA.lm)  )
raVar.lm<-lm(formula=RAVar~comp, data=RAwithPlans)
summary(raVar.lm)
plot(RAwithPlans$PlanNum,RAwithPlans$RiskAdjustmentSum)
par(new=TRUE)
plot(y2,0,145,add=TRUE,col="Red")


models<-list("No Fixed Effects"=noFE,"State Fixed Effects" =stateFE,"Year Fixed Effects"=yearFE,"State and Year Fixed Effects"=plans.RA.lm)
modelSummaryTable<-modelsummary::modelsummary(models,coef_omit = "Intercept",stars=TRUE)
save(modelSummaryTable,"fixedEffectsRegression.png")

insurers.lm<-feols(fml=RiskAdjustmentSum ~ comp+comp2+Age18+Age1825+ Age2634+Age3544+Age4554+Age65+Female+AIAN+White+NHPI+Multi+Asian+unknown | State+Year,data=RAwithPlans)
summary(insurers.lm)
save.image("workspace.RData")