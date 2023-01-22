library(readxl)
Risk_Adjustment <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\RA\\ra_reins.csv")
Risk_Adjustment<-subset(Risk_Adjustment,Year !=2018)
ra2019<-read_xlsx("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\RA\\2019 Risk Adjustment.xlsx")
RAData<-Risk_Adjustment #import data
ra2018<-read_xlsx("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\RA\\2018 Risk Adjustment.xlsx")
ra2019<-ra2019[1:7]
ra2018<-ra2018[1:7]
ra2018<-subset(ra2018, RA_IND>=1 | RA_IND<=-1)
colnames(ra2019)<-colnames(Risk_Adjustment)
colnames(ra2018)<-colnames(Risk_Adjustment)
Risk_Adjustment<-rbind(Risk_Adjustment,ra2019)
Risk_Adjustment<-rbind(Risk_Adjustment,ra2018)
Risk_Adjustment<-subset(Risk_Adjustment,Year!=2014)
states<-unique(RAData$STATE) #make list of states
years<-unique(Risk_Adjustment$Year)
RAsum <- data.frame(matrix(ncol=4,nrow=0)) #create empty final data set 
colnames(RAsum)<-c('State','RiskAdjustmentSum','comp','year') #name columns for dataset
tally=1 #tally for for loop
insurersWithoutRA<-subset(Risk_Adjustment, RA_IND==0)
Risk_Adjustment<-subset(Risk_Adjustment, RA_IND!=0)

for(y in years)
{
  for(x in states){
  comp=0
  currentRA<-subset(Risk_Adjustment,STATE==x & Year==y) #create dataframe of the x state
  currentValue<-sum(abs(as.numeric(currentRA$RA_IND)))/2 #sum and divide by two RA
  insurers<-currentRA$HIOS #create list of insurers for x state
  for(i in insurers)
  {
    comp=comp+1 #for loop to create number of insurers per state
  }# remember to look at insurers per year
  if(currentValue !=0){
  RAsum[tally,]<-c(x,currentValue,comp,y) #add to data set
  tally=tally+1
  }
}
}


RAsum<-subset(RAsum, year==2015 | year==2016 | year==2017 | year==2018| year==2019)
RAsum$RiskAdjustmentSum<-as.numeric(RAsum$RiskAdjustmentSum)
