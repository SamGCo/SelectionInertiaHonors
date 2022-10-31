library(readxl)
X2019_Risk_Adjustment <- read_excel("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\RA\\2019 Risk Adjustment.xlsx")
RA2019<-X2019_Risk_Adjustment #import data
states<-unique(RA2019$STATE) #make list of states

RAsum <- data.frame(matrix(ncol=3,nrow=0)) #create empty final data set 
colnames(RAsum)<-c('State','RiskAdjustmentSum','comp') #name columns for dataset
tally=1 #tally for for loop
for(x in states)
{
  comp=0
  currentRA<-subset(RA2019,STATE==x) #create dataframe of the x state
  currentValue<-sum(abs(currentRA$RA_IND))/2 #sum and divide by two RA
  insurers<-currentRA$`HIOS INSURANCE COMPANY NAME` #create list of insurers for x state
  for(i in insurers)
  {
    comp=comp+1 #for loop to create number of insurers per state
  }
  RAsum[tally,]<-c(x,currentValue,comp) #add to data set
  tally=tally+1
}



