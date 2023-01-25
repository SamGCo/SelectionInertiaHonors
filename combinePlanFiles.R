plans2015 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2015.csv")
plans2016 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2016.csv")
plans2017 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2017.csv")
plans2018 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2018.csv")
plans2019 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2019.csv")
plans2020 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2020.csv")
plans2021 <- read.csv("C:\\Users\\squam\\OneDrive\\Desktop\\HonorsResearch\\plans2021.csv")
totalPlans<-rbind(plans2015,plans2016,plans2017,plans2018,plans2019,plans2020,plans2021)
totalPlans <- totalPlans[!(totalPlans$CSR == 1 | totalPlans$CHILDONLY == 1), ]
#totalPlans <- totalPlans[!duplicated(totalPlans$PLANID), ]
totalPlans<-subset(totalPlans,actively_marketed!='false')
totalPlans<-subset(totalPlans,PLANMARKET!=2)
  