library(dplyr)
library(tidyverse)
library(stargazer)
library(pastecs)
library(olsrr)
library(plm)
library(lfe)

wealthoecdData <- read.csv("/Users/dunk/Thesis/Data/WEALTH_03042020200404908.csv")
credsuisse2019 <- read.csv("/Users/dunk/Thesis/Data/2019WealthData.csv")
credsuisse2018 <- read.csv("/Users/dunk/Thesis/Data/2018WealthData.csv")
credsuisse2017 <- read.csv("/Users/dunk/Thesis/Data/2017WealthData.csv")
credsuisse2017$X <- NULL
credsuisse2016 <- read.csv("/Users/dunk/Thesis/Data/2016WealthData.csv")
credsuisse2015 <- read.csv("/Users/dunk/Thesis/Data/2015WealthData.csv")
credsuisse2014 <- read.csv("/Users/dunk/Thesis/Data/2014WealthData.csv")
credsuisse2013 <- read.csv("/Users/dunk/Thesis/Data/2013WealthData.csv")
credsuisse2012 <- read.csv("/Users/dunk/Thesis/Data/2012WealthData1.csv") #Solved data structure issues with excel/Numbers
credsuisse2011 <- read.csv("/Users/dunk/Thesis/Data/2011WealthData.csv")
credsuisse2010 <- read.csv("/Users/dunk/Thesis/Data/2010WealthData.csv") #Put in 0's where NA most of the time, also note that under 1000 and under 10000 columns need to be merged, no over 1 million column
credsuisse2010$X <- NULL


credsuisselist <- list(credsuisse2011, credsuisse2012, credsuisse2013, credsuisse2014, credsuisse2015, credsuisse2016, 
                       credsuisse2017, credsuisse2018, credsuisse2019)


# any(is.na(credsuisse2017)) #Make sure to remove NA's
# sapply(credsuisse2017, function(x) sum(is.na(x))) # Reveals that there are 30 missing entries for over 1 million column, 1 for under 10,000 column, and 3 for 100,000 to 1 million column

credsuisse2010 <- credsuisse2010 %>% mutate(Year = 2010)
colnames(credsuisse2010) <- c("Country", "Adults (Thousands)", "Mean Wealth per Adult", "Median Wealth per Adult", 
                              "Under 1,000", "10,00-100,00", "100,00-100,000", "Over 100,000",
                              "Total","Gini","Year") # THIS ONE JUST FOR 2010

credsuisse2019 <- credsuisse2019 %>% mutate(Year = 2019)
credsuisse2018 <- credsuisse2018 %>% mutate(Year = 2018)
credsuisse2017 <- credsuisse2017 %>% mutate(Year = 2017)
credsuisse2016 <- credsuisse2016 %>% mutate(Year = 2016)
credsuisse2015 <- credsuisse2015 %>% mutate(Year = 2015)
credsuisse2014 <- credsuisse2014 %>% mutate(Year = 2014)
credsuisse2013 <- credsuisse2013 %>% mutate(Year = 2013)
credsuisse2012 <- credsuisse2012 %>% mutate(Year = 2012)
credsuisse2011 <- credsuisse2011 %>% mutate(Year = 2011)





testdf <- do.call("rbind", list(credsuisse2019, credsuisse2018, credsuisse2017, credsuisse2016, credsuisse2015, 
                                credsuisse2014, credsuisse2013, credsuisse2012, credsuisse2011)) #Make sure to add 2012, 2010 when possible, ask Todd about 2010 - how to modify
testdf1 <- #do a left join for credsuisse2010
  
  
countryList <- unique(as.character(wealthoecdData$Country))
oecdf <- testdf %>% filter(Country %in% countryList)
colnames(oecdf) <- c("Country", "Adults (Thousands)", "Mean Wealth per Adult", "Median Wealth per Adult", 
                     "Under 10,000", "10,000-100,000", "100,000-1 Million",
                     "Over 1 Million", "Total","Gini","Year")

### Reading in OECD country wide tax/social benefit data and then merging data frame

ginicoefficientData = read_csv("/Users/dunk/Downloads/DP_LIVE_29052020183720697.csv") #id is country make sure to account for country names
personalincomeData = read_csv("/Users/dunk/Downloads/DP_LIVE_14052020000648509.csv") #id is LOCATION
taxwedgeData = read_csv("/Users/dunk/Downloads/DP_LIVE_14052020060120339.csv") #LOCATION
taxrevenueData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020175635356.csv") #LOCATION
corpprofitData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020175843841.csv") #LOCATION
socialsecuritycontribData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020175914201.csv") #LOCATION
payrolltaxData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020175940921.csv") #LOCATION
goodsandservicesData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020180027421.csv") #LOCATION
socialbenshouseholdData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020174153455.csv") #LOCATION
socialspendingData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020174206627.csv") #LOCATION
publicunempspendingData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020174212424.csv") #LOCATION
publiclabormktspendingData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020174217033.csv") #LOCATION
propertytaxData = read_csv("/Users/dunk/Downloads/DP_LIVE_28052020174310236.csv") #LOCATION
gdpData = read_csv("/Users/dunk/Downloads/DP_LIVE_04082020233322920.csv")
#See about top marginal income tax rate, indicator variable for wealth tax?

gdpData1 <- gdpData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(gdpData1)[1] <- "Country" #Ok this works

ginicoefficientData1 <- ginicoefficientData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(ginicoefficientData1)[1] <- "Country" #Ok this works

taxwedgeData1 <- taxwedgeData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(taxwedgeData1)[1] <- "Country" #Ok this works

personalincomeData1 <- personalincomeData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(personalincomeData1)[1] <- "Country" #Ok this works

taxrevenueData1 <- taxrevenueData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(taxrevenueData1)[1] <- "Country" #Ok this works

corpprofitData1 <- corpprofitData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(corpprofitData1)[1] <- "Country" #Ok this works

socialsecuritycontribData1 <- socialsecuritycontribData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(socialsecuritycontribData1)[1] <- "Country" #Ok this works

payrolltaxData1 <- payrolltaxData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(payrolltaxData1)[1] <- "Country" #Ok this works

goodsandservicesData1 <- goodsandservicesData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(goodsandservicesData1)[1] <- "Country" #Ok this works

socialbenshouseholdData1 <- socialbenshouseholdData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(socialbenshouseholdData1)[1] <- "Country" #Ok this works

socialspendingData1 <- socialspendingData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(socialspendingData1)[1] <- "Country" #Ok this works

publicunempspendingData1 <- publicunempspendingData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(publicunempspendingData1)[1] <- "Country" #Ok this works

publiclabormktspendingData1 <- publiclabormktspendingData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(publiclabormktspendingData1)[1] <- "Country" #Ok this works

propertytaxData1 <- propertytaxData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ITA", "Italy", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "JPN", "Japan", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "KOR", "Korea", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LUX", "Luxembourg", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "MEX", "Mexico", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NLD", "Netherlands", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NZL", "New Zealand", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "NOR", "Norway", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "POL", "Poland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "PRT", "Portugal", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVK", "Slovak Republic", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ESP", "Spain", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SWE", "Sweden", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GBR", "United Kingdom", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "USA", "United States", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "EST", "Estonia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "SVN", "Slovenia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LVA", "Latvia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "LTU", "Lithuania", LOCATION))
names(propertytaxData1)[1] <- "Country" #Ok this works

# colnames(personalincomeData1) just testing to make sure
# initialdata <- merge(wealthoecdData, taxwedgeData1, by = 'Country')
# Merging the dataframes function
bigdata <- do.call("rbind", list(taxrevenueData1, corpprofitData1, socialsecuritycontribData1, personalincomeData1, 
                                 payrolltaxData1, goodsandservicesData1, socialbenshouseholdData1, socialspendingData1, taxwedgeData1,
                                 publicunempspendingData1, publiclabormktspendingData1, propertytaxData1, ginicoefficientData1, gdpData1))
bigdata$FREQUENCY <- NULL 
bigdata$`Flag Codes` <- NULL
bigdata1 <- bigdata %>% mutate(new = paste(INDICATOR, "in", SUBJECT, "as", MEASURE))
bigdata1 <- subset(bigdata1, select = -c(INDICATOR, SUBJECT, MEASURE))
bigdata1 <- spread(bigdata1, new, Value)


testdata <- oecdf %>% inner_join(bigdata1, by = c("Country","Year" = "TIME")) #Solution
testdata$Total <- NULL
testdata$Year <- as.Date(as.character(testdata$Year), format = "%Y")
testdata <- testdata %>% rename(wealthGini = Gini)
testdata <- testdata %>% relocate(`INCOMEINEQ in GINI as INEQ`, .after = wealthGini)
# testdata <- testdata %>% filter(wealthGini < 100) # Removing Denmark observations that make no sense
testdata <- testdata %>% mutate(`INCOMEINEQ in GINI as INEQ` = `INCOMEINEQ in GINI as INEQ` * 100) #Adjusting so same terms as wealth ratio
euList <- c("Austria","Belgium","Denmark","Estonia","Finland", "France","Germany","Greece","Hungary","Ireland", "Italy", 
            "Latvia","Luxembourg","Netherlands", "Norway", "Poland", "Portugal", "Slovenia","Spain","United Kingdom")
americasList <- c("United States","Canada")
asiaList <- c("Japan","Korea")
oceaniaList <- c("Australia", "New Zealand")

testdata <- testdata %>% 
  mutate(Region = case_when(Country %in% euList ~ "EU", Country %in% americasList ~ "Americas", Country %in% asiaList ~ "Asia",
                            Country %in% oceaniaList ~ "Oceania"))

#Run to here for dataframe
# Next plans, run stepwise regression on all data, incorporate 2010 data fully, write based on results, test a couple different models
# Find median tax rate for most recent year, mean gini in low and high tax countries by wealth and by income
# Group by region for comparison, bivariate statistics - look at relationship between one variable and another


averageratesmean <- testdata %>% group_by(Year) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = T)))
averageratesmedian <- testdata %>% group_by(Year) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = T))) # Median tax wedge is 38.02213 (PC_LC) in 2018, 
#median property tax (PC_GDP) is 1.8360, median corporate tax is 2.7900 (PC_GDP)

inequalitytaxcountrysummary <- testdata %>% group_by(Country) %>%
  summarise(`Mean Wealth Gini by Country` = mean(wealthGini, na.rm = T), 
            `Mean Income Gini by Country` = mean(`INCOMEINEQ in GINI as INEQ`, na.rm = T),
            `Tax Wedge` = mean(`TAXWEDGE in TOT as PC_LC`, na.rm = T),
            `Property Tax` = mean(`TAXPROPERTY in TOT as PC_GDP`, na.rm = T),
            `Corporate Tax` = mean(`TAXCORP in TOT as PC_GDP`, na.rm = T)) %>% arrange(desc(`Property Tax`))

summaryofalldata <- testdata %>% group_by(Country) %>%
  summarise(`Mean Wealth Gini by Country` = mean(wealthGini, na.rm = T)) # Summary stats on wealth ineq
summaryofalldataYear <- testdata %>% group_by(Year) %>%
  summarise(`Mean Wealth Gini by Year` = mean(wealthGini, na.rm = T)) #Kind of an upward trend, but very slight

fivenum(testdata$wealthGini)
ggplot(testdata, aes(x = Year, y = wealthGini, group = Country, color = Region)) + geom_boxplot() #Boxplot and five number summary






# Plots here

ggplot(data = testdata, aes(Year, wealthGini, color = Country)) + geom_line() + facet_grid( ~ Region) # Note United Kingdom has no grouping -  should I include with EU?

ggplot(testdata, aes(x = `INCOMEINEQ in GINI as INEQ`, y = wealthGini)) + geom_point() + 
  geom_smooth(aes(x = `INCOMEINEQ in GINI as INEQ`, y = wealthGini), method = "lm") + facet_wrap( ~ Year) #clearly no relationship
summary(lm(wealthGini ~ `INCOMEINEQ in GINI as INEQ`, data = testdata)) # This probably means that there is no interaction between wealth and income inequality

testdata$`Mean Wealth per Adult` <- as.numeric(as.character(testdata$`Mean Wealth per Adult`))

testdata %>% keep(is.numeric) %>% gather(-wealthGini, key = "var", value = "value") %>%
  ggplot(aes(value, wealthGini)) + geom_point() + geom_smooth(method = "lm") + facet_wrap( ~ var, scales = "free") # graph of all wealthGini ~ variable regressions


ggplot(data = testdata, aes(x = Year, y = as.numeric(as.character(`Mean Wealth per Adult`)), color = Country)) + geom_line()

# Dummy variable for each year, wealth tax implementation/removal, dummy variable by country to test against others,
# changes in tax wedge association with changes in wealth gini - panel fixed effects regression, first difference estimator
# More to the story than bivariate relationships, panel data analysis: same units of observation multiple points in time
# FOCUS ON: Pooled OLS, First Difference Estimator, Fixed Effects Estimator
# Table 1: Summary statistics on key explanatory variables
# Table 2: Patterns in data suggrestive of my hypothesis, panel A taxes vs income, panel B taxes vs wealth
# Splitting into high or low tax based on average (or quartiles) then show average income or wealth
# Correlation coefficients between taxes and income or wealth
# Create a tax index of principal components (google for in R)

pooling <- felm(formula = wealthGini ~ `TAXWEDGE in TOT as PC_LC` + `GDP in TOT as USD_CAP`, data = testdata)
pooling1 <- felm(formula = wealthGini ~ `TAXINCOME in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, 
                 data = testdata)
pooling2 <- felm(formula = wealthGini ~ `TAXCORP in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = testdata)
pooling3 <- felm(formula = wealthGini ~ `TAXPROPERTY in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = testdata)
pooling4 <- felm(formula = wealthGini ~ `TAXWEDGE in TOT as PC_LC` + `TAXINCOME in TOT as PC_GDP` + `TAXCORP in TOT as PC_GDP` + 
                   `TAXPROPERTY in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = testdata)
pooling5 <- felm(formula = wealthGini ~ `TAXWEDGE in TOT as PC_LC` + `TAXINCOME in TOT as PC_GDP` + `TAXCORP in TOT as PC_GDP` + 
                   `TAXPROPERTY in TOT as PC_GDP` + `GDP in TOT as USD_CAP` | Country + Year, data = testdata)

stargazer(pooling, pooling1, pooling2, pooling3, pooling4, type = "text") # table 2, summary statistics
print(summary(pooling5))

pooled <- plm()


# new_data <- old_data %>% group_by(Country) %>% arrange(Year) %>% mutate(d.variable = variable-lag(variable))
pdata2 <- pdata %>% keep(is.numeric())
pooled <- plm(formula = wealthGini ~ incomeGini + , data = pdata, model = "pooling")
summary(pooled)


pdata <- pdata.frame(testdata, index = c("Year","Country"))
view(pdata)
colnames(pdata)[50] <- "Tax.Wedge_PCLC"
colnames(pdata)[13] <- "GDP_PC"
colnames(pdata)[40] <- "Tax.Income_PCGDP"
colnames(pdata)[44] <- "Tax.Property_PCGDP"
colnames(pdata)[36] <- "Tax.Corporate_PCGDP"
colnames(pdata)[10] <- "incomeGini"
# Check taxes in both linear and first difference models as explanatory models
fd <- plm(formula = wealthGini ~ Tax.Wedge_PCLC - 1, data = pdata, model = "fd")
summary(fd)
fd1 <- plm(formula = wealthGini ~ GDP_PC+Tax.Wedge_PCLC+Tax.Income_PCGDP+Tax.Corporate_PCGDP+Tax.Property_PCGDP- 1, data = pdata, model = "fd")
print(summary(fd1)) # Taking first differences so no constant, all variables used in differences


data_demeaned <- testdata
colnames(data_demeaned)[10] <- "incomeGini"
data_demeaned <- data_demeaned %>% mutate(wealthGini = wealthGini - ave(wealthGini, Country)) %>% 
  mutate(newGini = incomeGini - ave(incomeGini, Country, FUN=function(x) mean(x, na.rm=T)))

summary(lm(wealthGini ~ incomeGini - 1, data = data_demeaned)) # 

fe_mod <- plm(wealthGini ~ incomeGini, data = pdata, index = c("Country", "Year"), model = "within")
print(summary(fe_mod))
mean(fixef(fe_mod))
fe_mod$formula
?plm
coeftest(fe_mod, vcov. = vcovHC, type = "HC1") # 
# Be careful with first difference models - not super important because fd picks up relationship between one year change in something and one
# year change in wealth inequality, which is very short term

# Regressions here
WealthCountryRegression <- testdata %>% group_by(Country) %>% 
  do(model = lm(wealthGini ~ `INCOMEINEQ in GINI as INEQ`, data = .)) %>% tidy(model) #Makes a dataframe of regression tables for each country
WealthRegionRegression <- testdata %>% group_by(Region) %>% 
  do(model = lm(wealthGini ~ `INCOMEINEQ in GINI as INEQ`, data = .)) %>% tidy(model)

#Running a regression on each part of the dataframe
testdata2 <- testdata %>% keep(is.numeric)
model <- lm(wealthGini ~ ., data = testdata2) # Fix
summary(model)
k <- ols_step_all_possible(model)
### TRY THIS IN A SEC
# df <- data.frame(x=rnorm(100),y1=rnorm(100),y2=rnorm(100))
# 
testdata3 <- testdata2 %>% gather(measure, value, -wealthGini) %>% nest(-measure) %>% 
  mutate(fit = map(testdata2, ~ lm(wealthGini ~ value, data = )), tidied = map(fit, tidy)) %>% unnest(tidied)
# result <- df %>%
#   gather(measure, value, -x) %>%
#   nest(-measure) %>%
#   mutate(fit = map(data, ~ lm(value ~ x, data = .x)),
#          tidied = map(fit, tidy)) %>%
#   unnest(tidied)

























## Comparison of Credit Suisse data with OECD wealth database
wealthoecdData$COUNTRY <- NULL # useless
wealthoecdData$POPULATION <- NULL # useless
wealthoecdData$Population <- NULL # useless
wealthoecdData$TIME <- NULL # redundant
wealthoecdData$COUNTRY <- NULL
wealthoecdData$COUNTRY <- NULL
wealthoecdData$Flag.Codes <- NULL
wealthoecdData$VAR <- NULL
colnames(wealthoecdData) <- c("Country", "Variable", "Year","Value","Flags")
wealthoecdData1 <- spread(wealthoecdData, Variable, Value) %>% arrange(Year)

a1 <- wealthoecdData1 %>% select(Country, Year, `Mean net wealth per household (current prices)`, 
                                 `Median net wealth per household (current prices)`)
a2 <- testdata %>% select(Country, Year, `Mean Wealth per Adult`, `Median Wealth per Adult`)

comparison <- inner_join(a1, a2, by = c("Country", "Year"))

























bigdata1 <- subset(bigdata, TIME > 2009)
bigdata1$FREQUENCY <- NULL
colnames(bigdata1) <- c("Country", "Variable", "Subject", "Measure", "Year", "Value", "Flags")

# This is fully OECD data
biggestdata <- bigdata1 %>% full_join(wealthoecdData, by = c("Country","Variable","Year","Value","Flags"))

biggestdata1 <- spread(biggestdata, Variable, Value)

# Testing just income inequality
incomeineq <- bigdata1  %>% filter(Country %in% countryList) %>% 
  filter(Measure == "INCOMEINEQ") %>% 
  filter(Subject == "GINI")

colnames(incomeineq)[6] <- "Year"
comparisondf <- oecdf %>% group_by(Country, Year) %>% left_join(incomeineq, by = c("Country", "Year"))

ggplot(comparisondf, aes(x = Value, y = Gini, color = Country)) + geom_point() + facet_wrap( ~ Year) + 
  geom_text(aes(label = ifelse(Value > 0.35, as.character(Country),''))) # Note that the Denmark entries are as in pdfs...


ols1 <- lm(wealthGini ~ I(100*Value), data = comparisondf)
summary(ols1)

# Merging Credit Suisse and OECD supplementary paramater data (rethink what needs to be controlled)

oecdsuppData <- spread(bigdata1, Variable, Value)

oecdfullData <- oecdf %>% left_join(oecdsuppData, by = c("Country","Year"))

oecdfullData$Year <- as.Date(as.character(oecdfullData$Year), format = "%Y")

colnames(oecdfullData)[names(oecdfullData)=="Gini"] <- "wealthGini"

ggplot(data = oecdfullData, aes(x = TAXCORP, y = wealthGini, color = Country)) + geom_line() + facet_wrap( ~ Year)

ggplot(oecdfullData, aes(x = Year, y = Gini, color = Country)) + geom_line()






