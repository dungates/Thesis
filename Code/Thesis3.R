library(dplyr)
library(tidyverse)
library(stargazer)


#Figure out how to include documentation with each download, if OECD summary can be put in the help part of RStudio
wealthoecdData <- read.csv("/Users/dunk/Downloads/WEALTH_03042020200404908.csv") #COUNTRY
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
#See about top marginal income tax rate, indicator variable for wealth tax?


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
gini
bigdata <- do.call("rbind", list(taxrevenueData1, corpprofitData1, socialsecuritycontribData1, personalincomeData1, 
                      payrolltaxData1, goodsandservicesData1, socialbenshouseholdData1, socialspendingData1, taxwedgeData1,
                      publicunempspendingData1, publiclabormktspendingData1, propertytaxData1, ginicoefficientData1))

testdata <- oecdf %>% inner_join(bigdata, by = c("Country","Year" = "TIME")) #Solution


#merging to wealth data frame
biggestdata <- merge(wealthoecdData, bigdata, by = 'Country')
# view(biggestdata)
nrow(biggestdata)
colnames(biggestdata)
#Make above a function

categories <- unique(biggestdata$Time) # We have data from 2012 2014 2011 2010 2016 2015 2013 2009
categories2 <- unique(biggestdata$Country) # We have data on Australia, Austria, Belgium, Canada, Denmark, Estonia, Finland,
# France, Germany, Greece, Hungary, Ireland, Italy, Japan, Korea, Latvia, Luxembourg, Netherlands, New Zealand, Norway, Poland,
# Portugal, Slovak Republic, Slovenia, Spain, United Kingdom, United States
categories3 <- unique(biggestdata$Value.x) # How to manipulate data now that viewing crashes Rstudio
categories4
categories4 <- unique(ginicoefficientData1$SUBJECT)
# Merge by country and time, do value sorting before making big data frame, ask Jerman if can't do
biggestdata1 <- spread(biggestdata, Variable, Value.x)

ggplot(biggestdata1, aes(x = Country, y = `Mean to median net wealth ratio`)) + 
  geom_col(color = "steelblue") + 
  facet_wrap( ~ Time) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


thesisplot1 <- biggestdata1 %>% filter(Time == 2012)
thesisplot2 <- biggestdata1 %>% filter(Time == 2016)
ggplot(data = thesisplot1, aes(x = Country, y = `Mean to median net wealth ratio`, color = Time)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data = thesisplot1, aes(x = Country, y = `Mean net wealth per household (current prices)`, color = Time)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## REORGANIZING biggestdata1

bigdata1 <- subset(bigdata, TIME > 2009)
bigdata1$FREQUENCY <- NULL
colnames(bigdata1) <- c("Country", "Variable", "Subject", "Measure", "Time", "Value", "Flags")



ggplot(bigdata1, aes(x = Country, y = SOCBENHH)) + geom_point() + facet_wrap( ~ TIME) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Reformatting wealthoecdData

wealthoecdData$COUNTRY <- NULL # useless
wealthoecdData$POPULATION <- NULL # useless
wealthoecdData$Population <- NULL # useless
wealthoecdData$TIME <- NULL # redundant
wealthoecdData$COUNTRY <- NULL
wealthoecdData$COUNTRY <- NULL
wealthoecdData$Flag.Codes <- NULL
wealthoecdData$VAR <- NULL

wealthoecdData1 <- spread(wealthoecdData, Variable, Value) %>% arrange(Time)








# Plotting to show the US is a consistent outlier
uswealthocdData <- wealthoecdData1 %>% filter(Country == "United States")

ggplot(wealthoecdData1, aes(x = `Mean net wealth per household (current prices)`, y = `Share of top 5% of wealth`, color = Country)) +
  geom_point() +
  facet_grid( ~ Time) + theme(legend.position = "none") + 
  geom_text(aes(label = ifelse(`Share of top 5% of wealth` > 50, as.character(Country),'')))



