### Code used to make initial dataframe - periodically updated
library(readr)

wealthoecdData <- read.csv("/Users/dunk/Thesis/Data/WEALTH_03042020200404908.csv") # From wealth OECD database which is woefully inadequate
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


# any(is.na(credsuisse2017)) #NA'S in 2017 Credit Suisse data are only from percentile wealth columns, can be ignored for now, emailing reasearchers
# sapply(credsuisse2017, function(x) sum(is.na(x))) # Reveals that there are 30 missing entries for over 1 million column, 1 for under 10,000 column, and 3 for 100,000 to 1 million column

credsuisse2010 <- credsuisse2010 %>% mutate(Year = 2010)
colnames(credsuisse2010) <- c("Country", "Adults (Thousands)", "Mean Wealth per Adult", "Median Wealth per Adult", 
                              "Under 1,000", "10,00-100,00", "100,00-100,000", "Over 100,000",
                              "Total","Gini","Year") # THIS ONE JUST FOR 2010

credsuisse2019 <- credsuisse2019 %>% mutate(Year = 2019)
credsuisse2019 <- credsuisse2019 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2018 <- credsuisse2018 %>% mutate(Year = 2018)
credsuisse2018 <- credsuisse2018 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2017 <- credsuisse2017 %>% mutate(Year = 2017)
credsuisse2017 <- credsuisse2017 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2016 <- credsuisse2016 %>% mutate(Year = 2016)
credsuisse2016 <- credsuisse2016 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2015 <- credsuisse2015 %>% mutate(Year = 2015)
credsuisse2015 <- credsuisse2015 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2014 <- credsuisse2014 %>% mutate(Year = 2014)
credsuisse2014 <- credsuisse2014 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2013 <- credsuisse2013 %>% mutate(Year = 2013)
credsuisse2013 <- credsuisse2013 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2012 <- credsuisse2012 %>% mutate(Year = 2012)
credsuisse2012 <- credsuisse2012 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2011 <- credsuisse2011 %>% mutate(Year = 2011)
credsuisse2011 <- credsuisse2011 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))

credsuisselist <- list(credsuisse2011, credsuisse2012, credsuisse2013, credsuisse2014, credsuisse2015, credsuisse2016, 
                       credsuisse2017, credsuisse2018, credsuisse2019)

testdf <- do.call("rbind", list(credsuisse2019, credsuisse2018, credsuisse2017, credsuisse2016, credsuisse2015, 
                                credsuisse2014, credsuisse2013, credsuisse2012, credsuisse2011)) #2010 is added later because different columns
write_csv(testdf, "/Users/dunk/Thesis/Data/WealthDataAllCountries.csv")
  
countryList <- unique(as.character(wealthoecdData$Country)) #List of OECD Countries
countryList <- append(countryList, x = c("Colombia","Czech Republic","Iceland","Israel","Lithuania","Mexico","Sweden","Switzerland","Turkey"))

oecdf <- testdf %>% filter(Country %in% countryList)
colnames(oecdf) <- c("Country", "Adults (Thousands)", "Mean Wealth per Adult", "Median Wealth per Adult", 
                     "Under 10,000", "10,000-100,000", "100,000-1 Million",
                     "Over 1 Million", "Total","Gini","Year")

credsuisse2010 <- credsuisse2010 %>% mutate(Country = gsub("Slovakia", "Slovak Republic", Country))
credsuisse2010 <- credsuisse2010 %>% filter(Country %in% countryList)
credsuisse2010 <- subset(credsuisse2010, select = -c(`Under 1,000`, `10,00-100,00`, `100,00-100,000`,`Over 100,000`))
credsuisse2010 <- credsuisse2010 %>% mutate(`Under 10,000` = paste(NA), `10,000-100,000` = paste(NA), 
                          `100,000-1 Million` = paste(NA), `Over 1 Million` = paste(NA))


oecdf <- oecdf %>% full_join(credsuisse2010)
write_csv(oecdf, "/Users/dunk/Thesis/Data/OECDFWealthDataframe.csv") # PRIMARY DATAFRAME
# read_csv("/Users/dunk/Thesis/Data/OECDFWealthDataframe.csv") to read in data

### Really bad code here, should have written a function

ginicoefficientData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_29052020183720697.csv") #id is country make sure to account for country names
personalincomeData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_14052020000648509.csv") #id is LOCATION
taxwedgeData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_14052020060120339.csv") #LOCATION
taxrevenueData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020175635356.csv") #LOCATION
corpprofitData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020175843841.csv") #LOCATION
socialsecuritycontribData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020175914201.csv") #LOCATION
payrolltaxData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020175940921.csv") #LOCATION
goodsandservicesData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020180027421.csv") #LOCATION
socialbenshouseholdData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020174153455.csv") #LOCATION
socialspendingData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020174206627.csv") #LOCATION
publicunempspendingData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020174212424.csv") #LOCATION
publiclabormktspendingData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020174217033.csv") #LOCATION
propertytaxData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_28052020174310236.csv") #LOCATION
gdpData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_04082020233322920.csv")
#See about top marginal income tax rate, indicator variable for wealth tax?

gdpData1 <- gdpData %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "AUT", "Austria", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "BEL", "Belgium", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CAN", "Canada", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
  mutate(LOCATION = if_else(LOCATION == "CHL", "Chile", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "COL", "Colombia", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "CZE", "Czech Republic", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "DNK", "Denmark", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FIN", "Finland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "FRA", "France", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "DEU", "Germany", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "GRC", "Greece", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "HUN", "Hungary", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISL", "Iceland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "IRL", "Ireland", LOCATION)) %>% 
  mutate(LOCATION = if_else(LOCATION == "ISR", "Israel", LOCATION)) %>% 
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
  mutate(LOCATION = if_else(LOCATION == "CHE", "Switzerland", LOCATION)) %>%
  mutate(LOCATION = if_else(LOCATION == "TUR", "Turkey", LOCATION)) %>%
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
# Maybe add proportion of retired people, social wealth fund as a dummy variable
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
testdata <- testdata %>% rename(incomeGini = `INCOMEINEQ in GINI as INEQ`)
# testdata <- testdata %>% filter(wealthGini < 100) # Removing Denmark observations that make no sense
testdata <- testdata %>% mutate(incomeGini = incomeGini * 100) #Adjusting so same terms as wealth ratio
euList <- c("Austria","Belgium","Czech Republic","Denmark","Estonia","Finland", "France","Germany","Greece","Hungary","Ireland", "Italy", 
            "Latvia","Lithuania","Luxembourg","Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia","Spain",
            "Sweden", "Switzerland", "Turkey", "United Kingdom")
americasList <- c("United States","Canada", "Mexico","Chile","Colombia")
asiaList <- c("Japan","Korea")
oceaniaList <- c("Australia", "New Zealand") #Israel not accounted for regionally

testdata <- testdata %>% 
  mutate(Region = case_when(Country %in% euList ~ "EU", Country %in% americasList ~ "Americas", Country %in% asiaList ~ "Asia",
                            Country %in% oceaniaList ~ "Oceania"))

# Run to here for dataframe make sure to include all the code above

write_csv(testdata, "/Users/dunk/Thesis/Data/PanelData.csv")



# Useful code
`%notin%` <- Negate(`%in%`) #Creating a notin functionj
countryList %notin% testdata$Country #The only country not in the dataframe is the third one in country list which is Iceland
sapply(testdata, function(x) length(unique(x))) # Gets unique number of values in each column - there are 36 countries












