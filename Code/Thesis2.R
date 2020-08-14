library(readxl)
library(tidyverse)
library(dplyr)
library(stargazer)

giniRegressionData = read_excel("/Users/dunk/Downloads/WIID_06MAY2020.xlsx")
personalIncomeData = read_csv("/Users/dunk/Downloads/DP_LIVE_14052020000648509.csv")
taxWedgeData = read_csv("/Users/dunk/Downloads/DP_LIVE_14052020060120339.csv")
?join
view(giniRegressionData)
#Subsetting to just year of 2018
GiniRegressionData <- subset(giniRegressionData, year == 2018)

PersonalIncomeData <- subset(personalIncomeData, TIME == 2018)
PersonalIncomeData2 <- subset(personalIncomeData, TIME == 2018)

TaxWedgeData <- subset(taxWedgeData, TIME == 2018)
#Subsetting personal income data so it is just percentage of gdp, making a separate data frame for total taxation
PersonalIncomeData <- subset(PersonalIncomeData, MEASURE == "PC_GDP")
PersonalIncomeData2 <- subset(PersonalIncomeData2, MEASURE == "PC_TOT_TAX")

#Removing non-OECD countries and two redundant entries for the US and Turkey
GiniRegressionData <- GiniRegressionData[-c(1,3,4,6,7,8,10,11,12,13,14,16,17,18,23,24,
                                            29,34,35,36,39,48,49,50,51,52,53,54,55),]
GiniRegressionData <- GiniRegressionData[-c(23,25),]
#Adding wealth gini to data
GiniRegressionData["gini_wealth"] <- NA
GiniRegressionData$gini_wealth <- c(76.4,65.9,80.7,83.6,71.1,76.7,81.6,68.2,66.2,
                                    68.9,78.8,65.5,80,73.6,70.8,
                                    79.1,72.2,73.6,64.6,69.7,86.5,87.1,74.7,85.2)
GiniRegressionData$gini_wealth
#Adding personal income to data
GiniRegressionData["tax_as_gdp"] <- NA
GiniRegressionData$tax_as_gdp <- c(9.365, 12.113, NA, 24.422, 5.5, 12.309, 10.37, 6.23, 5.17, 10.77, 5.9, 4.08, 3.42, 7.97, 12.05, 9.87, 5.31,
                                   6.61, 5.32, 7.62, 12.74, 3.75, 9.12, 9.91)
GiniRegressionData["tax_tot"] <- NA
GiniRegressionData$tax_tot <- c(22.2, 27.011, NA, 54.439, 16.457, 28.851, 27.164, 16.01, 14.159, 25.613, 19.188, 13.472, 21.38, 20.57, 36.871,
                                25.297, 15.172, 18.662, 14.611, 22.141, 28.991, 15.421, 27.195, 40.725)
#Adding tax wedge to data
GiniRegressionData["tax_wedge"] <- NA
GiniRegressionData$tax_wedge <- c(47.618571, 52.720688, NA, 35.369717, 36.169987, 42.448694, 49.527267, 40.955021, 45.041322, 47.769159,
                                  42.586822, 40.661721, 19.748265, 37.810053, 18.436050, 35.810379, 35.760358, 40.759037, 43.210380, 
                                  39.383498, 42.990994, 39.247185, 30.943337, 29.598968)

#Make sure its ok to have an NA for Colombia here, also ask Jerman why view() function isnt displaying added columns 
view(GiniRegressionData)


giniRegression = lm(gini_wealth ~ gini_reported, data = GiniRegressionData)
personalincometaxgdpRegression = lm(gini_wealth ~ tax_as_gdp, data = GiniRegressionData) #crazy results here as personal income tax as a percent of gdp increases wealth inequality also increases
personalincometaxtotRegression = lm(gini_wealth ~ tax_tot, data = GiniRegressionData) #slightly lower but this time statistically significant result that as personal income taxes increase there is an increase in wealth inequality
taxwedgeRegression = lm(gini_wealth ~ tax_wedge, data = GiniRegressionData) #This makes sense, native correlation
summary(lm(gini_wealth ~ tax_as_gdp + tax_wedge, data = GiniRegressionData))
#Next add tax on corporate profits, and social security contributions? (some type of social welfare accounting), property tax and inheritance tax also good
#How should I combine variables into regression, how to not overfit data, panel data

summary(taxwedgeRegression)
summary(personalincometaxgdpRegression)
summary(personalincometaxtotRegression)
#potentially higher income taxes as a reaction to higher inequality, panel data so countries effects over time can be tested, cor(x, method = c("pearson", "kendall", "spearman"))
summary(giniRegression)
#Initial finding: for each 0.34% increase in the GINI of income inequality there is a 1 percentage point increase in wealth inequality

#Adding interaction term for EU based on descriptive stats

GiniRegressionData <- GiniRegressionData %>% mutate(eu = case_when(eu == "EU" ~ 1, eu == "Non-EU" ~ 0))
giniRegressionEU = lm(gini_wealth ~ gini_reported*eu, data = GiniRegressionData)
summary(giniRegressionEU)
#Very interesting results, interaction term suggests there is a negative relationship between income inequality and wealth inequality for eu countries.


#Find which deciles are most correlated with wealth inequality, see about adding deciles for wealth

giniRegressionDec1 = lm(gini_wealth ~ d1, data = GiniRegressionData)
giniRegressionDec2 = lm(gini_wealth ~ d2, data = GiniRegressionData)
giniRegressionDec3 = lm(gini_wealth ~ d3, data = GiniRegressionData)
giniRegressionDec4 = lm(gini_wealth ~ d4, data = GiniRegressionData)
giniRegressionDec5 = lm(gini_wealth ~ d5, data = GiniRegressionData)
giniRegressionDec6 = lm(gini_wealth ~ d6, data = GiniRegressionData)
giniRegressionDec7 = lm(gini_wealth ~ d7, data = GiniRegressionData)
giniRegressionDec8 = lm(gini_wealth ~ d8, data = GiniRegressionData)
giniRegressionDec9 = lm(gini_wealth ~ d9, data = GiniRegressionData)
giniRegressionDec10 = lm(gini_wealth ~ d10, data = GiniRegressionData)
stargazer(giniRegressionDec1, giniRegressionDec2, giniRegressionDec3, 
          giniRegressionDec4, giniRegressionDec5, giniRegressionDec6, giniRegressionDec7, 
          giniRegressionDec8, giniRegressionDec9, giniRegressionDec10 ,type = "text")
view(GiniRegressionData)
summary(lm(gini_wealth ~ top5, data = GiniRegressionData))
#Only significant p-value is d8 which is very negative (-5.744) and only positive decile is d10 suggesting extreme wealth concentration at the top is positively correlated with higher income inequality
#Try with quintiles

##Start descriptive statistics

#Non EU countries have positive income and wealth gini correlation, significantly negative correlation for EU countries, social wealth, taxes to explain?
ggplot(data = GiniRegressionData, aes(x = gini_reported, y = gini_wealth, color = eu)) + 
  geom_point() + geom_smooth(aes(gini_reported*eu, gini_wealth), method = "lm", se = FALSE) + 
  geom_smooth(aes(gini_reported, gini_wealth), method = "lm", se = FALSE, color = "black") +
  theme_bw() + xlab("Income GINI") + ylab("Wealth GINI")
  
ggplot(data = GiniRegressionData, aes(x = gini_reported, y = gini_wealth, color = factor(eu))) + 
  geom_point() + geom_smooth(aes(group = eu), method = "lm", se = FALSE) +
  theme_bw() + xlab("Income GINI") + ylab("Wealth GINI")
#Not especially significant but by continent
ggplot(data = GiniRegressionData, aes(x = gini_reported, y = gini_wealth, color = region_wb)) + geom_point()

#Something new, probably mean median ratio
ggplot(data = GiniRegressionData, aes()) + geom_col()


#Wealth mean-median and income mean-median

df <- transform(GiniRegressionData, MeanMedianRatio = mean/median)
names(df)
view(df)
# if(any(is.na(df[,"MeanMedianRatio"]))) {True} Test for null values
# sum(is.na(df$MeanMedianRatio)) Better way to count null values
#Rename country column then merge datasets
# df %>% rename(Country = country) WHY DOESNT THIS WORK
names(df)[2] <- "Country" #Ok this works
colnames(df)
#Check variable names in datasets
jointdataset <- merge(df, WealthRatio3, by = 'Country')
view(jointdataset)
colnames(jointdataset)

#Plotting 
ggplot(data = jointdataset, aes(x = MeanMedianRatio, y = Value, color = region_wb)) + geom_point() + 
  geom_text(aes(label = ifelse(Value > 2.5, as.character(Country),'')))

ols1 = lm(Value ~ MeanMedianRatio, data = jointdataset)
anova(ols1)
summary(ols1)

x <- matrix(c(1,2,1,
       3,6,3,
       4,1,4),nrow = 3)
x[,2]
x
?tail


