library(readr)
library(fmsb)
library(tidyverse)
library(reshape2)
library(scales)
library(directlabels)

taxesList <- c("TAXCORP in TOT as PC_GDP", "TAXGOODSERV in TOT as PC_GDP","TAXINCOME in TOT as PC_GDP",  
               "TAXPAYROLL in TOT as PC_GDP", "TAXPROPERTY in TOT as PC_GDP", "TAXREV in TOT as PC_GDP", "TAXSS in TOT as PC_GDP",
               "TAXWEDGE in TOT as PC_LC")
# All as a percent of GDP* and converted to total measures if necessary
TaxesList <- c("Corporate Tax Rate", "Goods and Services Tax Rate", "Income Tax Rate", "Payroll Tax Rate", "Property Tax Rate",
               "Tax Revenue Rate", "Social Security Contribiutions Rate", "Tax Wedge")

transfersList <- c("PUBLMPEXP in TOT as PC_GDP", "PUBUNEMPEXP in TOT as PC_GDP","SOCBENHH in INCASH as PC_GDP",
                   "SOCEXP in TOTNET as PC_GDP")
# All as a percent of GDP* and converted to total measures if necessary except for social spending which is exclusively focused on
# government transfers
TransfersList <- c("Public Spending on Labor Markets", "Public Unemployment Spending", "Social Benefits to Households", 
                   "Social Spending")

RadarCharts <- ThesisData %>% rename("Corporate Tax Rate" = "TAXCORP in TOT as PC_GDP", 
                                     "Goods and Services Tax Rate" = "TAXGOODSERV in TOT as PC_GDP",
                                     "Income Tax Rate" = "TAXINCOME in TOT as PC_GDP",  
                                     "Payroll Tax Rate" = "TAXPAYROLL in TOT as PC_GDP", 
                                     "Property Tax Rate" = "TAXPROPERTY in TOT as PC_GDP", 
                                     "Tax Revenue Rate" = "TAXREV in TOT as PC_GDP", 
                                     "Social Security Contribiutions Rate" = "TAXSS in TOT as PC_GDP", 
                                     "Tax Wedge" = "TAXWEDGE in TOT as PC_LC",
                                     "Public Spending on Labor Markets" = "PUBLMPEXP in TOT as PC_GDP", 
                                     "Public Unemployment Spending" = "PUBUNEMPEXP in TOT as PC_GDP",
                                     "Social Benefits to Households" = "SOCBENHH in INCASH as PC_GDP",
                                     "Social Spending" = "SOCEXP in TOTNET as PC_GDP") %>% arrange(Region) %>%
  filter(Year == 2015 & Country != "Colombia") %>%
  select("Corporate Tax Rate", "Goods and Services Tax Rate", "Income Tax Rate", "Payroll Tax Rate", "Property Tax Rate",
         "Tax Revenue Rate", "Social Security Contribiutions Rate", "Tax Wedge", "Public Spending on Labor Markets", 
         "Public Unemployment Spending", "Social Benefits to Households", "Social Spending",
         "Country")

TaxesTransfersData <- ThesisData %>% rename("Corporate Tax Rate" = "TAXCORP in TOT as PC_GDP", 
                                            "Goods and Services Tax Rate" = "TAXGOODSERV in TOT as PC_GDP",
                                            "Income Tax Rate" = "TAXINCOME in TOT as PC_GDP",  
                                            "Payroll Tax Rate" = "TAXPAYROLL in TOT as PC_GDP", 
                                            "Property Tax Rate" = "TAXPROPERTY in TOT as PC_GDP", 
                                            "Tax Revenue Rate" = "TAXREV in TOT as PC_GDP", 
                                            "Social Security Contribiutions Rate" = "TAXSS in TOT as PC_GDP", 
                                            "Tax Wedge" = "TAXWEDGE in TOT as PC_LC",
                                            "Public Spending on Labor Markets" = "PUBLMPEXP in TOT as PC_GDP", 
                                            "Public Unemployment Spending" = "PUBUNEMPEXP in TOT as PC_GDP",
                                            "Social Benefits to Households" = "SOCBENHH in INCASH as PC_GDP",
                                            "Social Spending" = "SOCEXP in TOTNET as PC_GDP") %>%
  filter(Country != "Colombia") %>%
  select("Corporate Tax Rate", "Goods and Services Tax Rate", "Income Tax Rate", "Payroll Tax Rate", "Property Tax Rate",
         "Tax Revenue Rate", "Social Security Contribiutions Rate", "Tax Wedge", "Public Spending on Labor Markets", 
         "Public Unemployment Spending", "Social Benefits to Households", "Social Spending",
         "Country", "Year", "Date")

# The default radar chart 
data <- RadarCharts %>% filter(Country == "United States") %>% select(-Country)

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(60,12) , rep(0,12) , data)

radarchart(data, axistype=1, title = "US Distribution of Taxation and Social Spending as a Percent of GDP",

           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,

           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,12), cglwd=0.8,

           #custom labels
           vlcex=0.8
)


# Function to generate radar charts from dataframe
radarChartGen <- function(x) {
  # The default radar chart 
  data1 <- RadarCharts %>% filter(Country == x) %>% select(-Country)
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  data1 <- rbind(rep(60,12) , rep(0,12) , data1)
  
  radarchart(data1, axistype=1, title = paste(x),
             
             #custom polygon
             pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
             
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,12), cglwd=0.8,
             
             #custom labels
             vlcex=0.8,
             na.itp = F # This fixes NA values
  )
}


# Printing each radar chart in 2x2 done by region as earlier arranged
par(mar = rep(0.8,4))
par(mfrow = c(2,2))
for (i in 1:nrow(RadarCharts)) {
  radarChartGen(x = RadarCharts$Country[i])
}









# Creating bottom and top of dataframe as 0 and 60 and adding NA at end for country for all countries radar charts
a = rep(60,12)
b = rep(0,12)
df <- as.data.frame(rbind(a, b))
df <- cbind(df, NA)
# Renaming so we can rbind
rename <- colnames(RadarCharts)
colnames(df) <- rename

RadarCharts1 <- rbind(df, RadarCharts)

# Plotting by iterating over country
# Setting color
COL <- colorRampPalette(c("red", "blue"))(nrow(RadarCharts1)-2) 
par(mfrow = c(2,2))
p <- for (i in 3:nrow(RadarCharts1)) {
  radarchart(RadarCharts1[c(1,2,i), -1], pcol = COL[i-2], cglcol = "grey80", seg = 10, 
             title = paste(RadarCharts1$Country[i], "Distribution of Taxation as a Percentage of GDP"),
             pdensity = 20, pangle = 30, pfcol = COL[i-2])
} # How to fix??
library(magick)








mycolor <- "#1c6193"








personalincomeData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_14052020000648509.csv") #id is LOCATION
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

PersonalIncomeData <- personalincomeData1 %>% filter(MEASURE == "PC_GDP") %>%
  select(TIME, Country, Value) %>% filter(TIME >= 2000) %>% 
  mutate(Country = ifelse(Country == "CHE", "Switzlerland", Country),
         Country = ifelse(Country == "CHL", "Chile", Country),
         Country = ifelse(Country == "CZE", "Czech Republic", Country),
         Country = ifelse(Country == "OAVG", "OECD Average", Country),
         Country = ifelse(Country == "TUR", "Turkey", Country),
         Country = ifelse(Country == "ISL", "Israel", Country))

PersonalIncomeData %>% ggplot(aes(x = as.Date(paste0(TIME, "-01-01")), y = Value, group = Country)) + geom_line() + 
  labs(x = "", y = "Tax Rate") + theme_minimal() + 
  ggtitle("Personal Income Tax Rate in OECD Countries") +
  geom_dl(aes(label = ifelse(Country == "OECD Average", Country, '')), method = list(dl.combine("last.points")), cex = 0.4, 
          hjust = -1) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y", limits = as.Date(c("2000-01-01","2020-01-01")))

ginicoefficientData = read_csv("/Users/dunk/Thesis/Data/DP_LIVE_29052020183720697.csv") #id is country make sure to account for country names
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

GiniCoefficientData <- ginicoefficientData1 %>% filter(SUBJECT == "GINI") %>%
  select(TIME, Country, Value) %>% filter(TIME >= 2000) %>% 
  mutate(Country = ifelse(Country == "CHE", "Switzlerland", Country),
         Country = ifelse(Country == "CHL", "Chile", Country),
         Country = ifelse(Country == "CZE", "Czech Republic", Country),
         Country = ifelse(Country == "OAVG", "OECD Average", Country),
         Country = ifelse(Country == "TUR", "Turkey", Country),
         Country = ifelse(Country == "ISL", "Israel", Country)) %>% mutate(Value = Value*100)

GiniCoefficientData %>% ggplot(aes(x = as.Date(paste0(TIME, "-01-01")), y = Value)) + geom_line() + 
  facet_wrap( ~ Country) +
  labs(x = "", y = "") + theme_minimal() + 
  ggtitle("Gini Coefficient in OECD Countries") +
  geom_dl(aes(label = ifelse(Country == "OECD Average", Country, '')), method = list(dl.combine("last.points")), cex = 0.4) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y", limits = as.Date(c("2000-01-01","2020-01-01")))
