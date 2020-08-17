library(dplyr)
library(tidyverse)
library(stargazer)
library(plm)
library(lfe)
library(car)
library(reshape2)
library(ggpmisc)
library(lme4)

ThesisData <- read_csv("/Users/dunk/Thesis/Data/PanelData.csv")

#Extra dataframe
pdata <- pdata.frame(testdata, index = c("Year","Country"))
colnames(pdata)[50] <- "Tax.Wedge_PCLC"
colnames(pdata)[13] <- "GDP_PC"
colnames(pdata)[40] <- "Tax.Income_PCGDP"
colnames(pdata)[44] <- "Tax.Property_PCGDP"
colnames(pdata)[36] <- "Tax.Corporate_PCGDP"
colnames(pdata)[10] <- "incomeGini"

## List of codes 
# INCOMEINEQ = https://data.oecd.org/inequality/income-inequality.htm
# TAXINCOME = Income tax https://data.oecd.org/tax/tax-on-personal-income.htm#indicator-chart
# TAXWEDGE = Tax wedge https://data.oecd.org/tax/tax-wedge.htm#indicator-chart
# TAXREV = Tax revenue https://data.oecd.org/tax/tax-revenue.htm#indicator-chart
# TAXCORP = Tax on corporate profits https://data.oecd.org/tax/tax-on-corporate-profits.htm#indicator-chart Norway stands out
# TAXSS = Social security contributions https://data.oecd.org/tax/social-security-contributions.htm#indicator-chart
# TAXPAYROLL = Tax on payroll https://data.oecd.org/tax/tax-on-payroll.htm#indicator-chart
# TAXGOODSERV = Tax on goods and services https://data.oecd.org/tax/tax-on-goods-and-services.htm#indicator-chart
# SOCBENHH = Social benefits to households https://data.oecd.org/socialexp/social-benefits-to-households.htm#indicator-chart
# PUBLMPEXP = Public spending on labour markets https://data.oecd.org/socialexp/public-spending-on-labour-markets.htm#indicator-chart
# TAXPROPERTY = Tax on property https://data.oecd.org/tax/tax-on-property.htm#indicator-chart
# PUBUNEMPEXP = Public unemployment spending https://data.oecd.org/socialexp/public-unemployment-spending.htm
# SOCEXP = Social spending https://data.oecd.org/socialexp/social-spending.htm#indicator-chart
# GDP = Gross Domestic Product https://data.oecd.org/gdp/gross-domestic-product-gdp.htm

### REGRESSIONS

summary(lm(wealthGini ~ incomeGini, data = ThesisData)) # This probably means that there is no interaction between wealth and income inequality



## OLS Modelling
#Social benefits model
socspendmodel1 <- lm(wealthGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP`, data = ThesisData)
socspendmodel2 <- lm(wealthGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP` + `TAXSS in TOT as PC_GDP`, data = ThesisData)
socspendmodel3 <- lm(wealthGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP` + `TAXSS in TOT as PC_GDP` + `SOCBENHH in INCASH as PC_GDP`, data = ThesisData)
socspendmodel4 <- lm(wealthGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP` + `TAXSS in TOT as PC_GDP` + `SOCBENHH in INCASH as PC_GDP` + 
                       `SOCEXP in PUB as PC_GDP`, data = ThesisData)
socspendmodel5 <- lm(wealthGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP` + `TAXSS in TOT as PC_GDP` + `SOCBENHH in INCASH as PC_GDP` + 
                       `SOCEXP in PUB as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
socspendmodel6 <- lm(wealthGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP` + `TAXSS in TOT as PC_GDP` + `SOCBENHH in INCASH as PC_GDP` + 
                       `SOCEXP in PUB as PC_GDP` + `GDP in TOT as USD_CAP` + incomeGini, data = ThesisData)

#Tax model
taxmodel1 <- lm(wealthGini ~ `TAXINCOME in TOT as PC_GDP` + `TAXPAYROLL in TOT as PC_GDP` + `TAXGOODSERV in TOT as PC_GDP` +
                  `TAXCORP in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel2 <- lm(wealthGini ~ `TAXINCOME in TOT as PC_GDP` + `TAXPAYROLL in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel3 <- lm(wealthGini ~ `TAXCORP in TOT as PC_GDP` + `TAXGOODSERV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel4 <- lm(wealthGini ~ `TAXINCOME in TOT as PC_GDP` + `TAXPAYROLL in TOT as PC_GDP` + 
                  `TAXGOODSERV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel5 <- lm(wealthGini ~ `TAXINCOME in TOT as PC_GDP` + `TAXPAYROLL in TOT as PC_GDP` + 
                  `TAXGOODSERV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel6 <- lm(wealthGini ~ `TAXINCOME in TOT as PC_GDP` + `TAXPAYROLL in TOT as PC_GDP` + 
                  `TAXGOODSERV in TOT as PC_GDP` + `TAXCORP in TOT as PC_GDP` + `TAXREV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel7 <- lm(wealthGini ~ `TAXWEDGE in TOT as PC_LC` + `TAXCORP in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)


stargazer(taxmodel1, taxmodel2, taxmodel3, taxmodel4, taxmodel5, taxmodel6, taxmodel7, type = "text")
stargazer(socspendmodel1, socspendmodel2, socspendmodel3, socspendmodel4, socspendmodel5, socspendmodel6, type = "text")
# Seems like after accounting for income inequality a lot more of the variation in wealth inequality is explained by social spending than by taxes
# Test a couple other measures of policy - capital gains taxation and tax revnue by some measure
# Income and payroll tax consistently increase the wealthGini even after account for GDP, and other taxes including revenue as a percent of GDP
# Dummy variable for social welfare - decide which metric to use?
# Do some time series stuff and test same models with incomeGini as response variable



## Fixed Effects Models
fe1 <- plm(wealthGini ~ incomeGini, data = pdata, index = c("Country", "Year"), model = "within")
print(summary(fe1))
mean(fixef(fe1))
coeftest(fe1, vcov. = vcovHC, type = "HC1") # Be careful with first difference models - not super important because fd picks up 
# relationship between one year change in something and one year change in wealth inequality, which is very short term
summary(fixef(fe1, type = "dmean")) # Nothing here

fe2 <- plm(wealthGini ~ incomeGini, data = pdata, index = c("Country", "Year"), model = "within", effect = "twoways")
summary(fixef(fe2, type = "dmean")) # Definitely no fixed effects


## First Difference Models
fd1 <- plm(formula = wealthGini ~ Tax.Wedge_PCLC - 1, data = pdata, model = "fd")
summary(fd1)



## Random Effects Models
re1 <- plm(formula = wealthGini ~ incomeGini + GDP_PC, data = pdata, model = "random") # First rename all variables in pdata
summary(re1)




## Pooling Models
pooled1 <- plm(formula = wealthGini ~ incomeGini, data = pdata, model = "pooling")
summary(pooled1)

pooled2 <- pggls(wealthGini ~ log(GDP_PC) + incomeGini, data = pdata, model = "pooling")
summary(pooled2)


###
### PLOTS
###

#Regional plot
ggplot(data = ThesisData, aes(Year, wealthGini, color = Country)) + geom_line() + facet_grid( ~ Region) # Note United Kingdom is included with EU

# Yearly relationship between income and wealth
ggplot(ThesisData, aes(x = incomeGini, y = wealthGini)) + geom_point(aes(color = Region)) + 
  geom_smooth(aes(x = incomeGini, y = wealthGini), method = "lm", formula = y ~ x) + facet_wrap( ~ Year) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = y ~ x, parse = T) +
  geom_text(aes(label = ifelse(incomeGini > 40, Country, '')), vjust = -1) #clearly no relationship, 
                                                                           #for some reason Mexico, Chile, and Turkey are income outliers

# Wealth regressed on all OECD variables
ThesisData %>% keep(is.numeric) %>% gather(-wealthGini, key = "var", value = "value") %>%
  ggplot(aes(value, wealthGini)) + geom_point() + geom_smooth(method = "lm") + facet_wrap( ~ var, scales = "free") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = y ~ x, parse = T) 
# Variables that stand out as negatively related: TAXSS in TOT as PC_TOT_GDP, TAXWEDGE in TOT as PC_LC, PUBLMPEXP in JOBCREATION as PC_GDP 
# Variables that stand out as positively related: TAXPAYROLL in TOT as PC_TOT_GDP, TAXINCOME in TOT as PC_GDP, SOCEXP in PRIV as PC_GDP,
# PUBLMPEXP in PUBEMPSERV as PC_GDP, PUBLMPEXP in SUPEMP as PC_GDP
#account for variables that are factors in regressions

# Income regressed on all OECD varibles
ThesisData %>% keep(is.numeric) %>% gather(-incomeGini, key = "var", value = "value") %>%
  ggplot(aes(value, incomeGini)) + geom_point() + geom_smooth(method = "lm") + facet_wrap( ~ var, scales = "free") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), formula = y ~ x, parse = T)
# VERY strong relationships between many tax variables and social expenditures here, make prettier for easier comparison
# Try two axis plot, and side by side comparisons with wealth


ggplot(data = ThesisData, aes(x = Year, y = as.numeric(as.character(`Mean Wealth per Adult`)), color = Country)) + geom_line() +
  scale_y_continuous(labels = scales::comma) + labs(y = "Mean Wealth per Adult") +
  geom_text(aes(label = Country), data = ThesisData %>% filter(Year == "2019-08-13"), 
            color = "black", hjust = -0.05, size = 3, nudge_x = 0.5) +
  guides(color = "none") + coord_cartesian(xlim = c(as.Date("2010-08-13"), as.Date("2020-08-13")))











# Figure 1

ggplot()

# Figure 2
figure2data <- ThesisData %>% filter(Year == "2019-08-13")
figure2data <- melt(figure2data, id.var = "Country", measure.vars = c("Mean Wealth per Adult","Median Wealth per Adult"))
ggplot(data = figure2data, aes(x = Country, y = value, fill = variable)) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("red","blue")) + 
  scale_y_continuous(labels = scales::comma) + theme_bw() + labs(y = "Mean vs. Median Wealth in 2019") + 
  ggtitle("Mean vs. Median Wealth in 2019") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) # Organize highest to lowest mean or median wealth - or regional groupings

# Figure 3a
figure3data <- ThesisData %>% filter(Year == "2019-08-13")
figure3data <- melt(figure3data, id.vars = c("Country", "Region"), measure.vars = c("Under 10,000","10,000-100,000","100,000-1 Million","Over 1 Million"))
figure3data$value <- as.numeric(figure3data$value)
ggplot(data = figure3data, aes(x = Country, y = value, fill = variable)) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::comma) + theme_bw() + labs(y = "Percent in Group") + 
  ggtitle("Inequality Regionally and Grouped in 2019") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) #Same thing as previous graph organize by greatest amount in percentile over 1 million



# Figure 3b
ggplot(data = ThesisData, aes(x = incomeGini, y = wealthGini, color = Region)) + geom_point() + labs(x = "Income Gini", y = "Wealth Gini") +
  theme_bw()
  
#Ask Todd how to deal with Israel regionality



# Figure 4
figure4data <- ThesisData %>% filter(Year == "2017-08-13")
figure4data <- melt(figure4data, id.var = "Country", measure.vars = c("incomeGini","wealthGini"))
ggplot(data = figure4data, aes(x = Country, y = value, fill = variable)) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("purple","forestgreen")) + theme_bw() + labs(y = "Wealth Gini", x = "Income Gini") + 
  ggtitle("Wealth and Income Ginis in 2017") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) # Reminder that we need to add income inequality gini's for 2017-2019 (WIID)



# Figure 5

# Figure 6

# Figure 7

# Figure 8
ggplot(data = ThesisData, aes(x = Year, y = wealthGini, color = Country)) + geom_line() + 
  geom_line(aes(x = Year, y = incomeGini, color = Country), color = "black") +
  facet_wrap( ~ Country) +
  theme(legend.position = "none") #Add 2008 and 2003
ggplot(data = ThesisData, aes(x = Year, y = incomeGini, color = Country)) + geom_line() + facet_wrap( ~ Country) +
  theme(legend.position = "none") #Add 2018, 2019 and Australia, Chile, Colombia, Mexico, New Zealand needs to get filled in


# Figure 9



### SUMMARY STATISTICS/TABLES












