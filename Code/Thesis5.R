library(tidyverse)
library(stargazer)
library(plm)
library(readr)
library(lfe)
library(car)
library(reshape2)
library(ggpmisc)
library(lme4)
library(broom)
library(hrbrthemes)
library(lares)
library(ggpmisc)
ThesisData <- readr::read_csv("/Users/dunk/Thesis/Data/PanelData.csv", col_types = cols(`Under 10,000` = col_double(), 
                                                                                 `100,000-1 Million` = col_double(), 
                                                                                 `Over 1 Million` = col_double()))
# ThesisData <- ThesisData %>% mutate(Date = as.Date(paste0(Year,"-01-01")))
# ThesisData <- ThesisData %>% mutate(wealthTax = ifelse(Country == "Norway" | Country == "Switzerland" | Country == "Belgium" |
#                                                          Country == "Spain", 1, 0))

ThesisData$wealthTax <- factor(ThesisData$wealthTax, levels = c(0,1))
ThesisData$Region <- as.factor(ThesisData$Region)

# ThesisData <- ThesisData %>% relocate(Year, .after = Date)

# write_csv(ThesisData, "/Users/dunk/Thesis/Data/PanelData.csv")

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

#Extra dataframe
pdata <- pdata.frame(ThesisData, index = c("Year","Country"))
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
options("na.action" = "na.exclude") #idk why but this keeps needing to be coded
summary(lm(wealthGini ~ incomeGini, data = ThesisData)) # This probably means that there is no interaction between wealth and income inequality
# Top 10 correlations of all variables, removing additional income inequality measures
corr_cross(ThesisData, max_pvalue = 0.05, top = 10, ignore = c("INCOMEINEQ in P50P10 as RT","INCOMEINEQ in P90P10 as RT",
                                                               "INCOMEINEQ in P90P50 as RT","INCOMEINEQ in PALMA as RT",
                                                               "INCOMEINEQ in S80S20 as RT"))
# Top 10 correlations of wealth and income
corr_var(ThesisData, wealthGini, top = 10, ignore = c("Country"), max_pvalue = 0.05)
corr_var(ThesisData, incomeGini, top = 10, ignore = c("INCOMEINEQ in P50P10 as RT","INCOMEINEQ in P90P10 as RT",
                                                      "INCOMEINEQ in P90P50 as RT","INCOMEINEQ in PALMA as RT",
                                                      "INCOMEINEQ in S80S20 as RT", "Country"), max_pvalue = 0.05)
colnames(ThesisData)[40] <- "incomeTax"
corr_var(ThesisData, incomeTax, top = 10, ignore = c("Country"), max_pvalue = 0.05)
summary(lm(incomeGini ~ incomeTax, data = ThesisData))


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
sd(ThesisData$`TAXINCOME in TOT as PC_GDP`, na.rm = T)
ThesisData <- ThesisData %>% mutate(incomeTax = `TAXINCOME in TOT as PC_GDP`)
taxmodel1 <- lm(wealthGini ~ `incomeTax` + `TAXPAYROLL in TOT as PC_GDP` + `TAXGOODSERV in TOT as PC_GDP` +
                  `TAXCORP in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel2 <- lm(wealthGini ~ `incomeTax` + `TAXPAYROLL in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel3 <- lm(wealthGini ~ `TAXCORP in TOT as PC_GDP` + `TAXGOODSERV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel4 <- lm(wealthGini ~ `incomeTax` + `TAXPAYROLL in TOT as PC_GDP` + 
                  `TAXGOODSERV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel5 <- lm(wealthGini ~ `incomeTax` + `TAXPAYROLL in TOT as PC_GDP` + 
                  `TAXGOODSERV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel6 <- lm(wealthGini ~ `incomeTax` + `TAXPAYROLL in TOT as PC_GDP` + 
                  `TAXGOODSERV in TOT as PC_GDP` + `TAXCORP in TOT as PC_GDP` + `TAXREV in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
taxmodel7 <- lm(wealthGini ~ `TAXWEDGE in TOT as PC_LC` + `TAXCORP in TOT as PC_GDP` + `GDP in TOT as USD_CAP`, data = ThesisData)
# Think about difference between countries and difference between year

stargazer(taxmodel1, taxmodel2, taxmodel3, taxmodel4, taxmodel5, taxmodel6, taxmodel7, type = "html", 
          out = "/Users/dunk/Thesis/Correlations/TaxModel.htm",
          dep.var.labels = "Wealth Gini", 
          covariate.labels = 
            c("Income Tax", "Payroll Tax","Tax on Goods and Services", "Tax Wedge", "Corporate Tax", "Tax Revenue", "GDP"))
stargazer(socspendmodel1, socspendmodel2, socspendmodel3, socspendmodel4, socspendmodel5, socspendmodel6, type = "html", out = "/Users/dunk/Thesis/Correlations/SocModel.htm",
          dep.var.labels = "Wealth Gini", covariate.labels = c("Public Unemployment Spending", "Social Security Contributions", "Social Benefits to Households","Social Spending","GDP", "Income Gini"))

fullmodel <- lm(wealthGini ~ `TAXWEDGE in TOT as PC_LC` + `PUBUNEMPEXP in TOT as PC_GDP` +
                  `wealthTax`, data = ThesisData)
stargazer(fullmodel, type = "text")


# Seems like after accounting for income inequality a lot more of the variation in wealth inequality is explained by social spending than by taxes
# Test a couple other measures of policy - capital gains taxation and tax revnue by some measure
# Income and payroll tax consistently increase the wealthGini even after account for GDP, and other taxes including revenue as a percent of GDP
# Dummy variable for social welfare - decide which metric to use?
# Do some time series stuff and test same models with incomeGini as response variable
# Same stuff for dummy variable of wealth tax
wealthtax1 <- lm(wealthGini ~ wealthTax, data = ThesisData)
wealthtax2 <- lm(incomeGini ~ wealthTax, data = ThesisData)
wealthtax3 <- lm(wealthGini ~ wealthTax + `TAXWEDGE in TOT as PC_LC` + `SOCEXP in PUB as PC_GDP`, data = ThesisData)
wealthtax4 <- lm(incomeGini ~ wealthTax + `TAXWEDGE in TOT as PC_LC` + `SOCEXP in PUB as PC_GDP`, data = ThesisData)
wealthtax5 <- lm(wealthGini ~ wealthTax*`GDP in TOT as USD_CAP` , data = ThesisData)
stargazer(wealthtax1, wealthtax2, wealthtax3, wealthtax4, wealthtax5, type = "text") # region dummies, visual as well

# I want to model the effects of France removing its wealth tax in 2017, how should I do that?
# Country fixed effect, time invariant, switching on off wealth tax

incomeGini1 <- lm (incomeGini ~ `TAXWEDGE in TOT as PC_LC`, data = ThesisData)
incomeGini2 <- lm (incomeGini ~ incomeTax + `TAXPAYROLL in TOT as PC_GDP`, data = ThesisData)
incomeGini3 <- lm (incomeGini ~ `TAXPROPERTY in TOT as PC_GDP` + `TAXCORP in TOT as PC_GDP` + `TAXREV in TOT as PC_GDP`, data = ThesisData)
incomeGini4 <- lm (incomeGini ~ `TAXGOODSERV in TOT as PC_GDP`, data = ThesisData)
incomeGini5 <- lm (incomeGini ~ `PUBUNEMPEXP in TOT as PC_GDP` + `TAXSS in TOT as PC_GDP` + `SOCBENHH in INCASH as PC_GDP` + 
                     `SOCEXP in PUB as PC_GDP`, data = ThesisData)

stargazer(incomeGini1, incomeGini2, incomeGini3, incomeGini4, type = "html", 
          out = "/Users/dunk/Thesis/Correlations/IncomeTaxModel.htm",
          dep.var.labels = "Income Gini", 
          covariate.labels = c("Tax Wedge", "Income Tax", "Payroll Tax", "Property Tax", "Corporate Tax", "Tax Revenue", "Goods and Services Tax"))

incomeGin1 <- lm(incomeGini ~ `PUBLMPEXP in JOBCREATION as PC_GDP`, data = ThesisData)
incomeGin2 <- lm(incomeGini ~ `SOCEXP in TOTNET as PC_GDP`, data = ThesisData)
incomeGin3 <- lm(incomeGini ~ `SOCBENHH in INCASH as PC_GDP`, data = ThesisData)
incomeGin4 <- lm(incomeGini ~ `PUBUNEMPEXP in TOT as PC_GDP` + `SOCBENHH in INCASH as PC_GDP` +
                   `SOCEXP in TOTNET as PC_GDP` + `PUBLMPEXP in JOBCREATION as PC_GDP`, data = ThesisData)

stargazer(incomeGin1, incomeGin2, incomeGin3, incomeGin4, type = "html",
          out = "/Users/dunk/Thesis/Correlations/IncomeSocModel.htm",
          dep.var.labels = "Income Gini", 
          covariate.labels = c("Public Unemployment Spending", "Public Labor Market Spending on Job Creation", 
                               "Social Expenditures", "Social Benefits to Households in Cash"))

# Tables that answer each of the research questions, focus on taxation and transfer
# Report 5-10 different models
# Country and time fixed effects

p1 <- ThesisData %>% filter(Country == "France") %>% ggplot(aes(as.Date(paste0(Year, "-01-01")), wealthGini)) + 
  geom_line() + scale_x_date() + geom_line(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) +
  geom_point() + geom_point(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) + 
  geom_vline(linetype = "dashed", xintercept = as.Date("2017-01-01"), color = "red") + 
  annotate(geom = "text", x = as.Date("2014-08-01"), y = 40, label = "Abolition of Wealth Tax") +
  theme_bw() +
  labs(x = "", y = "") + ggtitle("France") + theme(axis.title = element_text(hjust = 0.5))
p2 <- ThesisData %>% filter(Country == "Norway") %>% ggplot(aes(as.Date(paste0(Year, "-01-01")), wealthGini)) + 
  geom_line() + scale_x_date() + geom_line(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) +
  geom_point() + geom_point(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) + 
  theme_bw() +
  labs(x = "", y = "") + ggtitle("Norway") + theme(axis.title = element_text(hjust = 0.5))
p3 <- ThesisData %>% filter(Country == "Switzerland") %>% ggplot(aes(as.Date(paste0(Year, "-01-01")), wealthGini)) + 
  geom_line() + scale_x_date() + geom_line(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) +
  geom_point() + geom_point(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) + theme_bw() +
  labs(x = "", y = "") + ggtitle("Switzerland") + theme(axis.title = element_text(hjust = 0.5))
p4 <- ThesisData %>% filter(Country == "Spain") %>% ggplot(aes(as.Date(paste0(Year, "-01-01")), wealthGini)) + 
  geom_line() + scale_x_date() + geom_line(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) +
  geom_point() + geom_point(aes(as.Date(paste0(Year, "-01-01")), incomeGini)) + 
  annotate(geom = "text", x = as.Date("2016-03-01"), y = 40, label = "Reintroduction of Wealth Tax") +
  geom_vline(linetype = "dashed", xintercept = as.Date("2013-06-01"), color = "red") + theme_bw() +
  labs(x = "", y = "") + ggtitle("Spain") + theme(axis.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 2, top = "Countries with Wealth Taxes", bottom = "Year", 
                        left = "Wealth/Income Gini")

ThesisData %>% filter(!is.na(incomeGini)) %>% ggplot(aes(x = Date, y = incomeGini, color = Country)) + geom_line() + 
  geom_text(aes(label = ifelse(incomeGini > 40 & Year > 2016, as.character(Country),''))) + geom_point() + theme_bw() +
  theme(legend.position = "none")

summary(lm(wealthGini ~ `TAXWEDGE in TOT as PC_LC`, data = ThesisData))
regionalols <- lm(wealthGini ~ incomeGini + factor(Region), data = ThesisData)
stargazer(regionalols, type = "html", out = "/Users/dunk/Thesis/Correlations/RegionModel.htm",
          dep.var.labels = "Wealth Gini", covariate.labels = c("Income Gini", "Asia", "EU", "Oceania"))
ols <- lm(wealthGini ~ incomeGini, data = ThesisData)


## Fixed Effects Models
# Here's an actually good one

fixed <- plm(wealthGini ~ incomeGini, data = ThesisData, index = c("Country","Year"), model = "within")
summary(fixed)
#fixed effects constants for each country
fixef(fixed)
#testing for fixed effects, null: ols better than fixed, so reject null hypothesis here?
pFtest(fixed, ols)
stargazer(fixed, type = "text")

# Bad one I think
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



# ## Random Effects Models
# re1 <- plm(formula = wealthGini ~ incomeGini + GDP_PC, data = pdata, model = "random") # First rename all variables in pdata
# summary Don't use




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
  geom_text(aes(label = Country), data = ThesisData %>% filter(Year == "2019-08-15"), 
            color = "black", hjust = -0.05, size = 3, nudge_x = 0.5) +
  guides(color = "none") + coord_cartesian(xlim = c(as.Date("2010-08-15"), as.Date("2020-08-15")))











# Figure 1
region_names <- c(Americas = "Americas", 
                  Asia = "Asia", 
                  EU = "EU", 
                  Oceania = "Oceania", 
                  NA = "Iceland and Israel")

ThesisData %>% filter(!is.na(Region)) %>% ggplot(aes(x = incomeGini, y = wealthGini)) + 
  facet_wrap( ~ Region, labeller = facet_labeller) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "gray4") +
  stat_poly_eq(aes(label = paste(..eq.label..)), formula = y ~ x, parse = T, label.x = "right") +
  labs(x = "Income Gini", y = "Wealth Gini") +
  theme_bw()

# Figure 2
figure2data <- ThesisData %>% filter(Year == "2019-08-15")
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
figure3data <- ThesisData %>% filter(Date == "2019-08-15")
figure3data <- melt(figure3data, id.vars = c("Country", "Region"), measure.vars = c("Under 10,000","10,000-100,000","100,000-1 Million","Over 1 Million"))
figure3data <- figure3data %>% arrange(variable, desc(value)) %>% 
  mutate(variable = factor(variable, levels = c("Under 10,000","10,000-100,000","100,000-1 Million","Over 1 Million")))
ggplot(data = figure3data, aes(x = reorder(Country, -value), y = value, fill = variable)) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) + theme_bw() + labs(y = "Proportion at Each Wealth Level") + 
  ggtitle("Distribution of Wealth in 2019") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) #Same thing as previous graph organize by greatest amount in percentile over 1 million



# Figure 3b
ggplot(data = ThesisData, aes(x = incomeGini, y = wealthGini, color = Region)) + geom_point() + labs(x = "Income Gini", y = "Wealth Gini") +
  theme_bw()
  
#Ask Todd how to deal with Israel regionality



# Figure 4
figure4data <- ThesisData %>% filter(Year == 2016)
figure4data <- figure4data %>% dplyr::mutate(incomeGini = ifelse(Country == "Chile", 44.4, incomeGini), 
                                             incomeGini = ifelse(Country == "Japan", 34.0, incomeGini))
figure4data <- melt(figure4data, id.var = c("Country", "Year"), measure.vars = c("incomeGini","wealthGini"))
figure4data <- figure4data %>% group_by(value) %>% dplyr::arrange(desc(value), by.group = T)
ggplot(data = figure4data, aes(x = reorder(Country, -value), y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired", labels = c("Income Gini", "Wealth Gini")) + 
  theme_bw() + labs(y = "Wealth Gini and Income Gini", x = "Ordered by Sum of Gini's") + 
  ggtitle("Wealth and Income Gini's in 2016") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_text(),
        plot.title = element_text(hjust = 0.5)) # Reminder that we need to add income inequality gini's for 2017-2019 (WIID)


# Figure 5: Total taxes over time figure this out later

figure5data <- ThesisData %>% select("TAXCORP in TOT as PC_GDP", "TAXGOODSERV in TOT as PC_GDP","incomeTax",
                                     "TAXPAYROLL in TOT as PC_GDP", "TAXPROPERTY in TOT as PC_GDP", "TAXREV in TOT as PC_GDP", 
                                     "TAXSS in TOT as PC_GDP",
                                     "TAXWEDGE in TOT as PC_LC", "Date") %>%
  group_by(Date) %>% dplyr::summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
figure5data <- melt(figure5data, id.vars = c("Date"),
                    measure.vars = c("TAXCORP in TOT as PC_GDP", "TAXGOODSERV in TOT as PC_GDP","incomeTax",
                                     "TAXPAYROLL in TOT as PC_GDP", "TAXPROPERTY in TOT as PC_GDP", "TAXREV in TOT as PC_GDP", 
                                     "TAXSS in TOT as PC_GDP",
                                     "TAXWEDGE in TOT as PC_LC"))
# figure5data <- ThesisData %>% select("Under 10,000","10,000-100,000","100,000-1 Million","Over 1 Million","Date","Country") %>% 
#   group_by(Date, Country) %>% dplyr::summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
# figure5data <- melt(figure5data, id.vars = c("Date"), 
#                     measure.vars = c("Under 10,000","10,000-100,000","100,000-1 Million","Over 1 Million","Date","Country"))
figure5data$value <- as.numeric(figure5data$value)
ggplot(figure5data, aes(x = Date, y = value, fill = variable)) +
  geom_area(stat = "identity") +
  scale_fill_ipsum(name = "Tax Type", labels = c("Corporate Tax", "Goods and Services Tax", "Income Tax", "Payroll Tax", 
                                                 "Property Tax", "Tax Revenue", "Social Security Contributions",
                                                 "Tax Wedge")) +
  scale_x_date() +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,150), breaks = scales::pretty_breaks(n = 20)) +
  labs(title="Average OECD Taxation by Group", y = "Percentage") +
  theme_ipsum_rc(grid="XY") +
  theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
  theme(legend.position="bottom")



# Figure 6: Tax wedge data
figure6data <- ThesisData %>% 


# Figure 7

# Figure 8
ggplot(data = ThesisData, aes(x = Year, y = wealthGini, color = Country)) + geom_line() + 
  geom_line(aes(x = Year, y = incomeGini, color = Country), color = "black") +
  facet_wrap( ~ Country) +
  theme(legend.position = "none") #Add 2008 and 2003
ggplot(data = ThesisData, aes(x = Year, y = incomeGini, color = Country)) + geom_line() + facet_wrap( ~ Country) +
  theme(legend.position = "none") #Add 2018, 2019 and Australia, Chile, Colombia, Mexico, New Zealand needs to get filled in


# Figure 9


# Mean Household Net Wealth

ThesisData %>% filter(Year == "2019-08-15") %>% ggplot(aes(x = reorder(Country, -`Mean Wealth per Adult`), y = `Mean Wealth per Adult`)) + geom_col(fill = "firebrick3") +
  theme_bw() + labs(y = "Mean Household Wealth", x = "Country") + 
  ggtitle("Mean Household Wealth in 2019") + 
  scale_y_continuous(labels = scales::dollar, breaks = c(0, 100000, 200000, 300000, 400000, 500000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Median Household Net Wealth
ThesisData %>% filter(Year == "2019-08-15") %>% ggplot(aes(x = reorder(Country, -`Median Wealth per Adult`), y = `Median Wealth per Adult`)) + geom_col(fill = "firebrick3") +
  theme_bw() + labs(y = "Median Household Wealth", x = "Country") + 
  ggtitle("Median Household Wealth in 2019") + 
  scale_y_continuous(labels = scales::dollar, breaks = c(0, 100000, 200000, 300000, 400000, 500000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Ratio of mean to median wealth
ratioPlot <- ThesisData %>% filter(Year == "2019") %>% 
  mutate(wealthRatio = `Mean Wealth per Adult` / `Median Wealth per Adult`)
ggplot(ratioPlot, aes(x = reorder(Country, -`wealthRatio`), y = `wealthRatio`)) + geom_col(fill = "firebrick3") +
  theme_bw() + labs(y = "Median Household Wealth", x = "Country") + 
  ggtitle("Ratio of Mean and Median Household Wealth in 2019") + 
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggplot(ratioPlot, aes(x = reorder(Country, -`wealthRatio`), y = `wealthRatio`)) + geom_col(fill = "firebrick3") +
  theme_bw() + labs(y = "Median Household Wealth", x = "Country") + 
  ggtitle("Ratio of Mean and Median Household Wealth in 2019") + 
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


# Income distribution
ThesisData %>% filter(Date == "2016-08-15") %>% filter(!is.na(Country)) %>% ggplot(aes(x = reorder(Country, -`incomeGini`), y = incomeGini)) + 
  geom_col(fill = "skyblue") +
  theme_bw() + labs(y = "Income Gini Coefficient", x = "Country") + 
  ggtitle("Income Gini Coefficient in 2017") + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


### SUMMARY STATISTICS/TABLES

thesisCols <- c("GDP in TOT as USD_CAP", "incomeGini", "wealthGini", "TAXCORP in TOT as PC_GDP", "TAXINCOME in TOT as PC_GDP",
          "TAXREV in TOT as PC_GDP", "TAXPAYROLL in TOT as PC_GDP", "TAXGOODSERV in TOT as PC_GDP", "TAXWEDGE in TOT as PC_LC",
          "TAXPROPERTY in TOT as PC_GDP", "TAXSS in TOT as PC_GDP", "SOCEXP in PUB as PC_GDP", 
          "PUBLMPEXP in EMPINCENTIVE as PC_GDP", "Mean Wealth per Adult", "Median Wealth per Adult")

stargazer(ThesisData[, thesisCols], type = "text", nobs = F, mean.sd = T, median = T, sd = T) # Needs work






