library(dplyr)
library(tidyverse)
library(stargazer)
library(plm)
library(lfe)
library(reshape2)

ThesisData <- read_csv("/Users/dunk/Thesis/Data/PanelData.csv")


### REGRESSIONS

summary(lm(wealthGini ~ incomeGini, data = ThesisData)) # This probably means that there is no interaction between wealth and income inequality












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












