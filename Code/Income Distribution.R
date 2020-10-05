library(readr)
# Making plots of taxation and transfers over last 10 years oecd
incomeDist <- read_csv("/Users/dunk/Thesis/Data/IDD_29092020234240132.csv")
view(incomeDist)

incomeDist <- incomeDist %>% filter(`METHODO` == "METH2012", Country %in% countryList, MEASURE == "ECTOTAL") %>%
  select(Country, Value, Measure, Year, Methodology, Unit)

incomeDist1 <- incomeDist %>% filter(`METHODO` == "METH2012", Country == "Norway", MEASURE == "ECTOTAL") %>% select(Country, Value, MEASURE, Year)

incomeDist <- incomeDist %>% mutate(LOCATION = if_else(LOCATION == "AUS", "Australia", LOCATION)) %>% 
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


incomeDist <- incomeDist %>% select(LOCATION, TIME, Value) %>% mutate(Date = paste0(TIME, "-01-01"))
incomeDist$Date <- as.Date(incomeDist$Date)
incomeDist <- subset(incomeDist, TIME >= 2010)
colorList <- c("gray8" , "gray9" , "gray10" , "gray11" , "gray12" , "gray13" , "gray14" , "gray15" , "gray16" , "gray17" , "gray18" , "gray19" , 
               "gray20" , "gray21" ,
               "gray22" , "gray23" , "gray24" , "gray25" , "gray26" , "gray27" , "gray28" , "gray29" , "gray30" , "gray31" , "gray32" , 
               "gray33" , "gray34" , "gray35" ,
               "gray36" , "gray37" , "gray38" , "gray39" , "gray40" , "gray41" , "gray42" , "gray43" , "gray44" , "gray45" , "gray46" ,
               "gray47")
ggplot(incomeDist, aes(Date, Value, color = LOCATION)) + geom_line() + geom_point() + 
  geom_dl(aes(label = LOCATION), method = list(dl.combine("first.qp"), rot = 30), cex = 0.5, hjust = 1) +
  theme_bw() + 
  scale_color_manual(values = colorList) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(limits = as.Date(c("2008-01-01", "2019-01-01"))) + 
  labs(y = "Tax Wedge Percentage", x = "Year") +
  theme(legend.position = "none")

ThesisData %>% filter(Year == "2019-08-15") %>% ggplot(aes(x = reorder(Country, -`Median Wealth per Adult`), y = `Median Wealth per Adult`)) + geom_col(fill = "firebrick3") +
  theme_bw() + labs(y = "Median Household Wealth", x = "Country") + 
  ggtitle("Median Household Wealth in 2019") + 
  scale_y_continuous(labels = scales::dollar, breaks = c(0, 100000, 200000, 300000, 400000, 500000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


incomeDist %>% filter(Year == 2017) %>% 
  ggplot(aes(x = Country, y = Value)) + geom_col()

