library(readr)

df <- readxl::read_xlsx("/Users/dunk/Thesis/Data/WIID_06MAY2020 (1).xlsx")
countryList <- unique(ThesisData$Country)
df <- df %>% dplyr::mutate(country = ifelse(country == "Czechia", "Czech Republic", country)) %>% 
  dplyr::mutate(country = ifelse(country == "Korea, Republic of", "Korea", country)) %>%  
  dplyr::mutate(country = ifelse(country == "Slovakia", "Slovak Republic", country))
df <- df %>% filter(year > 2009) %>% group_by(country) %>% distinct(year, .keep_all = T) %>% 
  ungroup %>% filter(country %in% countryList)
df <- df %>% select(country, gini_reported, year)
# df <- df %>% filter(country %in% countryList) should not change anything
# df <- df %>% filter(source == "United Nations" | source == "World Bank")
# df <- df %>% group_by(country) %>% distinct(year, .keep_all = T)
# df$source <- factor(df$source, levels = c("United Nations", "World Bank"))
# df <- df[order(df$source),]
# df[!duplicated(df[,c('country','year')])]
# `%notin%` <- Negate(`%in%`)
# which(ThesisData$Country %notin% df$country)
# df <- df %>% filter(country %in% countryList)
# List is now full try joining

colnames(df) <- c("country", "incomegini", "year")

test <- ThesisData %>% left_join(df, by = c("Country" = "country", "Year" = "year"))
test <- test %>% coalesce(incomeGini, incomegini)
test <- select(test, -incomegini)

# Fixing the US missing data??

write_csv(test, "/Users/dunk/Thesis/Data/PanelData.csv")


view(df)
