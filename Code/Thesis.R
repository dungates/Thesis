

meanWealth = read.csv("/Users/dunk/Downloads/WEALTH_06032020193247624.csv")
view(meanWealth)
betterData = spread(meanWealth, Variable, Value)
view(betterData)

ggplot(data = betterData) + 
  geom_col(aes(x = Country, y = "Share of top 10% of wealth"))

















