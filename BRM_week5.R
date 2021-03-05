df<-read.csv("store.csv")
colnames(df)
head(df)
# seasons
summary(lm(Sales~Summer+ Autumn + Spring, df))
summary(lm(Sales~Trend, df))
summary(fit1<-lm(Sales~Trend + Summer + Autumn + Spring, df))
summary(lm(Sales~Trend * Summer +Trend * Autumn + Trend *Spring, df))
library(ggplot2)
ggplot(df, aes(Trend, fit1$residuals))+
  geom_point(size=3)+
  facet_wrap(~Summer)

# lagged
# moderation
summary(lm(Sales~Promo, df))
summary(lm(Sales~Promo+SchoolHoliday, df))
summary(lm(Sales~Promo*SchoolHoliday, df))

