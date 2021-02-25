getwd()
df<-read.csv("Lectures/Egypt_SouthAfrica.csv")
colnames(df)

# create dummy variables
table(df$country)
df$countryD<-NA
df$countryD[df$country=="Egypt"]<-0
df$countryD[df$country=="South Africa"]<-1
tail(df$countryD)
table(df$country, df$countryD)

table(df$X011)
df$X011R<-NA
df$X011R[df$X011==0]<-1
df$X011R[df$X011>0 & df$X011<4]<-2
df$X011R[df$X011>=4]<-3
table(df$X011R, df$X011)

# exercise 1
t.test(df$G019~factor(df$country))
t.test(df$G019~factor(df$country), alternative="less")
t.test(df$G019~factor(df$country), alternative="greater")

cor(df[,c("B001","B002","B003")], use="pairwise.complete.obs")

for(var in c("B001","B002","B003")){
  newvar <- paste(var,"R", sep="")
  df[newvar] <- NULL
  df[df[var]==1 & !is.na(df[var]), newvar] <- 4
  df[df[var]==2 & !is.na(df[var]), newvar] <- 3
  df[df[var]==3 & !is.na(df[var]), newvar] <- 2
  df[df[var]==4 & !is.na(df[var]), newvar] <- 1
}
chisq.test(table(df$B001, df$B001R))
chisq.test(df$B001, df$B001R, correct=FALSE)
df$B001R<-5-df$B001 # alternative way without indexing

# average interitem correlation
cor_matrix <- cor(df[,c("B001R","B002R","B003")], use="pairwise.complete.obs")
mean(lower.tri(cor_matrix))
df$wtp <- (df$B001 + df$B002 + df$B003R)/3
tapply(df$wtp,df$country, mean, na.rm=T)

# calculate means
df$wtp <- (df$B001R + df$B002R + df$B003)/3
df$community <- (df$B018 + df$B019 + df$B020)/3
df$world <- (df$B021 + df$B022 + df$B023)/3

cor(df[,c("wtp","community","world")], use="pairwise.complete.obs")
cor(df[df$country=="Egypt",c("wtp","community","world")], use="pairwise.complete.obs")
cor(df[df$country=="South Africa",c("wtp","community","world")], use="pairwise.complete.obs")

cor(df[df$country=="Egypt",c("B001R","B002R","B003","B018","B019","B020", "B021","B022","B023")], use="pairwise.complete.obs")
cor(df[df$country=="South Africa",c("B001R","B002R","B003","B018","B019","B020", "B021","B022","B023")], use="pairwise.complete.obs")

# raw cronbach alpha
# calculate yourself
cor_matrix <- cor(df[,c("B001R","B002R","B003")], use="pairwise.complete.obs")
k <-  3
r_mean <- mean(cor_matrix[lower.tri(cor_matrix, diag = F)])
(k*r_mean)/(1+(k-1) * r_mean)

# check with 
library(psych)
alpha(cor(df[,c("B001R","B002R","B003")], use="pairwise.complete.obs"))

# calculate standardized cronbach's alpha
m<-var(df[,c("B001R","B002R","B003")], use="pairwise.complete.obs")
m<-var(df[,c("B001","B002","B003")], use="pairwise.complete.obs")
sum(diag(m))
sum(m)
sum(diag(m))/sum(m)







