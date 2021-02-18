df<-read.csv("Lectures/Egypt_SouthAfrica.csv")
colnames(df)

# lecture 5
summary(df$S017[df$country=="Egypt"])
summary(df$S017[df$country=="South Africa"])
tapply(df$S017[df$country=="Egypt"], df$X025R[df$country=="Egypt"], mean, na.rm=T)
tapply(df$S017[df$country=="South Africa"], df$X025R[df$country=="South Africa"], mean, na.rm=T)

# egypt
mu <- sum(df[df$country=="Egypt","X047"]*df$S017[df$country=="Egypt"], na.rm = T)/sum(df$S017[df$country=="Egypt"], na.rm = T)
summary(df[df$country=="Egypt","X047"])

t.test(df[df$country=="Egypt","X047"],mu=mu)

# south africa
mu <- sum(df[df$country=="South Africa","X047"]*df$S017[df$country=="South Africa"], na.rm = T)/sum(df$S017[df$country=="South Africa"], na.rm = T)
summary(df[df$country=="South Africa","X047"])

t.test(df[df$country=="South Africa","X047"],mu=mu)


# lecture 6
summary(df$X025)
tapply(df$X025, list(df$X001,df$country), mean, na.rm=T)
t.test(X025~X001, df[df$country=="Egypt",], alternative="greater")
t.test(X025~X001, df[df$country=="South Africa",], alternative="greater")
t.test(X025~X001, df[df$country=="Egypt",])
t.test(X025~X001, df[df$country=="South Africa",])


tapply(df$X047, list(df$X001,df$country), mean, na.rm=T)
t.test(X047~X001, df[df$country=="Egypt",], alternative="less")
t.test(X047~X001, df[df$country=="South Africa",], alternative="less")
t.test(X047~X001, df[df$country=="Egypt",])
t.test(X047~X001, df[df$country=="South Africa",])

summary(aov(X047~factor(X025R),df[df$country=="Egypt",]))
summary(aov(X047~factor(X025R),df[df$country=="South Africa",]))



