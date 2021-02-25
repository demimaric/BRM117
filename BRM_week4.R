# lecture 7
# examples in lecture:
df<-read.csv("supermarket-sales.csv", sep=",")
variables_names <- c('gross.income', 'Rating', "Total","Unit.price")

ggplot(df[1:10,],aes(x = Total, y=gross.income)) +
   geom_point(size = 5) +
   geom_smooth(method = "lm", se = FALSE)+ 
   theme(axis.text = element_text(size=18),
         axis.title = element_text(size=18))df[1:10,variables_names]

# exercises:
df<-read.csv("Lectures/Egypt_SouthAfrica.csv")

# exe 1
df$B003R[df$B003==1]<-4
df$B003R[df$B003==2]<-3
df$B003R[df$B003==3]<-2
df$B003R[df$B003==4]<-1

df$wtp <- (df$B001 + df$B002 + df$B003R)/3
df$work <- (df$C036 + df$C037 + df$C038 + df$C039+ df$C041)/5

cm<-cor(df[,c("wtp","work","X047")], use="complete.obs")

rxy<-cm[2,1]
rxz<-cm[3,1]
ryz<-cm[3,2]

(rxy-rxz*ryz)/(sqrt(1-ryz^2)*sqrt(1-rxz^2))

# check
install.packages("ppcor")
library(ppcor)
dff<-na.omit(df[,c("wtp","work","X047")])
pcor.test(dff$wtp, dff$work, dff$X047)

# exe 2
summary(lm(wtp~work, df))
summary(lm(wtp~work, dff)) # beetje gek, niet hetzelfde getal op de vierde decimaal!

# exe 3
summary(lm(wtp~X001,df))
t.test(wtp~X001,df)
df$gender[df$X001==1]<-0
df$gender[df$X001==2]<-1
summary(lm(wtp~gender,df))
t.test(wtp~X001,df)

### example code end of lecture

## Example at end of lecture:
df<-read.csv("Egypt_SouthAfrica.csv")
colnames(df)

table(df$B003)
df$B003R <- 5-df$B003
table(df$B003, df$B003R)

df$wtp <- (df$B001 + df$B002 + df$B003R)/3
summary(df$wtp)

table(df$X001)
t.test(df$wtp ~ df$X001)
t.test(wtp ~ X001, df)
t.test(wtp ~ X001, df, var.equal=T)

df$X001R <- ifelse(df$X001==1, 0, 1)
summary(fit<-lm(wtp ~ X001R, df))

par(mfrow=c(2,2))
plot(fit)

summary(fit<-lm(wtp ~ X001, df))
summary(fit<-lm(wtp ~ X001R, df))
summary(fit<-lm(wtp ~ factor(X001), df))


summary(fit<-lm(wtp ~ X001R + df$X047, df))

par(mfrow=c(2,2))
plot(fit)

### lecture 8
# exe 1
summary(lm(wtp~work+X047+X003R + X011,df))
dff<-na.omit(df[,c("wtp","work","X047","X003R","X011","X025")])
dff$wtpZ <- scale(dff$wtp)
dff$workZ <- scale(dff$work)
dff$X047Z <- scale(dff$X047)
dff$X011Z <- scale(dff$X011)
dff$X025Z <- scale(dff$X025)
dff$X003RZ <- scale(dff$X003R)
summary(lm(wtpZ~workZ+X047Z+X003RZ + X011Z + X025Z,dff))

# exe 2
summary(lm(work~factor(X003R), df))
summary(lm(work~relevel(factor(X003R),"4"), df))
summary(aov1<-aov(work~factor(X003R), df))
TukeyHSD(aov1)

# exe 3
summary(fit<-lm(work~X003R,df))
plot(fit$fitted, fit$residuals)
summary(fit2<-lm(work~X003R+I(X003R**2),df))
plot(fit2$fitted, fit2$residuals)
summary(fit3<-lm(log(work)~X003R,df))
plot(fit3$fitted, fit3$residuals)









colnames(df)
table(df$country)
df[c(1:5,4005:4010),c("country","X003R")]
df$X003Rf<-factor(df$X003R)
df[c(1:5,4005:4010),c("country","X003R","X003Rf")]

summary(lm(B001~X003Rf, df))


dff<-na.omit(df[,c("country","B001","X003R","X011")])
summary(dff)
summary(lm(scale(B001)~scale(X003R) + scale(X011), dff))

summary(df[,c("country","B001","X003R","X011")])
summary(lm(scale(B001)~scale(X003R) + scale(X011), df))
