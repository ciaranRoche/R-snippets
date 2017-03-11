# collection of snippets used in notes

library(MASS)
data(survey)





# Mode
age <- floor(survey$Age)   # round down ages 
table(age)
age.freq <- table(age)
names(age.freq[age.freq==max(age.freq)])





# Mean
x <-c(11, 140, 98, 23, 45, 14, 56, 78, 93, 200, 123)
mean(x)

mean (survey$Age)
# remember this variable had two outliers >70
mean (survey$Age[survey$Age<70])
mean (survey$Age, trim=.1)


# Median 
x <-c(11, 140, 98, 23, 45, 14, 56, 78, 93, 200, 123)
median(x)

median (survey$Age)
# dropping outliers (no change in median)
median (survey$Age[survey$Age<70])



# Range
x <- c(11, 10, 8, 4, 6, 7, 11, 6, 11, 7)
range(x)







# Variance
x <- c(11, 10, 8, 4, 6, 7, 11, 6, 11, 7)
var(x)

var(survey$Age)





# standard deviation
x <- c(11, 10, 8, 4, 6, 7, 11, 6, 11, 7)
sd(x)

sd(survey$Age)





# Properties of sd



















# moments
# calculate the 3rd central moment of Age in student survey dataset 
library(e1071)
age <- na.omit(survey$Age)
moment(age, center=TRUE, order=3)

moment(age, order=1)
mean(age)

moment(age, center=TRUE, order=2)
var(age)   # sample variance !
var(age) * (length(age)-1)/length(age)
 
skewness(age, type=1)
mu_2 <- moment(age,center=TRUE,order=2)
mu_3 <- moment(age,center=TRUE,order=3) 
mu_3 / mu_2^(3/2)

kurtosis(age, type=1)
mu_2 <- moment(age,center=TRUE,order=2)
mu_4 <- moment(age,center=TRUE,order=4) 
mu_4 / mu_2^2 - 3


age <- na.omit(survey$Age[survey$Age<70])
pulse <- na.omit(survey$Pulse)

png(filename="age_hist.png", width=600, heigh=300)
hist(age, breaks=seq(15,45,by=1))
dev.off()

png(filename="age_hist.png", width=600, heigh=300)
hist(pulse, breaks=seq(35,105,by=2))
dev.off()

c(skewness(age), skewness(pulse))
c(kurtosis(age), kurtosis(pulse))



# quartiles / percentiles
age <- survey$Age[survey$Age<70] # drop outliers
quantile(age)
quantile(survey$Age)

quantile(age, probs=seq(0,1,0.1))
quantile(age, probs=0.05)



# 5 number summary
fivenum(age)
summary(age)







#IQR
IQR(age)








# box plot
png('boxplot_1.png', width=600, heigh=200)
fivenum(pulse)
boxplot(pulse, horizontal=TRUE, xlab="Pulse (beat/min)")
dev.off()

png('boxplot_2.png', width=600, heigh=200)
boxplot(pulse, horizontal=TRUE, 
 col="gold", notch=TRUE,
 main="Pulse rate of students", xlab="Pulse (beat/min)")
dev.off()









# real data 
data(CO2)
png('uptake_all_hist.png', width=1200, heigh=600, pointsize=24)
hist(CO2$uptake, col="green", 
  xlim=c(5,50), xlab="CO2 Uptake (umol/m^2 sec)",
  main="Uptake of CO2 (All Observations)")
dev.off()

# hist 

breaks = seq(5,50,5) 
png('uptake_by_Treatment_where_Quebec_hist.png', width=800, heigh=1000, pointsize=24)
par(mfrow=c(2,1))
hist(CO2$uptake[CO2$Type=="Quebec" & CO2$Treatment=="nonchilled"], col="gold", 
  breaks=breaks, xlim=c(5,50), xlab="nonchilled",
  main="Uptake of CO2 (Type='Quebec')")
hist(CO2$uptake[CO2$Type=="Quebec" & CO2$Treatment=="chilled"], col="cyan", 
  breaks=breaks, xlim=c(5,50), xlab="chilled",main="")
dev.off()

png('uptake_by_Treatment_where_Mississippi_hist.png', width=800, heigh=1000, pointsize=24)
par(mfrow=c(2,1))
hist(CO2$uptake[CO2$Type=="Mississippi" & CO2$Treatment=="nonchilled"], col="gold", 
  breaks=breaks, xlim=c(5,50), xlab="nonchilled",
  main="Uptake of CO2 (Type='Mississippi')")
hist(CO2$uptake[CO2$Type=="Mississippi" & CO2$Treatment=="chilled"], col="cyan", 
  breaks=breaks, xlim=c(5,50), xlab="chilled",main="")
dev.off()

# ogive 

png('uptake_by_Treatment_where_Quebec_ogive.png', width=800, heigh=900, pointsize=24)
par(mfrow=c(1,1))
tmp <- CO2$uptake[CO2$Type=="Quebec" & CO2$Treatment=="nonchilled"]
tmp.cut <- cut(tmp, breaks, right=FALSE) 
tmp.freq <- table(tmp.cut)
tmp.cumfreq = c(0, cumsum(tmp.freq)) 
tmp.cumrelfreq <- tmp.cumfreq / length(tmp)
plot(breaks, tmp.cumrelfreq, col="gold", lwd=5,
     main="Uptake of CO2 (Type='Quebec')",
     xlab="CO2 Uptake (umol/m^2 sec)", ylab="Cumulative relative frequency")
lines(breaks, tmp.cumrelfreq, col="gold", lwd=5)
tmp.cut <- cut(CO2$uptake[CO2$Type=="Quebec" & CO2$Treatment=="chilled"], breaks, right=FALSE) 
tmp.freq <- table(tmp.cut)
tmp.cumfreq = c(0, cumsum(tmp.freq)) 
tmp.cumrelfreq <- tmp.cumfreq / length(tmp)
points(breaks,tmp.cumrelfreq, col="cyan", lwd=5)
lines(breaks, tmp.cumrelfreq, col="cyan", lwd=5)
legend("topleft", levels(CO2$Treatment), col=c("gold","cyan"), lty = 1, lwd=5)
dev.off()

png('uptake_by_Treatment_where_Mississippi_ogive.png', width=800, heigh=900, pointsize=24)
par(mfrow=c(1,1))
tmp <- CO2$uptake[CO2$Type=="Mississippi" & CO2$Treatment=="nonchilled"]
tmp.cut <- cut(tmp, breaks, right=FALSE) 
tmp.freq <- table(tmp.cut)
tmp.cumfreq = c(0, cumsum(tmp.freq)) 
tmp.cumrelfreq <- tmp.cumfreq / length(tmp)
plot(breaks, tmp.cumrelfreq, col="gold", lwd=5,
     main="Uptake of CO2 (Type='Mississippi')",
     xlab="CO2 Uptake (umol/m^2 sec)", ylab="Cumulative relative frequency")
lines(breaks, tmp.cumrelfreq, col="gold", lwd=5)
tmp.cut <- cut(CO2$uptake[CO2$Type=="Mississippi" & CO2$Treatment=="chilled"], breaks, right=FALSE) 
tmp.freq <- table(tmp.cut)
tmp.cumfreq = c(0, cumsum(tmp.freq)) 
tmp.cumrelfreq <- tmp.cumfreq / length(tmp)
points(breaks,tmp.cumrelfreq, col="cyan", lwd=5)
lines(breaks, tmp.cumrelfreq, col="cyan", lwd=5)
legend("topleft", levels(CO2$Treatment), col=c("gold","cyan"), lty = 1, lwd=5)
dev.off()

# box plot

png('uptake_all_boxplot.png', width=1200, heigh=400, pointsize=24)
boxplot(CO2$uptake, horizontal=TRUE, col="green", notch=TRUE,
  xlab="CO2 Uptake (umol/m^2 sec)",
  main="Uptake of CO2 (All Observations)")
dev.off()


png('uptake_by_Treatment_where_Quebec_boxplot.png', width=800, heigh=600, pointsize=24)
tmp <- CO2[CO2$Type=="Quebec",]
boxplot(formula=uptake~Treatment, data=tmp, horizontal=T, notch=F, 
  xlab="CO2 Uptake (umol/m^2 sec)", ylab=c("Treatment"),
  main="Uptake of CO2 (Type='Quebec')", col=c("gold","cyan"))
dev.off()

png('uptake_by_Treatment_where_Mississippi_boxplot.png', width=800, heigh=600, pointsize=24)
tmp <- CO2[CO2$Type=="Mississippi",]
boxplot(formula=uptake~Treatment, data=CO2[CO2$Type=="Mississippi",], 
  horizontal=T, notch=F, xlab="CO2 Uptake (umol/m^2 sec)", ylab=c("Treatment"),
  main="Uptake of CO2 (Type='Mississippi')", col=c("gold","cyan"))
dev.off()

png('uptake_by_Treatment_boxplot.png', width=1000, heigh=700, pointsize=24)
boxplot(formula=uptake~Treatment*Type, data=CO2, horizontal=T,
  xlab="CO2 Uptake (umol/m^2 sec)",
  ylab=c("Type and Treatment"),
  names=c("Quebec\nnonchill","\nchill","Mississippi\nnonchill","\nchill"),
  main="Uptake of CO2 (all observations)", col=c("gold","cyan"))
dev.off()