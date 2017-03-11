#Write R code to generate 10000 rolls of a coin and computes a frequency table of the results and a histogram.
#You should also comment on whether the output matches your expectations.

n = 10
sample(c("Heads", "Tails"), n, rep = T)
Flip1Coin = function(n) sample(c("Heads", "Tails"), n, rep = T)
Flip1Coin(n)
C = Flip1Coin(1000)
sum(C == "Heads") 
sum(C == "Tails")
sum(C != "Heads")
table(C)
prop.table(table(C))

hist(propTable,right=F ,col=dark_colors, main="Probability freq of rolls of a coin", xlab = "percent%")

-------------------------------------

#Write R code to generate 10000 rolls of a die and computes a frequency table of the results and a histogram.
#You should also comment on whether the output matches your expectations.

Roll1Die = function(n) sample(1:6, n, replace = T)
roll1 = NULL
for(i in 1:10000){
    roll1[i] = Roll1Die(1)
}
hist(roll1, right = F, density = 100, breaks=1:6, probability = T, main = "10000 roll of a die frequency", ylab = "frequency", xlab = "roll")

-------------------------------------

#Write R code to generate 10000 rolls of a pair of coins, counting the number of heads and computes a frequency table of the results and a histogram. You
#should also comment on whether the output matches your expectations.

Roll1Coin <- function(n) sample(1:2, n, rep=T)

 roll1 = NULL
 roll2 = NULL
 for(i in 1:10000){
     roll1[i] = Roll1Coin(1)
     roll2[i] = Roll1Coin(1)
 }
hist((roll1 + roll2), density = 100, prob = T, main = "Frequency of 2 Coin Flips", ylab = "Frequency", xlab = "Heads, Heads/Tails, Tails")

--------------------------------------

#Write R code to generate 10000 rolls of a pair of dice and computes a frequency table of the results and a histogram.
#You should also comment on whether the output matches your expectations.

Roll1Die = function(n) sample(1:6, n, replace = T)
roll1 = NULL
roll2 = NULL
for(i in 1:10000){
    roll1[i] = Roll1Die(1)
    roll2[i] = Roll1Die(1)
}
sum(roll1 == roll2)
hist((roll1 + roll2), right = F, density = 100, breaks=1:12, probability = T, main = "10000 rolls of 2 die frequency", ylab = "frequency", xlab = "roll")

----------------------------------------

#Based on the student survey dataset, what is the probability of selecting student at random who is left handed ?

Writing.hand <- survey$W.Hnd
writing.hand.na<-na.omit(Writing.hand)
table(writing.hand.na)

left.hand<-na.omit(survey$W.Hnd[survey$W.Hnd=="Left"])
S<-length(writing.hand.na)
E<-length(left.hand)
Pr <- E/S
Pr
[1] 0.07627119

------------------------------------------

#Based on the student survey dataset, what is the probability of selecting a male student at random who is left handed?

S<-length(na.omit(survey$W.Hnd))
E<-length(na.omit(survey$W.Hnd[survey$W.Hnd=="Left" & survey$Sex=="Male"]))
Pr<-E/S
Pr
[1] 0.04237288

E<-length(na.omit(survey$W.Hnd[survey$W.Hnd=="Left" & survey$Sex=="Female"]))
Pr
[1] 0.02966102

-----------------------------------------

#Based on the student survey dataset, what is the probability of selecting a male student at random who 18 year or younger?

S<-length(na.omit(survey$Age))
E<-length(na.omit(survey$Age[survey$Age<18 & survey$Sex=="Male"]))
Pr<-E/S
Pr
[1] 0.1350211