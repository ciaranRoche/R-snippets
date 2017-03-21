#UCBAdmissions

tmp <- as.data.frame(UCBAdmissions)               # convert tables to data frame
tmp <- lapply(tmp, function(x) rep(x,tmp$Freq))   # repeat rows based on given frequencies 
tmp <- as.data.frame(tmp)                         # convert to data frame
admit <- tmp[,c("Admit","Gender","Dept")]         # drop unneeded columns

-----------------------
#What is the probability that an applications is female ?
S<-length(admit$Gender)
E<-sum(admit$Gender=="Female")
Pr<-E/S
Pr
#0.4054353

----------------------
#What is the probability that an applications is female and is admitted ?
#Pr(F)
pr_f_and_a <- sum(admit$Gender=="Female" & admit$Admit=="Admitted")/nrow(admit)
#0.1230667

----------------------
#What is the probability that a female applications is admitted ?
pr_f_and_a <- sum(admit$Gender=="Female" & admit$Admit=="Admitted")/nrow(admit)
pr_f <- sum(admit$Gender=="Female")/nrow(admit)
pr_f_a/Pr_f
#0.3035422

----------------------
#What is the probability that a male applications is admitted ?
pr_m_and_a <- sum(admit$Gender=="Male" & admit$Admit=="Admitted")/nrow(admit)E<-sum(admit$Gender=="Male")
pr_m <- sum(admit$Gender=="Male")/nrow(admit)
pr_m_and_a/pr_m
#0.4451877

----------------------
#If you restrict your analysis to applications to Department A only, are males more likely to be admitted than females? (Give both probabilities)
pr_m_a_a <- sum(admit$Gender=="Male"&admit$Admit=="Admitted"&admit$Dept=="A")/nrow(admit)
pr_m <- sum(admit$Gender=="Male"&admit$Dept=="A")/nrow(admit)
pr_m_a_a/pr_m
#0.6206061
pr_f_a_a <- sum(admit$Gender=="Female"&admit$Admit=="Admitted"&admit$Dept=="A")/sum(admit$Dept=="A")
pr_f <- sum(admit$Gender=="Female"&admit$Dept=="A")/sum(admit$Dept=="A")
pr_f_a_a/pr_f
0.8240741

----------------------
#If you restrict your analysis to applications to Department B only, are males more likely to be admitted than females? (Give both probabilities)
pr_m_a_b <- sum(admit$Gender=="Male"&admit$Admit=="Admitted"&admit$Dept=="B")/sum(admit$Dept=="B")
pr_m <- sum(admit$Gender=="Male"&admit$Dept=="B")/sum(admit$Dept=="B")
pr_m_a_b/pr_m
0.6303571
pr_f_a_b <- sum(admit$Gender=="Female"&admit$Admit=="Admitted"&admit$Dept=="B")/sum(admit$Dept=="B")
pr_f <- sum(admit$Gender=="Female"&admit$Dept=="B")/sum(admit$Dept=="B")
pr_f_a_b/pr_f
0.68

----------------------
#If you restrict your analysis to applications to Department C only, are males more likely to be admitted than females? (Give both probabilities)
pr_m_a_c <- sum(admit$Gender=="Male"&admit$Admit=="Admitted"&admit$Dept=="C")/sum(admit$Dept=="C")
pr_m <- sum(admit$Gender=="Male"&admit$Dept=="C")/sum(admit$Dept=="C")
pr_m_a_c/pr_m
 0.3692308
pr_f_a_c <- sum(admit$Gender=="Female"&admit$Admit=="Admitted"&admit$Dept=="C")/sum(admit$Dept=="C")
pr_f <- sum(admit$Gender=="Female"&admit$Dept=="C")/sum(admit$Dept=="C")
pr_f_a_c/pr_f
0.3406408
----------------------
#If you restrict your analysis to applications to Department D only, are males more likely to be admitted than females? (Give both probabilities)
pr_m_a_d <- sum(admit$Gender=="Male"&admit$Admit=="Admitted"&admit$Dept=="D")/sum(admit$Dept=="D")
pr_m <- sum(admit$Gender=="Male"&admit$Dept=="D")/sum(admit$Dept=="D")
pr_m_a_d/pr_m
 0.3309353
pr_f_a_d <- sum(admit$Gender=="Female"&admit$Admit=="Admitted"&admit$Dept=="D")/sum(admit$Dept=="D")
pr_f <- sum(admit$Gender=="Female"&admit$Dept=="D")/sum(admit$Dept=="D")
pr_f_a_d/pr_f
0.3493333
----------------------
#If you restrict your analysis to applications to Department E only, are males more likely to be admitted than females? (Give both probabilities)
pr_m_a_e <- sum(admit$Gender=="Male"&admit$Admit=="Admitted"&admit$Dept=="E")/sum(admit$Dept=="E")
pr_m <- sum(admit$Gender=="Male"&admit$Dept=="E")/sum(admit$Dept=="E")
pr_m_a_e/pr_m
0.2774869
pr_f_a_e <- sum(admit$Gender=="Female"&admit$Admit=="Admitted"&admit$Dept=="E")/sum(admit$Dept=="E")
pr_f <- sum(admit$Gender=="Female"&admit$Dept=="E")/sum(admit$Dept=="E")
pr_f_a_e/pr_f
0.2391858
----------------------
#If you restrict your analysis to applications to Department F only, are males more likely to be admitted than females? (Give both probabilities)
pr_m_a_f <- sum(admit$Gender=="Male"&admit$Admit=="Admitted"&admit$Dept=="F")/sum(admit$Dept=="F")
pr_m <- sum(admit$Gender=="Male"&admit$Dept=="F")/sum(admit$Dept=="F")
pr_m_a_f/pr_m
0.05898123
pr_f_a_f <- sum(admit$Gender=="Female"&admit$Admit=="Admitted"&admit$Dept=="F")/sum(admit$Dept=="F")
pr_f <- sum(admit$Gender=="Female"&admit$Dept=="F")/sum(admit$Dept=="F")
pr_f_a_f/pr_f
0.07038123
----------------------
#

