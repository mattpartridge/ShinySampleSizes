# Data Prep
library(survival)
## FL
fl = flchain
fl$kl = fl$kappa/fl$lambda
fl$kl.med = as.factor(ifelse(fl$kl < median(fl$kl, na.rm = T), "Low", "High"))
fl$kl.2575 = as.factor(ifelse(fl$kl <= quantile(fl$kl)[2], "<25th", ifelse(fl$kl >= quantile(fl$kl)[4], ">75th", NA)))
fl$mgus = as.factor(fl$mgus)
fl$death = as.factor(fl$death)
fl$sex = as.factor(fl$sex)
## MGUS = 1
mgus = fl[fl$mgus == 1, ]
<<<<<<< HEAD
## Male
length(fl$sex[fl$sex == "M"])

=======
>>>>>>> origin/master

# TTE: SAMPLE SIZE AND POWER Get kaplan meyer estimate of hazard rate of death at 1 year. Hazard rates for MGUS = 1 x sex or some quartile of KL Rate (above/below median)

# Analysis
## One Mean
summary(mgus$kl) # 2.35
sd(mgus$kl) # 4.37
## One Proportion
<<<<<<< HEAD
# length(mgus$death[mgus$death == 1])/length(mgus$death) # 0.14
# length(fl$mgus[fl$mgus == 1])/length(fl$mgus) # 0.015
length(fl$sex [fl$sex == "F"])/length(fl$sex) # 0.55
=======
length(mgus$death[mgus$death == 1])/length(mgus$death) # 0.14
>>>>>>> origin/master
## Two Means
by(mgus$kl, mgus$sex, summary) # F = 2.01, M = 2.83
## Two Proportions
length(mgus$death[mgus$death == 1])/length(mgus$death) # 0.14
## Time to Event
km.mgus = survfit(Surv(futime, death) ~ mgus, data = fl)
summary(km.mgus) # NoMGUS = 0.034, MGUS = 0.01
max(fl$futime)/365.25 # Longest amount of follow up = 14.3 Years
length(fl$mgus[fl$mgus == 0])/length(fl$mgus[fl$mgus == 1]) # ~= 67:1 MGUS:NoMGUS (0.01)
by(fl$death, fl$mgus, summary)
5606/(5606+2153) # NoMGUS = .72 prop censored
99/(99+16) # MGUS = 0.86 prop censored

length(fl$mgus[fl$mgus==1])/length(fl$mgus[fl$mgus==0])
nSurvival(ratio = l/67)
<<<<<<< HEAD

# Time to Event Example
median(fl$kl[fl$sex == "M"]) # 0.86
ceiling(max(fl$futime[fl$sex == "M"])/365.25) # Study Duration = 15
summary(fl[fl$sex == "M", "kl.med"]); 1841/1683 # Allocation Ratio = 1.09
km.kl.M = survfit(Surv(futime, death) ~ kl.med, data = fl[fl$sex == "M", ])
summary(km.kl.M) # > Median = 0.04, < Median = 0.03
nrow(fl[fl$sex == "M", ]) # Sample Size = 3524,

median(fl$kl[fl$sex == "F"]) # 0.83
ceiling(max(fl$futime[fl$sex == "F"])/365.25) # Study Duration = 15
summary(fl[fl$sex == "F", "kl.med"]); 2096/2254 # Allocation Ratio = 0.93
km.kl.F = survfit(Surv(futime, death) ~ kl.med, data = fl[fl$sex == "F", ])
summary(km.kl.F) # > Median = 0.03, < Median = 0.02
nrow(fl[fl$sex == "F", ]) # Sample Size = 4350,

=======
>>>>>>> origin/master
