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

# TTE: SAMPLE SIZE AND POWER Get kaplan meyer estimate of hazard rate of death at 1 year. Hazard rates for MGUS = 1 x sex or some quartile of KL Rate (above/below median)

# Analysis
## One Mean
summary(mgus$kl) # 2.35
sd(mgus$kl) # 4.37
## One Proportion
length(mgus$death[mgus$death == 1])/length(mgus$death) # 0.14
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
