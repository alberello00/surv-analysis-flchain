library(survminer)
library(survival)
flchain<-flchain
#PRELIMINARY ANALYSIS
head(flchain)
#available variables: age, sex, sample.yr, kappa, lambda, flc.group, creatinine, mgus, futime, death, chapter
#all the descriptions of the variables are outside this script
attach(flchain)

#structure of the variables of interest
table(death) #we can see that the deaths are 2169 (of 7874)
summary(futime) #from zero to 5215
#the time is in days

#quantitative variables
summary(age) #from 50 to 101, the median is 63
summary(kappa) #from 0.010 to 20.5 and the median is 1.27
#as we can see from the quantiles, the values of kappa are really small, apart some outliers
summary(lambda) #from 0.040 to 26.6
#same for the variable lambda
summary(creatinine) #from 0.40 to 10.80 and same here as for kappa and lambda
#in creatinine we can see that there re 1350 NA

#qualitative variables
table(sex) #there are more female than male observation
table(mgus) #we can see that there are only 115 observation  had been diagnosed with monoclonal gammapothy
table(flc.grp)

###PREPROCESSING
# Change bad names
names(flchain)[names(flchain) == "sample.yr"] <- 'sample.year'
names(flchain)[names(flchain) == "flc.grp"] <- 'flc.group'

levels(flchain$sex) <- c('Female', 'Male')
levels(flchain$sex)

# Cast to Factor
flchain$flc.group <- as.factor(flchain$flc.group)

# Reduce many levels Factor
sort(table(flchain$chapter))
chapter <- c('Circulatory', 'Neoplasms', 'Respiratory', 'Mental', 'Nervous')
levels(flchain$chapter)[!levels(flchain$chapter) %in% chapter] <- 'Others'
sort(table(flchain$chapter))
#since the chapter variable has too many classes, we decided to group into smaller groups
#in according with the type of their primary cause of death

# convert num to int
flchain$age <- as.integer(flchain$age)
flchain$sample.year <- as.integer(flchain$sample.year)
flchain$futime <- as.integer(flchain$futime)

# Create useful variables
flchain <- transform(flchain,
                     flc       = kappa + lambda,
                     flc.ratio = kappa / lambda)

head(flchain)
#since we know that the variable flc.group was created by adding kappa and lambda we
#did the same but manually and then we also created the ratio

### UNIVARIATE PLOTS
library(ggplot2)

ggplot(flchain, aes(age)) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of age") 

ggplot(flchain, aes(sample.year)) +  geom_histogram(binwidth = 1) +
  ggtitle("Distribution of the year in which a blood sample was obtained.")

ggplot(flchain, aes(kappa)) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of kappa")
ggplot(flchain, aes(log(kappa))) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of kappa")

ggplot(flchain, aes(lambda)) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of lambda")
ggplot(flchain, aes(log(lambda))) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of lambda")

ggplot(flchain, aes(creatinine)) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of creatinine") #the plot itself removes the NA
ggplot(flchain, aes(log(creatinine))) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of creatinine") ##the plot itself removes the NA

ggplot(flchain, aes(flc)) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of flc")
ggplot(flchain, aes(log(flc))) + geom_histogram(binwidth = .5) + 
  ggtitle("Distribution of flc")

### UNIVARIATE ANALYSIS  
death<-as.numeric(death) #the function ggsurvplot requires the death variable as numeric
stset <- Surv(futime, death)
stset[1:10]

# estimate of the survival curves with the Kaplan-Meier method 
fitall <- survfit(stset~1, conf.type = "log-log")
#first we do not take into account any covariate 
ggsurvplot(fitall, data = flchain, risk.table = TRUE)

###BIVARIATE ANALYSIS
#first we do some plot to see graphically if there are some difference is the classes 
#or in the quantile for the variables 

# death - age
age_q <- cut(age, quantile(age), include.lowest =  T)
age_death <- survfit(stset ~ age_q, conf.type = "log-log")
ggsurvplot(age_death, data = flchain, main="risk of death for different age quantile")
#from the plot we can see that there can be difference between the age quantiles

# sex
sex_death <- survfit(stset ~ sex, conf.type = "log-log")
ggsurvplot(sex_death, data = flchain, main="risk of death for different age quantile",
           risk.table = TRUE)
#no difference between the two sex

# kappa
kappa_q = cut(kappa, quantile(kappa), include.lowest =  T)
kappa_death <- survfit(stset ~ kappa_q, conf.type = "log-log")
ggsurvplot(kappa_death, data = flchain,
           main="risk of death for different kappa quantile")
#there can be some difference, especially for the last quantile

# lambda
lambda_q = cut(lambda, quantile(lambda), include.lowest =  T)
lambda_death <- survfit(stset ~ lambda_q, conf.type = "log-log")
ggsurvplot(lambda_death, data = flchain, main="risk of death for different lambda quantile")
#same as before

# flc.gpr
flc_death <- survfit(stset ~ flc.grp, conf.type = "log-log")
ggsurvplot(flc_death, data = flchain, main="risk of death for different flc")
#the higher the group you are in, the more probability you have to die
#can see some difference

# creatinine
creat_q = cut(creatinine, quantile(creatinine, na.rm = T), include.lowest =  T)
creatinine_death <- survfit(stset ~ creat_q, conf.type = "log-log")
ggsurvplot(creatinine_death, data = flchain, main="risk of death for different creatinine quantile")
#can see some difference

# mgus
mgus_death <- survfit(stset ~ mgus, conf.type = "log-log")
ggsurvplot(mgus_death, data = flchain,  main="risk of death for different mgus value")
#can see some difference

#after the plot we will test if the curves are the same for each class of the variable 

#logrank tests: works well if we can assume proportional hazard
#we have to see the p-value- small pvalue indicates that the differences are significative
survdiff(stset~age_q) #there are differences between the classes
survdiff(stset~sex) #no differences
survdiff(stset~sample.yr) #differences 
survdiff(stset~kappa_q) #differences
survdiff(stset~lambda_q) #differences
survdiff(stset~flc.grp) #differences
survdiff(stset~creat_q) #differences
survdiff(stset~mgus) #differences

#To know if the difference between the observed and expected events (O-E) for one group
#is significantly different from 0 (that is, the two groups have significantly different
#hazard functions), you need an estimate of the variance of that difference in the case 
#(null hypothesis) that the groups have the same hazard function.
#At each time of an event, the variance in the expected number of events for a group is 
#related to the expected number of events for the group at that time, 
#the number of cases still at risk both overall and in the group at that time, 
#and the number of total observed events at that time

#wilcoxon tests (Prentice correction)
survdiff(stset~age_q, rho=1)
survdiff(stset~sex, rho=1)
survdiff(stset~sample.yr, rho=1)
survdiff(stset~kappa_q, rho=1)
survdiff(stset~lambda_q, rho=1)
survdiff(stset~flc.grp, rho=1)
survdiff(stset~creat_q, rho=1)
survdiff(stset~mgus, rho=1)
#same results there

#Now we estimate a multivariate Cox regression model
stset <- Surv(futime, death)

# complete model
#we exclude the variables sample.yr and chapter
mod1 <- coxph(stset ~ age + sex + kappa + lambda + flc.grp + creatinine + mgus)
mod1
#the significative variables are: age, sex, lamnda, flc.group
#we can say that the hazard ratio:
#- increases 10% each additional year age
#- increases of 32% if M
#- increases of 17% for each increment of the lambda portion
#- increase of 0.5% if we pas from a flc group to another one

#variable selection
mod2 <- coxph(stset ~ age + sex + I(kappa^1) + I(kappa^2) + lambda + 
                as.factor(flc.grp) + creatinine + mgus)
mod2
#adding the square of kappa (hp of non linear effect) and the different group
#creatinine and mgus are still not  significant so we remove them
#there is no difference between the group o we can remove them too
#(E' UN PROBLEMA PERCHE DICEVA CHE CERA DIFFERENZA PERCHEEEEEEEE)

mod3 <- coxph(stset ~ age + sex + I(kappa^1) + I(kappa^2) + lambda)
mod3
#now all the variables are significative and we can say that the hazard ratio:

ggforest(mod3, data = flchain) #perchè sia famale che male sono reference?
#poi non riesco a fare questo grafico considerando i quantili di age quindi ho deciso di usare solo age

summary(mod3)$conf.int
par(mfrow=c(1,1))
plot(survfit(mod3), main = "boh", xlab="days")
lines(fitall, col="grey")
#overall estimated hazard versus the kaplan mayer 
#(black is the cox estimates while the grey is the non parametric mayer estimate)
#overestimation

#PH ASSUMPTION 
#(NON SO IN QUALE MODELLO VA FATTO, SE QUELLO SIGNIFICATIVO O QUELLO COMPLETO)
#IN OGNI CASO LA PROCEDURA E LA SPIEGAZIONE SONO QUEESTI

#the aim is to check if there are relation with time
mod <- coxph(stset ~ age + sex + kappa + lambda + flc.grp + mgus)
cox.zph(mod, transform = "identity", terms=F) # time
#global test indicates that there is some issues with ph assumption
#the covariate that create problem is the one with the p-value that are less that 0.05
#we can have problem with age and lambda
#the overall ph is violated since the p-value is less that 0.05
#so we can say that the PH assumption is ok
cox.zph(mod, transform = "rank", terms=F) # ranks of time
#here is the same

#QUI VA MODIFICATO IL DATASET TOGLIENDO QUELLI CHE HANNO FUTIME =0
#oppure non fare la transformata log
cox.zph(mod1, transform = "log", terms=F) # log-time

#then we want to represent the scaled schoenfeld residual to respect to time
#we want to obtain a flat line
#if not there is a correlation with time
test.ph <- cox.zph(mod, transform="identity", terms=F) #we store the output
for(i in 1:6){plot(test.ph[i])}
#we can see in the y axis the variable that we are testing
#we do not obtain a straight line for age - so it has a relation with time

#let's see if we include the variable age quantile 
mod_q <- coxph(stset ~ age_q + sex + kappa + lambda + flc.grp + mgus)
test.ph <- cox.zph(mod_q, transform="identity", terms=F) 
#now the problem are with kappa, lamda and flc.group
#the quantile of age did not violate the PH assumption
for(i in 1:8){plot(test.ph[i])}
#but if we see the plot, the quantile of the age are not perfectly on a straight line
#while kappa, lambda yes
#flc.group is in doubt

#now for all the variable that has a correlation with time we must create the dummy variable
flchain$age_q_1=ifelse(age_q=="[50,55]",1,0)
flchain$age_q_2=ifelse(age_q=="(55,63]",1,0)
flchain$age_q_3=ifelse(age_q=="(63,72]",1,0)
flchain$age_q_4=ifelse(age_q=="(72,101]",1,0)

newmod <- coxph(stset ~ age + sex + kappa + lambda + flc.grp + mgus + 
                  tt(age_q_1) + tt(age_q_2) + tt(age_q_3) + tt(age_q_4),
                method="breslow",data=flchain,tt = function(x, t, ...) x * t)
newmod #not good

newmod2 <- coxph(stset ~ age + sex + kappa + lambda + flc.grp + mgus + tt(age),
                 method="breslow",data=flchain,tt = function(x, t, ...) x * t)
newmod2 

newmod3 <- coxph(stset ~ age + sex + I(kappa^1) + I(kappa^2) + lambda + tt(age),
                 method="breslow",data=flchain,tt = function(x, t, ...) x * t)
newmod3
#we can see that now tt(age) is significative but kappa and mgus not
#at the end of this procedure we can do variable selection (regsubset)
library(leaps)

#at the end of that we can compare mod3 with the new obtained model 
#in particular we can check
# Loglikelihood (null vs. fitted)
mod3$loglik
newmod3$loglik
#since the loglikelihood seems smaller in the newmod3 this model is to be preferred

# Likelihood ratio test statistics
#it's the same that we have in the output of the two models
2*diff(mod3$loglik)
2*diff(newmod3$loglik)

# AIC (we choose the model with the smallest AIC)
AIC(mod3, newmod3)
#we can see that they jave a different number of degrees of fredom
#since the models are not nested (different df) we have to correct for the degrees of freedom
-2*mod3$loglik[2]+2*5
-2*newmod3$loglik[2]+2*6
#we obtain the same result so the model to be preferred is the newmod3

plot(survfit(mod3), main = "model3", xlab="days")
plot(survfit(newmod3), main = "newmodel3", xlab="days")
lines(fitall, col="grey")
#------

#CHECK FOR LINEARITY
#for linear model usually we check this assumption by plotting residual vs fitted
#here similar procedure

#we first have to find the continuous variable and check for their linearity
#the continuous variables are: age, kappa, lambda
mod <- coxph(stset ~ age + sex + kappa + lambda + flc.grp + mgus)
mart.res <- residuals(mod, type="martingale")
scatter.smooth(mart.res ~ age)
scatter.smooth(mart.res ~ kappa)
scatter.smooth(mart.res ~ lambda)
#if the line is flat, linearity in not an issue, otherwise yes
#graphically we can see that for age there is no linearity, and also for kappa
#maybe lambda has a linear effect but we can check with the fractional polynomial

# fractional polynomials
library(mfp)
cox_mfp <- mfp(stset ~ fp(age)+ sex + fp(kappa) + fp(lambda) + flc.grp + mgus, 
               family=cox, method="breslow", verbose=T,data=flchain)
#since we are using the full model, in the first cycle the variables are ordered
#in association with the pvalue (the first one is the one with the smallest pvalue)
#let's evaluate age
#first line is the deviance for the model with all covariate except age
#second line full linear model including age, including transformation using power 1
#third line is with one fractional polynomial and the best one is 2
#last deviance for two fractional polynomial with the combination of -2 and 1

#the 0 means that it is applied the logarithm transformation
#two zero: ln(lambda) + ln(lambda)*ln(lambda)

#in the transformation R tells us which transformation scaling are applied to the variable 
cox_mfp
#the interesting part is the "Transformations of covariates" that tells us how to 
#use the variable in the models, the polynomial to use and how to modify them

#there is also the 3 deviances: null model, linear model (model with all the covariate with linear effect)
#and the deviance of the last model
#at the end there is the complete model, the best one 
cox_mfp$pvalues #this is useful for selecting the degree for the polynomial
#there are 3 p value because three test are performed
#p.null is the test when we compare having the polynomial with the one without the variables 

#so what we can say is that the best model is the one including
#age, sex, flcgrp, lambda, mgus as linear and kappa as non linear
#PERO NON CAPISCO PERCHE AGE SI INSERIESCE NORMALMENTE, FORSE PER LA QUESTIONE DEI QUANTILI

# deviance residuals
#they are function of the martingale residual, thy are transformed so that they are
#symmetric distributed around zero and has sd=1 so approximately follows N(0,1)
#the idea: we can plot them vs the covariate as for the martingale or having an overall plot
#of fitted values (linear predictor) vs deviance residuals
dev.res <- residuals(mod, type="deviance")
scatter.smooth(dev.res ~ age)
scatter.smooth(dev.res ~ kappa)
scatter.smooth(dev.res ~ lambda)
#we want to have a straight line (age no straight line) 

d.res <- residuals(mod,type="deviance")
s <- mod$linear.predictors
summary(d.res)
#the mean should be near zero
plot(s,d.res)
#we can see that there is a lot of variability
#this is also because we are using the model without the interaction of time or all the 
#transformation
abline(h=2, lty=3)
abline(h=-2, lty=3)
#we can check also for outliers


