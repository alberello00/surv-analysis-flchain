#install.packages('survminer')
#install.packages('survival')
# install.packages('flexsurv')
library(survminer)
library(flexsurv)
library(mfp)
library(survival)
data("flchain")
data = flchain

### DATA PREPROCESSING
# Change bad names
names(data)[names(data) == "sample.yr"] <- 'sample.year'
names(data)[names(data) == "flc.grp"] <- 'flc.group'

# Adjust abbreviated levels
levels(data$sex) <- c('Female', 'Male')
levels(data$sex)

# Cast to Factor
data$flc.group <- as.factor(data$flc.group)

# Reduce many levels Factor
sort(table(data$chapter))
chapter <- c('Circulatory', 'Neoplasms', 'Respiratory', 'Mental', 'Nervous')
levels(data$chapter)[!levels(data$chapter) %in% chapter] <- 'Others'
sort(table(data$chapter))

# Convert boolean to Factor
unique(data$mgus)
unique(data$death)

data$mgus <- factor(data$mgus,
                    labels = c('non-mgus', 'mgus'))
data$death <- factor(data$death,
                     labels = c('alive', 'dead'))

# convert num to int
data$age <- as.integer(data$age)
data$sample.year <- as.integer(data$sample.year)
data$futime <- as.integer(data$futime)

# Cut variables
table(data$sample.year)
data$recruits <- cut(data$sample.year,
                     breaks = c(1994, 1997, 2000, 2003),
                     labels = c('early', 'middle', 'late'))
table(data$recruits)

# Create useful variables
data <- transform(data,
                  flc       = kappa + lambda,
                  flc.ratio = kappa / lambda)

### UNIVARIATE PLOTS
ggplot(data, aes(age)) +
  geom_histogram(binwidth = .5)

ggplot(data, aes(sample.year)) +
  geom_histogram(binwidth = 1)

# Density of samples by epoch
ggplot(data, aes(recruits, (after_stat(count) / sum(after_stat(count))))) +
  geom_bar() +
  ylab('Frequency') +
  scale_y_continuous(breaks = seq(0, 1, .1))

# Frequency of kappa, lambda and creatinine sera
ggplot(data) +
  geom_freqpoly(aes(kappa, color = 'kappa'), binwidth = .1) +
  geom_freqpoly(aes(lambda, color = 'lambda'), binwidth = .1) +
  geom_freqpoly(aes(creatinine, color = 'creatinine'),
                data     = subset(data, !is.na(creatinine)),
                binwidth = .1) +
  scale_colour_manual(name   = 'Sera',
                      breaks = c('creatinine', 'kappa', 'lambda'),
                      values = c('green', 'red', 'blue')) +
  xlab('kappa, lambda and creatinine')

# Frequency of kappa, lambda and creatinine sera (log)
ggplot(data) +
  geom_freqpoly(aes(log(kappa), color = 'kappa'), binwidth = .1) +
  geom_freqpoly(aes(log(lambda), color = 'lambda'), binwidth = .1) +
  geom_freqpoly(aes(log(creatinine),
                    color = 'creatinine'),
                data  = subset(data,!is.na(creatinine)),
                binwidth = .1) +
  scale_x_continuous(breaks = seq(-4, 4, .5)) +
  scale_colour_manual(name   = 'Sera',
                      breaks = c('creatinine', 'kappa', 'lambda'),
                      values = c('green', 'red', 'blue')) +
  xlab('Log of creatinine, kappa and lambda')

str(flchain)
hist(age)
hist(kappa)
hist(log(kappa))
hist(lambda)
hist(log(lambda))
hist(flc.grp)

### QUESTION 1 - UNIVARIATE ANALYSIS ON ALL 

univariate_km = function(var) {
  surv_object <- Surv(futime, death)
  result_km <- survfit(surv_object ~ var, conf.type = "log-log")
  
  return(result_km)
}

#  we will not include variables: chapter (too many level)

# age
age_quantile <- cut(age, quantile(age), include.lowest =  T)
age_death = univariate_km(age_quantile)
plot(age_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of age quantiles'); legend("bottomleft", legend = c("[50,55]", "(55,63]", "(63, 72]", "(72, 101]"), lty = 1:2, col = c('red', 'blue', 'black', 'green'), horiz=T, cex = 0.8)


# sex
sex_death = univariate_km(sex)
plot(sex_death, col = c('red', 'blue'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of sex', ylim=c(0.65, 1)); legend("bottomleft", legend = c('M', 'F'), col = c('red', 'blue'), lty = 1:2, horiz = T)


# kappa
kappa_quantile = cut(kappa, quantile(kappa), include.lowest =  T)
kappa_death = univariate_km(kappa_quantile)
plot(kappa_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of kappa', ylim=c(0.3, 1));legend("bottomleft", legend = c('[0.01,0.96]', '(0.96,1.27]', '(1.27,1.68]', '(1.68,20.5]'), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8 )


# lambda
lambda_quantile = cut(lambda, quantile(lambda), include.lowest =  T)
lambda_death = univariate_km(lambda_quantile)
plot(lambda_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of lambda', ylim=c(0.3, 1));legend("bottomleft", legend = c('[0.04,1.2]', '(1.2,1.51]', '(1.51,1.92]', '(1.92,26.6]'), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8 )


# flc.gpr
flc.grp_death = univariate_km(flc.grp) # log-rang diviso in prime 5 seconde 5
plot(flc.grp_death, col = c('red', 'blue', 'black', 'green', 'purple', 'red', 'blue', 'black', 'green', 'purple'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of flc.grp', ylim=c(0.2, 1));legend("bottom", legend = c(1,2,3,4,5,6,7,8,9,10), col = c('red', 'blue', 'black', 'green', 'purple', 'red', 'blue', 'black', 'green', 'purple'), lty = 1:2, horiz = T, cex = 0.5)


# creatinine
creatinine_quanitle = cut(creatinine, quantile(creatinine, na.rm = T), include.lowest =  T)
creatinine_death = univariate_km(creatinine_quanitle)
plot(creatinine_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of creatinine', ylim=c(0.4, 1));legend("bottomleft", legend = c("[0.4,0.9]", "(0.9,1]", "(1,1.2]", "(1.2,10.8]"), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8)


# mgus
mgus_death = univariate_km(mgus)
plot(mgus_death, col = c('red', 'blue'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of mgus', ylim=c(0.6, 1));legend("bottomleft", legend = c(0,1), col = c('red', 'blue'), lty = 1:2, horiz = T, cex = 1)


### QUESTION 2  ##########################################################################
surv_object = Surv(futime, death)

# complete model
cox_multi_1 <- coxph(surv_object ~ age + sex + I(kappa^1) + I(kappa^2) + lambda + as.factor(flc.grp) + creatinine + mgus )
cox_multi_1

### QUESTION 3 COMMENTING ############################################


### QUESTION 4 CONF INT
ggforest(cox_multi_1, data = flchain)
summary(cox_multi_1)$conf.int


### QUESTION 5 VARIABLE SELECTION  ###################################
cox_multi_1
# here we remove creatinine
cox_multi_2 <- coxph(surv_object ~ age + sex + kappa + lambda + flc.grp + mgus)
cox_multi_2

# here we remove mgus
cox_multi_3 <- coxph(surv_object ~ age + sex + kappa + lambda + flc.grp)
cox_multi_3

# here we remove kappa 
cox_multi_4 <- coxph(surv_object ~ age + sex + lambda + flc.grp)
cox_multi_4
anova(cox_multi_4, cox_multi_2) # write some comments about the fact that creatinine has misisng values and therefore we choose as starting model the model without it (otherwise anova does not work)

ggforest(cox_multi_4, data = flchain)

### QUESTION 6 ###################################
# identity
ph_test = cox.zph(cox_multi_1, transform = 'identity')
ph_test

ph_test1 = cox.zph(cox_multi_1, transform = 'log')
ph_test1
''' 
globally we can notice a small pvalue, indicating that AT LEAST one covariate is not respecting
H0: all respect proportianal hazard
with a significance level of 5% we can state that age and lambda does not respect proportional hazard
'''

plot(ph_test[1], col = 'red', lwd = 4)
plot(ph_test1[1], col = 'red', lwd = 4)


# kaplan meyer
ph_test_2 <- cox.zph(cox_multi_4, transform = "km")
ph_test_2

'''
again age and lambda. Notice how the pval for lambda is not that small, and indeed the chart
is not perfectly significant
'''

plot(ph_test_2[1], col = 'red', lwd = 4)
plot(ph_test_2[3], col = 'red', lwd = 4, ylim=c(0,2))

# logarithm
ph_test_3 <- cox.zph(cox_multi_4, transform = "log")
##### remove rows with 0 as 'futime' 
ph_test_3

'''
here it is kinda weird as we have all NaN values
'''

### QUESTION 7 ###################################
# Cox model without PH
flchain$flc.grp <- as.factor(flchain$flc.grp)

# Run coxph with the updated data types
cox_noph <- coxph(surv_object ~ age + tt(age) + sex + I(kappa^1) + I(kappa^2) 
                  + lambda + as.factor(flc.grp) + creatinine + mgus ,
                  ties = "breslow",
                  tt = function(x, t, ...) x * log(t))

cox_noph1 <- coxph(surv_object ~ age + tt(age) + sex + I(kappa^1) + I(kappa^2) 
                  + lambda + as.factor(flc.grp) + creatinine + mgus ,
                  ties = "breslow",
                  tt = function(x, t, ...) x * t)

cox_noph
cox_noph1


### QUESTION 8 #########################################

#this just to show how the resulting scatter plot should be
#remember that to run the martingale residuals it's needed the model with all the covariates so cox_multi_1

cox_multi_2
mart.res <- residuals(cox_multi_2, type = "martingale")
scatter.smooth(mart.res ~ age, main = 'mar.res versus age', col = 'red')
scatter.smooth(mart.res ~ lambda, main = 'mar.res versus lambda')
scatter.smooth(mart.res ~ futime, main = 'mar.res versus futime')
scatter.smooth(mart.res ~ creatinine, main = 'mar.res versus creatinine')


cox_mfp <- mfp(Surv(futime, death) ~ fp(age) + sex + fp(kappa) + fp(lambda) + flc.grp + mgus , 
               family=cox, method="breslow", verbose=T, data=flchain
               )
cox_mfp
cox_mfp$pvalues
#above you can check that the scatter plot present the error value of the different length
#the martingale residuals should be computed with the complete model 

#cox_multi_1
#mart.res <- residuals(cox_multi_1, type = "martingale")
#scatter.smooth(mart.res ~ age)





### QUESTION 9 ######################################################
mod_expPH <- flexsurvreg(Surv(futime, death) ~ age + sex + lambda + as.factor(flc.grp), dist="exp", data=flchain)

mod_expPH # complete output
mod_expPH$res # coefficients
coef(mod_expPH) # coefficients
exp(coef(mod_expPH)[1])
AIC(mod_expPH) # AIC
logLik(mod_expPH) # log-likelihood

#weibull
mod_weiPH <- flexsurvreg(Surv(futime, death) ~ age + sex + lambda + as.factor(flc.grp), dist="weibullPH", data=flchain)
# hazard function monotonically decreasing

mod_weiPH 
mod_weiPH$res 
coef(mod_weiPH) 
exp(coef(mod_weiPH)[1:2])
AIC(mod_weiPH) 
logLik(mod_weiPH)




