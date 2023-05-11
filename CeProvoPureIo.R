install.packages('survminer')
install.packages('survival')
library(survminer)
library(survival)
data("flchain")
attach(flchain)

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
plot(age_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of age quantiles')
legend("bottomleft", legend = c("[50,55]", "(55,63]", "(63, 72]", "(72, 101]"), lty = 1:2, col = c('red', 'blue', 'black', 'green'), horiz=T, cex = 0.8)


# sex
sex_death = univariate_km(sex)
plot(sex_death, col = c('red', 'blue'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of sex', ylim=c(0.65, 1))
legend("bottomleft", legend = c('M', 'F'), col = c('red', 'blue'), lty = 1:2, horiz = T)


# kappa
kappa_quantile = cut(kappa, quantile(kappa), include.lowest =  T)
kappa_death = univariate_km(kappa_quantile)
plot(kappa_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of kappa', ylim=c(0.3, 1))
legend("bottomleft", legend = c('[0.01,0.96]', '(0.96,1.27]', '(1.27,1.68]', '(1.68,20.5]'), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8 )


# lambda
lambda_quantile = cut(lambda, quantile(lambda), include.lowest =  T)
lambda_death = univariate_km(lambda_quantile)
plot(lambda_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of lambda', ylim=c(0.3, 1))
legend("bottomleft", legend = c('[0.04,1.2]', '(1.2,1.51]', '(1.51,1.92]', '(1.92,26.6]'), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8 )


# flc.gpr
flc.grp_death = univariate_km(flc.grp) # log-rang diviso in prime 5 seconde 5
plot(flc.grp_death, col = c('red', 'blue', 'black', 'green', 'purple', 'red', 'blue', 'black', 'green', 'purple'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of flc.grp', ylim=c(0.2, 1))
legend("bottom", legend = c(1,2,3,4,5,6,7,8,9,10), col = c('red', 'blue', 'black', 'green', 'purple', 'red', 'blue', 'black', 'green', 'purple'), lty = 1:2, horiz = T, cex = 0.5)


# creatinine
creatinine_quanitle = cut(creatinine, quantile(creatinine, na.rm = T), include.lowest =  T)
creatinine_death = univariate_km(creatinine_quanitle)
plot(creatinine_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of creatinine', ylim=c(0.4, 1))
legend("bottomleft", legend = c("[0.4,0.9]", "(0.9,1]", "(1,1.2]", "(1.2,10.8]"), col = c('red', 'blue', 'black', 'green'), lty = 1:2, horiz = T, cex = 0.8)


# mgus
mgus_death = univariate_km(mgus)
plot(mgus_death, col = c('red', 'blue'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of mgus', ylim=c(0.6, 1))
legend("bottomleft", legend = c(0,1), col = c('red', 'blue'), lty = 1:2, horiz = T, cex = 1)


### QUESTION 2  ##########################################################################
surv_object = Surv(futime, death)

# complete model
cox_multi <- coxph(surv_object ~ age + sex + kappa + lambda + flc.grp + creatinine + mgus)
cox_multi

### QUESTION 3 COMMENTING ############################################


### QUESTION 4 CONF INT
ggforest(cox_multi, data = flchain)
summary(cox_multi)$conf.int


### QUESTION 5 VARIABLE SELECTION
anova(cox_multi)
# here we remove creatinine
cox_multi <- coxph(surv_object ~ age + sex + kappa + lambda + flc.grp + mgus)
cox_multi
anova(cox_multi)

# here we remove mgus
cox_multi <- coxph(surv_object ~ age + sex + kappa + lambda + flc.grp)
cox_multi
anova(cox_multi)

# here we remove kappa
cox_multi <- coxph(surv_object ~ age + sex + lambda + flc.grp)
cox_multi
anova(cox_multi)

ggforest(cox_multi, data = flchain)










