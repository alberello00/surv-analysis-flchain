library(survival)
data("flchain")

# QUESTION 1 - UNIVARIATE ANALYSIS ON ALL 

univariate_km = function(var) {
  surv_object <- Surv(futime, death)
  result_km <- survfit(surv_object ~ var, conf.type = "log-log")
  
  return(result_km)
}

#  we will not include variables: kappa, lambda (condensed into flc.grp)

age_quantile <- cut(age, quantile(age), include.lowest =  T)






age_death = univariate_km(age_quantile)
sex_death = univariate_km(sex)
sample.yr_death = univariate_km(sample.yr)
flc.grp_death = univariate_km(flc.grp)
creatinine_death = univariate_km(creatinine)
mgus_death = univariate_km(mgus)
chapter_death = univariate_km(chapter)

plot(age_death, col = c('red', 'blue', 'black', 'green'), lty = 1:2, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of age quantiles')
legend("bottomleft", legend = c("[50,55)", "[55,63)", "[63, 72)", "[72, 101)"), lty = 1:2, col = c('red', 'blue', 'black', 'green'))



plot(sample.yr_death,col='black', lty = 1, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of sample.yr')
plot(flc.grp_death,col='black', lty = 1, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of flc.grp')
plot(creatinine_death,col='black', lty = 1, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of creatinine')
plot(mgus_death,col='black', lty = 1, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of mgus')
plot(chapter_death,col='black', lty = 1, xlab = "follow up time", ylab = "estimated S(t)", main = 'Survival function as a function of futime')


# legend("topright", legend = c("treat", "control"), lty = c(1, 2),col=c("black","red"))


for (i in 1:8) {
  
}






# Fleming-Harringon (Nelson-Aalen) estimator
?survfit
result.na <- survfit(g ~ 1, conf.type="log-log", stype=2)
summary(result.na)

# cumulative hazard and corresponding standard error
round(result.na$cumhaz,4)
round(result.na$std.chaz,4)

plot(result.na)
abline(h=0.5, col="red",lty=3)

tt_full<-c(tt,1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
cens_full<- c(cens, rep(1,21))
control <- rep(c(0,1), each=21)

results <- survfit(Surv(tt_full, cens_full) ~ control, conf.type="log-log")
results

summary(results)
plot(results,col=c("black", "red"),lty = 1:2,xlab = "Weeks", ylab = "estimated S(t)")
legend("topright", legend = c("treat", "control"), lty = c(1, 2),col=c("black","red"))

quantile(results)


