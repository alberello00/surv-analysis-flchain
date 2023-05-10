library(survival)
data("flchain")
flchain
summary(flchain)

# La gammopatia monoclonale è una patologia rara, caratterizzata dall'accumulo nel midollo osseo
# e nel sangue di una proteina anomala (paraproteina, proteina monoclonale o M proteina). --> Patologia cancerogena

# Analisi fatta su campioni di sangue
#L'elettroforesi delle sieroproteine (proteine sieriche, cioè del siero, che circolano attraverso il sistema circolatorio)
# è un esame di screening che permette di separare le proteine del siero in base alla loro mobilità in campo elettrico.

# Le molecole delle Immunoglobuline (gli anticorpi) sono proteine composte da due catene peptidiche "pesanti"
# identiche unite a due catene "leggere" identiche.--> catene leggere kappa e lambda --> la sovrapproduzione indica malfunzionamenti
# Il rapporto kappa/lambda per avere valori non anomali è: 0.26-1.65


#La causa principale di questa malattia è l’alterazione di alcune cellule del midollo osseo e la loro conseguente produzione eccessiva
#di proteina monoclonale. L’accumulo di questa proteina riduce lo spazio delle cellule sane e si concentra anche nel sangue. 
#Nonostante non si sappiano precisamente le cause di questa alterazione, recenti ricerche sembrano
#individuarne le cause in malattie autoimmuni (es. artrite, remautide).

# Let's try something
#### Lab 1 (leukemia) ####
#Change into a survival variable
g <- Surv(flchain$futime, flchain$death)
class(g)

#Kaplan-Meier est
result.km <- survfit(g ~ 1, conf.type = "log-log")
result.km
summary(result.km)
plot(result.km, ylim = c(0.6, 1), col='blue')

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


