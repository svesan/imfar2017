library(survival)
data(cancer)
cancer <- within(cancer, {
  age.cat <- factor(as.numeric(cut(age, c(-Inf, 50, 60, 70, Inf))))
  meal.cat <- factor(as.numeric(cut(meal.cal, c(-Inf, quantile(meal.cal, c(0.25, 
                                                                           0.5, 0.75), na.rm = TRUE), Inf))))
  wt.cat <- factor(as.numeric(cut(wt.loss, c(-Inf, quantile(wt.loss, c(0.25, 
                                                                       0.5, 0.75), na.rm = TRUE), Inf))))
})

kmfit0 <- survfit(Surv(time = time, event = status) ~ 1, data = cancer, conf.type = "log-log")
kmfit1 <- survfit(Surv(time = time, event = status) ~ sex, data = cancer, conf.type = "log-log")

# cox proportional hazards
coxfit0 <- survfit(coxph(Surv(time = time, event = status) ~ strata(age.cat), data = cancer))

coxfit1 <- survfit(coxph(Surv(time = time, event = status) ~ strata(I(age > 45)), data = cancer))

kmplot(kmfit0, lty.ci = 2, col.band = NULL, dev = FALSE, atrisk = FALSE)

library(survminer)

ggsurvplot(coxfit0, atrisk = FALSE)

ggsurvplot(coxfit0, atrisk = FALSE, risk.table=TRUE)
