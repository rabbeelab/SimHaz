## ---- echo=F-------------------------------------------------------------
library(survival)
library(powerSurvEpi)
library(SimHaz)
library(knitr)
data(Oph)

## ----echo=T--------------------------------------------------------------
#============== Estimate Paramters =================

censor_time = median(Oph$times[Oph$status==0])
cox = coxph(Surv(time = times,event = status)~factor(group), data=Oph)
beta = summary(cox)$coef[,"coef"]

exp = nrow(Oph[Oph$group=='E',])/nrow(Oph)
fit = survfit((Surv(time = times,event = status)~group) ,dat=Oph)

## ----echo=F--------------------------------------------------------------
param = data.frame(exp,beta,fit$strata[1],censor_time,row.names = '')
kable(param,align ='c',row.names = NA,col.names = c('Effective exposure proportion',
                                     'Beta','Median control survival time',
                                     'Median time to censor'),
      caption = "Estimates from Oph time-fixed dataset")

## ----echo=T, cache=T-----------------------------------------------------
#Run power simulation on 450 subjects to get an N_effective value close to 354

power1 = getpower.method1(500, 450, duration = 6, med.TTE.Control = 6, 
                          rho = 1, med.TimeToCensor = 5, b=beta, exp.prop=exp,
                          type='fixed', prop.fullexp = full_exp, 
                          scenario = 'fixed(500)',maxrelexptime = 1/6, 
                          min.futime = 1, min.postexp.futime = 1)

## ---- echo=F-------------------------------------------------------------
simu = data.frame(names = c('N','N Effective','Total Events','Control Events','Exposed Events','Power'),simulation = as.numeric(power1[c(3,11,15,16,17,20)]), oph = c(NA, 354,154,89,65,"0.638*"))

kable(simu, digits = 2, align = 'c',col.names = c('Parameter','Simulation Results','Oph Data Event Count'),caption = "Simulation results vs Data")

## ----echo=F, cache=T, fig.width=15, fig.height = 7-----------------------
par(mfrow=c(1,2))
power1 = getpower.method1(500, 450, duration = 6, med.TTE.Control = 6, 
                          rho = 1, med.TimeToCensor = 5, b=beta, exp.prop=exp,
                          type='fixed', prop.fullexp = full_exp, 
                          scenario = 'fixed(500)',maxrelexptime = 1/6, 
                          min.futime = 1, min.postexp.futime = 1, simu.plot = T)

Oph_plot = Oph
Oph_plot$id = 1:354
Oph_plot$start = 0
Oph_plot$stop = Oph$times
Oph_plot[Oph$group == 'E', "x"] = 1
Oph_plot[Oph$group == 'C', "x"] = 0
plot_simuData(Oph_plot, "Oph Survival Data")

## ---- echo=F-------------------------------------------------------------
control = c(nrow(Oph[Oph$status==0 & Oph$group=='C' & Oph$times==6,]),nrow(Oph[Oph$status==1 & Oph$group=='C' & Oph$times==6,]))

exposed = c(nrow(Oph[Oph$status==0 & Oph$group=='E' & Oph$times==6,]),nrow(Oph[Oph$status==1 & Oph$group=='E' & Oph$times==6,]))

rows = c('0 (no event)','1 (event)')

time6 = data.frame(rows = rows, control=control, exposed=exposed)
kable(time6, align='c', col.names = c('','Control','Exposed'), caption = 'Oph Data Group vs Event Status (at time=6)')

## ---- echo=F,fig.width=5, fig.height = 3.5, fig.align='center'-----------
hist(Oph$times[Oph$group=='C' & Oph$status==1], main = 'Control Group Time to Event',
     xlab = 'Event Time (Years)')

## ---- echo=T, cache=T, fig.width=6, fig.height = 4-----------------------

power2 = getpower.method1(1500, 480, duration = 6, med.TTE.Control = 3.5, 
                          rho = 1, med.TimeToCensor = 5, b=beta, exp.prop=exp, 
                          type='fixed', prop.fullexp = full_exp, 
                          scenario = 'fixed(500)',maxrelexptime = 1/6, min.futime = 1,
                          min.postexp.futime = 1, simu.plot=T)


## ---- echo=F-------------------------------------------------------------
simu2 = data.frame(names = c('N','N Effective','Total Events','Control Events','Exposed Events','Power'),simulation = as.numeric(power2[c(3,11,15,16,17,20)]), oph = c(NA, 354,154,89,65,0.638))

kable(simu2, digits = 2, align = 'c',col.names = c('Parameter','Simulation Results','Oph Data Event Count'),caption = "Simulation results vs Data")

## ----eval=F, echo=T------------------------------------------------------
## for(i in seq(50,1000,50)){
##     power1 = getpower.method1(500, i, duration = 6, med.TTE.Control = 3.5,
##                               rho = 1, med.TimeToCensor = 5, b=beta,
##                               exp.prop=exp, type='fixed',
##                               prop.fullexp = full_exp, scenario = 'fixed(500)'
##                               ,maxrelexptime = 1/6, min.futime = 1,
##                               min.postexp.futime = 1, simu.plot=F)
## }

## ---- echo=F,fig.width=6, fig.height = 4---------------------------------
data = data.frame(sample_size = c(36.724,73.336,110.182,146.952,184.484,220.622,257.862,294.21,330.556,367.21,403.942,441.696,478.644,514.36,550.868,588.148,624.54,661.724,699.294,735.39), power = c(0.094,0.162,0.232,0.322,0.38,0.408,0.43,0.518,0.568,0.68,0.65,0.696,0.716,0.738,0.83,0.844,0.866,0.88,0.866,0.918))
plot(data, type='b',cex=0.75, main = 'Power vs Sample Size (Oph Data)',
    xlab = 'N Effective', ylab='Power')
abline(h=0.8, col='red')


