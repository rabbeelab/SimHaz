## ---- echo=F-------------------------------------------------------------
library(knitr)
library(xtable)
library(SimHaz)

## ----eval=F--------------------------------------------------------------
## getpower.method1(nSim = 500, N = 600, beta = 0.3,type="td",
##                  scenario='time dependent example' ,exp.prop=0.2,
##                  maxrelexptime=1/6, min.futime = 4)

## ---- cache=T, echo=F, warning=F-----------------------------------------
betas=seq(.1,.7,.1)
SS=c(600,800,1000)

final = NULL
for (l in 1:length(betas))
{ 
  for(s in 1:length(SS)){
    ret<- getpower.method1(nSim = 500, N=SS[s],beta=betas[l],type="td",scenario=paste('td', s),exp.prop=0.2,maxrelexptime=1/6, min.futime = 0)
    final = rbind(final,ret)
  }
}

for (l in 1:length(betas))
{ 
  for(s in 1:length(SS)){
    ret<- getpower.method1(nSim = 500, N=SS[s],beta=betas[l],type="fixed",scenario=paste('fixed', s),exp.prop=0.2,maxrelexptime=1/6, min.futime = 0)
    final = rbind(final,ret)
  }
}

## ---- echo=F-------------------------------------------------------------
final_table = final[final$i_beta=='0.3',]
kable(final_table[c(4,1,5,2,6,3),c(2,3,13,14,18,19,20)],caption="Simulation results for scenario 1 - exposure time generated from Uniform(0, follow-up time/6), no minimum follow-up time is considered",row.names = F, col.names = c('Type','Sample Size',"Beta hat",'Hazard ratio','Median Survival-Unexposed','Median Survival-Exposed','Power'), align = 'c', digits = c(1,3,3,3,3,3,3))

## ---- eval=F-------------------------------------------------------------
## #Time Dependent Plots
## plot_power(final, 600, type='td', exp.prop=0.2, min.futime=4, min.postexp.futime=0,
##            show.plot=TRUE, newplot=TRUE, col='orange', lty=2, lwd=2, pch=16)
## plot_power(final, 800, type='td', exp.prop=0.2, min.futime=4, min.postexp.futime=0,
##            show.plot=TRUE, newplot=FALSE, col='purple', lty=2, lwd=2, pch=16)
## plot_power(final, 1000, type='td', exp.prop=0.2, min.futime=4, min.postexp.futime=0,
##            show.plot=TRUE, newplot=FALSE, col='red', lty=2, lwd=2,pch=16)
## 
## #Time Fixed Plots
## plot_power(final,600, type='fixed', exp.prop=0.2, min.futime=0, min.postexp.futime=0,
##            show.plot=TRUE, newplot=FALSE, col='orange', lty=1, lwd=2, pch=16)
## plot_power(final, 800, type='fixed', exp.prop=0.2, min.futime=0, min.postexp.futime=0,
##            show.plot=TRUE, newplot=FALSE, col='purple', lty=1, lwd=2 ,pch=16)
## plot_power(final, 1000, type='fixed', exp.prop=0.2, min.futime=0, min.postexp.futime=0,
##            show.plot=TRUE, newplot=FALSE, col='red', lty=1, lwd=2, pch=16)
## 

## ---- fig.height=6, fig.width=8, echo=F----------------------------------
temp = plot_power(final,600,type='td',exp.prop=0.2,min.futime=0,min.postexp.futime=0,show.plot=TRUE,newplot=TRUE,
                         col='orange',lty=2,lwd=2,pch=16)
temp = plot_power(final,800,type='td',exp.prop=0.2,min.futime=0,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
               col='purple',lty=2,lwd=2,pch=16)
temp = plot_power(final,1000,type='td',exp.prop=0.2,min.futime=0,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
               col='red',lty=2,lwd=2,pch=16)

temp = plot_power(final,600,type='fixed',exp.prop=0.2,min.futime=0,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
                  col='orange',lty=1,lwd=2,pch=16)

temp = plot_power(final,800,type='fixed',exp.prop=0.2,min.futime=0,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
                  col='purple',lty=1,lwd=2,pch=16)
temp = plot_power(final,1000,type='fixed',exp.prop=0.2,min.futime=0,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
                  col='red',lty=1,lwd=2,pch=16)


abline(h=0.8, lty=3)
legend("bottomright",cex=0.9,legend=c('Sample Size 1000, time-fixed exposure', 'Sample Size 1000, time-dependent exposure','Sample Size 800, time-fixed exposure','Sample Size 800, time-dependent exposure','Sample Size 600, time-fixed exposure', 'Sample Size 600, time-dependent exposure'),lwd= rep(1,6),lty = c(1,3,1,3,1,3),col=c("red","red","orange","orange","purple","purple"))


## ---- eval=F-------------------------------------------------------------
## getpower.method1(nSim = 500, N=600, beta=0.3,type="td", scenario='time dependent',
##                  exp.prop=0.2, maxrelexptime=1/6, min.futime = 4,
##                  min.postexp.futime = 4)

## ---- cache=T, echo=F, warning=F-----------------------------------------
betas=seq(.1,.7,.1)
SS=c(600,800,1000)

final = NULL
for (l in 1:length(betas))
{ 
  for(s in 1:length(SS)){
    ret<- getpower.method1(nSim = 500, N=SS[s],beta=betas[l],type="td",scenario=paste('td', s),exp.prop=0.2,maxrelexptime=1/6, min.futime = 4, min.postexp.futime = 4)
    final = rbind(final,ret)
  }
}

for (l in 1:length(betas))
{ 
  for(s in 1:length(SS)){
    ret<- getpower.method1(nSim = 500, N=SS[s],beta=betas[l],type="fixed",scenario=paste('fixed', s),exp.prop=0.2,maxrelexptime=1/6, min.futime = 4)
    final = rbind(final,ret)
  }
}

## ----echo=F--------------------------------------------------------------
final_table = final[final$i_beta=='0.3',]

kable(final_table[c(4,1,5,2,6,3),c(2,3,13,14,18,19,20)],caption="Simulation results for scenario 2 - exposure time generated from Uniform(0, follow-up time/6), minimum follow-up time and minimum follow-up time after exposure are considered",row.names = F, col.names = c('Type','Sample Size',"Beta hat",'Hazard ratio','Median Survival-Unexposed','Median Survival-Exposed','Power'), align = 'c', digits = c(1,3,3,3,3,3,3))

## ---- fig.height=6, fig.width=8, echo=F----------------------------------

temp = plot_power(final,600,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=TRUE,
                         col='orange',lty=2,lwd=2,pch=16)
temp = plot_power(final,800,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE,
               col='purple',lty=2,lwd=2,pch=16)
temp = plot_power(final,1000,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE,
               col='red',lty=2,lwd=2,pch=16)

temp = plot_power(final,600,type='fixed',exp.prop=0.2,min.futime=4,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
                  col='orange',lty=1,lwd=2,pch=16)

temp = plot_power(final,800,type='fixed',exp.prop=0.2,min.futime=4,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
                  col='purple',lty=1,lwd=2,pch=16)
temp = plot_power(final,1000,type='fixed',exp.prop=0.2,min.futime=4,min.postexp.futime=0,show.plot=TRUE,newplot=FALSE,
                  col='red',lty=1,lwd=2,pch=16)


abline(h=0.8, lty=3)
legend("bottomright",cex=0.9,
       legend=c('Sample Size 1000, time-fixed exposure', 'Sample Size 1000, time-dependent exposure','Sample Size 800, time-fixed exposure','Sample Size 800, time-dependent exposure','Sample Size 600, time-fixed exposure', 'Sample Size 600, time-dependent exposure'),
       lwd= rep(1,6),lty = c(1,3,1,3,1,3),col=c("red","red","purple","purple","orange","orange"))

## ----eval=F--------------------------------------------------------------
## tdSim.method2(N, HR, lambda12, lambda23, lambda13,
##               exp.rate,rateC, min.futime, min.postexp.futime)

## ----echo=F--------------------------------------------------------------
lambda=log(2)/24
rateC=log(2)/14
#lambda12 = rateC * 10
lambda23 = lambda*exp(0.3)
lambda12 = lambda23 * 35
par(mfrow=c(1,2),oma=c(0,0,2,0))

dat <- tdSim.method2(500, duration=24, lambda12=lambda12, lambda23=lambda23, lambda13=lambda,rateC=rateC,exp.prop=0.2, min.futime = 4, min.postexp.futime = 4)
plot_simuData(dat, title='method2_filter')

dat <- tdSim.method1(500, duration=24,lambda, rho=1, beta=0.6, rateC=rateC,exp.prop=0.2,maxrelexptime=1/6, min.futime = 4, min.postexp.futime = 4)
plot_simuData(dat, title='method1_filter')

title("Figure 3. Incidence Plot of method 1 vs method 2", outer=T, cex.main=1)


## ----two_tables,echo=F,results='asis'------------------------------------
betas = seq(0.1,0.6,0.1)
lambda23 = lambda*exp(betas)
lambda12 = lambda23 * 45

table1 = data.frame(lambda = rep(lambda,6), rho = rep(1,6), beta = betas,rateC = rep(rateC,6), exp.prop = rep(0.2,6))
table2 = data.frame(lambda12 =lambda12, lambda23 = lambda23,
                    lambda13 = rep(lambda,6),rateC = rep(rateC,6), exp.prop = rep(0.2,6))

kable(table1,caption="Method 1 Input")
kable(table2,caption="Method 2 Input")


## ----echo=F--------------------------------------------------------------

par(mfrow=c(1,2),oma=c(0,0,2,0))
dat <- tdSim.method2(600, duration =24, lambda12=lambda12, lambda23=lambda23, lambda13=lambda, exp.prop=0.2,rateC=rateC, min.futime = 4, min.postexp.futime = 4)
plot_simuData(dat, title='method2_filter, beta=0.3')


dat <- tdSim.method1(600, duration=24,lambda, rho=1, beta=0.3, rateC=rateC,exp.prop=0.2,maxrelexptime=1/6, min.futime = 4, min.postexp.futime = 4)
plot_simuData(dat, title='method1_filter, beta=0.3')

title("Figure 4. Incidence plots of the two method for beta = 0.3", outer=T, cex.main=1)


## ----echo=F, cache=T, warning=F------------------------------------------

final = data.frame()

for (b in seq(0.1,0.6,0.1)){
  final = rbind(final,getpower.method1(nSim=500, N=600, med.TTE.Control =24, beta=b, exp.prop=0.2, type='td', scenario='method1_filter', maxrelexptime=1/6, min.futime = 4, min.postexp.futime = 4),
        
  getpower.method1(nSim=500, N=800, med.TTE.Control=24, beta=b, exp.prop=0.2, type='td', scenario='method1_filter', maxrelexptime=1/6, min.futime = 4, min.postexp.futime = 4),
  
  getpower.method1(nSim=500, N=1000, med.TTE.Control=24, beta=b, exp.prop=0.2, type='td', scenario='method1_filter', maxrelexptime=1/6, min.futime = 4, min.postexp.futime = 4))
}


final2 = data.frame()
for (b in seq(0.1,0.6,0.1)){
    lambda23 = lambda*exp(b)
    lambda12 = lambda23*45
  final2 = rbind(final2, getpower.method2(nSim=500, N=600, duration=24, exp.prop=0.2, rateC=rateC, scenario='method2_filter', lambda12=lambda12, lambda23=lambda23, lambda13=lambda, min.futime=4,min.postexp.futime = 4),
                getpower.method2(nSim=500, N=800, duration=24, exp.prop=0.2, rateC=rateC, scenario='method2_filter', lambda12=lambda12, lambda23=lambda23, lambda13=lambda, min.futime=4,min.postexp.futime = 4),
                getpower.method2(nSim=500,N=1000, duration=24, exp.prop=0.2, rateC=rateC, scenario='method2_filter', lambda12=lambda12, lambda23=lambda23, lambda13=lambda, min.futime=4,min.postexp.futime = 4))
  }

## ----echo=F--------------------------------------------------------------
final2$i_beta = c(rep(0.1,3),rep(0.2,3),rep(0.3,3),rep(0.4,3),rep(0.5,3),rep(0.6,3))
final2$i_type = 'td'
par(mfrow=c(1,1))

temp = plot_power(final,600,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=TRUE,
                        col='orange',lty=1,lwd=2,pch=16)
temp = plot_power(final,800,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE,
              col='purple',lty=1,lwd=2,pch=16)
temp = plot_power(final,1000,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE,
              col='red',lty=1,lwd=2,pch=16)
temp = plot_power(final2,600,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE, col='orange',lty=2,lwd=2,pch=16)
temp = plot_power(final2,800,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE, col='purple',lty=2,lwd=2,pch=16)
temp = plot_power(final2,1000,type='td',exp.prop=0.2,min.futime=4,min.postexp.futime=4,show.plot=TRUE,newplot=FALSE, col='red',lty=2,lwd=2,pch=16)

abline(h=0.8,lty=2,lwd=1)
legend("bottomright",cex=0.5,
       legend=c("Sample size 1000, method 1","Sample size 1000, method 2",
                "Sample size 800, method 1","Sample size 800, method 2",
                "Sample size 600, method 1","Sample size 600, method 2"),
       lty=c(1,3,1,3,1,3),lwd=c(2,2,2,2,2,2),col=c("red","red","purple","purple","orange","orange"))


