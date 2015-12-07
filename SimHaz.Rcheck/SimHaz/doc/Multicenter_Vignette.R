## ---- echo=F-------------------------------------------------------------
library(knitr)
library(xtable)
library(SimHaz)

## ---- echo=T-------------------------------------------------------------
input_df <- data.frame(cat_id = as.factor(1:40), center.size = rep(10,40), 
                       cat_exp.prop = rep(1/3, 40), med.TTE.Control=rep(14,40)) 

## ---- echo=F-------------------------------------------------------------
#xtable(rbind(head(input_df,n=3),tail(input_df,n=3), make.row.names=F))

## ---- eval=FALSE---------------------------------------------------------
## getpower.multicenter(nSim = 500, N = 100, beta = 0.7, df = input_df,
##                      method="strata", dist=NULL, type = "td",
##                      scenario = "strata", maxrelexptime = 1/6, min.futime = 4,
##                      min.postexp.futime = 4, output.fn = "output_mult.csv")

## ---- cache=T, echo=F----------------------------------------------------
#example 1, no center effect

input_df1 <- data.frame(cat_id = as.factor(1:10), center.size = rep(10,10), 
                        cat_exp.prop = rep(1/3, 10),
                        med.TTE.Control=rep(14,10))
table1 = data.frame()

for(i in c('independence','strata','frailty')){
    table1 <- rbind(table1,getpower.multicenter(nSim = 500, N = 100, beta = 0.7, df = input_df1, method=i,dist=NULL, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4,min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}


input_df2 <- data.frame(cat_id = as.factor(1:5), center.size = rep(20,5), 
                        cat_exp.prop = rep(1/3, 5),
                        med.TTE.Control=rep(14,5))
table2 = data.frame()

for(i in c('independence','strata','frailty')){
    table2 <- rbind(table2,getpower.multicenter(nSim = 500, N = 100, beta = 0.7, df = input_df2,method=i, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}


input_df3 <- data.frame(cat_id = as.factor(1:20), center.size = rep(10,20), 
                        cat_exp.prop = rep(1/3, 20),
                        med.TTE.Control=rep(14, 20))
table3 = data.frame()

for(i in c('independence','strata','frailty')){
    table3 <- rbind(table3,getpower.multicenter(nSim = 500, N = 200, beta = 0.7, df = input_df3,method=i, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

input_df4 <- data.frame(cat_id = as.factor(1:10), center.size = rep(20,10), 
                        cat_exp.prop = rep(1/3, 10),
                        med.TTE.Control=rep(14, 10))

table4 = data.frame()

for(i in c('independence','strata','frailty')){
    table4 <- rbind(table4,getpower.multicenter(nSim = 500, N = 200, beta = 0.7, df = input_df4, method=i, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

input_df5 <- data.frame(cat_id = as.factor(1:40), center.size = rep(10,40), 
                        cat_exp.prop = rep(1/3, 40),
                        med.TTE.Control=rep(14, 40))
table5 = data.frame()

for(i in c('independence','strata','frailty')){
    table5 <- rbind(table5,getpower.multicenter(nSim = 500, N = 400, beta = 0.7, df = input_df5,method=i, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

input_df6 <- data.frame(cat_id = as.factor(1:20), center.size = rep(20,20), 
                        cat_exp.prop = rep(1/3, 20),
                        med.TTE.Control=rep(14, 20))
table6 = data.frame()

for(i in c('independence','strata','frailty')){
    table6 <- rbind(table6,getpower.multicenter(nSim = 500, N = 400, beta = 0.7, df = input_df6, method=i, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

## ----xtable, results='asis',echo=F---------------------------------------
final1 = data.frame(rbind(table1,table2,table3,table4,table5,table6))
colnames(final1) = c('Method','Center Size','N','Beta','Power')
print(xtable(final1, caption = "Table 1: Equal Event Rates",,digits=c(0,0,0,0,5,3)), comment=F, hline.after = c(-1,0,3,6,9,12,15,18), booktabs=T,include.rownames = FALSE)

## ------------------------------------------------------------------------
input_df <- data.frame(cat_id = as.factor(1:3), center.size = rep(100,3), 
                       cat_exp.prop = rep(1/3, 3), med.TTE.Control=c(14,20,31)) 

## ----echo = F------------------------------------------------------------
kable(input_df)

## ---- cache=T, echo=F----------------------------------------------------
#example 2
input_df7 <- data.frame(cat_id = c("low","med","high"), center.size = rep(100,3), 
                        cat_exp.prop = rep(1/3, 3),
                        med.TTE.Control=c(14,20,31))

table7= data.frame()

for(i in c('marginal','fixed effects','strata','frailty')){
    table7 = rbind(table7, getpower.multicenter(nSim = 500, N = 300, beta = 0.7, df = input_df7,method=i, type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

## ---- results='asis', echo=F---------------------------------------------
final2 = data.frame(table7)
colnames(final2) = c('Method','Center Size','N','Beta','Power')

print(xtable(final2, caption = "Table 2: Fixed Different Event Rates for 3 Centers",digits=c(0,0,0,0,5,3)), comment=F,booktabs=T,include.rownames = FALSE)


## ------------------------------------------------------------------------
input_df <- data.frame(cat_id = as.factor(1:20), center.size = rep(10,20), 
                        cat_exp.prop = rep(1/3, 20),
                        med.TTE.Control=rep(14,20)) 

## ---- echo=F-------------------------------------------------------------
#xtable(rbind(head(input_df,n=3),tail(input_df,n=3),make.row.names=F))

## ---- eval=F-------------------------------------------------------------
## 
## getpower.multicenter(nSim = 500 , N = 100, beta = 0.7, df = input_df,
##                      method="strata", dist= "gamma", type = "td",
##                      scenario = "strata", maxrelexptime = 1/6, min.futime =4,
##                      min.postexp.futime = 4, output.fn = "output_mult.csv")
## 

## ---- cache=T, echo=F, warning=F-----------------------------------------
#example 3 gamma distributed

input_df8 <- data.frame(cat_id = as.factor(1:10), center.size = rep(10,10), 
                        cat_exp.prop = rep(1/3, 10),
                        med.TTE.Control=rep(14,10))

table8 = data.frame()

for(i in c('strata','frailty')){
    table8 <- rbind(table8, getpower.multicenter(nSim = 500, N = 100, beta = 0.7, df = input_df8,method=i, dist="gamma", type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

input_df9 <- data.frame(cat_id = as.factor(1:5), center.size = rep(20,5), 
                        cat_exp.prop = rep(1/3, 5),
                        med.TTE.Control=rep(14,5))

table9 = data.frame()

for(i in c('strata','frailty')){
    table9 <- rbind(table9, getpower.multicenter(nSim = 500, N = 100, beta = 0.7, df = input_df9,method=i, dist="gamma", type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

input_df10 <- data.frame(cat_id = as.factor(1:20), center.size = rep(10,20), 
                        cat_exp.prop = rep(1/3, 20),
                        med.TTE.Control=rep(14,20))

table10 = data.frame()

for(i in c('strata','frailty')){
    table10 <- rbind(table10, getpower.multicenter(nSim = 500, N = 200, beta = 0.7, df = input_df10,method=i, dist="gamma", type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}


input_df11 <- data.frame(cat_id = as.factor(1:10), center.size = rep(20,10), 
                        cat_exp.prop = rep(1/3, 10),
                        med.TTE.Control=rep(14,10))

table11 = data.frame()

for(i in c('strata','frailty')){
    table11 <- rbind(table11, getpower.multicenter(nSim = 500, N = 200, beta = 0.7, df = input_df11,method=i, dist="gamma", type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}


input_df12 <- data.frame(cat_id = as.factor(1:40), center.size = rep(10,40), 
                        cat_exp.prop = rep(1/3, 40),
                        med.TTE.Control=rep(14,40))
table12 = data.frame()

for(i in c('strata','frailty')){
    table12 <- rbind(table12,getpower.multicenter(nSim = 500, N = 400, beta = 0.7, df = input_df12,method=i, dist="gamma",type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}


input_df13 <- data.frame(cat_id = as.factor(1:20), center.size = rep(20,20), 
                         cat_exp.prop = rep(1/3, 20),
                         med.TTE.Control=rep(14,20))
table13 = data.frame()

for(i in c('strata','frailty')){
    table13 = rbind(table13, getpower.multicenter(nSim = 500, N = 400, beta = 0.7, df = input_df13,method=i , dist="gamma",type = "td", scenario = i, maxrelexptime = 1/6, min.futime = 4,min.postexp.futime = 4)[1,c(1,8,3,17,24)])
}

## ---- results='asis',echo=F----------------------------------------------
final3 = data.frame(rbind(table8,table9,table10,table11,table12,table13))
colnames(final3) = c('Method','Center Size','N','Beta','Power')

print(xtable(final3, caption = "Table 3: Gamma Centered Event Rates",digits=c(0,0,0,0,5,3)), comment=F, hline.after = c(-1,0,2,4,6,8,10,12), booktabs=T,include.rownames = FALSE)


