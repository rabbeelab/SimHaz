simulWeib_matching <- function(N_match, duration, lambda, rho, beta, rateC,matching.ratio=3, min.futime)
{ 
  # covariate --> N Bernoulli trials
  N <- N_match*(1+matching.ratio)
  expose <- rep(c(1,rep(0,matching.ratio)), N_match)
  match_id <- rep(1:N_match, each=1+matching.ratio)
  # Weibull latent event times
  v <- runif(n=N)
  Tlat <- (- log(v) / (lambda * exp(expose * beta)))^(1 / rho)
  # censoring times
  C <- rexp(n=N, rate=rateC)
  C=pmin(C,rep(duration,length(C)))
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  start = rep(0,length(time)) #all start at 0
  if(min.futime==0){
    return(data.frame(id=1:N,start=start,stop=time,status=status,x=expose, match_id=match_id))
  }
  else{
    return(data.frame(id=1:N,start=start,stop=time,status=status,x=expose, match_id=match_id)[which(time>min.futime),])
  }
}

# regular version to generate time-dependent dataset
# fullyexp.p: fully exposed proportion, the default value is 0, can take values in [0, 1)
# maxrelexp.t: maximum relative exposuret time, the default value is 1, can take values in (0, 1]
# min.postexp.fut: minimum post-exposure follow-up time
#' @export
tdSim.exposure.matching1<-function(N_match, duration=24,lambda, rho=1, beta, rateC,matching.ratio=3,
                        prop.fullexp=0,maxrelexptime=1,min.futime=0, min.postexp.futime=0){

  data <- simulWeib_matching(N_match, duration, lambda, rho, beta, rateC,matching.ratio, min.futime)
  if(prop.fullexp==0){
    data_tdexposed<-data[data$x==1,]
    to_merge <- data[data$x==0,]
  }
  else{
    id_tdexposed<-sample(x = data[data$x==1,]$id,size = floor(nrow(data[data$x==1,])*(1-prop.fullexp)))
    data_tdexposed<-data[data$id %in% id_tdexposed,]
    data_fullexposed <- data[(!(data$id %in% id_tdexposed)) & data$x==1,]
    to_merge <- rbind(data[data$x==0,], data_fullexposed)
  }
  data_tdexposed$t_exposed<-runif(nrow(data_tdexposed),0,data_tdexposed$stop*maxrelexptime)
  if(min.postexp.futime>0){
    if(sum(data_tdexposed$stop-data_tdexposed$t_exposed>min.postexp.futime) == 0){
      print('no exposure left')
    }
    data_tdexposed <- data_tdexposed[data_tdexposed$stop-data_tdexposed$t_exposed>min.postexp.futime,]
  }
  new_data1<-data_tdexposed
  new_data2<-data_tdexposed
  new_data1$id<-data_tdexposed$id
  new_data1$start<-data_tdexposed$start
  new_data1$stop<-data_tdexposed$t_exposed
  new_data1$status<-0
  new_data1$x<-0
  new_data2$id<-data_tdexposed$id
  new_data2$start<-data_tdexposed$t_exposed
  new_data2$stop<-data_tdexposed$stop 
  new_data2$x<-1
  new_data2$status<-data_tdexposed$status      
  merged_tdexposed<-subset(na.omit(merge(new_data1,new_data2,all.x=TRUE,all.y=TRUE))) 
  merged_tdexposed$t_exposed<-NULL
  full_data<-merge(merged_tdexposed,to_merge,all.x=TRUE,all.y=TRUE)
  return(full_data)
}

getpower.matching1<-function(nSim, N_match,duration=24,med.TTE.Control=24,rho=1,med.TimeToCensor=14,beta,
                             matching.ratio,type,scenario, method,
                           prop.fullexp=0,maxrelexptime=1,min.futime=0,min.postexp.futime=0,
                           output.fn,simu.plot=FALSE) 
{ 
  lambda=log(2)/med.TTE.Control
  rateC=log(2)/med.TimeToCensor
  #numsim=500
  res=matrix(0,nSim,10)
  colnames(res)=c("N.eff","N.effexp.p","betahat","HR","signif","events",
                  "events_c","events_exp","medsurvt_c","medsurvt_exp")
  alpha=.05
  if(simu.plot){
    set.seed(999)
      dat <- tdSim.exposure.matching1(N_match=N_match, duration=duration,lambda=lambda, rho=rho, beta=beta, 
                                      rateC=rateC,matching.ratio=matching.ratio,
                                      prop.fullexp=prop.fullexp,maxrelexptime=maxrelexptime,min.futime=min.futime, 
                                      min.postexp.futime=min.postexp.futime)
    plot_simuData(dat)
  }
  set.seed(999)
  for(k in 1:nSim)
  {
    if(type == "fixed"){
    dat<-simulWeib_matching(N_match=N_match,duration=duration,lambda=lambda,rho=rho,beta=beta,
                            rateC=rateC, matching.ratio=matching.ratio,
                            min.futime=min.futime)
  }else{
    dat <- tdSim.exposure.matching1(N_match=N_match, duration=duration,lambda=lambda, rho=rho, beta=beta, 
                                    rateC=rateC,matching.ratio=matching.ratio,
                                    prop.fullexp=prop.fullexp,maxrelexptime=maxrelexptime,min.futime=min.futime, 
                                    min.postexp.futime=min.postexp.futime)
  }
    if(method == "marginal"){
      fit <- coxph(Surv(start,stop, status) ~ factor(x)+cluster(match_id), data=dat)
      sfit <- survfit(Surv(start,stop, status) ~ factor(x)+cluster(match_id), data=dat)
    }
    else if(method == "strata"){
      fit <- coxph(Surv(start,stop, status) ~ factor(x)+strata(match_id), data=dat)
      sfit <- survfit(Surv(start,stop, status) ~ factor(x)+strata(match_id), data=dat)
    }else{
      stop("Method not implemented")
    }
    res[k,"betahat"] <- summary(fit)$coef[,"coef"]
    res[k,"HR"] <- summary(fit)$coef[,"exp(coef)"]
    res[k,"signif"] <- ifelse(summary(fit)$coef[,"Pr(>|z|)"]<alpha,1,0)
    res[k,"events"] <- sum(dat$status)
    res[k,"events_c"] <- summary(sfit)$table[1,'events']
    res[k,"events_exp"] <- summary(sfit)$table[2,'events']
    res[k,"medsurvt_c"] <- summary(sfit)$table[1,'median']
    res[k,"medsurvt_exp"] <- summary(sfit)$table[2,'median']
  }
  df=data.frame(i_scenario=scenario,
                i_type=type,
                i_N_match=N_match,
                i_matching.ratio=matching.ratio,
                i_min.futime=min.futime,
                i_min.postexp.futime=min.postexp.futime,
                i_lambda=log(2)/med.TTE.Control,
                i_rho=rho,
                i_rateC=rateC,                       
                i_beta=beta,
                N_eff=mean(res[,"N.eff"]),
                N_effexp_p=mean(res[,"N.effexp.p"]),
                bhat=mean(res[,"betahat"]),
                HR=mean(res[,"HR"]),                     
                d=mean(res[,"events"]),
                d_c=mean(res[,"events_c"]),
                d_exp=mean(res[,"events_exp"]),
                mst_c=mean(na.omit(res[,"medsurvt_c"])),
                mst_exp=mean(na.omit(res[,"medsurvt_exp"])),
                pow=mean(res[,"signif"]),
                variance=var(res[,"betahat"])
  )
  if(file.exists(output.fn)){
    write.table(df,file=output.fn,row.names=FALSE,col.names=FALSE,append=TRUE,sep=",")
  }
  else{
    write.table(df,file=output.fn,row.names=FALSE,col.names=TRUE,sep=",")
  }
  return(df)
}


getpower.matching_opt<-function(nSim, N,duration=24,med.TTE.Control=24,rho=1,med.TimeToCensor=14,beta,
                                exp.prop,type,scenario, method,
                             prop.fullexp=0,maxrelexptime=1,min.futime=0,min.postexp.futime=0,
                             output.fn,simu.plot=FALSE) 
{ 
  N_population = 100000
  lambda=log(2)/med.TTE.Control
  rateC=log(2)/med.TimeToCensor
  #numsim=500
  res=matrix(0,nSim,10)
  colnames(res)=c("N.eff","N.effexp.p","betahat","HR","signif","events",
                  "events_c","events_exp","medsurvt_c","medsurvt_exp")
  alpha=.05
  if(simu.plot){
    set.seed(999)
    if(type == "fixed"){
      population <- simulWeib(N=N_population, duration=duration,lambda=lambda, rho=rho, beta=beta, rateC=rateC,
                       exp.prop=exp.prop,min.futime=min.futime)
      fit <- coxph(Surv(start,stop, status) ~ factor(x), data=population)
      real_betahat <- summary(fit)$coef[,"coef"]
    }
    else{
      population <- tdSim.method1(N=N_population, duration=duration,lambda=lambda, rho=rho, beta=beta, rateC=rateC,
                           exp.prop=exp.prop,prop.fullexp=prop.fullexp,maxrelexptime=maxrelexptime,
                           min.futime=min.futime,min.postexp.futime=min.postexp.futime)
      fit <- coxph(Surv(start,stop, status) ~ factor(x), data=population)
      real_betahat <- summary(fit)$coef[,"coef"]
    }
    plot_simuData(dat)
  }
  set.seed(999)
  if(type == "fixed"){
    population <- simulWeib(N=N_population, duration=duration,lambda=lambda, rho=rho, beta=beta, rateC=rateC,
                            exp.prop=exp.prop,min.futime=min.futime)
    fit <- coxph(Surv(start,stop, status) ~ factor(x), data=population)
    real_betahat <- summary(fit)$coef[,"coef"]
  }
  else{
    population <- tdSim.method1(N=N_population, duration=duration,lambda=lambda, rho=rho, beta=beta, rateC=rateC,
                                exp.prop=exp.prop,prop.fullexp=prop.fullexp,maxrelexptime=maxrelexptime,
                                min.futime=min.futime,min.postexp.futime=min.postexp.futime)
    fit <- coxph(Surv(start,stop, status) ~ factor(x), data=population)
    real_betahat <- summary(fit)$coef[,"coef"]
  }
  ratios = c(1,1/9,0.25,0.333,0.5,2:5, 9,19)
  result <- NULL
  for (ratio in ratios){
  print(ratio)
  for(k in 1:nSim)
  {
    if (ratio >= 1){
    sampled_ids <- sample(population[population$x==1,"id"],size=floor(N/(ratio+1)))
    exposed <- population[population$id %in% sampled_ids,]
    exposed$match_id <- exposed$id
    unexposed_ids <- sample(setdiff(unique(population$id),population[population$x==1,"id"]), size= ratio * floor(N/(ratio+1)))
    unexposed <- population[population$id %in% unexposed_ids,]
    unexposed$match_id <- rep(sampled_ids, each=ratio)
    }else{
      unexposed_ids <- sample(setdiff(unique(population$id),population[population$x==1,"id"]), size= floor((ratio*N)/(ratio+1)))
      unexposed <- population[population$id %in% unexposed_ids,]
      unexposed$match_id <- unexposed$id
      sampled_ids <- sample(population[population$x==1,"id"],size=floor(1 / ratio) * floor((ratio*N)/(ratio+1)))
      exposed <- population[population$id %in% sampled_ids,]
      exposed$match_id <- rep(unexposed$match_id, each= floor(1 / ratio))
    }
    dat<-rbind(exposed, unexposed)
    if(method == "marginal"){
      fit <- coxph(Surv(start,stop, status) ~ factor(x)+cluster(match_id), data=dat)
      sfit <- survfit(Surv(start,stop, status) ~ factor(x)+cluster(match_id), data=dat)
    }
    else if(method == "strata"){
      fit <- coxph(Surv(start,stop, status) ~ factor(x)+strata(match_id), data=dat)
      sfit <- survfit(Surv(start,stop, status) ~ factor(x)+strata(match_id), data=dat)
    }else{
      stop("Method not implemented")
    }
    res[k,"betahat"] <- summary(fit)$coef[,"coef"]
    res[k,"HR"] <- summary(fit)$coef[,"exp(coef)"]
    res[k,"signif"] <- ifelse(summary(fit)$coef[,"Pr(>|z|)"]<alpha,1,0)
    res[k,"events"] <- sum(dat$status)
    res[k,"events_c"] <- summary(sfit)$table[1,'events']
    res[k,"events_exp"] <- summary(sfit)$table[2,'events']
    res[k,"medsurvt_c"] <- summary(sfit)$table[1,'median']
    res[k,"medsurvt_exp"] <- summary(sfit)$table[2,'median']
  }
  if(ratio==1){
    s2 = sqrt(var(res[,"betahat"]))
    s1 = s2
  }else{
    s1 = sqrt(var(res[,"betahat"]))
  }
  bhat = mean(res[,"betahat"])
  df=data.frame(ratio=ratio,
                bhat=bhat,
                pow=mean(res[,"signif"]),
                actual_beta=real_betahat,
                bias=bhat-real_betahat,
                variance=var(res[,"betahat"]),
                RE = (s1^2) / (s2^2))
  result <-  rbind(result, df)
  }
  if(file.exists(output.fn)){
    write.table(result,file=output.fn,row.names=FALSE,col.names=FALSE,append=TRUE,sep=",")
  }
  else{
    write.table(result,file=output.fn,row.names=FALSE,col.names=TRUE,sep=",")
  }
  return(result)
}

  
