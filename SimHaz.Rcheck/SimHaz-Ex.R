pkgname <- "SimHaz"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SimHaz')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SimHaz-package")
### * SimHaz-package

flush(stderr()); flush(stdout())

### Name: SimHaz-package
### Title: Simulated Survival and Hazard Analysis for time-dependent
### Aliases: SimHaz-package SimHaz

### ** Examples

# Simulate a dataset of 600 subjects with time-dependent exposure without
# considering minimum follow-up time or minimum post-exposure follow-up time.
# Specifically, set the duration of the study to be 24 months; the median time to
# event for control group to be 24 months; exposure effect to be 0.3; median time
# to censoring to be 14 months; and exposure proportion to be 20%.

df1 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exp.prop = 0.2, 
   prop.fullexp  = 0, maxrelexptime = 1, min.futime = 0,
   min.postexp.futime = 0)
ret <- getpower.method1(nSim = 500, N = 600, b = 0.3, exp.prop = 0.2, type = "td", scenario = 
" ", maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4,
output.fn = "output.csv")




cleanEx()
nameEx("getpower.clst")
### * getpower.clst

flush(stderr()); flush(stdout())

### Name: getpower.clst
### Title: Calculate power for the Cox proportional hazard model with
###   time-dependent exposure using method 1 with clustering
### Aliases: getpower.clst
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Install the survival package if needed.

library(survival)

# Create a clustering data frame as input with 3 categories and a 20% weighted
# exposure proportion.
  
	input_df <- data.frame(cat_id = c('lo', 'med', 'hi'), cat_prop = c(0.65, 0.2, 0.15),
cat_exp.prop = c(0.1, 0.3, 0.5))

# Run 500 simulations. Each time simulate a dataset of 600 subjects with
# time-dependent exposure with both minimum follow-up time (4 months) and
# minimum post-exposure follow-up time (4 months) imposed. Also consider a
# quick exposure after entering the study for each exposed subject. Set the
# maximum relative exposure time to be ⅙. 

# Set the duration of the study to be 24 months; the median time to event for
# control group to be 24 months; exposure effect to be 0.3; median time to
# censoring to be 14 months.

ret <- getpower.clst(nSim = 500, N = 600, beta = 0.3, df = input_df, type = "td", scenario = "clustering", maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4, output.fn = "output_clst.csv",) 




cleanEx()
nameEx("getpower.method1")
### * getpower.method1

flush(stderr()); flush(stdout())

### Name: getpower.method1
### Title: Calculate power for the Cox proportional hazard model with
###   time-dependent exposure using method 1
### Aliases: getpower.method1

### ** Examples

# Install the survival package if needed.

library(survival)

# Run 500 simulations. Each time simulate a dataset of 600 subjects with
# time-dependent exposure with both minimum follow-up time (4 months) and
# minimum post-exposure follow-up time (4 months) imposed. Also consider a
# quick exposure after entering the study for each exposed subject. Set the
# maximum relative exposure time to be ⅙. 

# Set the duration of the study to be 24 months; the median time to event for
# control group to be 24 months; exposure effect to be 0.3; median time to
# censoring to be 14 months; and exposure proportion to be 20%.

ret <- getpower.method1(nSim = 500, N = 600, b = 0.3, exp.prop = 0.2, type = "td", scenario = 
" ", maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4,
output.fn = "output.csv")




cleanEx()
nameEx("getpower.method2")
### * getpower.method2

flush(stderr()); flush(stdout())

### Name: getpower.method2
### Title: Calculate power for the Cox proportional hazard model with
###   time-dependent exposure using method 1
### Aliases: getpower.method2
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nSim = 500, N, duration = 24, scenario, lambda12, lambda23 = NULL, 
    lambda13, HR = NULL, exp.prop, rateC, min.fut, min.postexp.fut, 
    filename, simu.plot = FALSE) 
{
    set.seed(999)
    try(if (lambda23 == NULL & HR == NULL) {
        stop("either lambda23 or HR(Hazard ratio) must be set")
    })
    if (lambda23 == NULL & HR != NULL) {
        lambda23 = lambda13 * HR
    }
    res = matrix(0, numsim, 10)
    colnames(res) = c("N.eff", "N.effexp.p", "betahat", "HR", 
        "signif", "events", "events_c", "events_exp", "medsurvt_c", 
        "medsurvt_exp")
    alpha = 0.05
    if (simu.plot) {
        dat <- tdSim.method2(N, duration, lambda12 = lambda12, 
            lambda23 = lambda23, lambda13 = lambda, exp.prop = exp.prop, 
            rateC = rateC, min.fut = min.fut, min.postexp.fut = min.postexp.fut)
        plot_simuData(dat)
    }
    for (k in 1:nSim) {
        dat <- tdSim.method2(N, duration, lambda12 = lambda12, 
            lambda23 = lambda23, lambda13 = lambda, exp.rate = exp.prop, 
            rateC = rateC, min.fut = min.fut, min.postexp.fut = min.postexp.fut)
        fit <- coxph(Surv(start, stop, status) ~ factor(x), data = dat)
        sfit <- survfit(Surv(start, stop, status) ~ factor(x), 
            data = dat)
        res[k, "N.eff"] <- length(unique(dat$id))
        res[k, "N.effexp.p"] <- sum(dat$x)/length(unique(dat$id))
        res[k, "betahat"] <- summary(fit)$coef[, "coef"]
        res[k, "HR"] <- summary(fit)$coef[, "exp(coef)"]
        res[k, "signif"] <- ifelse(summary(fit)$coef[, "Pr(>|z|)"] < 
            alpha, 1, 0)
        res[k, "events"] <- sum(dat$status)
        res[k, "events_c"] <- summary(sfit)$table[1, "events"]
        res[k, "events_exp"] <- summary(sfit)$table[2, "events"]
        res[k, "medsurvt_c"] <- summary(sfit)$table[1, "median"]
        res[k, "medsurvt_exp"] <- summary(sfit)$table[2, "median"]
    }
    df = data.frame(i_scenario = scenario, i_N = N, i_min.postexp.fut = min.postexp.fut, 
        i_min.postexp.fut = min.postexp.fut, i_exprate = er, 
        i_lambda12 = lambda12, i_lambda23 = lambda23, i_lambda13 = lambda13, 
        i_rateC = rateC, N_eff = mean(res[, "N.eff"]), N_effexp_p = mean(res[, 
            "N.effexp.p"]), bhat = mean(res[, "betahat"]), HR = mean(res[, 
            "HR"]), d = mean(res[, "events"]), d_c = mean(res[, 
            "events_c"]), d_exp = mean(res[, "events_exp"]), 
        mst_c = mean(na.omit(res[, "medsurvt_c"])), mst_exp = mean(na.omit(res[, 
            "medsurvt_exp"])), pow = mean(res[, "signif"]))
    if (file.exists(filename)) {
        write.table(df, file = filename, row.names = FALSE, col.names = FALSE, 
            append = TRUE, sep = ",")
    }
    else {
        write.table(df, file = filename, row.names = FALSE, col.names = TRUE, 
            sep = ",")
    }
    return(df)
  }



cleanEx()
nameEx("plot_incidence")
### * plot_incidence

flush(stderr()); flush(stdout())

### Name: plot.incidence
### Title: Make an incidence plot from simulated data.
### Aliases: plot.incidence
### Keywords: ~kwd1 ~kwd2

### ** Examples

dat <- tdSim.method2(500, lambda12=1.36, lambda23=0.0389, lambda13=0.0288, exp.rate=0.2,rateC=0.05, filterA=4, filterB=4)
plot_simuData(dat, title='method2_filter')



cleanEx()
nameEx("plot_power")
### * plot_power

flush(stderr()); flush(stdout())

### Name: plot_power
### Title: Plot power curves for survival analysis with time-dependent
###   exposure
### Aliases: plot_power
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Read in .csv file as a data frame

  tb <-  read.csv("output.csv", header = TRUE, sep = ",")

	# Visualize the subsetted data frame of interest and create a new plot

visualize1 <- plot_power(table_df = tb, N = 600, type = "td", exp.prop = 0.2,
min.futime = 4, min.postexp.futime = 4, plot = TRUE, newplot = TRUE, co l = "red", lty = 1, lwd = 2, pch = 16)

# Add a different power curve to the previously created plot

visualize2 <- plot_power(table_df = tb, N = 600, type = "td", exp.prop=0.2,
min.futime = 4, min.postexp.futime = 0, plot = TRUE, newplot = FALSE, col = "blue", lty = 1, lwd = 2, pch = 16)




cleanEx()
nameEx("tdSim.clst")
### * tdSim.clst

flush(stderr()); flush(stdout())

### Name: tdSim.clst
### Title: Simulate 1 dataframe (1 simulation) of time-dependent exposure
###   under method 1 with a clustering data frame
### Aliases: tdSim.clst
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Create a clustering data frame as input with 3 categories and a 20% weighted
# exposure proportion.
  
	input_df <- data.frame(cat_id = c('lo', 'med', 'hi'), cat_prop = c(0.65, 0.2, 0.15),
cat_exp.prop = c(0.1, 0.3, 0.5))

# Simulate a dataset of 600 subjects with time-dependent exposure. Consider
# both minimum follow-up time (4 months) and minimum post-exposure follow-up # time (4 months). Also consider a quick exposure after entering the study for
# each exposed subject. Set the maximum relative exposure time to be ⅙. 

# Set the duration of the study to be 24 months; the median time to event for
# control group to be 24 months; exposure effect to be 0.3; median time to
# censoring to be 14 months.

	df_tdclst <- tdSim.clust(N = 600, duration = 24, lambda = log(2)/24, rho = 1,
beta = 0.3, rateC = log(2)/14, df = input_df, prop.fullexp = 0, maxrelexptime = 1/6,
min.futime = 4, min.postexp.futime = 4)



cleanEx()
nameEx("tdSim.method1")
### * tdSim.method1

flush(stderr()); flush(stdout())

### Name: tdSim.method1
### Title: Simulate 1 dataframe (1 simulation) of time-dep exposure under
###   method 1
### Aliases: tdSim.method1
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Simulate a dataset of 600 subjects with time-dependent exposure without
# considering minimum follow-up time or minimum post-exposure follow-up time.
# Specifically, set the duration of the study to be 24 months; the median time to
# event for control group to be 24 months; exposure effect to be 0.3; median time
# to censoring to be 14 months; and exposure proportion to be 20%.

df1 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exprate = 0.2, 
   fullyexp.p  = 0, maxrelexp.t = 1, min.fut = 0,
   min.postexp.fut = 0)

# Simulate a dataset of 600 subjects with time-dependent exposure with
# both minimum follow-up time (4 months) and minimum post-exposure
# follow-up time (4 months) imposed. Other parameters remain the same as
# in the first case.

df2 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exprate = 0.2, 
   fullyexp.p  = 0, maxrelexp.t = 1, min.fut = 4,
   min.postexp.fut = 4)

# Simulate a dataset of 600 subjects with time-dependent exposure with
# both minimum follow-up time (4 months) and minimum post-exposure
# follow-up time (4 months) imposed. Also consider a quick exposure after entering the study for each exposed subject. Set the maximum relative exposure time to be ⅙. Other parameters remain the same as in the first case.

df3 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exprate = 0.2, 
   fullyexp.p  = 0, maxrelexp.t = 1/6, min.fut = 4,
   min.postexp.fut = 4)



cleanEx()
nameEx("tdSim.method2")
### * tdSim.method2

flush(stderr()); flush(stdout())

### Name: tdSim.method2
### Title: Simulate 1 dataframe (1 simulation) of time-dep exposure under
###   method 2
### Aliases: tdSim.method2
### Keywords: ~kwd1 ~kwd2

### ** Examples

sim_data <- tdSim.method2(500, duration=24,lambda12=1.3,
lambda23=0.04, lambda13=0.03, exp.prop=0.2,rateC=0.05,
min.futime=4, min.postexp.futime=4)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
