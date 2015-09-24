\name{getpower.method2}
\alias{getpower.method2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate power for the Cox proportional hazard model with
time-dependent exposure using method 1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This functions runs nSim (Number of simulations, specified by the user) Monte Carlo simulations, each time it calls
tdSim.method2 internally. The function returns a data frame of scenario-specific input parameters- and it also output statistical power. The user has the option to append the output to a file with file name specified in the input parameters list.
}
\usage{
getpower.method2(nSim = 500, N, duration = 24, scenario, lambda12,
    lambda23 = NULL, lambda13, HR = NULL, exp.prop, rateC, min.fut, 
    min.postexp.fut, filename, simu.plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{nSim}{
 Number of simulations
 }
 \item{N}{
 Number of subjects needs to be screened
 }
 \item{duration}{Length of the study in Months. The default value is 24 (months)
 }
 \item{scenario}{
 Any text string inputted by the user as an option to name a scenario that is being simulated. The use can simply put " " if he/she decides to not name the scenario
 }
 \item{lambda12}{
 Lambda12 parameter to control time to exposure
 }
 \item{lambda23}{
 Lambda23 parameter to control time to event after exposure
 }
 \item{lambda13}{
 Lambda13 parameter to control time to event in the control group
 }
 \item{HR}{
 Hazard Ratio. This input is optional. If HR is set and lambda23 is not set, lambda23 = lambda13*HR
 }
 \item{exp.prop}{
 A numeric value between 0 and 1 (not include 0 and 1) that represents the proportion of subjects that are assigned with an exposure
 }
 \item{rateC}{
 Rate of the exponential distribution to generate censoring times
 }
 \item{min.fut}{
 A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
 }
 \item{min.postexp.fut}{
 A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
 }
 \item{output.fn}{
 A .csv filename to write in the output. If the filename does not exist, the function will create a new .csv file for the output
 }
 \item{simu.plot}{
 A logical value indicating whether to output an incidence plot or not.The default value is FALSE
 }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
