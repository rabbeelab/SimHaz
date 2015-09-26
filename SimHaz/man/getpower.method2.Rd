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
getpower.method2(nSim = 500, N, duration = 24, scenario, lambda12, lambda23 = NULL, lambda13, HR = NULL, exp.prop, rateC, min.futime, min.postexp.futime, filename, simu.plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{nSim}{Number of simulations}
  \item{N}{Number of subjects needs to be screened}
  \item{duration}{Length of the study in Months. The default value is 24 (months)}
  \item{scenario}{
Any text string inputted by the user as an option to name a scenario     	that is being simulated. The use can simply put “ ” if he/she decides
		to not name the scenario
}
 \item{lambda12}{Lambda12 parameter to control time to exposure}
  \item{lambda23}{Lambda23 parameter to control time to event after exposure}
  \item{lambda13}{Lambda13 parameter to control time to event in the control group}
  \item{HR}{Hazard Ratio. This input is optional. If HR is set and lambda23 is not set, lambda23 = lambda13*HR}
   \item{exp.prop}{A numeric value between 0 and 1 (not include 0 and 1) that represents the proportion of subjects that are assigned with an exposure}
  \item{rateC}{Rate of the exponential distribution to generate censoring times}
  \item{min.futime}{
A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
}
  \item{min.postexp.futime}{
A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
}
  A .csv filename to write in the output. If the filename does not exist,
    the function will create a new .csv file for the output}
    \item{simu.plot}{A logical value indicating whether to output an incidence plot
or not.The default value is FALSE}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data.frame object with columns corresponding to
  \item{i_scenario}{Scenario name specified by the user}
  \item{i_N}{Number of subjects needs to be screened, specified by the user}
  \item{i_min.futime}{Minimum follow-up time to be considered, specified by the user}
  \item{i_min.postexp.futime}{Minimum post-exposure follow-up time to be
considered, specified by the user}
  \item{i_exp.prop}{Exposure rate specified by the user}
  \item{i_lambda12}{Lambda12 parameter to control time to exposure}
  \item{i_lambda23}{Lambda23 parameter to control time to event after exposure}
  \item{i_lambda13}{Lambda13 parameter to control time to event in the control group}
  \item{i_rateC}{Rate of the exponential distribution to generate censoring times.
  Calculated from median time to censoring, which is specified by the user.
i_beta  Input value of regression coefficient (log hazard ratio)}
  \item{N_eff}{Simulated number of evaluable subjects, which is the resulting number of
subjects with or without considering minimum follow-up time and/or minimum post-exposure follow-up time}
  \item{N_effexp_p}{Simulated proportion of exposed subjects with or without
considering minimum follow-up time and/or minimum
post-exposure follow-up time}
  \item{bhat}{Simulated value of regression coefficient (log hazard ratio)}
  \item{HR}{Simulated value of hazard ratio}
  \item{d}{Simulated number of events in total}
  \item{d_c}{Simulated number of events in control group}
  \item{d_exp}{Simulated number of events in exposed group}
  \item{mst_c}{Simulated median survival time in control group}
  \item{mst_exp}{Simulated median survival time in exposed group}
  \item{pow}{Simulated statistical power from the Cox regression model on data with
time-dependent exposure}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Danyi Xiong, Teeranan Pokaprakarn, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ret <- getpower.method2(nSim=500, N=600, duration=24, scenario="test",lambda12=1.3, lambda23=0.04, lambda13=0.03, HR=NULL,exp.prop=0.2,rateC=0.05, 
                          min.futime=4, min.postexp.futime=4,filename="database.csv", simu.plot=FALSE) 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
