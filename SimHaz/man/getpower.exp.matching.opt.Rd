\name{getpower.exp.matching.opt}
\alias{getpower.exp.matching.opt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate betahat bias and variance for different matching ratios in the case of exposure matching in the Cox proportional Hazard model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is used to explore what matching ratios should be used in the case of exposure matching. It simulate a large population (100000 subjects) in order to estimate a betahat value of that population. Then in repeated simulation, a subset of that population is drawn and exposure matching is done according to a user-specified list of ratios in order to compare the bias in the betahat estimate from the betahat value estimated from the large populationas well as the varaince of the betahat estimate for each ratio.
}
\usage{

getpower.exp.matching.opt(nSim, N, ratios=c(1,0.25,0.333,0.5,2,3,4,5),duration=24,
    med.TTE.Control=24,rho=1,med.TimeToCensor=14,beta, exp.prop,type,scenario, 
    method,  prop.fullexp=0,maxrelexptime=1,min.futime=0,min.postexp.futime=0,
    output.fn=NULL,simu.plot=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nSim}{
  Number of simulations.
  }
  \item{N}{
  Number of subjects to be screened.
  }
  \item{ratios}{
  Specification for matching ratio as a list of numbers.
  For 1:1, the user should specify 1.input value 3 corresponds to 1:3 (exposed : unexposed). input value 0.25 corresponds to 4:1 (ie. 1:0.25). A list of c(1,0.25, 2) corresponds to the following matching ratios: 1:1, 4:1, 1:2
  }
  \item{duration}{
  Length of the study in months; the default value is 24 (months).
  }
  \item{med.TTE.Control}{
  Median time to event for control group; the default value is 24 (months).
  }
  \item{rho}{
  Shape parameter of the Weibull distribution. Default is 1, which will generate survival times by using the exponential distribution.
  } 
  \item{med.TimeToCensor}{
  Median time to censoring for all subjects. The default value is 14 (months).
  }
  \item{beta}{
  A numeric value that represents the exposure effect, which is the regression coefficient (log hazard ratio) that represent the magnitude of the relationship between the exposure covariate and the risk of an event.
  }
  \item{exp.prop}{
  test
  }
  \item{type}{
  A text string indicating the what type of dataset is of interest. Either one of "fixed" or "td" should be inputted.
  }

  \item{scenario}{
  Any text string inputted by the user as an option to name a scenario that is being simulated. The use can simply put " "  if he/she decide to not name the scenario.
  }
  \item{method}{
  Specified which Cox model approach to analyze the data with clusters(matching sets in this case). Can be one of the following: 'frailty', 'fixed effects', 'strata', 'Model with Independence Assumption'.
  }
  \item{prop.fullexp}{
  A numeric value in interval [0, 1) that represents the proportion of exposed subjects that are fully exposed from the beginning to the end of the study. The default value is 0, which means all exposed subjects have an exposure status transition at some point during the study. Only applies when type is "td". When type is "td", the value is automatically 1.
   }
  \item{maxrelexptime}{
  A numeric value in interval (0, 1] that represents the maximum relative exposure time. Suppose this value is p, the exposure time for each subject is then uniformly distributed from 0 to p*subject's time in the study. The default value is 1, which means all exposed subjects have an exposure status transition at any point during the time in study.
  }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study.
  }
  \item{min.postexp.futime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure.
  }
  \item{output.fn}{
  A .csv filename to write in the output. If the filename does not exist, the function will create a new .csv file for the output.
  }
  \item{simu.plot}{
  A logical value indicating whether or not to output an incidence plot. The default value is FALSE.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The function calculates power based on the Cox regression model, which calls the coxph function from the survival library using the the simulated data from tdSim.method1.
}
\value{
A data.frame object with columns corresponding to
  \item{i_scenario}{
  Scenario name specified by the user
  }
  \item{i_type}{
  Dataset type specified by the user
  }
  \item{i_min.futime}{
  Minimum follow-up time to be considered, specified by the user
  }
  \item{i_min.postexp.futime}{
  Minimum post-exposure follow-up time to be considered, specified by the user
  }
  \item{i_lambda}{
  Value of the scale parameter of the Weibull distribution to generate survival times. Calculated from median time to event for control group, which is specified by the user.
  }
  \item{i_rho}{
  User-specified value of the shape parameter of the Weibull distribution to generate survival times
  }
  \item{i_rateC}{
  Rate of the exponential distribution to generate censoring times. Calculated from median time to censoring, which is specified by the user. i_beta  Input value of regression coefficient (log hazard ratio).
  }
  \item{i_beta}{
  value of the input beta
  }
  \item{N_match}{
  Number of matching sets
  }
  \item{N_exposed}{
  Number of exposed subjects
  }
  \item{N_unexposed}{
  Number of unexposed subjects
  }
  \item{bhat}{
  Simulated value of regression coefficient (log hazard ratio)
  }
  \item{pow}{
  Simulated statistical power from the Cox regression model on data with
  time-dependent exposure
  }
  \item{actual_beta}{
  Value of betahat based on the population of 100000 subjects.
  }
  \item{bias}{
  Value of betahat - actual_betahat(approximated from the big population)
  }
  \item{variance}{
  Variance of the betahat from the simulations
  }
  \item{RE1}{
   The variance of betahat of the ratio in that row divided by variance of   betahat for the 1:1 matching
  }
  \item{RE2}{
  The variance of betahat of the ratio in that row divided by variance of the betahat for the closest ratio based on the exposure proportion in the population
  }
}
\references{
%% ~put references to the literature/web site here ~
Therneau T (2015). A Package for Survival Analysis in S. version 2.38,
 
http://CRAN.R-project.org/package=survival
}
\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
# Install the survival package if needed.

library(survival)

# We recommend setting nSim to at least 500. It is set to 10 in the example to
# reduce run time for CRAN submission.

# Run 5 simulations. Each time simulate a dataset of 400 subjects with
# time-dependent exposure with both minimum follow-up time (4 months) and
# minimum post-exposure follow-up time (4 months) imposed. Also consider a
# quick exposure after entering the study for each exposed subject. Set the
# maximum relative exposure time to be 1/6. 

# Set the duration of the study to be 24 months; the median time to event for
# control group to be 24 months; exposure effect to be 0.3; median time to
# censoring to be 14 months; and exposure proportion to be 20%.

marginal = getpower.exp.matching.opt(nSim=5, N=400, ratios=c(1), duration=24,
    med.TTE.Control=24, rho=1,med.TimeToCensor=14,beta=0.5, exp.prop=0.3, 
    type="td",scenario="opt_exp_matching", method="marginal",prop.fullexp=0,
    maxrelexptime=1,min.futime=0,min.postexp.futime=0, output.fn="opt_matching",
    simu.plot=FALSE)

}

\keyword{Power_Calculation}
