\name{getpower.multicenter}
\alias{getpower.multicenter}

\title{
Calculate power for the Cox proportional hazard model with
time-dependent exposure and multiple centers using method1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This functions runs nSim (Number of simulations, specified by the user) Monte Carlo simulations, each time calling tdSim.multicenter internally. The function returns a data frame of scenario-specific input parameters- and also output statistical power. The user has the option to append the output to a file with file name specified in the input parameters list.
}
\usage{
getpower.multicenter(nSim,N,duration=24,rho=1,beta,med.TimeToCensor=14,
	df,dist=NULL, method, type, scenario, prop.fullexp=0, 
	maxrelexptime=1, min.futime=0, min.postexp.futime=0, output.fn,simu.plot=FALSE) 
}
\arguments{
  \item{nSim}{
  Number of simulations.
  }
  \item{N}{
  Number of subjects to be screened.
  }
  \item{duration}{
  Length of the study in months; the default value is 24 (months).
  }
  \item{rho}{
  Shape parameter of the Weibull distribution. Default is 1, which will generate survival times by using the exponential distribution.
  } 
  \item{beta}{
  A numeric value that represents the exposure effect, which is the regression coefficient (log hazard ratio) that represent the magnitude of the relationship between the exposure covariate and the risk of an event.
  }
  \item{med.TimeToCensor}{
  Median time to censoring for all subjects. The default value is 14 (months).
  }
  
  \item{df}{
   A user-specified n by 4 clustering data frame with columns corresponding to cat_id (category id, which is the physician site id. It can be either text strings or integers), center.size (number of subjects within each center), cat_exp.prop (proportion of exposed subjects in each center), and med.TTE.Control (median time to control event for each center). n rows corresponds to n different centers
  }
  
  \item{dist}{
  The distribution of the center effect across centers. Default is NULL. If dist= 'gamma', then a random frailty effect from a gamma distribution with scale 0.5 and shape 2 is used.
  }
  
  \item{method}{
  Specified which Cox model approach to analyze the data with centers. Can be one of the following: 'frailty', 'fixed effects', 'strata', 'Model with Independence Assumption'. 
  }
  
  \item{type}{
  A text string indicating the what type of dataset is of interest. Either one of "fixed" or "td" should be inputted.
  }
  \item{scenario}{
  Any text string inputted by the user as an option to name a scenario that is being simulated. The use can simply put " "  if he/she decide to not name the scenario.
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
The function calculates power based on the Cox regression model, which calls the coxph function from the survival library using the the simulated data from tdSim.multicenter.
}
\value{
A data.frame object with columns corresponding to
  \item{i_scenario}{
  Scenario name specified by the user
  }
  \item{i_type}{
  Dataset type specified by the user
  }
  \item{i_N}{
  Number of subjects to be screened, specified by the user
  }
  \item{i_min.futime}{
  Minimum follow-up time to be considered, specified by the user
  }
  \item{i_min.postexp.futime}{
  Minimum post-exposure follow-up time to be considered, specified by the user
  }
  \item{i_exp.prop}{
  Exposure rate specified by the user
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
  \item{N_eff}{
  Simulated number of evaluable subjects, which is the resulting number of subjects with or without considering minimum follow-up time and/or minimum post-exposure follow-up time.
  }
  \item{N_effexp_p}{
  Simulated proportion of exposed subjects with or without considering minimum follow-up time and/or minimum post-exposure follow-up time.
  }
  \item{bhat}{
  Simulated value of regression coefficient (log hazard ratio)
  }
  \item{HR}{
  Simulated value of hazard ratio
  }
  \item{d}{
  Simulated number of events in total
  }
  \item{d_c}{
  Simulated number of events in control group
  }
  \item{d_exp}{
  Simulated number of events in exposed group
  }
  \item{mst_c}{
  Simulated median survival time in control group
  }
  \item{mst_exp}{
  Simulated median survival time in exposed group
  }
  \item{pow}{
  Simulated statistical power from the Cox regression model on data with
time-dependent exposure
}
}
\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}


\examples{
# We recommend setting nSim to at least 500. It is set to 10 in the example to
# reduce run time for CRAN submission.

# Run 10 simulations. Each time simulate a dataset of 300 subjects

input_df1 <- data.frame(cat_id = c("low","med","high"), center.size = rep(100,3), 
                        cat_exp.prop = rep(1/3, 3),
                        med.TTE.Control=c(14,20,31))

df_strat <- getpower.multicenter(nSim = 10, N = 300, beta = 0.7, 
								 df = input_df1,method="strata",  type = "td", 
								 scenario = "strata", maxrelexptime = 1/6, 
								 min.futime = 4, min.postexp.futime = 4, 
								 output.fn = "output_mult1.csv")
	
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}

\keyword{Power_Calculation}
