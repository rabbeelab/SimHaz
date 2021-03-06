\name{tdSim.multicenter}
\alias{tdSim.multicenter}

\title{
Simulate 1 dataframe (1 simulation) of time-dependent exposure with multiple centers
}
\description{
This function allows the user to input a data frame with multi-center
parameters and generates a simulated dataset with time-dependent exposure.
In particular, the output dataset has a column corresponding to the center id, which will be used as a clustering variable in the Cox regression model in power calculation.
}
\usage{

tdSim.multicenter(N,duration=24,rho=1,beta,rateC,df,
    prop.fullexp=0,maxrelexptime=1,min.futime=0,
    min.postexp.futime=0, dist=NULL)  
}

\arguments{
  \item{N}{
  Number of subjects needs to be screened
  }
  \item{duration}{
  Length of the study in Months. The default value is 24 (months)
  }
  \item{rho}{
  Shape parameter of the Weibull distribution, which is defaulted as 1, as we generate survival times by using the exponential distribution
  }
  \item{beta}{
  A numeric value that represents the exposure effect, which is the
  regression coefficient (log hazard ratio) that represent the magnitude of
the relationship between the exposure covariate and the risk of an event
}
  \item{rateC}{
  Rate of the exponential distribution to generate censoring times, which is calculated as log(2) / median time to censoring
  }
  \item{df}{
  A user-specified n by 4 clustering data frame with columns corresponding to cat_id (category id, which is the physician site id. It can be either text strings or integers), center.size (number of subjects within each center), cat_exp.prop (proportion of exposed subjects in each center), and med.TTE.Control (median time to control event for each center). n rows corresponds to n different centers
  }
  \item{prop.fullexp}{
  A numeric value in interval [0, 1) that represents the proportion of exposed subjects that are fully exposed from the beginning to the end of the study. The default value is 0, which means all exposed subjects have an exposure status transition at some point during the study
  }
  \item{maxrelexptime}{
   A numeric value in interval (0, 1] that represents the maximum relative exposure time. Suppose this value is p, the exposure time for each subject is then uniformly distributed from 0 to p*subject's time in the study. The default value is 1, which means all exposed subjects have an exposure status transition at any point during the time in study.  
   }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
  }
  \item{min.postexp.futime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
  }
  \item{dist}{
  The distribution of the center effect across centers. Default is NULL. If dist= 'gamma', then a random frailty effect from a gamma distribution with scale 0.5 and shape 2 is used.
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
The current version of this function allows the user to input a data frame with at
least 3 categories of physician sites, because the function uses a multinomial
distribution to assign subjects into each category according to the corresponding category proportion
}

\value{
  A data.frame object with columns corresponding to
  \item{id}{
  Integer that represents a subject's identification number
  }
  \item{start}{
  For counting process formulation. Represents the start of each time interval
  }
  \item{stop}{
  For counting process formulation. Represents the end of each time interval
  }
  \item{status}{
  Indicator of event. status = 1 when event occurs and 0 otherwise
  }
  \item{x}{
  Indicator of exposure. x = 1 when exposed and 0 otherwise
  }
  \item{clst_id}{
  For clustering in the Cox proportional hazard model. Represents label of each subject's corresponding physician site
  }
}
\references{
T. Therneau and C. Crowson (2015). Using Time Dependent Covariates and Time Dependent Coefficients in the Cox Model.

https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf
}
\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
# Create a clustering data frame as input with 3 categories and a 20% weighted
# exposure proportion.
  
input_df1 <- data.frame(cat_id = c("low","med","high"), 
                        center.size = rep(100,3),  
                        cat_exp.prop = rep(1/3, 3), 
                        med.TTE.Control=c(14,20,31))

df_strat <- tdSim.multicenter(N = 300, duration =24, rateC = log(2)/14, beta = 0.7, 
    df = input_df1, maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simulation}
