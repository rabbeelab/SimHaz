\name{tdSim.exp.matching}
\alias{tdSim.exp.matching}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Simulate 1 dataframe (1 simulation) of time-dep exposure under method 2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function simulate Survival Data. It generates a simulated dataset with time-dependent exposure under method 2 with a user-specified list of parameters as input.
}
\usage{

tdSim.exp.matching(N_match, duration=24,lambda, rho=1, beta,
	rateC,matching.ratio=3,prop.fullexp=0,maxrelexptime=1,min.futime=0,
	min.postexp.futime=0)
}

\arguments{
  \item{N_match}{
  Number of subjects
  }
  \item{duration}{
  Length of the study in Months. The default value is 24 (months)
  }
  \item{lambda}{
  Scale parameter of the Weibull distribution, which is calculated as log(2) / median time to event for control group
  }
  \item{rho}{
  Shape parameter of the Weibull distribution, which is defaulted as 1, as we generate survival times by using the exponential distribution
  }
  \item{beta}{
  A numeric value that represents the exposure effect, which is the regression coefficient (log hazard ratio) that represent the magnitude of the relationship between the exposure covariate and the risk of an event
}
  \item{rateC}{
  Rate of the exponential distribution to generate censoring times, which is calculated as log(2) / median time to censoring
  }
  
  \item{matching.ratio}{
  Matching ratio used in exposure matching.For 1:1, the user should specify 1.input value 3 corresponds to 1:3 (exposed : unexposed). input value 0.25 corresponds to 4:1 (ie. 1:0.25).
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Simulata a Survival dataset using a modified version of illness-death model controlled by lambda12, lambda23, lambda13
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
}

\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{


df1 = tdSim.exp.matching<-function(N_match, duration=24,lambda, rho=1, 
	beta, rateC,matching.ratio=3,  prop.fullexp=0,maxrelexptime=1,min.futime=0,
	min.postexp.futime=0)

}
\keyword{Simulation}

