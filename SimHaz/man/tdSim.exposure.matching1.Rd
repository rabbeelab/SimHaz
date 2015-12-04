\name{tdSim.exposure.matching1}
\alias{tdSim.exposure.matching1}
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

tdSim.exposure.matching1(N_match,duration, lambda, rho, beta, rateC,
	 matching.ratio, prop.fullexp, maxrelexptime,  
	 min.futime = 0, min.postexp.futime = 0)
}

\arguments{
  \item{N_match}{
  Number of subjects
  }
  \item{duration}{
  Duration of the study. This is used in censoring
  }
  \item{lambda}{
  
  }
  
  \item{rho}{
 
  }
  
  \item{beta}{
 
  }

  \item{rateC}{
  Rate of the exponential distribution to generate censoring times
  }
  
  \item{matching.ratio}{
 
  }
  
  \item{prop.fullexp}{
 
  }
  
  \item{maxrelexptime}{
 
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{Simulation}

