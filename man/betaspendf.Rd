\name{betaspendf}
\alias{betaspendf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The default beta spending function
%%  ~~function to do ... ~~
}
\description{The default beta spending function
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
betaspendf(t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{The normalized look times
%%     ~~Describe \code{t} here~~
}
}

\value{
numeric
}

\examples{
t<-c(0.33,0.67,1)
betas<-betaspendf(t)

## The function is currently defined as
function (t) 
0.15 * t^3
}

