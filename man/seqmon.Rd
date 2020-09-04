\name{seqmon}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{seqmon}
\title{Generic function that calculates boundary crossing probabilities used for monitoring clinical trials
%%  ~~function to do ... ~~
}
\description{Finds the probability that a sequence of standard normal random variables \eqn{z_1, z_2,\ldots,z_m} derived from a normal stochastic process with independent increments will cross a lower and and upper boundary.

}
\usage{
seqmon(a, b, t, int = rep(500, length(t)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{Lower boundary as a numeric vector of length \eqn{m}
%%     ~~Describe \code{a} here~~
}
  \item{b}{Upper boundary as a numeric vector of length \eqn{m}

}
  \item{t}{Information times as a numeric vector of length \eqn{m}

}
  \item{int}{number of intervals that the Z-space is partitioned into for calculation purposes, increasing this will improve accuracy, this is also a numeric vector of length \eqn{m}

}
}

\value{
Produces a numeric vector of length \eqn{2 m} the first \eqn{m} components are the probability that the \eqn{z_k} will be less than \eqn{a_k} for some \eqn{k\le i}  and be less than \eqn{b_k} for all \eqn{k \le i}.  The second \eqn{m} components are the probability that the \eqn{z_k} will be greater than \eqn{b_k} for some \eqn{k\le i}  and be greater than \eqn{a_k} for all \eqn{k \le i}.

Note that the last probability in the sequence is the overall significance level of a sequential design that uses \code{a} and \code{b} as upper and lower boundaries. To get power you subtract the \eqn{\mu \sqrt(t)} from \code{a} and \code{b} where \eqn{\mu} is the mean of \eqn{z_m} under the alternative hypothesis.
}
\references{Schoenfeld, David A. "A simple algorithm for designing group sequential clinical trials." Biometrics 57.3 (2001): 972-974.}
\examples{
%% Haybittle Peto boundary with futility stopping under null
seqmon(a=c(0,0,0), b=c(qnorm(1-0.005),qnorm(1-0.005),2.025), 
         t=c(.33,.66,1), int = rep(500, 3))
%% Haybittle Peto boundary with futility stopping under alternative with 80% power in non-sequential trial
t=c(.33,.66,1)
u=(qnorm(.8)+qnorm(1-0.025))
seqmon(a=c(0,0,0)-u*sqrt(t), b=c(qnorm(1-0.005),qnorm(1-0.005),2.025)-u*sqrt(t), 
         t=c(.33,.66,1), int = rep(500, 3))

}
