\name{updateDesign-methods}
\docType{methods}
\alias{updateDesign-methods}
%\alias{updateDesign,sequential.design-method}
\title{ ~~ Methods for Function \code{updateDesign}  ~~}
\description{
This method updates the design at a certain look.
}
\section{Methods}{
\describe{
The trial needs to be updated sue to a change in the future look times. The total alpha spending and total beta spending at the end of the trial are not updated. 
No historical information such as the past look times, the past alpha and beta spent, or the baseline spending function is updated.
The alpha or beta spending functions may be updated whenever necessary. The efficacy and futility boundaries are updated when needed.
The details are given in Proschan, Lan, and Wittes(2006) and Schoenfeld (2001).
\item{\code{signature(theObject = "sequential.design")}}{
%%  ~~describe this method here~~
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
