\name{control}
\alias{control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Track internal variables during session
}
\description{
An internal class used to track internal variables
}
\usage{
control(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{variables a list of variables
}}


\value{
A control reference class object is created. Nothing is returned.
}
\author{
Awo Ashiabor
}
\examples{

##create a control
myControl <- control$new()

##set a variable
myControl$addVariable("packageName", "RSQLAlchemy")

##retrieve a variable
myControl$variables[["packageName"]]

\dontrun{
myControl <- control$new()
myControl$addVariable("packageName", "RSQLAlchemy")
myControl$variables[["packageName"]]
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ control }
