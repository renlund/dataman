% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceq.r
\name{ceq}
\alias{ceq}
\title{An identity function that returns TRUE for confirmable equalities}
\usage{
ceq(x, y, na.equal = FALSE)
}
\arguments{
\item{x}{Numeric (or boolean) vector to be compared with \code{y}}

\item{y}{Numeric (or boolean) vector to be compared with \code{x}}

\item{na.equal}{if \code{TRUE} the comparison of \code{NA} with  \code{NA}
will return TRUE}
}
\description{
This function will return true if the comparison \code{'=='} 
is TRUE. It will return false if \code{'=='} returns \code{FALSE} or \code{NA}.
Optionally it can return \code{TRUE} if both things compared are \code{NA}.
}
\examples{
x <- c(0,1,NA,2); y <- c(0,2,NA,NA)
ceq(x,y)
ceq(x,y,na.equal=TRUE)
}
\author{
Henrik Renlund
}
