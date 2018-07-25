\name{AESOP}
\alias{AESOP}
\docType{data}
\title{
AESOP Fibre File
}
\description{
Basic AESOP fibre information.
}
\usage{data("AESOP")}
\format{
  A data frame with 2436 observations on the following 9 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{x0_mm}}{a numeric vector}
    \item{\code{y0_mm}}{a numeric vector}
    \item{\code{res}}{a numeric vector}
    \item{\code{sector}}{a numeric vector}
    \item{\code{EdgeFlag}}{a numeric vector}
    \item{\code{Spectro}}{a factor with levels \code{HR} \code{LR-A} \code{LR-B}}
    \item{\code{Slitlet}}{a numeric vector}
    \item{\code{Slitpos}}{a numeric vector}
  }
}
\details{
This is the base file, which is added to on load using the hidden .onLoad function in the zzz.R file.
}
\source{
The file is mostly as per http://wiki.4most.eu/local--files/4most-facility-simulator/aesop-cfg.txt, sent by Wing-Fai Thi by email 09/07/2018.
}
\examples{
data(AESOP)
str(AESOP)
plot(AESOP[,c('x0_mm', 'y0_mm')], asp=1)
}
\keyword{datasets}
