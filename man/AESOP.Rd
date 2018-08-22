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
\note{
This file contains one entry per Fiber/Positioner, and gives the coordinates and details of each fiber.

\itemize{
\item Plate scale: 0.0594 mm/asec
\item Closest pitch between fibres: 9.542mm (160.6397 asec)
\item Fibre size is 0.085 (1.430976 asec)
\item FoV is diameter: 535mm (2.501871 deg)
\item Radius of curvature at focal plane: 4,653 mm
\item 3D avoidance below the focal plane is 0.1mm (1.683502 asec) ignoring the fibre radius
\item Tip avoidance on the focal plane is 0.9mm (15.15152 asec)
\item Spine length is 250mm
\item Patrol requirement is 11.5mm (193.6027 asec), but in practice they expect 12.3mm (207.0707 asec) +/- 0.5 (8.417508 asec), i.e. 11.8mm (198.6532 asec) - 12.8mm (215.4882 asec)
}
}
\source{
Original filename: /home/tdwelly/4most/OpSim/inputs_to_system_model/AESOP/info_2016-04-05/aesop-cfg.json
The file used here is mostly as per http://wiki.4most.eu/local--files/4most-facility-simulator/aesop-cfg.txt, sent by Wing-Fai Thi by email 09/07/2018.
}
\examples{
data(AESOP)
str(AESOP)
plot(AESOP[,c('x0_mm', 'y0_mm')], asp=1)
}
\keyword{datasets}
