\name{TileAESOP}
\alias{TileAESOP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Best Location for a 'Greedy' AESOP Tile
}
\description{
Given coordinates and weights for a data set, this function will compute the optimal position to place AESOP given a 'greedy' approach to tiling.
}
\usage{
TileAESOP(RA_data, Dec_data, weight_data = 1, RAlo = 129, RAhi = 141, Declo = -2, Dechi = 3, grid = 0.1, Nsamp = 10000, rad = sqrt(4.06/pi))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RA_data}{
Numeric vector; Right Ascensions of targets to be fibred (degrees). For convenience, if \option{RA_data} is a two column data.frame or matrix then the first column will be interpreted as \option{RA_data} and the second column as \option{Dec_date}.
}
  \item{Dec_data}{
Numeric vector; Declinations of targets to be fibred (degrees).
}
  \item{weight_data}{
Numeric vector; greedy observing weights of all potential targets. Higher weights target will make it more likely that the optimal AESOP position is selected to be there.
}
  \item{RAlo}{
Numeic scalar; the assumed low right ascension extent of the data (degrees).
}
  \item{RAhi}{
Numeic scalar; the assumed high right ascension extent of the data (degrees).
}
  \item{Declo}{
Numeic scalar; the assumed low declination extent of the data (degrees).
}
  \item{Dechi}{
Numeic scalar; the assumed high declination extent of the data (degrees).
}
  \item{grid}{
Numeic scalar; the grid step size for potential AESOP tile centres (degrees).
}
  \item{Nsamp}{
Numeic integer; number of sample points to select from the target data (too large will make the tiling very slow).
}
  \item{rad}{
Numeic scalar; the effective radius of AESOP assumed (we apprpoximate the field of view to be circular for speed).
}
}
\details{
The basic stratgey followed is this:\cr

- At all grid positions all potential AESOP targets are found within the AESOP FOV by the specified \option{RA_data} and \option{Dec_data}.\cr

- The targets are weighted by their \option{weight_data} and summed for each potential AESOP pointing.\cr

- The position with the largest weighted sum is selected as the optimal 'greedy' position to place AESOP at.
}
\value{
A two element numeric vector containing the AESOP central RA and Dec.
}
\references{
Robotham et al, 2010, PASA, 27, 76
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{FibreAESOP}}, \code{\link{checkgood}}
}
\examples{
## None yet
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
