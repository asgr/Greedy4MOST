\name{Greedy4MOST}
\alias{Greedy4MOST}
\alias{denmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Greedy 4MOST Tiler
}
\description{
This is the highest level interface to the greedy tiling code, and is the interface most people will/should use.
}
\usage{
Greedy4MOST(tiles = 1:10, RA_data, Dec_data, pri_data, T_data, weight_data='T_data',
T_AESOP = 20, RAlo = 157.3, RAhi = 225, Declo = -4, Dechi = 4, grid = 0.1, Nsamp = 10000,
rad = sqrt(4.06/pi), pri_base = 0, verbose = TRUE, seed = 666)

denmap(RA_data, Dec_data, pri_data=pri_base, RAlo=157.3, RAhi=225, Declo=-4, Dechi=4,
grid=0.05, rad=grid, pri_base=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tiles}{
Integer vector; the length of \option{tiles} specifies the number of greedy AESOP tiles to be assigned to the data, and the value in this vector will be used for the \option{Tile} output column generated.
}
  \item{RA_data}{
Numeric vector; Right Ascensions of targets to be fibred (degrees). For convenience, if \option{RA_data} is a two column data.frame or matrix then the first column will be interpreted as \option{RA_data} and the second column as \option{Dec_date}.
}
  \item{Dec_data}{
Numeric vector; Declinations of targets to be fibred (degrees).
}
  \item{pri_data}{
Integer vector; priorities of all potential targets. Higher priorities will be assigned preferentially in the case of limited fibres and collisions.
}
  \item{T_data}{
Numeric vector; Estimated remaining exposure time observing weights of all potential targets. Higher weight (so remaining exposure time) targets will make it more likely that the optimal AESOP position is selected to be there (minutes).
}
  \item{weight_data}{
Numeric vector; greedy observing weights of all potential targets. Higher weight targets will make it more likely that the optimal AESOP position is selected to be there. If left with the default 'T-data' then then \option{weight_data} is continually adjusted to be \option{T_data} (i.e. it uses the dynamic reduced by \option{T_AESOP} exposure time). If set to 'pri_data' then the dynamic \option{pri_data} is used.
}
  \item{T_AESOP}{
Numeric scalar; the time of exposure per tile (minutes).
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
Numeic scalar; for \code{Greedy4MOST} the effective radius of AESOP assumed (we approximate the field of view to be circular for speed); for \code{denmap} the radius to use for local density.
}
  \item{pri_base}{
Numeric scalaer; the baseline (so starting reference) priority. Anything below this should be considered to be observed, and anything equal or above still needs some exposure time.
}
  \item{verbose}{
Logical; should tile position messages be printed to the screen?
}
  \item{seed}{
Integer scalar; set the random seed for sampling (default of 666 ensures repeatability).
}
}
\details{
In the output list the \option{pri_data} and \option{T_data} of the data element is updated such that \option{T_AESOP} is subtracted from \option{T_data} every time that target gets a fibre. When \option{T_data} si less than or equal to 0 for the first time \option{pri_data} is set to \option{pri_base}-10 (so 90 if the default \option{pri_base}=100 is used). If objects that have already been successfully observed (so \option{pri_data}<\option{pri_base}) get a further fibre then \option{pri_data} is reduced by 1 each time, so a value of \option{pri_data}=85 means it received 5 more exposures than needed to meet its success criterion.

In \code{denmap} the default radius (\option{rad}) of 0.05 degrees is close to 11.5/0.0594/3600=0.054, which is the most problematic scale for AESOP fibres since only at most 18 or 19 (12 or 13 low res) fibres can exist within 11.5mm of a target (given the 9.5mm pitch and 11.5 mm patrol radius). This ends up being ~3 arc minutes, which happens to be a typical scale for groups and clusters on the sky.
}
\value{
A three element list containing:
\item{data}{An updated data.table containing: RA_data, Dec_data, pri_data, T_data, success}
\item{fibreout}{A data.table containing the individual fibre assigments per tile: Tile, fibreID, galaxyID, sep.}
\item{tileout}{A data.table containing the tile position information: Tile, RA_AESOP, Dec_AESOP}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{TileAESOP}}, \code{\link{FibreAESOP}}, \code{\link{checkgood}}
}
\examples{
## None yet.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
