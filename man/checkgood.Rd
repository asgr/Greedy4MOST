\name{checkgood}
\alias{checkgood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check for a Good 3D AESOP Assignment
}
\description{
Checks whether any assigned AESOP fibres are colliding (or rather too close) in 3D space.
}
\usage{
checkgood(xy_start, xy_end, z_start = 0, z_end = 250, threshold = 0.1 + 0.085)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy_start}{
Numeric matrix; two columns specifying the xy starting (the spine projection location) AESOP fibres positions (mm).
}
  \item{xy_end}{
Numeric matrix; two columns specifying the xy ending (the spine target location on the focal plane) AESOP fibres positions (mm).
}
  \item{z_start}{
z location of the AESOP plate, which is 0 as standard (mm).
}
  \item{z_end}{
z location of the AESOP focal plane, which is 250 as standard (mm).
}
  \item{threshold}{
The 3D fibre-fibre avoidance criterion for AESOP, as per 'VIS-DER-4MOST-47110-0900-0001_4_00 Fibre Positioner Design and Analysis Report.pdf' emailed for Jakob Walcher on 16/07/2018. Not clear if this should include the radius of the fibre or not, since the specification just says fibres cannot be closer than 0.1mm and the fibre diameter is 0.085mm, so currently adding them together.
}
}
\details{
The basic stratgey followed is this:\cr

- Find all fibres [i] that have a fibre [j] nearer to their own start position, i.e. where distance(xy_start[i,], xy_end[i,]) > distance(xy_start[i,], xy_end[j,]). In this situation fibres i and j must have their closest approach within the physical extent of their fibres (rather than outside of AESOP).\cr

- Compute the 3D space minimum line separation between all such fibres pairs using \code{\link{linedist}}\cr

- Return the i and j IDs of pairs that are found to have a closest approach <= \option{threshold}.
}
\value{
A two column matrix containing the i and j IDs of fibres that are considered to be colliding in 3D space.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{TileAESOP}}, \code{\link{FibreAESOP}}, \code{\link{linedist}},
}
\examples{
## None yet
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
