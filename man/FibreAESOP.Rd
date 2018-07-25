\name{FibreAESOP}
\alias{FibreAESOP}
\alias{MoveAESOP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Place AESOP Fibres on a Selection of Targets
}
\description{
Given a set of priorities, this function optimally assigns AESOP fibres to targets, taking care to not violate the various AESOP fibre collision rules.
}
\usage{
FibreAESOP(RA_data, Dec_data, RA_AESOP = 0, Dec_AESOP = 0, pri_data = 9, res_data = "lo",
assign_AESOP = TRUE, avoid = 0.9, threshold = 0.1 + 0.085)
MoveAESOP(RA_AESOP=0, Dec_AESOP=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RA_data}{
Numeric vector; Right Ascensions of targets to be fibred (degrees). For convenience, if \option{RA_data} is a two column data.frame or matrix then the first column will be interpreted as \option{RA_data} and the second column as \option{Dec_date}.
}
  \item{Dec_data}{
Numeric vector; Declinations of targets to be fibred (degrees).
}
  \item{RA_AESOP}{
Numeric scalar; right ascension to place AESOP at.
}
  \item{Dec_AESOP}{
Numeric scalar; declination to place AESOP at.
}
  \item{pri_data}{
Integer vector; priorities of all potential targets. Higher priorities will be assigned preferentially in the case of limited fibres and collisions.
}
  \item{res_data}{
Character vector; designates whether the target in question should be assigned a low resolution (lo) or high resolution (hi) fibre. If length 1, then all targets are assumed to have the same require resolution fibre assignment. If not length 1, then must be same length as \option{RA_data}.
}
  \item{assign_AESOP}{
Logical vector; designates whether the target should be assigned a fibre at all. If length 1, then all targets are assumed to need a fibre. If not length 1, then must be same length as \option{RA_data}.
}
  \item{avoid}{
The focal plane fibre avoidance criterion for AESOP, as per 'VIS-DER-4MOST-47110-0900-0001_4_00 Fibre Positioner Design and Analysis Report.pdf' emailed for Jakob Walcher on 16/07/2018.
}
  \item{threshold}{
The 3D fibre-fibre avoidance criterion for AESOP, as per 'VIS-DER-4MOST-47110-0900-0001_4_00 Fibre Positioner Design and Analysis Report.pdf' emailed for Jakob Walcher on 16/07/2018. Not clear if this should include the radius of the fibre or not, since the specification just says fibres cannot be closer than 0.1mm and the fibre diameter is 0.085mm, so currently adding them together.
}
}
\details{
In \code{FibreAESOP} the basic stratgey followed is this:\cr

- Find all fibres that might collide on the focal plane given the \option{avoid} tolerance. If there are collision, remove the lower priority colliding target.\cr

- Assign as many highest priority targets as possible given patrol radius contraints of AESOP.\cr

- If fibres remain, go the next lowest priority and assign more fibres (repeat this process until out of fibres or targets).\cr

- Once all fibres are assigned, send the assignment list to \code{\link{checkgood}}, which check exhaustively for collisions in 3D space given the \option{threshold} avoidance criterion. If there are collisions in 3D then remove all the lower priority (or if same priority, remove the one with a larger ID) colliding target/s and repeat the above process from scratch.\cr

- Return output once either all objects that can be assigned are assigned, or out of fibres. The output is guaranteed to meet the AESOP avoidance criteria as advertised, so it should be useable without issue (hopefully).\cr

\code{MoveAESOP} is really an internal function to rotate AESOP as required to a target position. It is called within \code{FibreAESOP}, and the assumption is most users will not use it directly.
}
\value{
FibreAESOP: various outputs in a list, which are not finalised yet.

MoveAESOP: The coordinates of the rotated version of AESOP, ready for fibre assignment.
}
\author{
Aaron Robotham
}

\note{
The patrol radius is not an argument since this might eventually vary per fibre, and it should be included as part of the AESOP fibre definition file. Currently we assume a value of 11.5mm, 'VIS-DER-4MOST-47110-0900-0001_4_00 Fibre Positioner Design and Analysis Report.pdf' emailed for Jakob Walcher on 16/07/2018. It appears AESOP expects larget patrol radii than the nominal spec, so this should be monitored.
}

\seealso{
\code{\link{TileAESOP}}, \code{\link{checkgood}}
}
\examples{
## None yet
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
