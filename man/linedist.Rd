\name{linedist}
\alias{linedist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Nearest Separation Between Infinite Lines
}
\description{
This function is mostly to be used by \code{\link{checkgood}} to determine if fibres are colliding or not.
}
\usage{
linedist(start1 = cbind(0, 0, 0), end1 = cbind(0, 0, 250), start2 = cbind(0, 0, 0),
end2 = cbind(0, 0, 250))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start1}{
Numeric matrix; the three column xyz starting position defining a reference infinite line.
}
  \item{end1}{
Numeric matrix; the three column xyz ending position defining a reference infinite line.
}
  \item{start2}{
Numeric matrix; the three column xyz starting position defining a comparison infinite line.
}
  \item{end2}{
Numeric matrix; the three column xyz ending position defining a comparison infinite line.
}
}
\details{
Computes the cloest approach between all refence and comparison line pairs. The dimensions of all inputs must be the same!
}
\value{
Numeric vector containing the minimum separation between lines.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{checkgood}}
}
\examples{
## None yet
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
