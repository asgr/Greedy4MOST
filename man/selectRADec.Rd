\name{selectRADec}
\alias{selectRADec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select Right Ascensions and Declination Between Limits
}
\description{
A utility function to correctly select RAs and Decs between limits. This can be fiddly due to the wrapping nature of coordinates.
}
\usage{
selectRADec(RA, Dec, RAlo = 129, RAhi = 141, Declo = -2, Dechi = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RA}{
Numeric vector; Right Ascensions of targets (degrees).
}
  \item{Dec}{
Numeric vector; Declinations of targets (degrees).
}
  \item{RAlo}{
Numeric scalar; the low Right Ascension limit (degrees).
}
  \item{RAhi}{
Numeric scalar; the high Right Ascension limit (degrees).
}
  \item{Declo}{
Numeric scalar; the low Declination limit (degrees).
}
  \item{Dechi}{
Numeric scalar; the high Declination limit (degrees).
}
}
\details{
This code can correctly handle wrapping coordinates, i.e. if \option{RAlo}=350 and \option{RAhi}=10 then objects will be selected between the 20 degrees crossing RA=0 rather than the 340 degrees crossing RA=180. if the latter is required then instead supply \option{RAlo}=10 and \option{RAhi}=350. Similar logic works for \option{Declo} and \option{Dechi}, i.e. if \option{Declo}>\option{Dechi} then selection is made to the poles.
}
\value{
List containing:
\item{data}{Two column matrix containing RA and Dec positions selected from \option{RA} and \option{Dec}.}
\item{select}{Integer vector containing element IDs selected from \option{RA} and \option{Dec}.}
}
\author{
Aaron Robotham
}

\examples{
## None yet
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
