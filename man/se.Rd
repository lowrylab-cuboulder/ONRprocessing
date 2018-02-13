\name{se}
\alias{se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate standard error
}
\description{
Very simple function to calculate standard error. Really only to be used by other functions
}
\usage{
se(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
      a numeric vector
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the standard error
}
\references{
NA
}
\author{
David G. Smith
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    sd(x)/sqrt(length(x))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stats }% use one of  RShowDoc("KEYWORDS")
