\name{groupData}
\alias{groupData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group Data
}
\description{
Very simple function that just calculates averages and standard errors for a scraped dataset
}
\usage{
groupData(df, trmt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
      data frame that is to be manipulted
}
  \item{trmt}{
      ignore. doesn't do anything
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function scraps all the individual animal data for averages and standard errors
}
\references{
NA
}
\author{
David G. Smith
}
\note{
This function is primarily invoked by other functions
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
function (df, trmt) 
{
    meta <- df[, 1:4]
    a <- df[, -c(1:4)]
    return(data.frame(meta, avg = rowMeans(a), se = apply(a, 
        1, se)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ average }% use one of  RShowDoc("KEYWORDS")
\keyword{ SE }% __ONLY ONE__ keyword per line
