\name{squeezeData}
\alias{squeezeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bin time intervals
}
\description{
This function will average all the data points into a given time interval. This is useful for compressing the data and making it easier to visualize.
}
\usage{
squeezeData(df, bin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
      a "cleaned" data frame (use the cleanData function) that has also been scraped
}
  \item{bin}{
      the time interval (in minutes) to average over
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a data frame that has been "squeezed"
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
function (df, bin) 
{
    a <- df[, -c(1, 2, 3, 4)]
    meta <- df[, c(1, 2, 3, 4)]
    meta <- meta[bin * (1:(nrow(meta)/bin) - 1), ]
    v <- vector()
    b <- list()
    for (i in 1:ncol(a)) {
        for (j in 0:(floor(nrow(a)/bin) - 1)) {
            v[j] <- mean(a[, i][(1:bin) + bin * j])
        }
        b[[i]] <- v
    }
    c <- data.frame(matrix(nrow = length(b[[i]]), ncol = ncol(a)))
    for (i in 1:ncol(a)) {
        c[, i] <- b[[i]]
    }
    names(c) <- names(a)
    c <- data.frame(meta, c)
    return(c)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ binning }% use one of  RShowDoc("KEYWORDS")

