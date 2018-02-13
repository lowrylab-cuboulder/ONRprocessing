\name{plotSmooths}
\alias{plotSmooths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create and plot smoothed data
}
\description{
This function implements the 'smooth.spline' function with spar parameters ranging from 0.1 to 0.9. The intention is to smooth 1 week of data for 1 treatment
group. 9 smoothed data sets are returned as well as a plot of all 9 smoothed datasets. It is currently assumed that the input is the 'avg' data frame returned from the sepTrmt function, so plotSmooths looks for a column called 'avg'. 
}
\usage{
plotSmooths(df,weekn,trmt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
      the 'avg' data frame obtained from the 'sepTrmt' function. To be supplied as NameOfYourObject$avg
}
  \item{weekn}{
      an integer indicating the desired week to be examined
  }
  \item{trmt}{
      an integer indicating what treatment group to examine. The value should be either 1 (m. vaccae) or 2 (vehicle).
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
stuff.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
dgs
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
function (df) 
{
    spline1 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.1)$y, method = "spar = 0.1")
    spline2 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.2)$y, method = "spar = 0.2")
    spline3 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.3)$y, method = "spar = 0.3")
    spline4 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.4)$y, method = "spar = 0.4")
    spline5 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.5)$y, method = "spar = 0.5")
    spline6 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.6)$y, method = "spar = 0.6")
    spline7 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.7)$y, method = "spar = 0.7")
    spline8 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.8)$y, method = "spar = 0.8")
    spline9 <- data.frame(x = df$minutes, y = smooth.spline(df$temp, 
        spar = 0.9)$y, method = "spar = 0.9")
    smoothPlot <- ggplot(rbind(spline1, spline2, spline3, spline4, 
        spline5, spline6, spline7, spline8, spline9), aes(x, 
        y)) + geom_point(data = a, aes(minutes, temp), alpha = 0.05, 
        col = "red") + geom_line(col = "blue") + facet_wrap(~method) + 
        geom_vline(xintercept = ((1:7) + 14) * 1440)
    splines <- list(spline1 = spline1, spline2 = spline2, spline3 = spline3, 
        spline4 = spline4, spline5 = spline5, spline6 = spline6, 
        spline7 = spline7, spline8 = spline8, spline9 = spline9, 
        plot = smoothPlot)
    return(splines)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }% use one of  RShowDoc("KEYWORDS")

