\name{plotAnimals}
\alias{plotAnimals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot individual animals
}
\description{
Plots all animals within specified treatment group for any given week. This is useful for identifying outlier animals.
}
\usage{
plotAnimals(df, trmt, week)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
      data frame that is labeled as the 'long' object returned from the sepTrmt function. 
}
  \item{trmt}{
      treatment code. Either 1 or 2 (1 = M. vaccae, 2 = vehicle)
}
  \item{week}{
      the week integer
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
just makes a graph
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
function (df, trmt, week) 
{
    dayMod = (week - 1) * 7
    ggplot(df[df$trmt == trmt & df$week == week, ], aes(x = minutes, 
        y = value)) + geom_point(aes(colour = factor(variable))) + 
        geom_smooth(method = "loess", span = 0.1, aes(colour = factor(variable))) + 
        geom_vline(xintercept = ((1:7) + dayMod) * 1440)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }% use one of  RShowDoc("KEYWORDS")

