\name{sepTrmt}
\alias{sepTrmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Separate treatment groups
}
\description{
Given a scraped data frame (either NLD or CDR), this function will restructure the data
}
\usage{
sepTrmt(trmt, cohort)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trmt}{
      A character vector of animal IDs associated with a given treatment combination (i.e. M. vaccae CDR animals)
}
  \item{cohort}{
      A scraped data frame associated with a stressor group (either CDR or NLD)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list object with 4 data frames
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{Mvac}{a data frame with only M. vaccae treated animals}
\item{Veh}{a data frame with only vehicle treated animals}
\item{long}{a data frame with all animals in long format}
\item{Avg}{a data frame with the averages and standard errors from both treatment groups}
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
function (trmt, cohort) 
{
    v <- vector()
    for (i in trmt) {
        v = c(v, grep(i, names(cohort)))
    }
    MvCDR <- cleanData(cbind(cohort[, 1:4], cohort[, v]))
    vehCDR <- cleanData(cohort[, -v])
    mvLong <- melt(MvCDR, id = c("minutes", "week", "eday", "zt"))
    mvLong$trmt <- 1
    vehLong <- melt(vehCDR, id = c("minutes", "week", "eday", 
        "zt"))
    vehLong$trmt <- 2
    dataLong <- rbind(mvLong, vehLong)
    mvAvg <- groupData(MvCDR)
    mvAvg$trmt <- 1
    vehAvg <- groupData(vehCDR)
    vehAvg$trmt <- 2
    dataAvg <- rbind(mvAvg, vehAvg)
    data <- list(Mvac = MvCDR, Veh = vehCDR, long = dataLong, 
        avg = dataAvg)
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ restructure }% use one of  RShowDoc("KEYWORDS")

