\name{cleanData}
\alias{cleanData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clean data}
\description{cleanData replaces NA values, values over 40, and values under 33 with the mean of the 10 preceding and 10 proceeding values.}
\usage{
cleanData(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
      data frame obtained after running the scrape function (i.e. the first 4 columns are independent variables)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns your cleaned data frame!
}
\references{
NA
}
\author{
David Smith
}
\note{
This function is primarily just invoked by other functions. It also takes a while to run
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
    for (i in 5:ncol(df)) {
        for (j in 1:nrow(df)) {
            if (is.na(df[j, i]) & (j < 11 | j > nrow(df) - 11)) {
                df[j, i] <- mean(df[, i], na.rm = T)
            }
            if (is.na(df[j, i])) {
                mean_index <- c(j - 10:j + 10)
                df[j, i] <- mean(df[mean_index, i], na.rm = T)
            }
        }
        df[, i][df[, i] > 40 | df[, i] < 33] <- mean(df[, i], 
            na.rm = T)
    }
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clean }% use one of  RShowDoc("KEYWORDS")
\keyword{ removeNAs }% __ONLY ONE__ keyword per line
