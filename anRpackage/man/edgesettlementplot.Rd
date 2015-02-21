\name{edgesettlementplot}
\alias{edgesettlementplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edgesettlementplot(data, station)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{station}{
%%     ~~Describe \code{station} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (data, station) 
{
    windows(8, 6)
    legendtxt <- vector("character")
    for (i in 1:length(station)) {
        x <- data[["Distance"]]
        y <- vector("numeric")
        for (j in 1:length(x)) {
            y[j] <- data[[paste0("X", x[j])]][station[i]]
        }
        if (i == 1) {
            plot(x, y, ann = F, type = "o", col = i)
            title(main = "Settlement Depth vs Distance from Shell", 
                xlab = "Distance from shell (ft)", ylab = "Settlement Depth (in)")
        }
        else {
            lines(x, y, col = i)
            points(x, y, col = i)
        }
        legendtxt[i] <- paste0("Station ", station[i])
    }
    legend("bottomright", legendtxt, lty = rep(1, length(station)), 
        pch = rep(1, length(station)), col = 1:length(station))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
