\name{outofroundness}
\alias{outofroundness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
outofroundness(elevation, H, D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{elevation}{
%%     ~~Describe \code{elevation} here~~
}
  \item{H}{
%%     ~~Describe \code{H} here~~
}
  \item{D}{
%%     ~~Describe \code{D} here~~
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
function (elevation, H, D) 
{
    N <- length(elevation)
    theta <- seq(0, 2 * pi * (N - 1)/N, 2 * pi/N)
    K <- (H * N^2)/(D * 19.739)
    A0 <- mean(elevation)
    A1 <- 2 * mean((elevation - A0) * cos(theta))
    B1 <- 2 * mean((elevation - A0) * sin(theta))
    dispTot <- K * (elevation - A0 - A1 * cos(theta) - B1 * sin(theta))
    disp <- c(0, dispTot[1:(N - 2)] - 2 * dispTot[2:(N - 1)] + 
        dispTot[3:N], 0)
    disp[1] <- dispTot[N] - 2 * dispTot[1] + dispTot[2]
    disp[N] <- dispTot[N - 1] - 2 * dispTot[N] + dispTot[1]
    return(disp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
