\name{cosinefit}
\alias{cosinefit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cosinefit(xdata, ydata, xinterp = seq(xdata[1], xdata[length(xdata)], length = 100), nmode = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdata}{
%%     ~~Describe \code{xdata} here~~
}
  \item{ydata}{
%%     ~~Describe \code{ydata} here~~
}
  \item{xinterp}{
%%     ~~Describe \code{xinterp} here~~
}
  \item{nmode}{
%%     ~~Describe \code{nmode} here~~
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
function (xdata, ydata, xinterp = seq(xdata[1], xdata[length(xdata)], 
    length = 100), nmode = 1) 
{
    x <- list()
    y <- list()
    formulastr <- "ydata ~ "
    for (i in 1:nmode) {
        x[[i]] = cos(i * xdata)
        y[[i]] = sin(i * xdata)
        if (i != 1) {
            formulastr <- paste(formulastr, "+")
        }
        formulastr <- paste(formulastr, "x[[", as.character(i), 
            "]] + y[[", as.character(i), "]]", sep = "")
    }
    mod <- do.call("lm", list(as.formula(formulastr)))
    b <- vector("numeric", length = 2 * nmode + 1)
    for (i in 1:length(b)) {
        b[i] = mod$coefficients[[i]]
    }
    yinterp <- b[1]
    for (i in 1:nmode) {
        bcoef <- sqrt(b[2 * i]^2 + b[2 * i + 1]^2)
        ccoef <- atan2(b[2 * i + 1], b[2 * i])
        yinterp <- yinterp + bcoef * cos(i * xinterp - ccoef)
    }
    return(yinterp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
