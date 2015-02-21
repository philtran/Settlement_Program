\name{edgesettlementanalysis}
\alias{edgesettlementanalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edgesettlementanalysis(edgedata, breakoverdata, R, D, orientation = 90, slope = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edgedata}{
%%     ~~Describe \code{edgedata} here~~
}
  \item{breakoverdata}{
%%     ~~Describe \code{breakoverdata} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
}
  \item{D}{
%%     ~~Describe \code{D} here~~
}
  \item{orientation}{
%%     ~~Describe \code{orientation} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
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
function (edgedata, breakoverdata, R, D, orientation = 90, slope = 0) 
{
    B <- edgedata - breakoverdata + slope * R
    Bmax <- maxedgesettlement(R, D, orientation)
    print(paste0("For R=", R, " ft, Bmax=", format(round(Bmax, 
        2), nsmall = 2), " inches, 0.75*Bmax =", format(round(0.75 * 
        Bmax, 2), nsmall = 2), " inches"))
    print("Station No.  |  B (inch)  |  Action")
    for (i in 1:length(B)) {
        if (i < 10) {
            stationstr <- paste0("        0", i, "   |")
        }
        else {
            stationstr <- paste0("        ", i, "   |")
        }
        if (B[i] >= 0) {
            mainstr <- paste0(stationstr, "    ", format(round(B[i], 
                2), nsmall = 2), "    |")
        }
        else {
            mainstr <- paste0(stationstr, "   ", format(round(B[i], 
                2), nsmall = 2), "    |")
        }
        if (B[i] > Bmax) {
            mainstr <- paste0(mainstr, "  B has exceeded Bmax!  Repairs required")
        }
        else if (B[i] <= Bmax && B[i] >= 0.75 * Bmax) {
            mainstr <- paste0(mainstr, "  B is within 0.75*Bmax; Inspection required")
        }
        else {
            mainstr <- paste0(mainstr, "  B is within bounds")
        }
        print(mainstr)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
