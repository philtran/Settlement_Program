\name{settlementanalysis}
\alias{settlementanalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
settlementanalysis(xdata, ydata, D, analysis = 1, short = F, maxL = 32, plotgraph = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdata}{
%%     ~~Describe \code{xdata} here~~
}
  \item{ydata}{
%%     ~~Describe \code{ydata} here~~
}
  \item{D}{
%%     ~~Describe \code{D} here~~
}
  \item{analysis}{
%%     ~~Describe \code{analysis} here~~
}
  \item{short}{
%%     ~~Describe \code{short} here~~
}
  \item{maxL}{
%%     ~~Describe \code{maxL} here~~
}
  \item{plotgraph}{
%%     ~~Describe \code{plotgraph} here~~
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
function (xdata, ydata, D, analysis = 1, short = F, maxL = 32, 
    plotgraph = F) 
{
    if (length(xdata) == length(ydata)) {
        if (analysis == 1) {
            if (length(xdata) >= 3) {
                U <- xdata - ydata
                if (short == F) {
                  S <- vector("numeric", length = length(U))
                  S[2:(length(U) - 1)] <- U[2:(length(U) - 1)] - 
                    1/2 * (U[1:(length(U) - 2)] + U[3:length(U)])
                  S[1] <- U[1] - (1/2 * U[length(U)] + 1/2 * 
                    U[2])
                  S[length(U)] <- U[length(U)] - (1/2 * U[length(U) - 
                    1] + 1/2 * U[1])
                  seq1 <- 1:length(U)
                }
                else {
                  nsettle <- ceiling(pi * D/maxL)
                  nstep <- max(1, floor(length(U)/nsettle))
                  idmax <- which(abs(U) == max(abs(U)))[1]
                  S <- vector("numeric", length = ceiling(length(U)/nstep))
                  (enforce_cyclic <- function(x, L) {
                    for (i in 1:length(x)) {
                      if (x[i] == 0) {
                        x[i] <- L
                      }
                    }
                    return(x)
                  })
                  seq1 <- sort(enforce_cyclic(seq(from = idmax, 
                    by = nstep, length.out = length(S))\%\%length(U), 
                    length(U)))
                  seq2 <- enforce_cyclic((seq1 - nstep)\%\%length(U), 
                    length(U))
                  seq3 <- enforce_cyclic((seq1 + nstep)\%\%length(U), 
                    length(U))
                  S <- U[seq1] - 1/2 * (U[seq2] + U[seq3])
                }
                output <- list(S = S, idvec = seq1)
                return(output)
            }
            else {
                return("Error: Not enough data points")
            }
        }
        else if (analysis == 2) {
            sgn = sign(ydata[2] - ydata[1])
            idextrema <- vector("numeric")
            id <- 1
            for (i in 2:(length(ydata) - 1)) {
                if (sign(ydata[i + 1] - ydata[i]) != sgn) {
                  idextrema[id] <- i
                  id <- id + 1
                  sgn <- sign(ydata[i + 1] - ydata[i])
                }
            }
            if (sign(ydata[length(1)] - ydata[length(ydata)]) != 
                sgn) {
                idextrema[id] <- length(ydata)
                id <- id + 1
                sgn <- ydata[1] - ydata[length(ydata)]
            }
            if (sign(ydata[2] - ydata[1])) {
                idextrema[id] <- 1
            }
            idpeak <- vector("numeric")
            id <- 1
            for (i in 2:(length(ydata[idextrema]) - 1)) {
                if (ydata[idextrema][i + 1] < ydata[idextrema][i]) {
                  idpeak[id] <- idextrema[i]
                  id <- id + 1
                }
            }
            if (ydata[idextrema][1] < ydata[idextrema][length(idextrema)]) {
                idpeak[id] <- idextrema[length(idextrema)]
                id <- id + 1
            }
            if (ydata[idextrema][2] < ydata[idextrema][1]) {
                idpeak[id] <- idextrema[1]
            }
            idpeak <- sort(idpeak)
            Sarc <- vector("numeric", length = length(idpeak) - 
                1)
            for (i in 1:(length(idpeak) - 1)) {
                Sarc[i] <- xdata[idpeak[i + 1]] - xdata[idpeak[i]]
            }
            Sarc[length(idpeak)] <- xdata[idpeak[1]] + 2 * pi - 
                xdata[idpeak[length(idpeak)]]
            S <- vector("numeric", length = length(Sarc))
            idvec <- vector("numeric", length = length(S))
            idmod <- 1:length(xdata)
            if (idpeak[1] > 1) {
                idmod <- c(idmod[idpeak[1]:length(idmod)], idmod[1]:(idmod[idpeak[1] - 
                  1]))
            }
            arc <- 1
            Smax <- 0
            count <- 1
            for (i in 1:(length(xdata))) {
                if (idmod[i] == idpeak[arc\%\%length(idpeak) + 
                  1]) {
                  arc <- arc\%\%length(idpeak) + 1
                  Smax <- 0
                  count <- count + 1
                }
                if (arc != length(idpeak)) {
                  Si <- (ydata[idpeak[arc + 1]] - ydata[idpeak[arc]])/(xdata[idpeak[arc + 
                    1]] - xdata[idpeak[arc]]) * (xdata[idmod[i]] - 
                    xdata[idpeak[arc]]) + ydata[idpeak[arc]] - 
                    ydata[idmod[i]]
                }
                else {
                  if (idmod[i] >= idpeak[arc]) {
                    Si <- (ydata[idpeak[1]] - ydata[idpeak[arc]])/(xdata[idpeak[1]] + 
                      2 * pi - xdata[idpeak[arc]]) * (xdata[idmod[i]] - 
                      xdata[idpeak[arc]]) + ydata[idpeak[arc]] - 
                      ydata[idmod[i]]
                  }
                  else {
                    Si <- (ydata[idpeak[1]] - ydata[idpeak[arc]])/(xdata[idpeak[1]] + 
                      2 * pi - xdata[idpeak[arc]]) * (xdata[idmod[i]] + 
                      2 * pi - xdata[idpeak[arc]]) + ydata[idpeak[arc]] - 
                      ydata[idmod[i]]
                  }
                }
                if (abs(Si) > Smax) {
                  Smax <- abs(Si)
                  S[count] <- Smax
                  idvec[count] <- idmod[i]
                }
            }
            if (plotgraph == T) {
                plot(xdata, ydata - mean(ydata), type = "l")
                lines(xdata[sort(idpeak)], ydata[sort(idpeak)] - 
                  mean(ydata), col = "blue")
                points(xdata[idpeak], ydata[idpeak] - mean(ydata), 
                  col = "blue")
                windows(6, 6)
                plot(xdata[idvec], S[idmod[idvec]], col = "red")
            }
            output <- list(S = S, idvec = idvec)
            return(output)
        }
        else {
            return("Error: Wrong Analysis type (1 or 2)")
        }
    }
    else {
        return("Error: Input vectors must be of the same lengths")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
