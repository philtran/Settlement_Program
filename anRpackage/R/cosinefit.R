cosinefit <-
function(xdata,ydata,xinterp=seq(xdata[1],xdata[length(xdata)],length=100),nmode=1) {
  # Created a function that performs a cosine fit to any set of data
  # Inputs: xdata <- numeric vector of measured data on X-axis
  #         ydata <- numeric vector of measured data on Y-axis
  #         xinterp <- numeric vector of X-data to be interpolate
  #              (default: using x-data bounds with 100 elements)
  #         nmode <- integer of number of modes of cosine to use
  #               (default: 1)
  # Output: yinterp <- Interpolated Y-values corresponding to xinterp based upon the cosine fit
  
  # Initializing Parameters
  x <- list()
  y <- list()
  formulastr <- "ydata ~ "
  
  # Iterating through modes
  for (i in 1:nmode) {
    x[[i]] = cos(i*xdata)
    y[[i]] = sin(i*xdata)
    if (i != 1) {
      formulastr <- paste(formulastr,"+")
    }
    formulastr <- paste(formulastr,"x[[",as.character(i),"]] + y[[",as.character(i),"]]",sep="")
  }
  
  # Computing Linear Model
  mod <- do.call("lm",list(as.formula(formulastr)))
  
  # Extracting Coefficients from Linear Model
  b <- vector("numeric",length=2*nmode+1)
  for (i in 1:length(b)) {
    b[i] = mod$coefficients[[i]]
  }
  # Creating yinterpolation
  yinterp <- b[1]
  for (i in 1:nmode) {
    bcoef <- sqrt(b[2*i]^2 + b[2*i+1]^2)
    ccoef <- atan2(b[2*i+1],b[2*i])
    yinterp <- yinterp + bcoef*cos(i*xinterp-ccoef)
  }
  return(yinterp)
}
