outofroundness <-
function(elevation,H,D) {
  #Performing basic Out-of-Roundness Calculations
  # Inputs:
  #   elevation - corresponding to measured elevation points at even intervals around the tank
  #   H - Corresponding to the height of the tank
  #   D - Corresponding to the diameter of the tank
  # NOTE: All units must be consistent (of the same units)
  
  # Performing Out-of-Roundness Calculation (Following Koczwara's example)
  # Source: "Simple method calculates tank shell distortion" by F.A. Koczwara
  N <- length(elevation)
  theta <- seq(0,2*pi*(N-1)/N,2*pi/N)
  K <- (H*N**2)/(D*19.739)
  A0 <- mean(elevation)
  A1 <- 2*mean((elevation-A0)*cos(theta))
  B1 <- 2*mean((elevation-A0)*sin(theta))
  dispTot <- K*(elevation-A0-A1*cos(theta)-B1*sin(theta))
  disp <- c(0,dispTot[1:(N-2)]-2*dispTot[2:(N-1)]+dispTot[3:N],0)
  disp[1] <- dispTot[N]-2*dispTot[1]+dispTot[2]
  disp[N] <- dispTot[N-1]-2*dispTot[N]+dispTot[1]
  return(disp)
}
