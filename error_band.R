library(rgl)

# Settlement Analysis File
# Initial Set-up: Wipe/close everything
rm(list=ls())
cat("\14")
graphics.off()
for (i in rgl.dev.list()) {
  rgl.close()
}
path <- "C:/Users/Phil Tran/Dropbox/Work/PT-WorkSpace/R Files"
setwd(path)


data <- read.csv("./Enbridge Kerrobert/Terminal_Tank52_Data.csv",header=T)
z <- data[["Elevation"]]              # Units of FT
H <- data[["H"]][!is.na(data[["H"]])] # Units of FT
D <- data[["D"]][!is.na(data[["D"]])] # Units of FT
Y <- data[["Y"]][!is.na(data[["Y"]])] # Units of PSI
E <- data[["E"]][!is.na(data[["E"]])] # Units of PSI
N <- length(z)
theta <- seq(0,2*pi*(N-1)/N,2*pi/N)

source("cosinefit.R")
y <- cosinefit(theta,z,theta)
y <- y*12
err <- 3/8

windows(6,4)
plot(theta,z*12,type="p",col="blue")
lines(theta,y,lty=2,col="green")
lines(theta,y+err,col="black")
lines(theta,y-err,col="black")

znew <- vector("numeric",length=length(z))
for (i in 1:length(z)) {
  if (z[i]*12 < (y[i]-err)) {
    znew[i] <- z[i] + err/12
  } else if (z[i]*12 > (y[i]+err)) {
    znew[i] <- z[i] - err/12
  } else {
    znew[i] <- z[i]
  }
}

ynew <- cosinefit(theta,znew,theta)
windows(6,4)
plot(theta,znew*12,type="p",col="blue")
lines(theta,ynew*12,lty=2,col="green")
lines(theta,ynew*12+err,col="black")
lines(theta,ynew*12-err,col="black")

source("settlementanalysis.R")
output <- settlementanalysis(ynew,znew,D,analysis=1,short=T)
S <- output$S

L <- (2*pi/(length(S)))*D/2
Smax <- (L^2*Y*11)/(2*E*H)

# Comparing Smax with our settlement values
if (max(abs(S)) > Smax) {
  print("Model Exceeded Maximum!")
} else {
  print("Model within Bounds")
}