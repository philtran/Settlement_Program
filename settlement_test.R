library(rgl)

# Settlement Analysis File
# Initial Set-up: Wipe/close everything
rm(list=ls())
cat("\14")
graphics.off()
for (i in rgl.dev.list()) {
  rgl.close()
}
#path <- "C:/Users/Phil Tran/Dropbox/Work/PT-WorkSpace/R Files"
#setwd(path)

# Grabbing Data
# Data must be in a .csv file; the file must have three columns, each with the following headings:
# "Elevation" - corresponding to measured elevation points at even intervals around the tank
# "H" - Corresponding to the height of the tank
# "D" - Corresponding to the diameter of the tank
# "Y" - Corresponding to the Yield Stress of the Shell Material
# "E" - Corresponding to the Young's Modulus of the Shell Material
# NOTE: All units must be consistent (of the same units specified below)

data <- read.csv("Tank1008_Data.csv",header=T)
z <- data[["Elevation"]]              # Units of FT
H <- data[["H"]][!is.na(data[["H"]])] # Units of FT
D <- data[["D"]][!is.na(data[["D"]])] # Units of FT
Y <- data[["Y"]][!is.na(data[["Y"]])] # Units of PSI
E <- data[["E"]][!is.na(data[["E"]])] # Units of PSI
N <- length(z)
theta <- seq(0,2*pi*(N-1)/N,2*pi/N)

# Performing Cosine Curve fit for Settlement data
source("cosinefit.R")
y1 = cosinefit(theta,z,theta) # Cosine Curve with 1 Mode
y2 = cosinefit(theta,z,theta,nmode=2) # Cosine Curve with 2 Modes

# Peforming Out-of-Roundness Calculations
source("outofroundness.R")
disp <- outofroundness(elevation=z,H=H,D=D)

# Performing Settlement Analysis
source("settlementanalysis.R")
output1 <- settlementanalysis(theta,z,D,analysis=2,short=T,plotgraph=F) # Settlement for 1 MOde
output2 <- settlementanalysis(theta,z,D,analysis=2,short=T) # Settlement for 2 Modes

S <- output1$S