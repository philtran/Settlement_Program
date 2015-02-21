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

# Grabbing Data

data <- read.csv("TK 1002 Settlement.csv",header=T)
Distance <- data[["Distance"]][!is.na(data[["Distance"]])]
N <- length(data[["X0"]])
theta <- seq(0,2*pi*(N-1)/N,2*pi/N)
D <- 2*Distance[length(Distance)]
open3d(windowRect=c(50,50,700,700))
for (i in 1:length(Distance)) {
  z <- data[[paste0("X",Distance[i])]]
  x <- (D/2-Distance[i])*cos(theta)
  y <- (D/2-Distance[i])*sin(theta)
  plot3d(c(x,x[1]),c(y,y[1]),c(z,z[1]),type="l",add=T,col="blue")
}
# zmatrix <- matrix(nrow=N,ncol=length(Distance))
# for (i in 1:N) {
#   for (j in 1:length(Distance)) {
#     zmatrix[i,j] <- data[[paste0("X",Distance[j])]][i]
#   }
# }

#-------------------------------------
Rlist <- c(3,5)
m <- 0.4
# Rlist <- 1:4
# m <- 0

source("edgesettlementanalysis.R")
for (R in Rlist) {
  edgesettlementanalysis(data[["X0"]],data[[paste0("X",R)]],R=R,D=D,orientation=30,slope=m)
}


source("edgesettlementplot.R")
edgesettlementplot(data,c(1,4,6))