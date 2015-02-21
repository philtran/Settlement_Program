library(rgl)
library(plotrix)

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
# Data must be in a .csv file; the file must have three columns, each with the following headings:
# "Elevation" - corresponding to measured elevation points at even intervals around the tank
# "H" - Corresponding to the height of the tank
# "D" - Corresponding to the diameter of the tank
# "Y" - Corresponding to the Yield Stress of the Shell Material
# "E" - Corresponding to the Young's Modulus of the Shell Material
# NOTE: All units must be consistent (of the same units specified below)

data <- read.csv("./Enbridge Kerrobert/Terminal_Tank52_Data.csv",header=T)
# data <- read.csv("./Tank107_Data.csv",header=T)
# data <- read.csv("./72281-150-101r.csv",header=T)
z <- data[["Elevation"]]              # Units of FT
H <- data[["H"]][!is.na(data[["H"]])] # Units of FT
D <- data[["D"]][!is.na(data[["D"]])] # Units of FT
Y <- data[["Y"]][!is.na(data[["Y"]])] # Units of PSI
E <- data[["E"]][!is.na(data[["E"]])] # Units of PSI
N <- length(z)
theta <- seq(0,2*pi*(N-1)/N,2*pi/N)

# Performing Cosine Curve fit for Settlement data
source("cosinefit.R")
############
nmode <- 2
############
y1 <- cosinefit(theta,z,theta) # Cosine Curve with 1 Mode
y2 <- cosinefit(theta,z,theta,nmode=nmode) # Cosine Curve with 2 Modes
thinterp <- seq(theta[1],theta[length(theta)],length=100)
y1interp <- cosinefit(theta,z,xinterp=thinterp)
y2interp <- cosinefit(theta,z,xinterp=thinterp,nmode=nmode)

# Peforming Out-of-Roundness Calculations
source("outofroundness.R")
disp <- outofroundness(elevation=z,H=H,D=D)

# Performing Settlement Analysis
source("settlementanalysis.R")
output1 <- settlementanalysis(y1,z,D,analysis=1,short=T) # Settlement for 1 MOde
output2 <- settlementanalysis(y2,z,D,analysis=1,short=T) # Settlement for 2 Modes
# output1 <- settlementanalysis(theta,z,analysis=2,plotgraph=T)
# output2 <- settlementanalysis(theta,z,analysis=2,plotgraph=T)
S1 <- output1$S
S2 <- output2$S

# Permissible Differential Settlement
# Smax,ft = (L^2*Y*11)/(2*E*H) (from API 653 Appendix B, B.3.2.1)
# Where L - arc length between measurement pts (ft)
# Y - Yield strength of shell material (psi)
# E - Young's Modulus (psi)
# H - Tank Height (ft)
L <- (2*pi/(length(S1)))*D/2
Smax <- (L^2*Y*11)/(2*E*H)

# Comparing Smax with our settlement values
if (max(abs(S1)) > Smax) {
  print("1 Mode Model Exceeded Maximum!")
} else {
  print("1 Mode Model within Bounds")
}
if (max(abs(S2)) > Smax) {
  print(paste0(as.character(nmode)," Modes Model Exceeded Maximum!"))
} else {
  print(paste0(as.character(nmode)," Modes Model within Bounds"))
}


# R^2 stuff
Syy <- sum((z-mean(z))^2)
SSE1 <- sum((z-y1)^2)
SSE2 <- sum((z-y2)^2)
R21 <- (Syy-SSE1)/Syy
R22 <- (Syy-SSE2)/Syy


# Plotting Allowable Settlement
windows(6,6)
idvec1 <- output1$idvec
plot(idvec1,12*abs(S1),ann=F,xlim=c(0,N),ylim=c(0,1.2*12*max(c(abs(S1),Smax))))
points((idvec1)[abs(S1)<=Smax],12*(abs(S1))[abs(S1)<=Smax],col="blue")
points((idvec1)[abs(S1)>Smax],12*(abs(S1))[abs(S1)>Smax],col="red")
lines(1:N,12*Smax*rep(1,N),col="red",lty=2,lwd=2)
title(main="Allowable Settlement for S1",xlab="Station No.",ylab="Settlement (in)")
legend("topleft",c("Allowable Settlement","Passed","Failed"),col=c("red","blue","red"),lty=c(2,NA,NA),pch=c(NA,1,1),lwd=c(2,NA,NA))
text(N-2,1.05*(Smax*12),paste0("Smax=",format(round(Smax*12,2),nsmall=2)," inches"))
text(which(abs(S1)==max(abs(S1)))[1],0.95*max(abs(S1*12)),paste0("Si=",format(round(max(abs(S1*12)),2),nsmall=2)," inches"))

windows(6,6)
idvec2 <- output2$idvec
plot(idvec1,12*abs(S2),ann=F,xlim=c(0,N),ylim=c(0,1.2*12*max(c(abs(S2),Smax))))
points((idvec2)[abs(S2)<=Smax],12*(abs(S2))[abs(S2)<=Smax],col="blue")
points((idvec2)[abs(S2)>Smax],12*(abs(S2))[abs(S2)>Smax],col="red")
lines(1:N,12*Smax*rep(1,N),col="red",lty=2,lwd=2)
title(main=paste0("Allowable Settlement for S",as.character(nmode)),xlab="Station No.",ylab="Settlement (in)")
legend("topleft",c("Allowable Settlement","Passed","Failed"),col=c("red","blue","red"),lty=c(2,NA,NA),pch=c(NA,1,1),lwd=c(2,NA,NA))
text(N-2,1.05*(Smax*12),paste0("Smax=",format(round(Smax*12,2),nsmall=2)," inches"))
text(which(abs(S2)==max(abs(S2)))[1],0.95*max(abs(S2*12)),paste0("Si=",format(round(max(abs(S2*12)),2),nsmall=2)," inches"))


# Plotting Cosine Curve Interpolation
windows(9,6)
plot(theta*180/pi,z,type="p",col="black",ann=F)
lines(thinterp*180/pi,y1interp,col="red")
lines(thinterp*180/pi,y2interp,col="blue")
points(theta[which(abs(S1)>Smax)]*180/pi,z[which(abs(S1)>Smax)],pch=21,col="red",bg="red")
legend("topright",c(as.expression("Original"),as.expression(bquote("1 Mode," ~ R^2*"="*.(format(round(R21,2),nsmall=2)))),as.expression(bquote(.(as.character(nmode))*" Modes,"~R^2*"="*.(format(round(R22,2),nsmall=2))))),col=c("black","red","blue"),lty=c(NA,1,1),pch=c(1,NA,NA),cex=0.7)
title("Cosine Curve Fitting of Settlement Data",xlab="Angle (Degrees)",ylab="Elevation (ft)")


# Doing Error-band stuff
if (max(abs(S1)) > Smax) {
  ###############################
  err1 <- 3/8 #inches
  ###############################
  znew1 <- vector("numeric",length=length(z))
  for (i in 1:length(z)) {
    if (z[i]*12 < (y1[i]-err1)) {
      znew1[i] <- z[i] + err1/12
    } else if (z[i]*12 > (y1[i]*12+err1)) {
      znew1[i] <- z[i] - err1/12
    } else {
      znew1[i] <- z[i]
    }
  }
  ynew1 <- cosinefit(theta,znew1,theta)
  outputnew1 <- settlementanalysis(ynew1,znew1,D,analysis=1,short=T)
  Snew1 <- outputnew1$S
  # Comparing Smax with our settlement values
  if (max(abs(Snew1)) > Smax) {
    print("New 1 Mode Model Exceeded Maximum!")
  } else {
    print("New 1 Mode Model within Bounds")
  }
  windows(6,6)
  par(mfrow=c(2,1))
  plot(theta*180/pi,z*12,type="p",col="blue",ann=F)
  lines(theta*180/pi,y1*12,lty=2,col="green")
  lines(theta*180/pi,y1*12+err1,col="black")
  lines(theta*180/pi,y1*12-err1,col="black")
  title(main=paste0("1 Mode: Original, ",err1, "\" tol."),xlab="Angle (Degrees)",ylab="Elevation (in)")
  plot(theta*180/pi,znew1*12,type="p",col="blue",ann=F)
  lines(theta*180/pi,ynew1*12,lty=2,col="green")
  lines(theta*180/pi,ynew1*12+err1,col="black")
  lines(theta*180/pi,ynew1*12-err1,col="black")
  title(main =paste0("1 Mode: Adjusted, ",err1, "\" tol."),xlab="Angle (Degrees)",ylab="Elevation (in)")
  
  windows(6,6)
  idvecnew1 <- outputnew1$idvec
  plot(idvecnew1,12*abs(Snew1),ann=F,xlim=c(0,N),ylim=c(0,1.2*12*max(c(abs(Snew1),Smax))))
  points((idvecnew1)[abs(Snew1)<=Smax],12*(abs(Snew1))[abs(Snew1)<=Smax],col="blue")
  points((idvecnew1)[abs(Snew1)>Smax],12*(abs(Snew1))[abs(Snew1)>Smax],col="red")
  lines(1:N,12*Smax*rep(1,N),col="red",lty=2,lwd=2)
  title(main="Allowable Settlement for S1: Adjusted",xlab="Station No.",ylab="Settlement (in)")
  legend("topleft",c("Allowable Settlement","Passed","Failed"),col=c("red","blue","red"),lty=c(2,NA,NA),pch=c(NA,1,1),lwd=c(2,NA,NA))
  text(N-2,1.05*(Smax*12),paste0("Smax=",format(round(Smax*12,2),nsmall=2)," inches"))
  text(which(abs(Snew1)==max(abs(Snew1)))[1],0.95*max(abs(Snew1*12)),paste0("Si=",format(round(max(abs(Snew1*12)),2),nsmall=2)," inches"))
  
}
if (max(abs(S2)) > Smax) {
  ###############################
  err2 <- 3/8 # inches
  ###############################
  znew2 <- vector("numeric",length=length(z))
  for (i in 1:length(z)) {
    if (z[i]*12 < (y2[i]-err2)) {
      znew2[i] <- z[i] + err2/12
    } else if (z[i]*12 > (y2[i]*12+err2)) {
      znew2[i] <- z[i] - err2/12
    } else {
      znew2[i] <- z[i]
    }
  }
  ynew2 <- cosinefit(theta,znew2,theta,nmode=nmode)
  outputnew2 <- settlementanalysis(ynew2,znew2,D,analysis=1,short=T)
  Snew2 <- outputnew2$S
  # Comparing Smax with our settlement values
  if (max(abs(Snew2)) > Smax) {
    print(paste0("New ",as.character(nmode)," Mode Model Exceeded Maximum!"))
  } else {
    print(paste0("New ",as.character(nmode)," Mode Model within Bounds"))
  }
  windows(6,6)
  par(mfrow=c(2,1))
  plot(theta*180/pi,z*12,type="p",col="blue",ann=F)
  lines(theta*180/pi,y2*12,lty=2,col="green")
  lines(theta*180/pi,y2*12+err2,col="black")
  lines(theta*180/pi,y2*12-err2,col="black")
  title(main=paste0(as.character(nmode)," Modes: Original, ",err2, "\" tol."),xlab="Angle (Degrees)",ylab="Elevation (in)")
  plot(theta*180/pi,znew2*12,type="p",col="blue",ann=F)
  lines(theta*180/pi,ynew2*12,lty=2,col="green")
  lines(theta*180/pi,ynew2*12+err2,col="black")
  lines(theta*180/pi,ynew2*12-err2,col="black")
  title(main=paste0(as.character(nmode)," Modes: Adjusted, ",err2, "\" tol."),xlab="Angle (Degrees)",ylab="Elevation (in)")
  
  windows(6,6)
  idvecnew2 <- outputnew2$idvec
  plot(idvecnew2,12*abs(Snew2),ann=F,xlim=c(0,N),ylim=c(0,1.2*12*max(c(abs(Snew2),Smax))))
  points((idvecnew2)[abs(Snew2)<=Smax],12*(abs(Snew2))[abs(Snew2)<=Smax],col="blue")
  points((idvecnew2)[abs(Snew2)>Smax],12*(abs(Snew2))[abs(Snew2)>Smax],col="red")
  lines(1:N,12*Smax*rep(1,N),col="red",lty=2,lwd=2)
  title(main="Allowable Settlement for S2: Adjusted",xlab="Station No.",ylab="Settlement (in)")
  legend("topleft",c("Allowable Settlement","Passed","Failed"),col=c("red","blue","red"),lty=c(2,NA,NA),pch=c(NA,1,1),lwd=c(2,NA,NA))
  text(N-2,1.05*(Smax*12),paste0("Smax=",format(round(Smax*12,2),nsmall=2)," inches"))
  text(which(abs(Snew2)==max(abs(Snew2)))[1],0.95*max(abs(Snew2*12)),paste0("Si=",format(round(max(abs(Snew2*12)),2),nsmall=2)," inches"))
}


# Plotting Out-of-Roundness Cross-Section
###########################
scale <- 10
##########################
windows(6,6)
par(pty="s")
linetype <- c(2,1)
linecolor <- c("black","green")
linewidth <- c(3,3)

disp2 <- spline(c(theta,2*pi+theta),c(disp,disp),n=10*length(theta))
plot((D/2+scale*disp2$y[which(disp2$x>=theta[length(theta)])])*cos(disp2$x[which(disp2$x>=theta[length(theta)])]),(D/2+scale*disp2$y[which(disp2$x>theta[length(theta)])])*sin(disp2$x[which(disp2$x>theta[length(theta)])]),type="l",ann=F,col=linecolor[2],lwd=linewidth[2],pty="s")

lines(D/2*cos(c(thinterp,thinterp[1])),D/2*sin(c(thinterp,thinterp[1])),col=linecolor[1],lty=linetype[1],lwd=linewidth[1])
title(main="Shell Out-of-Roundness Calculation")
legend(0.65*min(D/2*cos(theta)),0.65*max(D/2*sin(theta)),c("Original","Out-of-Round",paste0("scale=",as.character(scale),":1")),cex=0.7, pt.cex = 1,col=c(linecolor,NA),lty=c(linetype,NA),lwd=c(linewidth,NA))
points((D/2+scale*c(disp,disp[1]))*cos(c(theta,theta[1])),(D/2+scale*c(disp,disp[1]))*sin(c(theta,theta[1])))



# Plotting Bottom Shell
z2 <- spline(c(theta,2*pi+theta),c(z,z),n=10*length(theta));
xplot <- D/2*c(cos(theta),cos(theta[1]))
yplot <- D/2*c(sin(theta),sin(theta[1]))
# zplot <- c(z,z[1])
xplotz <- D/2*cos(z2$x[which(z2$x>=theta[length(theta)])])
yplotz <- D/2*sin(z2$x[which(z2$x>=theta[length(theta)])])
zplot <- z2$y[which(z2$x>=theta[length(theta)])]
xinterpplot <- D/2*c(cos(thinterp),cos(thinterp[1]))
yinterpplot <- D/2*c(sin(thinterp),sin(thinterp[1]))
y1plot <- c(y1interp,y1interp[1])
y2plot <- c(y2interp,y2interp[1])

open3d(windowRect=c(50,50,700,700))
#plot3d(xplot,yplot,zplot,type="l",col="black",xlab="",ylab="",zlab="")
plot3d(xplotz,yplotz,zplot,type="l",col="black",xlab="",ylab="",zlab="")
plot3d(xinterpplot,yinterpplot,y1plot,type="l",add=T,col="red",xlab="",ylab="",zlab="")
plot3d(xinterpplot,yinterpplot,y2plot,type="l",add=T,col="blue",xlab="",ylab="",zlab="")
decorate3d(zlab="Elevation (ft)")
legend3d("topright",c("Original","1 Mode",paste0(as.character(nmode)," Modes")),col=c("black","red","blue"),lty=c(1,1,1))

######################################################
# Use the following lines when you want to take a snapshot of the 3D plot
#  filename3d <- "3Dplot"
#  rgl.postscript(file=paste0("Enbridge Kerrobert/",filename3d,".pdf"),fmt="pdf")
#######################################################



# Plotting Settlement
# S1plot <- c(S1,S1[1])
# S2plot <- c(S2,S2[1])
# xplot2 <- c(xplot[output1$idvec],xplot[output1$idvec][1])
# yplot2 <- c(yplot[output1$idvec],yplot[output1$idvec][1])
# 
# open3d(windowRect=c(50,50,700,700))
# plot3d(xplot2,yplot2,S1plot,type="l",col="red",add=F)
# plot3d(xplot2,yplot2,S2plot,type="l",col="blue",add=T)
# text3d(x=0.7*min(xplot),y=0.3*min(yplot),z=c(1.4*min(c(S1plot,S2plot)),1.15*min(c(S1plot,S2plot))),c("1st Mode","2nd Mode"),col="black")
# lines3d(x=c(0.35*min(xplot),0.45*min(xplot)),y=0.3*min(yplot),z=1.4*min(c(S1plot,S2plot)),col="red")
# lines3d(x=c(0.35*min(xplot),0.45*min(xplot)),y=0.3*min(yplot),z=1.15*min(c(S1plot,S2plot)),col="blue")