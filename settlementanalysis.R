settlementanalysis <- function(xdata,ydata,D,analysis=1,short=F,maxL=32,plotgraph=F) {
  # Performing a settlement analysis based upon API 653 Appendix B
  # Inputs:
  #   analysis - flag to determine which settlement analysis
  #     1 - Analysis with clear planar tilt (Default)
  #     2 - Analysis without clear planar tilt
  
  
  if (length(xdata)== length(ydata)) {
    if (analysis == 1) {
      # xdata -> y-interpolation for Cosine Curve
      # ydata -> z (elevation) data
      if (length(xdata) >= 3) {
        U <- xdata-ydata
        
        if (short == F) {
          S <- vector("numeric",length=length(U))
          S[2:(length(U)-1)] <- U[2:(length(U)-1)]-1/2*(U[1:(length(U)-2)]+U[3:length(U)]);
          S[1] <- U[1] - (1/2*U[length(U)] + 1/2*U[2])
          S[length(U)] <- U[length(U)] - (1/2*U[length(U)-1]+1/2*U[1])
          seq1 <- 1:length(U)
        } else {
          nsettle <- ceiling(pi*D/maxL);
          nstep <- max(1,floor(length(U)/nsettle))
          idmax <- which(abs(U)==max(abs(U)))[1]
          S <- vector("numeric",length=ceiling(length(U)/nstep))
          
          (enforce_cyclic <- function(x,L) {
            # Function to make sure data containts no "0's"
            for (i in 1:length(x)) {
              if (x[i] == 0) {x[i] <- L}
            }
            return(x)
          })
          
          seq1 <- sort(enforce_cyclic(seq(from=idmax,by=nstep,length.out=length(S))%%length(U),length(U)))
          seq2 <- enforce_cyclic((seq1-nstep)%%length(U),length(U))
          seq3 <- enforce_cyclic((seq1+nstep)%%length(U),length(U))
          S <- U[seq1] - 1/2*(U[seq2]+U[seq3])
        }
        output <- list("S"=S,"idvec"=seq1)
        return(output)
      } else {
        return("Error: Not enough data points")
      }
    } else if (analysis == 2) {
      # No clear Planar Tilt
      # xdata - Angle Values
      # ydata - Elevation Values
      
      # Finding Points where Elevation changes direction
      # Initializing Variables
      sgn = sign(ydata[2]-ydata[1])
      idextrema <- vector("numeric")
      id <- 1
      # Looping through each elevation point and storing points where elevation changes direction
      for (i in 2:(length(ydata)-1)) {
        if (sign(ydata[i+1]-ydata[i]) != sgn) {
            idextrema[id] <- i
            id <- id + 1
            sgn <- sign(ydata[i+1]-ydata[i])
        }
      }
      # Edge cases
      if (sign(ydata[length(1)]-ydata[length(ydata)]) != sgn) {
        idextrema[id] <- length(ydata)
        id <- id + 1
        sgn <- ydata[1]-ydata[length(ydata)]
      }
      if (sign(ydata[2]-ydata[1])) {
        idextrema[id] <- 1
      }
      
      # Finding peak values
      idpeak <- vector("numeric")
      id <- 1
      for (i in 2:(length(ydata[idextrema])-1)) {
        if (ydata[idextrema][i+1] < ydata[idextrema][i]) {
          idpeak[id] <- idextrema[i]
          id <- id + 1
        }
      }
      # Edge Cases
      if (ydata[idextrema][1] < ydata[idextrema][length(idextrema)]) {
        idpeak[id] <- idextrema[length(idextrema)]  
        id <- id+1
      }
      if (ydata[idextrema][2] < ydata[idextrema][1]) {
        idpeak[id] <- idextrema[1]
      }
      idpeak <- sort(idpeak)
      Sarc <- vector("numeric",length=length(idpeak)-1)
      for (i in 1:(length(idpeak)-1)) {
        Sarc[i] <- xdata[idpeak[i+1]]-xdata[idpeak[i]]
      }
      Sarc[length(idpeak)] <- xdata[idpeak[1]]+2*pi-xdata[idpeak[length(idpeak)]]
      
      S <- vector("numeric",length=length(Sarc))
      idvec <- vector("numeric",length=length(S))
      idmod <- 1:length(xdata)
      if (idpeak[1] > 1) {
        #arc <- length(idpeak)
        #xdatamod <- c(xdata[idpeak[1]:length(xdata)],xdata[1]:(xdata[idpeak[1]-1]))
        idmod <- c(idmod[idpeak[1]:length(idmod)],idmod[1]:(idmod[idpeak[1]-1]))
      } #else {
        #arc <- 1
        #xdatamod <- xdata
      #}
      arc <- 1
      Smax <- 0
      count <- 1
      # TO-DO: Change it so that it loops across starting from a single arc, rather than splitting the last arc
      #xdatamod <- c(xdata[idpeak[1]:length(xdata)],xdata[1]:(xdata[idpeak[1]-1]))
      
      
      for (i in 1:(length(xdata))) {
        #if (i==idpeak[arc%%length(idpeak)+1]) {
        if (idmod[i]==idpeak[arc%%length(idpeak)+1]) {
          arc <- arc%%length(idpeak)+1
          Smax <- 0
          count <- count+1
        }
#         if (arc != length(idpeak)) {
#           Si <- (ydata[arc+1]-ydata[arc])/(xdata[arc+1]-xdata[arc])*(xdata[i]-xdata[arc])+ydata[arc]-ydata[i]
#         } else {
#           if (i >= idpeak[arc]) {
#             Si <- (ydata[1]-ydata[arc])/(xdata[1]+2*pi-xdata[arc])*(xdata[i]-xdata[arc])+ydata[arc]-ydata[i]
#           } else {
#             Si <- (ydata[1]-ydata[arc])/(xdata[1]+2*pi-xdata[arc])*(xdata[i]+2*pi-xdata[arc])+ydata[arc]-ydata[i]
#           }
#         }
        if (arc != length(idpeak)) {
          Si <- (ydata[idpeak[arc+1]]-ydata[idpeak[arc]])/(xdata[idpeak[arc+1]]-xdata[idpeak[arc]])*(xdata[idmod[i]]-xdata[idpeak[arc]])+ydata[idpeak[arc]]-ydata[idmod[i]]
        } else {
          if (idmod[i] >= idpeak[arc]) {
            Si <- (ydata[idpeak[1]]-ydata[idpeak[arc]])/(xdata[idpeak[1]]+2*pi-xdata[idpeak[arc]])*(xdata[idmod[i]]-xdata[idpeak[arc]])+ydata[idpeak[arc]]-ydata[idmod[i]]
          } else {
            Si <- (ydata[idpeak[1]]-ydata[idpeak[arc]])/(xdata[idpeak[1]]+2*pi-xdata[idpeak[arc]])*(xdata[idmod[i]]+2*pi-xdata[idpeak[arc]])+ydata[idpeak[arc]]-ydata[idmod[i]]
          }
        }
        #print(Si)
        if (abs(Si) > Smax) {
          Smax <- abs(Si)
          S[count] <- Smax
          #idvec[count] <- i
          idvec[count] <- idmod[i]
        }
      }
      
      
#       
#       for (i in 1:(length(Sarc)-1)) {
#         xarc <- xdata[xdata>xdata[idpeak[i]] & xdata<xdata[idpeak[i+1]]]
#         yarc <- ydata[xdata>xdata[idpeak[i]] & xdata<xdata[idpeak[i+1]]]
#         S[i] <- max(abs((ydata[idpeak[i+1]]-ydata[idpeak[i]])/(xdata[idpeak[i+1]]-xdata[idpeak[i]])*(xarc-xdata[idpeak[i]])+ydata[idpeak[i]]-yarc))
#       }
#       #xarc <- xdata[xdata<xdata[idpeak[1]] | xdata>xdata[idpeak[length(idpeak)]]]
#       xarc <- c(xdata[xdata<xdata[idpeak[1]]]+2*pi,xdata[xdata>xdata[idpeak[length(idpeak)]]])
#       yarc <- ydata[xdata<xdata[idpeak[1]] | xdata>xdata[idpeak[length(idpeak)]]]
#       S[length(S)] <- max(abs((ydata[idpeak[1]]-ydata[idpeak[length(idpeak)]])/(xdata[idpeak[1]]+2*pi-xdata[idpeak[length(idpeak)]])*(xarc-xdata[idpeak[length(idpeak)]])+ydata[idpeak[length(idpeak)]]-yarc))
      


      #print(Si)
      if (plotgraph==T) {
        plot(xdata,ydata-mean(ydata),type="l")
        lines(xdata[sort(idpeak)],ydata[sort(idpeak)]-mean(ydata),col="blue")
        points(xdata[idpeak],ydata[idpeak]-mean(ydata),col="blue")
        windows(6,6)
        plot(xdata[idvec],S[idmod[idvec]],col="red")
      }
      #return(ydata[idextrema])
      
      output <- list("S"=S,"idvec"=idvec)
      return(output)
      
    } else {
      return("Error: Wrong Analysis type (1 or 2)")
    }
  } else {
    return("Error: Input vectors must be of the same lengths")
  }
}