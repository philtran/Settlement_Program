maxedgesettlement <-
function(R,D,orientation="parallel") {
  # Function that determines the maximum allowable edge settlement based upon API 653 Appendix B.3.4
  # Inputs: 
  #   R - Radius of settled area, ft
  #   D - Diameter of tank, ft
  #   orientation - Orientation of welds to shell
  #     Options:
  #       -"parallel"
  #       -"perpendicular"
  #       - angle (double), degrees
  # Output:
  #   Bmax - Maximum allowed edge settlement, inches
  
  # Checking orientation to determine if angle or character string
  if (typeof(orientation)=="character" && length(orientation)==1) {
    if (tolower(orientation)=="parallel") {
      alpha <- 90
    } else if (tolower(orientation)=="perpendicular") {
      alpha <- 0
    } else {
      cat("Error! Possible Options for orientation: \"parallel\",\"perpendicular\" or an angle (double)\n")
      return()
    }
  } else if (typeof(orientation)=="double" && length(orientation)==1) {
    alpha <- orientation
  } else {
    cat("Error! Possible Options for orientation: \"parallel\",\"perpendicular\" or an angle (double)\n")
    return()
  }
  
  # Checking Edge Cases
  if (typeof(R) == "integer") {
    R <- as.double(R)
  }
  if (typeof(R) != "double" || length(R) != 1) {
    cat("Error!  R must be a single value of type \"double\"\n")
    return()
  } else {
    if (R <=0 || R >8) {
      cat("Error!  R - out of range (0,8]\n")
      return()
    }
  }
  if (typeof(D) == "integer") {
    D <- as.double(D)
  }
  if (typeof(D) != "double" || length(D) != 1) {
    cat("Error!  D must be a single value of type \"double\"\n")
    return()
  } else {
    if (D<25 || D >160) {
      cat("Error!  D - out of range [25,160]\n")
      return()
    }
  }
  
  # Creating Sub-function to determine Be (Perpendicular)
  (findBe <- function(R,D) {
    if (D==20) {
      R1 <- 3.4
      R2 <- 3.4
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 3
    } else if (D==25) {
      R1 <- 3
      R2 <- 3
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 3.33
    } else if (D==30) {
      R1 <- 2.6
      R2 <- 2.6
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 3.65
    } else if (D==35) {
      R1 <- 2.25
      R2 <- 2.25
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 3.93
    } else if (D==40) {
      R1 <- 1.9
      R2 <- 1.9
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 4.2
    } else if (D==45) {
      R1 <- 1.8
      R2 <- 1.8
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 4.45
    } else if (D==50) {
      R1 <- 1.7
      R2 <- 1.7
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 4.7
    } else if (D==55) {
      R1 <- 1.6
      R2 <- 1.6
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 4.93
    } else if (D==60) {
      R1 <- 1.5
      R2 <- 1.5
      R3 <- 6
      Be1 <- 2
      Be2 <- 2
      Be3 <- 5.15
    } else if (D==65) {
      R1 <- 1.5
      R2 <- 1.6
      R3 <- 6
      Be1 <- 2
      Be2 <- 2.23
      Be3 <- 5.36
    } else if (D==70) {
      R1 <- 1.5
      R2 <- 1.7
      R3 <- 6
      Be1 <- 2
      Be2 <- 2.45
      Be3 <- 5.58
    } else if (D==75) {
      R1 <- 1.5
      R2 <- 1.8
      R3 <- 6
      Be1 <- 2
      Be2 <- 2.68
      Be3 <- 5.79
    } else if (D==80) {
      R1 <- 1.5
      R2 <- 1.9
      R3 <- 6
      Be1 <- 2
      Be2 <- 2.9
      Be3 <- 6
    } else if (D==85) {
      R1 <- 1.5
      R2 <- 2
      R3 <- 6
      Be1 <- 2
      Be2 <- 3.05
      Be3 <- 6.18
    } else if (D==90) {
      R1 <- 1.5
      R2 <- 2.1
      R3 <- 6
      Be1 <- 2
      Be2 <- 3.2
      Be3 <- 6.35
    } else if (D==95) {
      R1 <- 1.5
      R2 <- 2.2
      R3 <- 6
      Be1 <- 2
      Be2 <- 3.35
      Be3 <- 6.53
    } else if (D==100) {
      R1 <- 1.5
      R2 <- 2.3
      R3 <- 6
      Be1 <- 2
      Be2 <- 3.5
      Be3 <- 6.7
    } else if (D==105) {
      R1 <- 1.5
      R2 <- 2.37
      R3 <- 6
      Be1 <- 2
      Be2 <- 3.7
      Be3 <- 6.84
    } else if (D==110) {
      R1 <- 1.5
      R2 <- 2.45
      R3 <- 6
      Be1 <- 2
      Be2 <- 3.9
      Be3 <- 6.98
    } else if (D==115) {
      R1 <- 1.5
      R2 <- 2.53
      R3 <- 6
      Be1 <- 2
      Be2 <- 4.1
      Be3 <- 7.11
    } else if (D==120) {
      R1 <- 1.5
      R2 <- 2.6
      R3 <- 6
      Be1 <- 2
      Be2 <- 4.3
      Be3 <- 7.25
    } else if (D==125) {
      R1 <- 1.5
      R2 <- 2.73
      R3 <- 6
      Be1 <- 2
      Be2 <- 4.5
      Be3 <- 7.44
    } else if (D==130) {
      R1 <- 1.5
      R2 <- 2.85
      R3 <- 6
      Be1 <- 2
      Be2 <- 4.7
      Be3 <- 7.63
    } else if (D==135) {
      R1 <- 1.5
      R2 <- 2.98
      R3 <- 6
      Be1 <- 2
      Be2 <- 4.9
      Be3 <- 7.81
    } else if (D==140) {
      R1 <- 1.5
      R2 <- 3.1
      R3 <- 6
      Be1 <- 2
      Be2 <- 5.1
      Be3 <- 8
    } else if (D==145) {
      R1 <- 1.5
      R2 <- 3.15
      R3 <- 5.85
      Be1 <- 2
      Be2 <- 5.3
      Be3 <- 8
    } else if (D==150) {
      R1 <- 1.5
      R2 <- 3.2
      R3 <- 5.7
      Be1 <- 2
      Be2 <- 5.5
      Be3 <- 8
    } else if (D==155) {
      R1 <- 1.5
      R2 <- 3.3
      R3 <- 5.6
      Be1 <- 2
      Be2 <- 5.7
      Be3 <- 8
    } else if (D==160) {
      R1 <- 1.5
      R2 <- 3.4
      R3 <- 5.5
      Be1 <- 2
      Be2 <- 5.9
      Be3 <- 8
    } else {
      R1 <- 0
    }
    
#     if (R <= R1) {
#       return (Be1)
#     } else if (R > R1 && R <= R2) {
#       return ((Be2-Be1)/(R2-R1)*R + (Be1-(Be2-Be1)/(R2-R1)*R1))
#     } else if (R > R2 && R <= R3) {
#       return ((Be3-Be2)/(R3-R2)*R + (Be2-(Be3-Be2)/(R3-R2)*R2))
#     } else {
#       return (Be3)
#     }
    
    if (R1 != 0) {
      if (D > 40 && D < 140) {
        Z <- 1
      } else {
        Z <- 0
      }
      if ((R-R2)>((R3-R2)/2)) {
        X <- ((R3-R)/(R3-R2))*0.2*Z
      } else {
        X <- ((R-R2)/(R3-R2))*0.2*Z
      }
      if (R <= R1) {
        return (Be1)
      } else if (R>R1 && R<= R2) {
        return((R2-R)/(R2-R1)*(Be1-Be2)+Be2)
      } else if (R>R2 && R<= R3) {
        return((R3-R)/(R3-R2)*(Be2-Be3)+(Be3+X))
      } else {
        return (Be3)
      }
    } else {
      D1 <- D-D%%5
      D2 <- D1+5
      Bmax1 <- findBe(R,D1)
      Bmax2 <- findBe(R,D2)
      return((Bmax2-Bmax1)/(D2-D1)*D+(Bmax1-(Bmax2-Bmax1)/(D2-D1)*D1))
    }
  })
  
  
  # Creating Sub-function to determine Bew (Parallel)
  (findBew <- function(R,D) {
    if (D==20) {
      R1 <- 4.5
      R2 <- 4.5
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 2.47
    } else if (D==25) {
      R1 <- 3.93
      R2 <- 3.93
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 2.73
    } else if (D==30) {
      R1 <- 3.36
      R2 <- 3.36
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 3.03
    } else if (D==35) {
      R1 <- 3
      R2 <- 3
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 3.23
    } else if (D==40) {
      R1 <- 2.6
      R2 <- 2.6
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 3.45
    } else if (D==45) {
      R1 <- 2.5
      R2 <- 2.5
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 3.65
    } else if (D==50) {
      R1 <- 2.22
      R2 <- 2.22
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 3.9
    } else if (D==55) {
      R1 <- 2.17
      R2 <- 2.17
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 4.05
    } else if (D==60) {
      R1 <- 1.9
      R2 <- 1.9
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 4.3
    } else if (D==65) {
      R1 <- 1.9
      R2 <- 1.9
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 4.38
    } else if (D==70) {
      R1 <- 1.77
      R2 <- 1.77
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 4.55
    } else if (D==75) {
      R1 <- 1.63
      R2 <- 1.63
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 4.72
    } else if (D==80) {
      R1 <- 1.5
      R2 <- 1.5
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2
      Bew3 <- 4.9
    } else if (D==85) {
      R1 <- 1.5
      R2 <- 1.55
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.13
      Bew3 <- 5.05
    } else if (D==90) {
      R1 <- 1.5
      R2 <- 1.6
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.25
      Bew3 <- 5.2
    } else if (D==95) {
      R1 <- 1.5
      R2 <- 1.65
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.38
      Bew3 <- 5.35
    } else if (D==100) {
      R1 <- 1.5
      R2 <- 1.7
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.5
      Bew3 <- 5.45
    } else if (D==105) {
      R1 <- 1.5
      R2 <- 1.76
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.6
      Bew3 <- 5.63
    } else if (D==110) {
      R1 <- 1.5
      R2 <- 1.83
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.7
      Bew3 <- 5.75
    } else if (D==115) {
      R1 <- 1.5
      R2 <- 1.89
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.8
      Bew3 <- 5.88
    } else if (D==120) {
      R1 <- 1.5
      R2 <- 1.95
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 2.9
      Bew3 <- 6
    } else if (D==125) {
      R1 <- 1.5
      R2 <- 2.02
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.03
      Bew3 <- 6.11
    } else if (D==130) {
      R1 <- 1.5
      R2 <- 2.09
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.17
      Bew3 <- 6.23
    } else if (D==135) {
      R1 <- 1.5
      R2 <- 2.16
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.3
      Bew3 <- 6.34
    } else if (D==140) {
      R1 <- 1.5
      R2 <- 2.23
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.43
      Bew3 <- 6.45
    } else if (D==145) {
      R1 <- 1.5
      R2 <- 2.29
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.57
      Bew3 <- 6.56
    } else if (D==150) {
      R1 <- 1.5
      R2 <- 2.36
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.7
      Bew3 <- 6.68
    } else if (D==155) {
      R1 <- 1.5
      R2 <- 2.43
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.83
      Bew3 <- 6.79
    } else if (D==160) {
      R1 <- 1.5
      R2 <- 2.5
      R3 <- 6
      Bew1 <- 2
      Bew2 <- 3.95
      Bew3 <- 6.9
    } else {
      R1 <- 0
    }
    
#     if (R <= R1) {
#       return (Bew1)
#     } else if (R > R1 && R <= R2) {
#       return ((Bew2-Bew1)/(R2-R1)*R + (Bew1-(Bew2-Bew1)/(R2-R1)*R1))
#     } else if (R > R2 && R <= R3) {
#       return ((Bew3-Bew2)/(R3-R2)*R + (Bew2-(Bew3-Bew2)/(R3-R2)*R2))
#     } else {
#       return (Bew3)
#     }
    if (R1 != 0) {
      if (D > 35) {
        Z <- 1
      } else {
        Z <- 0
      }
      if ((R-R2)>((R3-R2)/2)) {
        X <- ((R3-R)/(R3-R2))*0.2*Z
      } else {
        X <- ((R-R2)/(R3-R2))*0.2*Z
      }
      if (R <= R1) {
        return (Bew1)
      } else if (R>R1 && R<= R2) {
        return((R2-R)/(R2-R1)*(Bew1-Bew2)+Bew2)
      } else if (R>R2 && R<= R3) {
        return((R3-R)/(R3-R2)*(Bew2-Bew3)+(Bew3+X))
      } else {
        return(Bew3)
      }
    } else {
      D1 <- D-D%%5
      D2 <- D1+5
      Bmax1 <- findBew(R,D1)
      Bmax2 <- findBew(R,D2)
      return((Bmax2-Bmax1)/(D2-D1)*D+(Bmax1-(Bmax2-Bmax1)/(D2-D1)*D1))
    }
  })
  
  # Computing Max Edge Settlement, Bmax
  Be <- findBe(R,D)
  Bew <- findBew(R,D)
  Bmax <- Be-(Be-Bew)*sin(alpha*pi/180)
  return(Bmax)
  
}
