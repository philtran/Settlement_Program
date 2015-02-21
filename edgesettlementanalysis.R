source("maxedgesettlement.R")
edgesettlementanalysis <- function(edgedata,breakoverdata,R,D,orientation=90,slope=0) {
  # Given data, determines amount of edge settlement and compares with allowable edge settlement
  
  # Determining Edge Settlement
  B <- edgedata - breakoverdata + slope*R
  #Bew <- maxedgesettlement(R,D,"parallel")
  #Be <- maxedgesettlement(R,D,"perpendicular")
  # Determining Allowable Edge Settlement
  Bmax <- maxedgesettlement(R,D,orientation)
  
  
  # Checking if the Edge Settlement is within allowable and printing results
  print(paste0("For R=",R," ft, Bmax=",format(round(Bmax,2),nsmall=2)," inches, 0.75*Bmax =",format(round(0.75*Bmax,2),nsmall=2)," inches"))
  print("Station No.  |  B (inch)  |  Action")
  for (i in 1:length(B)) {
    # Creating Left Entries of Table of Results
    if (i < 10) {
      stationstr <- paste0("        0",i,"   |")
    } else {
      stationstr <- paste0("        ",i,"   |")
    }
    if (B[i] >= 0) {
      mainstr <- paste0(stationstr,"    ",format(round(B[i],2),nsmall=2),"    |")
    } else {
      mainstr <- paste0(stationstr,"   ",format(round(B[i],2),nsmall=2),"    |")
    }
      
    # Primary check to see if within Allowable Edge Settlement  
    if (B[i] > Bmax) {
      mainstr <- paste0(mainstr,"  B has exceeded Bmax!  Repairs required")
    } else if (B[i] <= Bmax && B[i] >= 0.75*Bmax) {
      mainstr <- paste0(mainstr,"  B is within 0.75*Bmax; Inspection required")
    } else {
      mainstr <- paste0(mainstr,"  B is within bounds")
    }
    print(mainstr)
  }
}