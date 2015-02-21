edgesettlementplot <- function(data,station) {
  # Creates Edge Settlement Plots
  windows(8,6)
  legendtxt <- vector("character")
  for (i in 1:length(station)) {
    x <- data[["Distance"]]
    y <- vector("numeric")
    for (j in 1:length(x)) {
      y[j] <- data[[paste0("X",x[j])]][station[i]]
    }
    if (i == 1) {
      plot(x,y,ann=F,type="o",col=i)
      title(main="Settlement Depth vs Distance from Shell",xlab="Distance from shell (ft)",ylab="Settlement Depth (in)")
    } else {
      lines(x,y,col=i)
      points(x,y,col=i)
    }
    legendtxt[i] <- paste0("Station ",station[i])
  }
  legend("bottomright",legendtxt,lty=rep(1,length(station)),pch=rep(1,length(station)),col=1:length(station))
}