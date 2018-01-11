
# Pvec if the vector of probabilities predicted by your model
# Cvec is the vector of 0's and 1's indicating the realized
# classifications of each observation.


ROCPlot <- function(Pvec,Cvec,Plot=T,Add=F) {
  NHam <- sum(Cvec==0)
  NSpam <- sum(Cvec==1)
  PvecS <- unique(sort(Pvec))
  x <- rep(NA,length(PvecS))
  y <- rep(NA,length(PvecS))
  for(i in 1:length(PvecS)) {
    x[i] <- sum(Pvec>=PvecS[i]&Cvec==0)/NHam
    y[i] <- sum(Pvec>=PvecS[i]&Cvec==1)/NSpam
  }
  x <- c(0,x,1)
  y <- c(0,y,1)
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  AUC <- sum((x[2:length(x)]-x[1:(length(x)-1)])*(y[2:length(y)]+y[1:(length(y)-1)])/2)
  
  if(Add) {
    plot(x,y,type="l",xlim=c(0,1),ylim=c(0,1),xlab="P( classified + | Is - )",ylab="P( classified + | Is + )")
    title("ROC Curve")
    mtext(paste("AUC =",round(AUC,3)),side=3,line=0.5)
    abline(0,1)
    par(pty="m")
  } else {
    if(Plot) {
      par(pty="s")
      plot(x,y,type="l",xlim=c(0,1),ylim=c(0,1),xlab="P( classified + | Is - )",ylab="P( classified + | Is + )")
      title("ROC Curve")
      mtext(paste("AUC =",round(AUC,3)),side=3,line=0.5)
      abline(0,1)
      par(pty="m")
    }
  }
  
  
  invisible(list(x=c(0,x,1),y=c(0,y,1),AUC=AUC))
}
