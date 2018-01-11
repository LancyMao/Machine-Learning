
DataOrig <- read.table("spambasedata-Orig.csv",sep=",",header=T,
                       stringsAsFactors=F)
DataCoded <- read.table("spambasedata-Coded.csv",sep=",",header=T,
                       stringsAsFactors=F)

# ord <- sample(nrow(DataCoded))
# save(ord,file="SpamdataPermutation.RData")
load(file="SpamdataPermutation.RData")
DataCoded <- DataCoded[ord,]

# Doing a 60-40 split
TrainInd <- ceiling(nrow(DataCoded)*0.6)
TrainData <- DataCoded[1:TrainInd,]
ValData <- DataCoded[(TrainInd+1):nrow(DataCoded),]

PSpam <- mean(TrainData$IsSpam)

# First Look at two variables credit and free

wh <- TrainData[["IsSpam"]]==1
whVars <- c("freq.credit","freq.free","freq.money")
table(TrainData[wh,whVars])

tmp <- table(TrainData[wh,whVars])
PFeatGivSpam <- tmp/sum(tmp)

tmp <- table(TrainData[!wh,whVars])
PFeatGivNotSpam <- tmp/sum(tmp)

# Look at counts for 3 variables credit, free, and money

whVars <- c("freq.credit","freq.free","freq.money")

table(TrainData[wh,whVars])

# Now look at the general problem

fn <- function(x) {
  Probs <- table(x)
  Probs <- Probs/sum(Probs)
  return(Probs)
}
PGivenSpam <- lapply(TrainData[TrainData$IsSpam==1,],FUN=fn)
PGivenHam <- lapply(TrainData[TrainData$IsSpam!=1,],FUN=fn)

fn <- function(DataRow,PGivenSpam,PGivenHam,PSpam) {
  tmp1 <- 1.0
  tmp2 <- 1.0
  for(x in names(DataRow)) {
    tmp1 <- tmp1*PGivenSpam[[x]][DataRow[x]]
    tmp2 <- tmp2*PGivenHam[[x]][DataRow[x]]
  }
  out <- tmp1*PSpam/(tmp1*PSpam+tmp2*(1-PSpam))
  return(out)
}

whVars <- colnames(TrainData)[c(7,8,9,16,20,24)]
whVars <- colnames(TrainData)[c(1,7,8,9,16,20,24,25)]
#whVars <- colnames(TrainData)[16]
PSpam <- apply(ValData[,whVars,drop=F],1,FUN=fn,PGivenSpam,PGivenHam,PSpam)
hist(PSpam)

source("ROCPlot.r")
ROCPlot(PSpam,ValData[,"IsSpam"])

