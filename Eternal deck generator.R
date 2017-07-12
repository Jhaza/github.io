library(stats)



##Reads in the decklist file.

cardlist <- read.csv("Card Data.csv",header=TRUE, stringsAsFactors = FALSE)
cardrefs <- read.csv("Eternal set numbers.csv", header=TRUE,stringsAsFactors = FALSE)
tmp1 <- rep("(", dim(cardrefs)[1])
tmp2 <- rep("#", dim(cardrefs)[1])
tmp3 <- rep(")", dim(cardrefs)[1])
cardrefs[,2] <- paste(tmp1, cardrefs[,2], sep="")
cardrefs[,3] <- paste(tmp2, cardrefs[,3], tmp3, sep="")
##Reads in the card data file. Rename "Card Data.csv" above as appropriate. 
isToken <- cardlist$Rarity == "Token"
cardlist <- cardlist[!isToken,]

tmp <- cardlist[-c(28,30,32,34,36),]
cpool <- rbind(tmp, tmp, tmp, tmp)
powers <- cardlist[c(1,2,3,4,5),]
powers <- rbind(powers, powers, powers, powers)
cpool <- rbind(cpool, powers, powers)
cpool <- cpool[order(cpool$Index),][,-ncol(cpool)]

#not_valid <- as.logical(1)
#while(not_valid){
  nPow <- 25 + rbinom(1,25,.20)
  drawnP <- sample(156, size=nPow, replace = FALSE)
  drawnNP <- sample(c(157:1936), size=75-nPow, replace=FALSE)
  drawn <- c(drawnP, drawnNP)
  tmp <- cpool[drawn,]
  deck <- as.data.frame(table(tmp[,4]), stringsAsFactors=FALSE)
##  isPow <- tmp$Type=="Power"
##  hasPow <- as.data.frame(table(rbind(tmp[isPow,4],tmp[isPow,5])))
##  names(hasPow) <- c("Color", "Count")
##  not_valid <- FALSE
##  for (i in c(1:75)){
##    if (!isPow[i]){
##      if(not_valid == FALSE & (hasPow[which(hasPow$Color==tmp[i,4]),2] < tmp[i,6] | hasPow[which(hasPow$Color==tmp[i,5]),2] < tmp[i,7])) {
##       not_valid <- TRUE 
##      }
##    }
##  }
#}

refInd <- list()
for(i in c(1:dim(deck)[1])){
  refInd[i] <- which(cardrefs$Name==deck[i,1])
}
refInd <- unlist(refInd)

outfile <- as.data.frame(cbind(deck[,2], deck[,1],cardrefs[refInd,2],cardrefs[refInd,3]))

outfile <- paste(outfile[,1], outfile[,2], outfile[,3], outfile[,4], sep = " ")

cat(outfile, sep="\n")



