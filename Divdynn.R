setwd("/Users/max/Desktop/Illustration")

install.packages("divDyn")

library(divDyn)

dat <- read.csv("pbdb_data_final.csv",  skip = 20, sep=";" )

dat <- dat[dat$family=="Cypraeidae",]

data(stages)
data(tens)

data(keys)

# the 'stg' entries (lookup)
stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)

# convert to numeric
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)

# empty container
dat$stg <- rep(NA, nrow(dat))

# select entries, where
stgCondition <- c(
  # the early and late interval fields indicate the same stg
  which(stgMax==stgMin),
  # or the late_intervar field is empty
  which(stgMax==-1))

# in these entries, use the stg indicated by the early_interval
dat$stg[stgCondition] <- stgMin[stgCondition] 

url <- "https://github.com/divDyn/ddPhanero/blob/master/doc/1.0.1/dd_phanero.pdf"
temp <- read.csv(file = url)


# a. categorize interval names to bin numbers
# categorize is the new function of the package
binMin<-categorize(dat[,"early_interval"],keys$binInt)
binMax<-categorize(dat[,"late_interval"],keys$binInt)
# convert to numeric
binMin<-as.numeric(binMin)
binMax<-as.numeric(binMax)
# b. resolve max-min interval uncertainty
# final variable (empty)
dat$bin <- rep(NA, nrow(dat))
# use entries, where
binCondition <- c(
  # the early and late interval fields indicate the same bin
  which(binMax==binMin),
  # or the late_interval field is empty
  which(binMax==-1))
# in these entries, use the bin indicated by the early_interval
dat$bin[binCondition] <- binMin[binCondition]

table(dat$stg)

sum(table(dat$stg))

# which is a
sum(table(dat$stg))/nrow(dat)

# omit unbinned
dats <- dat[!is.na(dat$stg),]

# omit Paleozoic 
dats <- dats[dats$stg>52,]


bsFull <- binstat(dats, tax="genus", bin="stg", 
                  coll="collection_no", ref="reference_no")

bsFull$occs

bs <- binstat(dats, tax="genus", bin="stg", 
              coll="collection_no", ref="reference_no", duplicates=FALSE)
bs$occs


tsplot(stages, boxes="sys", boxes.col="systemCol", 
       shading="series", xlim=c(250, 0), ylim=c(0,2000))


tsplot(stages, boxes="sys", boxes.col="systemCol", 
       shading="series", xlim=c(250, 0), ylim=c(0,2000), ylab="Number occurrences")
lines(stages$mid, bs$occs)


tp <- function(...) tsplot(stages, boxes="sys", boxes.col="systemCol", 
                           shading="series", xlim=52:95, ...)


tp(ylim=c(0,350), ylab="Number of collections") 
lines(stages$mid, bs$colls)

dd <- divDyn(dats, tax="genus", bin="stg")

?divDyn

# simple diversity
tp(ylim=c(0,300), ylab="richness")    
lines(stages$mid, dd$divRT, lwd=2)


cor.test(dd$divRT, bs$occs, method="spearman")


## 
##  Spearman's rank correlation rho
## 
## data:  dd$divRT and bs$occs
## S = 6167.7, p-value = 0.0007426
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.5002229


vignette("handout")


cr50 <- subsample(dats, tax="genus", bin="stg", 
                  q=50, coll="collection_no", duplicates=FALSE, iter=300)


tp(ylim=c(0,100), ylab="Subsampled richness")   
lines(stages$mid, cr50$divRT, lwd=2)

sqsDD <- subsample(dats, tax="genus", bin="stg", q=0.5, type="sqs",
                   duplicates=FALSE, coll="collection_no", iter=300)

tp(ylim=c(0,100), ylab="Subsampled richness (SQS, 0.5)")    
lines(stages$mid, sqsDD$divRT, lwd=2)

# turnover rates
tp(ylim=c(0,1), ylab="per capita turnover rates")   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="green")
legend("topright", legend=c("extinction", "origination"), 
       col=c("red", "green"), lwd=2)






# subset
zdat <- dats[dats$ECOLOGY=="z", ]
azdat <- dats[dats$ECOLOGY=="az" | dats$ECOLOGY=="ap", ]

# divDyn
zDD <- divDyn(zdat, tax="genus", bin="stg")
azDD <- divDyn(azdat, tax="genus", bin="stg")   




tp(ylim=c(0,200), ylab="Raw richness")  
lines(stages$mid, zDD$divRT, lwd=2, col="blue")
lines(stages$mid, azDD$divRT, lwd=2, col="orange")

# legend function
legZAZ <- function(...) 
  legend(legend=c("z", "az"), col=c("blue", "orange"), 
         lty=c(1,1), bg="white", cex=1.3, ...)
legZAZ("topleft")




# plot
tp(ylim=c(0,1), ylab="Per capita extinctions")  
# lines
lines(stages$mid, zDD$extPC, lwd=2, col="blue")
lines(stages$mid, azDD$extPC, lwd=2, col="orange")
# legend
legZAZ("topleft")


# plot
tp(ylim=c(0,1), ylab="Per capita originations") 
# lines
lines(stages$mid, zDD$oriPC, lwd=2, col="blue")
lines(stages$mid, azDD$oriPC, lwd=2, col="orange")
# legend
legZAZ("topleft")


zdat$ecol <- "z"
azdat$ecol <- "az"


rs<-ratesplit(rbind(zdat, azdat), sel="ecol", tax="genus", bin="stg")

rs

tp(ylim=c(0,1), ylab="Per capita originations") 
lines(stages$mid, zDD$oriPC, lwd=2, col="blue")
lines(stages$mid, azDD$oriPC, lwd=2, col="orange")
legZAZ("topleft")

# display selectivity with points
# select the higher rates
selIntervals<-cbind(zDD$oriPC[rs$ori], azDD$oriPC[rs$ori])
groupSelector<-apply(selIntervals, 1, function(x) x[1]<x[2])
# draw the points
points(stages$mid[rs$ori[groupSelector]], azDD$oriPC[rs$ori[groupSelector]],
       pch=16, col="orange", cex=2)
points(stages$mid[rs$ori[!groupSelector]], zDD$oriPC[rs$ori[!groupSelector]],
       pch=16, col="blue", cex=2)


tp(ylim=c(0,1), ylab="Per capita extinctions")  
lines(stages$mid, zDD$extPC, lwd=2, col="blue")
lines(stages$mid, azDD$extPC, lwd=2, col="orange")
legZAZ("topleft")

# display selectivity with points
# select the higher rates
selIntervals<-cbind(zDD$extPC[rs$ext], azDD$extPC[rs$ext])
groupSelector<-apply(selIntervals, 1, function(x) x[1]<x[2])
# draw the points
points(stages$mid[rs$ext[groupSelector]], azDD$extPC[rs$ext[groupSelector]],
       pch=16, col="orange", cex=2)
points(stages$mid[rs$ext[!groupSelector]], zDD$extPC[rs$ext[!groupSelector]],
       pch=16, col="blue", cex=2)