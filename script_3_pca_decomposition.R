setwd("~/Personal/R-Work/Demand Prediction")

#load the libraries
#install.packages("data.table")
#install.packages("DataCombine")
library(data.table)
library(DataCombine)

xy <- fread(input = "xy_best.csv")
xy <- as.data.frame(xy)
dim(xy);dim(na.omit(xy))

xy <- na.omit(xy)
y <- xy$y
xx <- xy[,2:ncol(xy)] 


pc <- prcomp(xx, center = TRUE, scale = FALSE)
summary(pc)
plot(pc)


pc.use <- 20 # explains 99% of variance
#####################3


xy_new <- as.data.frame(cbind(y,pc$x[,1:pc.use]))
View(xy_new)

fit <- lm(data = xy_new)
summary(fit)
























###################to be used later..
plot(cumsum(pc$sdev^2/sum(pc$sdev^2))) #cumulative explained variance

pc.use <- 3 # explains 99% of variance
trunc <- pc$x[,1:pc.use] %*% t(pc$rotation[,1:pc.use])

#and add the center (and re-scale) back to data
if(pc$scale != FALSE){
  trunc <- scale(trunc, center = FALSE , scale=1/pc$scale)
}
if(pc$center != FALSE){
  trunc <- scale(trunc, center = -1 * pc$center, scale=FALSE)
}
dim(trunc); dim(xx)

##################
RAN <- range(cbind(xx, trunc))
BREAKS <- seq(RAN[1], RAN[2],,100)
COLS <- rainbow(length(BREAKS)-1)
par(mfcol=c(1,2), mar=c(1,1,2,1))
image(as.matrix(xx), main="Original matrix", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BREAKS, col=COLS)
box()
image(trunc, main="Truncated matrix (3 PCs)", xlab="", ylab="", xaxt="n", yaxt="n", breaks=BREAKS, col=COLS)
box()