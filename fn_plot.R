#plots axial data
sm.plot <- function(data, name1="raw.eps", name2="double.eps", path="~",cols="blue",antipodal="lightblue",bins=16,kd=TRUE,arrow=TRUE,medarrow=FALSE,sym=16,bimodal=FALSE,name3="quad.eps") {
data <- circular(data,type="angles",units="radians",template="geographics")
## place data on -90 to 90 degrees
for (i in 1:length(data)){
if (data[i] > pi/2) {
data[i] <- data[i]-pi
}
if (data[i] <= -pi/2) {
data[i] <- data[i]+pi
}
}
for (i in 1:length(data)){
if (data[i] > pi/2) {
data[i] <- data[i]-pi
}
if (data[i] <= -pi/2) {
data[i] <- data[i]+pi
}
}

setwd(path)
#Mean for plotting
smSS <-data*2
m <- mean(smSS)
tm <- m/2
R <- rho.circular(smSS) #Mean resultant length
trigm2 <- trigonometric.moment(smSS, p=2, center=TRUE) #Second trigonometric moment for skewness/kurtosis
#Median for plotting
med <- median.circular(smSS)
tmed <- med/2

if (bimodal == T){
#Mean for plotting
smSS <-data*4
m4 <- mean(smSS)
m <- mean(smSS)/2
tm <- m/2
R <- rho.circular(smSS) #Mean resultant length
trigm2 <- trigonometric.moment(smSS, p=2, center=TRUE) #Second trigonometric moment for skewness/kurtosis
#Median for plotting
med <- median.circular(smSS)
tmed <- med/4

}



ty <- "p"
#Save axial plot as raw.eps
postscript(file=name1, onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')
smrc <- data


plot(smrc,pch=sym,col=cols,stack=T,shrink=1.2,bins=180,ticks=T,type=ty)
rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
points(smrc+pi,pch=sym,col=antipodal,stack=T,shrink=1.2,bins=180,type=ty)
#rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)


#visualise axial data in roseplot
sma <- smrc+pi
rose.diag(sma,bins=bins,col="lightgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)

#Kernal density for axial data
if (kd == TRUE){
smapp <- append(sma,smrc)
lines(density.circular(smapp,bw=10),lwd=2,lty=3)
}
#Plotmean
if (arrow == TRUE){
arrows.circular(tm,col="red",lwd=2,shrink=R)
arrows.circular(tm+pi,col="red",lwd=2,shrink=R)
}
if (medarrow == TRUE){
arrows.circular(tmed,col="darkgreen",lwd=2,shrink=R) #median arrows
arrows.circular(tmed+pi,col="darkgreen",lwd=2,shrink=R) 
}

if (bimodal == T){


if (arrow == TRUE){
arrows.circular(tm+pi/2,col="red",lwd=2,shrink=R)
arrows.circular(tm+pi+pi/2,col="red",lwd=2,shrink=R)
}
if (medarrow == TRUE){
arrows.circular(tmed+pi/2,,col="darkgreen",lwd=2,shrink=R) #median arrows
arrows.circular(tmed+pi+pi/2,col="darkgreen",lwd=2,shrink=R) 
}

}
#Legend
#legend("bottom", c("Mean", "Fast Azimuths", "Antipodal Azimuths"), xpd = TRUE, horiz = TRUE, inset = c(0, 
   #0), bty = "n", pch = c("-", "o", "o"), col = c("red","blue","lightblue"), cex = 1)

dev.off()

#Double Angle plot
#Save as double.eps

postscript(file=name2, onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')

plot(smSS,pch=sym,col=cols,stack=T,shrink=1.2,bins=180,ticks=T,type=ty)
rose.diag(smSS,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
if (kd == TRUE){
lines(density.circular(smSS,bw=10),lwd=2,lty=3)
}
if (medarrow == TRUE){
arrows.circular(med,col="darkgreen",lwd=2,shrink=R) #median
}
if (arrow == TRUE){
arrows.circular(m,col="red",lwd=2,shrink=R)
}

if (bimodal == T){

if (medarrow == TRUE){
arrows.circular(med+pi,col="darkgreen",lwd=2,shrink=R) #median
}
if (arrow == TRUE){
arrows.circular(m+pi,col="red",lwd=2,shrink=R)
}

}

#Legend
#legend("bottom", c("Mean", "Double Angle Azimuths"), xpd = TRUE, horiz = TRUE, inset = c(0, 
    #0), bty = "n", pch = c("-", "o"), col = c("red","blue"), cex = 1)

dev.off()

if (bimodal == T){
postscript(file=name3, onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')

plot(smSS,pch=sym,col=cols,stack=T,shrink=1.2,bins=180,ticks=T,type=ty)
rose.diag(smSS,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
if (kd == TRUE){
lines(density.circular(smSS,bw=10),lwd=2,lty=3)
}
if (medarrow == TRUE){
arrows.circular(m4,col="darkgreen",lwd=2,shrink=R) #median
}
if (arrow == TRUE){
arrows.circular(m4,col="red",lwd=2,shrink=R)
}



#Legend
#legend("bottom", c("Mean", "Double Angle Azimuths"), xpd = TRUE, horiz = TRUE, inset = c(0, 
    #0), bty = "n", pch = c("-", "o"), col = c("red","blue"), cex = 1)

dev.off()

}

invisible()
}
