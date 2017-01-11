#plots axial data
sm.plot_2 <- function(data, name1="raw.eps", name2="double.eps", wd="~/R_MFAST",ty="p",cols="blue",antipodal="lightblue",bins=16,kd=TRUE,arrow=TRUE,medarrow=FALSE) {
setwd(wd)
#Mean for plotting
smSS <-data*2
m <- mean(smSS)
tm <- m/2
R <- rho.circular(smSS) #Mean resultant length
trigm2 <- trigonometric.moment(smSS, p=2, center=TRUE) #Second trigonometric moment for skewness/kurtosis
#Median for plotting
med <- median.circular(smSS)
tmed <- med/2

#Save axial plot as raw.eps
postscript(file=name1, onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')
smrc <- data


plot(smrc,pch=16,col=cols,stack=T,shrink=1.2,bins=180,ticks=T,type=ty)
rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=16)
points(smrc+pi,pch=16,col=antipodal,stack=T,shrink=1.2,bins=180,type=ty)
#rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=16)


#visualise axial data in roseplot
sma <- smrc+pi
rose.diag(sma,bins=bins,col="lightgrey",prop=1.3,add=TRUE,shrink=1.2,pch=16)

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
#Legend
#legend("bottom", c("Mean", "Fast Azimuths", "Antipodal Azimuths"), xpd = TRUE, horiz = TRUE, inset = c(0, 
   #0), bty = "n", pch = c("-", "o", "o"), col = c("red","blue","lightblue"), cex = 1)

dev.off()

#Double Angle plot
#Save as double.eps

postscript(file=name2, onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')

plot(smSS,pch=16,col=cols,stack=T,shrink=1.2,bins=180,ticks=T,type=ty)
rose.diag(smSS,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=16)
if (kd == TRUE){
lines(density.circular(smSS,bw=10),lwd=2,lty=3)
}
if (medarrow == TRUE){
arrows.circular(med,col="darkgreen",lwd=2,shrink=R) #median
}
if (arrow == TRUE){
arrows.circular(m,col="red",lwd=2,shrink=R)
}
#Legend
#legend("bottom", c("Mean", "Double Angle Azimuths"), xpd = TRUE, horiz = TRUE, inset = c(0, 
    #0), bty = "n", pch = c("-", "o"), col = c("red","blue"), cex = 1)

dev.off()

invisible()
}
