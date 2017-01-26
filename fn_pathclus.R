sm.pathclus <- function(data,hvec=NULL,kmax=7,seed=NULL,path="~",plotextra=TRUE,rot=180) {

#reference points for converting to x,y co-ordinates
ymin <- -38.6346
xmin <- 176.021

ymax <- -38.5134
xmax <- 176.51


uniquest <- unique(data$stat)
sn <- 0
#Vector of station elevations
if (is.null(hvec)){
hvec <- rep(0,length(uniquest))
}
for (s in uniquest) {
sn <- sn+1
station <- s
print(s)
statn <- subset(data, stat == station)

if (length(statn$fast) <= 55) {
n <- length(statn$fast)
rtest <- rayleigh.test(statn$fast*2)
print(s)
print("Less than 55 measurements")
print(rtest$p)

pval <- round(rtest$p,digits=3)
textname <- paste0(station,"_cluster",1,"_p-val_",pval)
filen <- paste0(path,"/",textname)
cuspids <- statn$cuspid
write.table(cuspids,file=filen,col.names=FALSE,row.names=FALSE,quote=FALSE)

}
if (length(statn$fast) > 55) {

evlols <- statn$evlo
evlals <- statn$evla
evdp <- statn$depthkm

val <- hvec[sn]

evdp <- evdp+val

print(val)




#la <- -38.62264
#lo <- 176.17208

la <- statn$slat[[1]]
lo <- statn$slon[[1]]

dx <- (evlols-xmin)*111.320*cos(rad(evlals))
dy <- (evlals-ymin)*110.574

sx <- (lo-xmin)*111.320*cos(rad(la))
sy <- (la-ymin)*110.574

diffx <- sx-dx
diffy <- sy-dy

#convert to spherical
r <- sqrt(diffx^2+diffy^2+evdp^2)
theta <- acos(evdp/r)
phi <- atan2(diffy,diffx)

r2 <- 1 
x <- r2*sin(theta)*cos(phi)
y <- r2*sin(theta)*sin(phi)
z <- r2*cos(theta)

m <- cbind(x,y,z)
if (is.null(seed)){
}else{
set.seed(seed)
}
kmax <- 7

vMFs <- lapply(1:kmax, function(K)
movMF(m,k=K,control=list(nruns=20)))
#
bics <- sapply(vMFs, BIC)

print(bics)

pr <- which.min(bics)
it <- 0 #iterations for later tests

fMF <- vMFs[[pr]]
colz <- brewer.pal(pr,"Dark2")
clus <- predict(fMF)
cols <- colz[clus]
if (plotextra == TRUE){
postscript(file=paste0(path,"/",station,"_2D.eps"), onefile=FALSE, horizontal=FALSE,width=7,height=7,paper='special')
par(pty="s")
plot(dx,dy,col=cols,xlab="x (km)",ylab="y (km)",asp=1,cex.axis=1.5,cex.lab=1.5)
points(sx,sy,pch=17)
dev.off()
}
g <- 0
if (plotextra == TRUE){

postscript(file=paste0(path,"/",station,"_3D.eps"), onefile=FALSE, horizontal=FALSE,width=7,height=7,paper='special')
par(mfrow = c(1, 1))
M <- mesh(seq(0, 2*pi, length.out = 100),
seq(0, pi/2, length.out = 100))
u <- M$x ; v <- M$y
x2 <- cos(u)*sin(v)
y2 <- sin(u)*sin(v)
z2 <- cos(v)
# full panels of box are drawn (bty = "f")
#scatter3D(x2, y2, -z2, pch = ".", col = "lightgrey", bty = "f", cex = 2, colkey = FALSE,zlim=c(-1,1),theta=0)
scatter3D(x2, y2, -z2, pch = ".", col = "lightgrey", bty = "f", cex = 2, colkey = FALSE,zlim=c(-1,1),theta=rot)

points3D(x,y,-z,colvar=clus,col=brewer.pal(pr,"Dark2"),zlim=c(-1,1),add=TRUE,pch = ".",cex=5,colkey=FALSE,cex.axis=1.5,cex.lab=1.5)
dev.off()
}

for (i in 1:pr) {
cluster <- subset(statn$fast, clus == i)
n1 <- paste0(path,"/",station,"_azclusraw_",i)
n2 <- paste0(path,"/",station,"_azclusdub_",i)

cls <- colz[[i]]
if (plotextra == TRUE){
sm.plot(cluster,name1=n1,name2=n2,cols=cls)
}
n <- length(cluster)
rtest <- rayleigh.test(cluster*2)
print(rtest$p)

pval <- round(rtest$p,digits=3)
textname <- paste0(station,"_cluster",i,"_p-val_",pval)
filen <- paste0(path,"/",textname)
cuspids <- subset(statn$cuspid, clus == i)
write.table(cuspids,file=filen,col.names=FALSE,row.names=FALSE,quote=FALSE)



}

}
}

invisible()
}
