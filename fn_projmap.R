sm.proj <- function(summf,lm=2,pierce=1.5,savpath="~",savnam="proj.png",mlat="NULL",mlon="NULL",zoom=13,xvec="NULL",yvec="NULL",hvec=NULL) {

sm <- summf

smrcA <- sm$fast

#Input for Station locations
stationnames <- unique(sm$stat)
stat <- sm$stat

lnstat <- length(stationnames)
for (k in 1:lnstat){
getstnam <- stationnames[k]
subdat <- subset(sm, stat == getstnam)
LA=subdat$stla[1]
LO=subdat$stlo[1]
}

yr <- sm$year
dy <- sm$doy_det
evla <- sm$evla
evlo <- sm$evlo

tlag <- sm$tlag
slat <- as.numeric(sm$slat)
slon <- as.numeric(sm$slon)

stat <- droplevels(stat)
cl <- length(levels(stat))
# http://tools.medialab.sciences-po.fr/iwanthue/

if (length(unique(stat)) == 10) {
pl <- palette(c(rgb(142,131,60,maxColorValue=255),
rgb(163,97,199,maxColorValue=255),
rgb(130,181,61,maxColorValue=255),
rgb(102,132,204,maxColorValue=255),
rgb(211,155,68,maxColorValue=255),
rgb(75,185,179,maxColorValue=255),
rgb(202,88,66,maxColorValue=255),
rgb(93,190,115,maxColorValue=255),
rgb(198,92,138,maxColorValue=255),
rgb(69,125,65,maxColorValue=255)))
print("10")
cols <- pl[stat]
} 

if (length(unique(stat)) <= 8 && length(unique(stat)) > 2) {
rn <- brewer.pal(length(unique(stat)),"Dark2")
cols <- rn[stat]
}
#rn <- viridis(length(unique(stat)))

if (length(unique(stat)) == 9) {
pl <- palette(c(rgb(121,128,197,maxColorValue=255),
rgb(137,168,60,maxColorValue=255),
rgb(108,106,216,maxColorValue=255),
rgb(196,116,59,maxColorValue=255),
rgb(186,89,195,maxColorValue=255),
rgb(96,161,108,maxColorValue=255),
rgb(193,97,159,maxColorValue=255),
rgb(69,178,204,maxColorValue=255),
rgb(204,86,106,maxColorValue=255)))
print("9")
cols <- pl[stat]
} 

if (length(unique(stat)) <= 2 | length(unique(stat)) > 9) {
rn <- rainbow(length(unique(stat)))
cols <- rn[stat]
}

if (is.null(hvec)){
hvec <- rep(0,length(unique(stat)))
}
dp <- sm$depthkm - hvec[stat] #you must subtract elevation to get the projection in the right place. This is easy to prove by drawing straight line ray paths

smrc1 <- data.frame(smrcA,evla,evlo,dp,tlag,slat,slon,cols)

smrc <- smrc1$smrcA


ymin <- -38.6346
xmin <- 176.021

ymax <- -38.5134
xmax <- 176.51
smrc1
dymax <- (ymax-ymin)*110.574
dxmax <- (xmax-xmin)*111.320*cos(rad(ymax))

dx <- (smrc1$evlo-xmin)*111.320*cos(rad(smrc1$evla))
dy <- (smrc1$evla-ymin)*110.574

dxs <- (smrc1$slon-xmin)*111.320*cos(rad(smrc1$slat))
dys <- (smrc1$slat-ymin)*110.574

dxst <- (slon-xmin)*111.320*cos(rad(slat))
dyst <- (slat-ymin)*110.574

#dev.new(width=10,height=10)



## projection plots
pj <- pierce #piercing depth for projection

x <- dx-dxs
y <- dy-dys
theta <- atan2(y,x)
dist <- sqrt(x^2+y^2)
dtheta <- atan2(smrc1$dp,dist)

a <- pj/tan(dtheta)

xn <- a*cos(theta)
yn <- a*sin(theta)

xc <- xn+dxs
yc <- yn+dys

#lm <- 2 #length multiplier (values are automtically doubled ie 0.4s is 0.8km in length using a 1x multiplier)

pjl <- smrc1$tlag*lm

xv1 <- pjl*cos(pi/2-as.numeric(smrc))+xc
yv1 <- pjl*sin(pi/2-as.numeric(smrc))+yc

xv2 <- pjl*cos(pi/2-as.numeric(smrc)+pi)+xc
yv2 <- pjl*sin(pi/2-as.numeric(smrc)+pi)+yc

segments(xc,yc,x1=xv1,y1=yv1,col=smrc1$cols,)
segments(xc,yc,x1=xv2,y1=yv2,col=smrc1$cols)

la1 <- yv1/110.574+ymin
lo1 <- xv1/(111.320*cos(rad(la1)))+xmin

la2 <- yv2/110.574+ymin
lo2 <- xv2/(111.320*cos(rad(la2)))+xmin

lac <- yc/110.574+ymin
loc <- xc/(111.320*cos(rad(lac)))+xmin

seg1 <- as.data.frame(cbind(as.numeric(loc),as.numeric(lac),as.numeric(lo1),as.numeric(la1)))
seg2 <- as.data.frame(cbind(as.numeric(loc),as.numeric(lac),as.numeric(lo2),as.numeric(la2)))

pointz <- as.data.frame(cbind(as.numeric(slon),as.numeric(slat)))
pname <- sm$stat

if (mlat == "NULL" | mlon == "NULL"){

medlac <- median(lac)
medloc <- median(loc)
} else {

medlac <- mlat
medloc <- mlon
}


for (i in stationnames) {
subs <- subset(sm, stat == i)
loS <- subs$slon[1]
laS <- subs$slat[1]
data <- as.data.frame(cbind(as.numeric(loS),as.numeric(laS)))
statname <- i
if (i != stat[1]){
names1 <- rbind(names1,data)
statlist <- rbind(statlist,as.character(statname))
} else {
names1 <- data
statlist <- as.character(statname)
}
}

if (length(statlist) > 1) {
v <- statlist[,1]
}else{
v <- statlist
}


row.names(names1) <- as.data.frame(v)[,1]



lblz <- rownames(names1)
lblz <<- as.character(lblz)

if (yvec == "NULL" | xvec == "NULL") {
yvec <- rep.int(0,length(statlist))
#yvec[which(statlist == "RT01")] <- -0.004

xvec <- rep.int(0,length(statlist))
}



map <- get_googlemap(center = c(lon = medloc, lat = medlac),maptype="terrain",zoom=zoom,legend=right,color="bw")
p <- ggmap(map) + geom_segment(
aes(x = V1, y = V2, xend = V3, yend = V4),
colour = cols, size = 0.3, data = seg1) + geom_segment(
aes(x = V1, y = V2, xend = V3, yend = V4),
colour = cols, size = 0.3, data = seg2) + geom_point(aes(x = V1, y = V2), data = pointz,pch=24,color="black",bg=cols,size=2.5) + labs(x = 'Longitude', y = 'Latitude') + geom_text(data = names1, aes(x = V1, y = V2, label = lblz), 
          size = 4, color="Black", fontface=2, vjust = 0, hjust = -0.5,nudge_y=yvec,nudge_x=xvec,show.legend=FALSE)
ggsave(savnam,plot=p,device="png",path=savpath)

}