sm.createTESSA <- function(summfile,name="out.summ",path="~") {
dat <- summfile
bl <- rep(NA,length(dat$fast))
summ <- cbind(bl,bl,dat$slat,dat$slon,bl,dat$year,dat$doy_det,dat$evla,dat$evlo,bl,dat$depthkm,bl,dat$baz,dat$spol,dat$Dspol,dat$wbeg,dat$wend,bl,bl,dat$SNR,dat$tlag,dat$Dtlag,deg(dat$fast),dat$Dfast,dat$anginc,bl,bl,dat$timestamp,bl,dat$nyquist,dat$gradeABCNR,dat$filt_lo,dat$filt_HI,bl,bl,bl,dat$lambdamax,dat$ndf,dat$lambda_min)
summ <- as.data.frame(summ)
filen <- paste0(path,"/",name)
write.table(summ,file=filen,na="",row.names=FALSE,col.names=F,quote=FALSE,sep=",")
}