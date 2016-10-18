#reads in all .Summ files in a particular folder, filters, converts to radians and applies defines data as circular geographic
#New version filters out non AB graded measurements and only keeps one of the filters (events aren't repeated)

sm.readraw <- function(path, station=NULL,tlagmax=1,minsnr=3) {
sm.cleansumm(path)
setwd(path)
swtc <- 0
gobjects <- ls(envir=.GlobalEnv)

file_list <- list.files(pattern = "\\.summ$")

for (file in file_list) {

  if (exists("sm") && !('sm' %in% gobjects)) {
    temp_dataset <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','distevstat','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
    sm <- rbind(sm, temp_dataset)
    rm(temp_dataset)

  }
  if (!exists("sm")) {
    sm <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','distevstat','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
  }

if (exists("sm") && ('sm' %in% gobjects) && (swtc == 1)) {
    temp_dataset <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','distevstat','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
    sm <- rbind(sm, temp_dataset)
    rm(temp_dataset)

  }
 if (exists("sm") && ('sm' %in% gobjects) && (swtc == 0)) {
    sm <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','distevstat','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
  swtc <- 1
}


 }

sm <- sm[sm$gradeABCNR %in% c("ACl","BCl","A","B"), ] #Keep only A and B grade measurements

sm$fast <- rad(sm$fast) #Convert to radians

sm$fast <- circular(sm$fast,type="angles",units="radians",template="geographics")

#grading. requires 20SNR & Dfast
subs <- sm
finalgrade <- 1
for (i in 1:length(subs$fast)){
	if ((subs$tlag[i] < (0.8*tlagmax)) && (subs$SNR[i] > minsnr) && (subs$Dfast[i] < 10)){
		if (finalgrade[1] != 1) {
			finalgrade <- rbind(finalgrade,"AB")
		} else {
			finalgrade <- "AB"
		}
	} else {
		if (finalgrade[1] != 1) {
			finalgrade <- rbind(finalgrade,"NA")
		} else {
			finalgrade <- "NA"
		}
	}
}

subs <- cbind(subs,finalgrade)

#subs <- subs[subs$finalgrade %in% c("AB"), ]  # keep AB grade measurements
#subs <- subs[subs$lambdamax > minl, ]

if (is.null(station[1])) {
} else {

subs <- subs[subs$stat %in% c(station), ]
}

return(subs)

}




