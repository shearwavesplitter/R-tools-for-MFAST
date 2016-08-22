#reads in all .Summ files in a particular folder, filters, converts to radians and applies defines data as circular geographic
#New version filters out non AB graded measurements and only keeps one of the filters (events aren't repeated)

sm.read_vl <- function(path, tlagmax=0.2,station="NULL",minl=0,minsnr=3) {
sm.cleansumm(path)
setwd(path)
swtc <- 0
gobjects <- ls(envir=.GlobalEnv)

file_list <- list.files(pattern = "\\.summ$")

for (file in file_list) {

  if (!exists("sm")) {
    sm <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
  }

  if (exists("sm") && !('sm' %in% gobjects)) {
    temp_dataset <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
    sm <- rbind(sm, temp_dataset)
    rm(temp_dataset)

  }
 if (exists("sm") && ('sm' %in% gobjects) && (swtc == 0)) {
    sm <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
  swtc <- 1
}
if (exists("sm") && ('sm' %in% gobjects) && (swtc == 1)) {
    temp_dataset <- read.csv(file, sep = ",")[ ,c('event','stat','slat','slon','cuspid','year','doy_det','evla','evlo','depthkm','mag','baz','spol','Dspol','wbeg','wend','SNR','tlag','Dtlag','fast','Dfast','anginc','anginc_corr','type_ini','timestamp','comment','nyquist','gradeABCNR','filt_lo','filt_HI','bandang','pickgrade','lambdamax','ndf','lambda_min','ttime','maxfreq')]
    sm <- rbind(sm, temp_dataset)
    rm(temp_dataset)

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

subs <- subs[subs$finalgrade %in% c("AB"), ]  # keep AB grade measurements
subs <- subs[subs$lambdamax > minl, ]

if (station[1] == "NULL") {
} else {

subs <- subs[subs$stat %in% c(station), ]
}
###############

#Filtering out the same event with different filters. Requires event to be read in
l <- nchar(as.character(subs$event))

filt <- as.numeric(substr(subs$event,l,l))

subs$event <- filt

unstat <- unique(subs$stat)


fd <- subs

for (z in unstat) {

print(z)

addv <- 1

subs <- subset(fd,fd$stat == z)



for (k in 1:3) {

	for (i in 1:length(subs$stat)) {

		if (subs$event[i] == k && subs$gradeABCNR[i] == "ACl" && !(subs$cuspid[i] %in% addv)){
			if (addv[1] != 1) {
				addv <- rbind(addv,as.character(subs$cuspid[i]))
			} else {
				addv <- as.character(subs$cuspid[i])
			}
			if (exists("uniquev")) {
				uniquev <- rbind(uniquev,subs[i,])
			} else {
				uniquev <- subs[i,]
			}
		}
	}

}

for (k in 1:3) {
	for (i in 1:length(subs$stat)) {
		if (subs$event[i] == k && subs$gradeABCNR[i] == "BCl" && !(subs$cuspid[i] %in% addv)){
			if (addv[1] != 1) {
				addv <- rbind(addv,as.character(subs$cuspid[i]))
			} else {
				addv <- as.character(subs$cuspid[i])
			}
			if (exists("uniquev")) {
				uniquev <- rbind(uniquev,subs[i,])
			} else {
				uniquev <- subs[i,]
			}
		}
	}

}


}

#############

uniquev$finalgrade <- NULL

colnames(uniquev)[1] <- "filtnum"

return(uniquev)

}
