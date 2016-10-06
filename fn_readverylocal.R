#reads in all .Summ files in a particular folder, filters, converts to radians and applies defines data as circular geographic
#New version filters out non AB graded measurements and only keeps one of the filters (events aren't repeated)

sm.read_vl <- function(path, tlagmax=0.2,station="NULL",minl=0,minsnr=3,type=2) {
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

subs <- subs[subs$finalgrade %in% c("AB"), ]  # keep AB grade measurements
subs <- subs[subs$lambdamax > minl, ]

if (station[1] == "NULL") {
} else {

subs <- subs[subs$stat %in% c(station), ]
}
###############

#Filtering out the same event with different filters. Requires event to be read in. Type 1: First filter is kept. Does not check if there are differences between filters
if (type != 2) {
print("Type 1")

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

}
#############
#Type 2 grading based on castelazzi. At least two (out of a possible three) filters have to give a similar result. The filter with the lowest error is kept
if (type == 2) {
unstat <- unique(subs$stat)
print("Type 2")
for (j in 1:length(unstat)){
station <- unstat[j]
print(as.character(station))
sub2 <- subset(subs, stat == station)


l <- nchar(as.character(sub2$event))

filt <- as.numeric(substr(sub2$event,l,l))

sub2$event <- filt

unev <- unique(sub2$cuspid)



for (i in 1:length(unev)){
if(exists('add')){
rm(add)
}
eventn <- unev[i]
fsub <- subset(sub2, cuspid == eventn)
if (length(fsub$fast) == 1){
add <- fsub
add$finalgrade <- "F1"
} 

if (length(fsub$fast) == 2){
diff <- abs(fsub$fast[1]-fsub$fast[2])
if (diff > pi) {
diff <- 2*pi-abs(diff)
}

if (diff < rad(10)){

n <- which.min(fsub$Dfast)
add <- fsub[n[1],]
add$finalgrade <- "F2"
}

} 
if (length(fsub$fast) == 3){ #If there are three the grading is set to F3 if all three measurements are within 10 degrees of their mean. These are graded F2 if at least two of the three are within 10 degrees of eachother. i.e. takes the lowest two closest togther and then the one with the lowest Dfast and grades in F2
m <- mean.circular(fsub$fast*2)/2
dif1 <- abs(fsub$fast[1]-m)
if (dif1 > pi) {
dif1 <- 2*pi-abs(dif1)
}
dif2 <- abs(fsub$fast[2]-m)
if (dif2 > pi) {
dif2 <- 2*pi-abs(dif2)
}
dif3 <- abs(fsub$fast[3]-m)
if (dif3 > pi) {
dif3 <- 2*pi-abs(dif3)
}
if (max(c(dif1,dif2,dif3)) < rad(10)){
n <- which.min(fsub$Dfast)
add <- fsub[n[1],]
add$finalgrade <- "F3"
}else{
dif12 <- abs(fsub$fast[1]-fsub$fast[2])
if (dif12 > pi) {
dif12 <- 2*pi-abs(dif12)
}
dif13 <- abs(fsub$fast[1]-fsub$fast[3])
if (dif13 > pi) {
dif13 <- 2*pi-abs(dif13)
}
dif23 <- abs(fsub$fast[2]-fsub$fast[3])
if (dif23 > pi) {
dif23 <- 2*pi-abs(dif23)
}
min2 <- which.min(c(dif12,dif13,dif23))
if (min2 == 1 && dif12 < rad(10)){
fsub <- rbind(fsub[1,],fsub[2,])
n <- which.min(fsub$Dfast)
add <- fsub[n[1],]
add$finalgrade <- "F2b"
}
if (min2 == 2 && dif13 < rad(10)){
fsub <- rbind(fsub[1,],fsub[3,])
n <- which.min(fsub$Dfast)
add <- fsub[n[1],]
add$finalgrade <- "F2b"
}
if (min2 == 3 && dif23 < rad(10)){
fsub <- rbind(fsub[2,],fsub[3,])
n <- which.min(fsub$Dfast)
add <- fsub[n[1],]
add$finalgrade <- "F2b"
}
}

}

if(exists('add')){
if(exists('uniquev')){
uniquev <- rbind(uniquev,add)
}else{
uniquev <- add
}
}

}
}

}
return(uniquev)

}