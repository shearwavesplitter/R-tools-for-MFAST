#reads in all .Summ files in a particular folder, filters, converts to radians and applies defines data as circular geographic
#New version filters out non AB graded measurements and only keeps one of the filters (events aren't repeated)

sm.readraw <- function(path, station=NULL,tlagmax=1,minsnr=3) {
sm.cleansumm(path)

file_list <- list.files(pattern = "\\.summ$")

tabs <- paste0(path,"/",file_list)
res <- lapply(tabs,read.csv)
sm <- do.call(rbind , res)

if (is.null(station[1])) {
} else {

sm <- sm[sm$stat %in% c(station), ]
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


return(subs)

}




