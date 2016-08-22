sm.getevents <- function(summ,events) {
dat <- summ
if (is.data.frame(events)){
ev <- as.character(events[,1])
} else{
ev <- events
}
datev  <- dat$cuspid

evlist <- intersect(ev,datev)

dat <- subset(dat, cuspid %in% evlist)

return(dat)
}