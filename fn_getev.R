sm.getevents <- function(summ,events,station=NULL) {
dat <- summ
if (is.null(station[1])) {
}else{
dat <- subset(dat, dat$stat == station)
}
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