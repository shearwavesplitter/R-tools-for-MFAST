sm.stde <- function(data,seed=NULL,iter=9999) {
	if (is.null(seed)) {
}else{
		set.seed(seed)
	}
	vals <- data*2
	n <- length(vals)

	for (j in 1:iter){
		samp <- sample(vals,size=n,replace=TRUE)
		m <- mean.circular(samp)
		if (j == 1) {
			means <- m
		} else {
			means <- c(means,m)
		}
	}

	sd <- sd.circular(means*2)
	#sd <- asin(sd)
sd <- sd/2
return(sd)
}