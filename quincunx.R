quinc <- function(x = 100, depth = 10) {
	bounce_list <- vector(mode = "numeric", length = x)
	for(i in 1:x) {
		bounce_list[i] <- sum(sample(c(-.5, .5), depth, replace = TRUE))
	}

	hist(bounce_list)
}
