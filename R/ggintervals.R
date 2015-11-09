#' creates a bootstrapped interval similar to ggplot2's mean_cl_boot, but uses 
#' the better BCA bootstrap method
#' 
#' @param x a vector of values to bootstrap a mean from
#' @param conf.int the size of the bootstrapped confidence interval
#' @param replicates the number of bootstrap replicates
#' @return a data frame in a format suitable for plotting with ggplot2
#'   containing the bootstrapped mean and lower and upper bounds
#'   
#' @export
#' 
mean_cl_boot_bca <- function(x, conf.int = 0.95, replicates = 1000) {
  boot_out <- boot::boot(x, function(x, d) mean(x[d]), R = replicates)
  result <- boot::boot.ci(boot_out, type = "bca")
  dat <- data.frame(y = mean(x), ymin = result$bca[4], ymax = result$bca[5])
}

#' creates a confidence level for the mean of independent binary data, using the
#' exact test (which yields intervals that are a little conservative for small
#' sample sizes, but guaranteed to capture >= 0.95 probability)
#' 
#' @param x a vector of (numeric) binary 0s and 1s
#' @param conf.int the size of the confidence interval
#'   
#' @export
#' 
bin_int <- function(x, conf.int = 0.95) {
  x <- x[!is.na(x)]
  stopifnot(is.numeric(x))
  interval <- binom.test(sum(x), length(x), conf.level = conf.int)$conf.int
  dat <- data.frame(y = mean(x), ymin = interval[1], ymax = interval[2])
}
