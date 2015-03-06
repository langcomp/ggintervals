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
