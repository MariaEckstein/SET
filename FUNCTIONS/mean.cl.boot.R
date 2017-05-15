mean.cl.boot = function(x, B = 1000, tr = .2, ci = 0.95) {
  
  # x = rgamma(100, 1)
  # mean_cl_boot(x)
  b.mean = one.boot(x, mean, R = B, tr = tr)
  b.median = one.boot(x, median, R = B)
  boot_mean = b.mean[[1]]
  boot_median = b.median[[1]]
  boot_cis = boot.ci(b.mean, type = c("perc"), conf = ci)[["percent"]][c(4, 5)]
  
  all = c(boot_mean, boot_cis)
  names(all) = c("y", "ymin", "ymax")
  all
}
