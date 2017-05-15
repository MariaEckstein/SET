create_rolling_time_bins = function(data = subj_file, time_bin, smooth_times, ddply_columns) {
  
  library("plyr"); library("zoo")
  
  # Add raw_time column (in sec; starting at 0), which contains the raw Timestamp, combined of seconds and microseconds
  data$raw_time = with(data, TimestampSec + TimestampMicrosec/1000000)
  data$raw_time = data$raw_time - data$raw_time[1]
  
  ### Create a vector containing the wanted bin times
  min_time = 0
  max_time = data$raw_time[length(data$raw_time)]
  bins = seq(min_time, max_time, time_bin)
  
  ### Add bin_time column to the data file, which indicates the 50-ms bin in which each data point lies
  data$bin_time = min_time                                               # Initiate the bin_time column at 0
  for (bin in bins) {
    data$bin_time[data$raw_time > bin] = bin    # Go over bins and replace initial value with higher values according to raw_time
  }
  
  ### Average over data points in the same time bin (using ddply)
  # bin_dat = ddply(data, .(Subject, TrialId, CRESP, RESP, ACC, RT, Category, SETornoSET, Position_false_shape, bin_time, lateralization), summarize,
  #                 bin_pupil = mean(interp_av_pupil, na.rm = T))
  bin_dat = ddply(data, c(ddply_columns, "bin_time"), summarize,
                  bin_pupil = mean(interp_av_pupil, na.rm = T))
  
  # ### Fill up eventually missing bin_time values (when a new trial starts, up to 0.5 seconds of data are missing - add these time bins and set them to NA)
  # fixd_bin_dat = bin_dat[1,]
  # 
  # # for (subj in unique(data$Subject)) {
  #   for (trial in unique(data$TrialId)) {
  #     t_bin_dat = subset(bin_dat, TrialId == trial)
  #     
  #     # Create a new trial file that has all bins and fill them up with data from the original file
  #     t_fixd_bin_dat = t_bin_dat[1,]
  #     t_fixd_bin_dat$bin_time = NULL
  #     t_fixd_bin_dat$bin_pupil = NA
  #     t_fixd_bin_dat = cbind(t_fixd_bin_dat, bin_time = seq(min(t_bin_dat$bin_time), max(t_bin_dat$bin_time), time_bin))
  #     
  #     # fixd_bin_dat$CurrentObject[fixd_bin_dat$bin_time %in% st_bin_dat$bin_time] = as.character(st_bin_dat$CurrentObject)
  #     t_fixd_bin_dat$bin_pupil[t_fixd_bin_dat$bin_time %in% t_bin_dat$bin_time] = t_bin_dat$bin_pupil
  #     
  #     fixd_bin_dat = rbind(fixd_bin_dat, t_fixd_bin_dat)
  #   }
  # # }
  # # fixd_bin_dat = fixd_bin_dat[-1,]
  
  ### Calculate rolling averages over 150ms to get a smooth data point every 50ms
  # for (subj in unique(fixd_bin_dat$Subject)) {
    # for (trial in unique(fixd_bin_dat$TrialId)) {
      # Get the rolling averages for each trial (smooth multiple times, as indicated in smooth_times)
      roll_pupil_mean  = rollapply(data = bin_dat$bin_pupil,
                                   width = 7, mean, na.rm = T, fill = NA, partial = T, align = "center")
      for (i in 1:(smooth_times-1)) {
        roll_pupil_mean = rollapply(data = roll_pupil_mean,
                                    width = 3, mean, na.rm = T, fill = NA, partial = T, align = "center")
      }
      bin_dat$roll_pupil = roll_pupil_mean
    # }
  # }
  return(bin_dat)
}
