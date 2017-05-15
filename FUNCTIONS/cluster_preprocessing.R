
file_dir = "/home/bunge/maria/Desktop/seqSET/raw_data/"
# library("plyr"); library("zoo")

### Find subject data files and create folder to put the preprocessed ones
filenames = list.files(file_dir, pattern = "*.csv")

### Set up the loop over all files
for (filename in filenames[21:25]) {
  subj_file = vector()
  subj_file = read.csv(file = file.path(file_dir, filename), header = T, na.strings = c("", "NA", "-1"))
  filename = paste("latSET", subj_file$Subject[1], ".csv", sep = "")
  
  ### Remove training trials
  subj_file = subset(subj_file, TrainorExp == "Exp")
  subj_file$Category = factor(subj_file$Category)
  subj_file$SETornoSET = factor(subj_file$SETornoSET, levels = c("SET", "noSET"))
  
  ### Keep only necessary columns
  keep_columns = c("Subject", "ID", "TimestampSec", "TimestampMicrosec", "DiameterPupilLeftEye", "ValidityLeftEye", "DiameterPupilRightEye", "ValidityRightEye", "TrialId", "CRESP", "RESP", "ACC", "RT", "Category", "SETornoSET", "TrainorExp", "Position_false_shape", "CurrentObject")
  
  subj_file = subset(subj_file[,keep_columns])
  
  #########################################
  ### Filter and interpolate pupil data ###
  #########################################
  
  ### Create subj_file and filter stats
  source("FUNCTIONS/filter_and_interpolate_raw_pupil_data.R")
  subj_file_and_filterstats = filter_and_interpolate_raw_pupil_data(file = subj_file,
                                                                    output_dir = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/subj_files_nb",
                                                                    se = 5,
                                                                    numPoints = 80,
                                                                    max.interp = 12,
                                                                    min.percent.of.valid.data = 30)
  subj_file = subj_file_and_filterstats[[1]]
  filter_stats = subj_file_and_filterstats[[2]]
  
  
  #############################################################
  ### Exclude trials with less than 50% of valid pupil data ###
  #############################################################
  
  # source("FUNCTIONS/exclude_trials_with_too_much_invalid_data.R")
  # subj_file_and_remove_stats = exclude_trials_with_too_much_invalid_data(file = subj_file, trial_threshold = 50)
  # subj_file = subj_file_and_remove_stats[[1]]
  # remove_stats = subj_file_and_remove_stats[[2]]
  
  #########################################
  ### Bin the data into pieces of 50ms ###
  #########################################
  
  ### Add fixations to previous trial (only necessary for data collected with OLD SET version!!! [all 2014 data & subject 301 of the 2015 data])
  source("FUNCTIONS/add_fixations_to_previous_trial.R")
  if (subj_file$Subject[1] %in% c(seq(3, 41), 301)) {
    subj_file = add_fixations_to_previous_trial(subj_file)
  }
  
  ### Make each trial start at time 0
  source("FUNCTIONS/start_each_trial_at_time_0.R")
  subj_file = start_each_trial_at_time_0(subj_file)
  
  
  ### Calculate pupil dilation relative to baseline period
  source("FUNCTIONS/calculate_relative_pupil_dilation.R")
  subj_file = calculate_relative_pupil_dilation(subj_file, baseline_window_length = 200, bin_duration = 16.66)
  
  ### Downsample to 20 Hertz and smooth
  source("FUNCTIONS/create_rolling_time_bins.R")
  subj_file = create_rolling_time_bins(data = subj_file, time_bin = 0.05, smooth_times = 3, ddply_columns = c("Subject", "TrialId", "CRESP", "RESP", "ACC", "RT", "Category", "SETornoSET", "Position_false_shape", "bin_time"))
  subj_file$bin_time = as.numeric(as.character(subj_file$bin_time))
  
  ###########################
  ### Save each subj_file ###
  ###########################
  write.csv(subj_file, paste("/home/bunge/maria/Desktop/seqSET/preproc_data/preprocessed_", filename, sep = ""), row.names = F)
}
