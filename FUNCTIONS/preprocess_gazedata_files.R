preprocess_gazedata_files = function(inputfile_directory, input_filename_pattern, inputfile_format = ".gazedata", m_trial_duration = 6,
                           output_filename, outputfile_directory,
                           FUNCTION_dir = "C:/Users/Maria/MEGAsync/Berkeley/R scripts/sequentialset/FUNCTIONS",
                           filter_and_interpolate_max_interp = 12,
                           smooth_times = 3,
                           relative_pupil_baseline_window,
                           data_year = "NA",
                           mark_saccade_trials = F,
                           save_saccade_trials = F) {
  
  #################
  ### Debugging ###
  #################

# inputfile_directory = raw_file_dir
# input_filename_pattern = "lateralizedSET_"
# inputfile_format = ".gazedata"
# m_trial_duration = trial_duration
# output_filename = "latSET"
# outputfile_directory = preprocessing_output_dir
# FUNCTION_dir = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/FUNCTIONS"
# filter_and_interpolate_max_interp = 12
# smooth_times = 3
# relative_pupil_baseline_window = c(0, 0.2)
# data_year = data_year
# mark_saccade_trials = T
# save_saccade_trials = T
  
  
  ############
  ### Code ###
  ############
  
  ### Set up the loop over all files
  filenames = list.files(inputfile_directory, pattern = input_filename_pattern)
  filter_stats = data.frame(Subject = NA, initial_valid_percent = NA, filtered_valid_percent = NA, interpolated_valid_percent = NA, outside_fixations = NA, saccade_trials = NA)[0,]
  
  for (filename in filenames) {
    subj_file = vector()
    if (inputfile_format == ".csv") {
      subj_file = read.csv(file.path(inputfile_directory, filename), header = T, na.strings = c("", "NA", "-1"))
    } else {
      subj_file = read.csv(file.path(inputfile_directory, filename), header = T, na.strings = c("", "NA", "-1"), sep = "\t")
      filename = paste(output_filename, subj_file$Subject[1], ".csv", sep = "")
    }
    
    ### Add columns so that the datafiles of all studies have the same columns
    if (!"TrainorExp" %in% colnames(subj_file)) {
      subj_file$TrainorExp = "Exp"
    }
    if (!"lateralization" %in% colnames(subj_file)) {
      subj_file$lateralization = NA
    }
    if (!"Position_false_shape" %in% colnames(subj_file)) {
      subj_file$Position_false_shape = NA
    }
    if (!"number_of_rules_broken" %in% colnames(subj_file)) {
      subj_file$number_of_rules_broken = NA
    }
    if (!"rule_broken" %in% colnames(subj_file)) {
      subj_file$rule_broken = NA
    }
    
    ### Get sampling rate (can be either 60 or 120 in our Tobii)
    sampling_rate = 60
    time_points = subj_file$TimestampSec + subj_file$TimestampMicrosec / 1000000
    time_differences = diff(time_points)
    if (median(time_differences, na.rm = T) < 0.01) {
      sampling_rate = 120
    }
    
    ### Remove training trials (for the studies that have them)
    subj_file = subset(subj_file, TrainorExp == "Exp")
    
    ### Remove unnecessary columns
    if (mark_saccade_trials) {
      remove_columns = c("Session", "TETTime", "RTTime", "CursorX", "CursorY", "XCameraPosLeftEye", "YCameraPosLeftEye", "XCameraPosRightEye", "YCameraPosRightEye", "DistanceLeftEye", "DistanceRightEye", "TrainorExp")
    } else {
      remove_columns = c("Session", "TETTime", "RTTime", "CursorX", "CursorY", "XGazePosLeftEye", "YGazePosLeftEye", "XCameraPosLeftEye", "YCameraPosLeftEye", "XGazePosRightEye", "YGazePosRightEye", "XCameraPosRightEye", "YCameraPosRightEye", "DistanceLeftEye", "DistanceRightEye", "TrainorExp")
    }
    subj_file = subset(subj_file, select = colnames(subj_file)[!colnames(subj_file) %in% remove_columns])
    
    
    #########################################
    ### Filter and interpolate pupil data ###
    #########################################
    
    ### Create subj_file and filter_stats
    source(file.path(FUNCTION_dir, "filter_and_interpolate_raw_pupil_data.R"))
    subj_file_and_filterstats = filter_and_interpolate_raw_pupil_data(file = subj_file,
                                                                      output_dir = inputfile_directory,
                                                                      se = 5,
                                                                      numPoints = 80,
                                                                      max.interp = filter_and_interpolate_max_interp,
                                                                      min.percent.of.valid.data = 30)
    subj_file = subj_file_and_filterstats[[1]]
    subj_filter_stats = subj_file_and_filterstats[[2]]
    
    ### Remove more columns
    remove_columns = c("ID", "CRESP", "RESP", "DiameterPupilLeftEye", "ValidityLeftEye", "DiameterPupilRightEye", "ValidityRightEye")
    subj_file = subset(subj_file, select = colnames(subj_file)[!colnames(subj_file) %in% remove_columns])
    
    ### Add fixations to previous trial (only necessary for OLD SET version!!! [2014 data & subject 301 of the 2015 data])
    source(file.path(FUNCTION_dir, "add_fixations_to_previous_trial.R"))
    if (data_year == 2013) {
      subj_file = add_fixations_to_previous_trial(subj_file)
    }
    
    ### Exclude trials with less than 50% of valid pupil data
    # no
    
    ### Mark trials with fixations outside fixation cross
    # Get individual time_points with outside fixations
    subj_file$outside_fixations = F
    subj_file$outside_fixations[subj_file$XGazePosLeftEye  < 0.375] = T
    subj_file$outside_fixations[subj_file$XGazePosLeftEye  > 0.625] = T
    subj_file$outside_fixations[subj_file$XGazePosRightEye < 0.375] = T
    subj_file$outside_fixations[subj_file$XGazePosRightEye > 0.625] = T
    # Mark trials with more than 1 outside fixation during presentation phase (first 500msec)
    subj_file$saccade_trial = F
    for (trial in unique(subj_file$TrialId)) {
      trial_dat = subset(subj_file, TrialId == trial)
      stim_presentation_phase = trial_dat[1:(500*(sampling_rate/1000)),]
      if (sum(stim_presentation_phase$outside_fixations) > 2) {
        subj_file$saccade_trial[subj_file$TrialId == trial] = T
      }
    }
    # Save saccade trials as csv to look at later
    if (save_saccade_trials) {
      subj_saccade_trials = subset(subj_file, saccade_trial == T)
      if (nrow(subj_saccade_trials) > 0) {
        write.csv(subj_saccade_trials, paste(outputfile_directory, "preprocessing_data/saccade_trials_", subj_file$Subject[1], ".csv", sep = ""), row.names = F)
      }
    }
    # Save number of outside fixations and saccade trials to filer_stats data.frame
    subj_filter_stats$outside_fixations = sum(subj_file$outside_fixations)
    subj_filter_stats$saccade_trials = length(unique(subj_file$TrialId[subj_file$saccade_trial]))
    filter_stats = rbind(filter_stats, subj_filter_stats)
    # Remove eye position columns
    remove_columns = c("XGazePosLeftEye", "YGazePosLeftEye", "XGazePosRightEye", "YGazePosRightEye", "outside_fixations")
    subj_file = subset(subj_file, select = colnames(subj_file)[!colnames(subj_file) %in% remove_columns])
    
    ### Create rolling time bins
    source(file.path(FUNCTION_dir, "create_rolling_time_bins.R"))
    remove_columns = c("interp_av_pupil", "filtered_av_pupil", "TimestampSec", "TimestampMicrosec", "raw_time", "CurrentObject", "Shape1", "Shape2", "Shape3")
    subj_file = create_rolling_time_bins(data = subj_file, time_bin = 0.05, smooth_times = smooth_times,
                                         ddply_columns = colnames(subj_file)[!colnames(subj_file) %in% remove_columns])
    # subj_file$bin_time = as.numeric(as.character(subj_file$bin_time))
    
    ### Start each trial at time 0
    source(file.path(FUNCTION_dir, "start_each_trial_at_time_0.R"))
    subj_file = start_each_trial_at_time_0(subj_file, m_trial_duration = m_trial_duration)
    
    ### Calculate pupil dilation relative to baseline period
    source(file.path(FUNCTION_dir, "calculate_relative_pupil_dilation.R"))
    subj_file = calculate_relative_pupil_dilation(subj_file,
                                                  baseline_window_length = 1000*(relative_pupil_baseline_window[2]-relative_pupil_baseline_window[1]),
                                                  bin_duration = 50,
                                                  baseline_prev_trial = F)
    
    
    ###########################
    ### Save each subj_file ###
    ###########################
    
    ### Change column names (span, set, oddball, rules_broken, rule)
    colnames(subj_file)[colnames(subj_file) %in% c("Category", "SETornoSET", "number_of_rules_broken", "rule_broken", "Position_false_shape")] = c("span", "set", "rules_broken", "rule", "oddball")
    
    ### Save the file
    write.csv(subj_file, paste(outputfile_directory, "preprocessed_", filename, sep = ""), row.names = F)
  }
  
  ### Return filter_stats
  return(filter_stats)
}
