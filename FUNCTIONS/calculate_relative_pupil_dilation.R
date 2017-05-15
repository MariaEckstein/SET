calculate_relative_pupil_dilation = function(data = subj_file, baseline_window_length = 200, bin_duration = 50, baseline_prev_trial = F) {
  
  # Prepare baseline window
  row_shifts = ceiling(baseline_window_length/bin_duration)
  next_baseline = NA
  
  # For each trial, subtract pre-trial baseline
  for (subj in unique(data$Subject)) {
    for (trial in unique(data$TrialId)) {
      
      if (!baseline_prev_trial) { # Use baseline of the beginning of the same trial
        
        # Get pupil dilation of the trial
        trial_pupil = subset(data, Subject == subj & TrialId == trial)$roll_pupil
        
        # Get baseline dilation for this trial
        baseline = mean(trial_pupil[1:row_shifts], na.rm = T)
        # If no successful measurements during baseline window, use later measurements
        if (is.na(baseline)) {
          baseline = mean(trial_pupil[row_shifts:2*row_shifts], na.rm = T)
        }
        if (is.na(baseline)) {
          baseline = mean(trial_pupil[2*row_shifts:3*row_shifts], na.rm = T)
        }
        if (is.na(baseline)) {
          baseline = mean(trial_pupil[3*row_shifts:4*row_shifts], na.rm = T)
        }
        
        # Subtract baseline dilation from each data point during the trial
        data$rel_pupil[data$Subject == subj & data$TrialId == trial] = trial_pupil - baseline
        
      } else { # Use baseline from the end of the last trial
      
        # Get pupil for this trial and baseline window for the next
        trial_pupil = subset(data, Subject == subj & TrialId == trial)$roll_pupil
        base_window = c(rep(NA, length(trial_pupil) - row_shifts), rep("baseline", row_shifts))  # all rows are NA except for the last 4, which are "baseline" and indicate that these rows are the baseline for the next trial
        
        # For the very first trial, or when there was no baseline data in the previous trial, use the first 200ms as a baseline
        if (trial == unique(data$TrialId)[1] | is.na(next_baseline)) {
          next_baseline = mean(trial_pupil[1:row_shifts], na.rm = T)
        }
        
        # Subtract baseline dilation from each data point during the trial
        data$rel_pupil[data$Subject == subj & data$TrialId == trial] = trial_pupil - next_baseline
        
        # Get baseline dilation for the next trial
        next_baseline = mean(trial_pupil[base_window == "baseline"], na.rm = T)
      }
    }
  }
  
  return(data)
}
