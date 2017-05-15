start_each_trial_at_time_0 = function(data, m_trial_duration = 9) {
  
  # Subtract first raw_time of each trial from all the other trial raw_times to make it start from 0
  for (subj in unique(data$Subject)) {
    for (trial in unique(data$TrialId)) {
      
      # Find the Timestamp of the first data point in each trial
      trial_time = subset(data, Subject == subj & TrialId == trial)$bin_time
      start_time = trial_time[1]
      
      # Subtract the first Timestamp from all Timestamps in this trial    
      data$bin_time[data$Subject == subj & data$TrialId == trial] = trial_time - start_time
    }
  }
  
  # Get rid of eventual errors (all trials stop automatically after 10 sec)
  return(subset(data, bin_time < m_trial_duration))
}
