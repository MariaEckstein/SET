exclude_trials_with_too_much_invalid_data = function(file, trial_threshold = 50) {

  # trial_threshold indicates in percent how much valid each trial must at least have. For example, if trial_threshold = 50, all pupil data points from trials that have more than 50% NAs will be replaced with NAs. That means, data from those trials will not contribute to any subsequent analyses.
  
  
  ## 0 create new folders to put output files and plots
  # dir.create(file.path(file.directory, "outputfiles2"))   #for output files
  allfiles_removed = data.frame(file[1,], remove_trial = NA)
  remove_stats = data.frame(Subject = NA, removed_trials = NA)
  
  ## 1. read in files
  # filenames <- list.files(file.directory, pattern="*.csv")
  for (subj in unique(file$Subject)) {
    seqSET.file <- vector()
    seqSET.file = subset(file, Subject == subj)
    # seqSET.file <- read.csv(paste(file.directory, "/", file.name, sep=""))
    # filename.nocsv <- unlist(strsplit(file.name, split='.csv', fixed=TRUE))[1]   #filename.nocsv is only the name of the file, without ending '.csv'
    
  ## 2. calculate how much valid data there is in each trial
    seqSET.file$remove_trial <- seqSET.file$interp_av_pupil  # create new column in data file that will have invalid trials removed; start with all the data that interp_av_pupil has
    remove.counter <- 0   #counts how many trials' data points have been removed
    for (trial in unique(seqSET.file$TrialId)) {
      trial.dat <- subset(seqSET.file, TrialId == trial & TrainorExp == 'Exp' & CurrentObject != 'Fixation')$interp_av_pupil
      valid.points <- sum(!is.na(trial.dat)) / length(trial.dat) * 100
  
  ## 3. replace trials that have less than trial_threshold % of valid points with NA
      if (valid.points < trial_threshold) {
        seqSET.file$remove_trial[seqSET.file$TrialId == trial & seqSET.file$TrainorExp == 'Exp' & seqSET.file$CurrentObject != 'Fixation'] <- NA  # subsequently remove data from the column if trials don't have enough data
        # cat('Subject ', seqSET.file[1, 'Subject'], ' does not have enough data in trial ', trial, '. Data points of this trial will be removed.\n', sep='')
        remove.counter <- remove.counter + 1
      }
    }
    
    remove_stats_row = data.frame(Subject = subj, removed_trials = remove.counter)
    remove_stats = rbind(remove_stats, remove_stats_row)
    
    allfiles_removed = rbind(allfiles_removed, seqSET.file)
    
  # write.csv(seqSET.file, file=paste(file.directory, "/Outputfiles2/", filename.nocsv, '.', remove.counter, '_invalid_trials_removed.csv', sep=''))
  }
  ## Finalize allfiles_removed and remove_stats (remove first row, which was initialized to NA's)
  allfiles_removed = allfiles_removed[2:nrow(allfiles_removed),]
  remove_stats <- remove_stats[2:nrow(remove_stats),]
  
  ## Return allfiles and filter_stats
  return(list(allfiles_removed, remove_stats))
}
