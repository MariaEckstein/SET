add_fixations_to_previous_trial = function(data) {
  
  ### Remove the very first fixation of each subject (because it happens BEFORE the first trial and is therefore irrelevant)
  data_without_first_fixation = data[1,]
  
  for (subj in unique(data$Subject)) {
    subj_dat = subset(data, Subject == subj)
    subj_rows = 1:nrow(subj_dat)
    first_keep_row = subj_rows[!subj_dat$CurrentObject == "Fixation" & !is.na(subj_dat$CurrentObject)][1]    # Finds the end of the very first fixation period = the first row to keep
    
    subj_dat_without_first_fixation = subj_dat[first_keep_row:nrow(subj_dat),]
    data_without_first_fixation = rbind(data_without_first_fixation, subj_dat_without_first_fixation)
  }
  
  data_without_first_fixation = data_without_first_fixation[-1,]
  data = data_without_first_fixation
  
  ### Get CurrentObject column right (it is always empty in the first trials of fixation in a new trial and that creates problems because rows' TrialIds are not changed when they are empty on CurrentObject)
  data_rows = 1:nrow(data)
  CurrentObject_empty = data_rows[is.na(data$CurrentObject)]                  # Row numbers, in which CurrentObject is empty
  
  for (row in CurrentObject_empty) {
    # if (data$CurrentObject[row-1] == "FourthItem" | data$CurrentObject[row-2] == "FourthItem" | data$CurrentObject[row-3] == "FourthItem" | data$CurrentObject[row-4] == "FourthItem") {
      data$CurrentObject[row] = "Fixation"                                    # Fill the empty space in CurrentObject with "Fixation" if it should be "Fixation"
    # }
  }
  
  ### Replace all columns during fixation with what is indicated 4 second earlier, i.e., during the trial preceding fixation (with a sampling rate of 60Hz, 3.3sec = 200rows)
  data_rows = 1:nrow(data)
  fixation_rows = data_rows[data$CurrentObject == "Fixation"]
  
  data$TrialId[fixation_rows] = data$TrialId[fixation_rows - 200]
  data$CRESP[fixation_rows] = data$CRESP[fixation_rows - 200]
  data$RESP[fixation_rows] = data$RESP[fixation_rows - 200]
  data$ACC[fixation_rows] = data$ACC[fixation_rows - 200]
  data$RT[fixation_rows] = data$RT[fixation_rows - 200]
  data$Category[fixation_rows] = data$Category[fixation_rows - 200]
  data$SETornoSET[fixation_rows] = data$SETornoSET[fixation_rows - 200]
  data$Position_false_shape[fixation_rows] = data$Position_false_shape[fixation_rows - 200]
  
  return(data)
}
