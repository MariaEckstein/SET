super_quick_calculate_percent_valid_data = function(filedir = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/subj_files/data2015/raw_data") {
  
  filenames = list.files(filedir, pattern = "*.csv")
  
  valid_pupil_percent = data.frame(left = NA, right = NA, both = NA, subjID = NA)
  
  row = 1
  for (filename in filenames) {
    
    dat = read.csv(paste(filedir, "/", filename, sep = ""))
    
    subji = dat[1, "Subject"]
    
    nrow_dat = nrow(dat[!is.na(dat$ValidityLeftEye),])
    sum_invalid_left = sum(dat$ValidityLeftEye[!is.na(dat$ValidityLeftEye)] == 4)
    sum_invalid_right = sum(dat$ValidityRightEye[!is.na(dat$ValidityRightEye)] == 4)
    
    percent_left = (nrow_dat - sum_invalid_left) / nrow_dat
    percent_right = (nrow_dat - sum_invalid_right) / nrow_dat
    percent_both = (percent_left + percent_right)/2
    
    valid_pupil_percent[row, "left"] = percent_left
    valid_pupil_percent[row, "right"] = percent_right
    valid_pupil_percent[row, "both"] = percent_both
    valid_pupil_percent[row, "subjID"] = subji
    
    row = row + 1
    
    print(valid_pupil_percent)
  }
}
