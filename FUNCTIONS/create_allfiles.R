create_allfiles = function(inputfile_directory = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/subj_files/", 
                           outputfile_directory = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/",
                           data_year) {
  
  
  #################
  ### Debugging ###
  #################
  
  # inputfile_directory = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/subj_files/"
  # outputfile_directory = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/"
  # data_year = "learn"
  
  
  ############
  ### Code ###
  ############
  
  ### Find the files
  filenames = list.files(inputfile_directory, pattern = "*.csv")
  
  ### Prepare allfiles dataframe
  allfiles = read.csv(file.path(inputfile_directory, filenames[1]), header = T, nrow = 1)[0,]
  
  ### Read in each data frame and attach it to allfiles
  for (filename in filenames) {
    subj_file = read.csv(file.path(inputfile_directory, filename), header = T)
    # subj_file$CRESP = subj_file$RESP = subj_file$Position_false_shape = NULL
    # colnames(subj_file)[colnames(subj_file) == "Category"] = "span"
    # colnames(subj_file)[colnames(subj_file) == "SETornoSET"] = "set"
    # colnames(subj_file)[colnames(subj_file) == "Category"] = "span"
    # subj_file$exp_feature = subj_file$rules_broken = subj_file$rule = subj_file$oddball = NA
    # 
    # write.csv(subj_file, file.path(inputfile_directory, filename), row.names = F)
    allfiles = rbind(allfiles, subj_file)
  }
  
  ### Add block column
  if (data_year %in% c(2013, 2015, "both")) {
    allfiles$block[allfiles$TrialId <= 41] = 1
    allfiles$block[allfiles$TrialId >= 44] = 2
  } else if (data_year == 2016) {
    ## randomly cut into 5 blocks of 60 trials
    allfiles$block = 5
    allfiles$block[allfiles$TrialId <= 285*4/5] = 4
    allfiles$block[allfiles$TrialId <= 285*3/5] = 3
    allfiles$block[allfiles$TrialId <= 285*2/5] = 2
    allfiles$block[allfiles$TrialId <= 285*1/5] = 1
  }
  
  write.csv(allfiles, paste(outputfile_directory, "preprocessed_allfiles_", data_year, ".csv", sep = ""), row.names = F)
  return(allfiles)
}
