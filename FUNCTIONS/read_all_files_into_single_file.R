read_all_files_into_single_file = function(file_dir) {
  
  ## Find subject data files
  filenames = list.files(file_dir, pattern = "*.csv")
  
  ## Put the first subject file into allfiles
  allfiles = read.csv(file = file.path(file_dir, filenames[1]), header = T,
                      colClasses = c(rep("numeric", 24), rep("factor", 2), rep("numeric", 2), rep("factor", 11)))
  
  ## Add all other subject files to allfiles
  for (file_name in filenames[2:length(filenames)]) {
    seqSET_file = vector()
    seqSET_file = read.csv(file = file.path(file_dir, file_name), header = T, na.strings = c("", "NA"),
                           colClasses = c(rep("numeric", 24), rep("factor", 2), rep("numeric", 2), rep("factor", 11)))
    
    allfiles = rbind(allfiles, seqSET_file)
  }
  
  ## Remove training trials
  allfiles = subset(allfiles, TrainorExp == "Exp")
  allfiles$SETornoSET = factor(allfiles$SETornoSET)
  allfiles$CurrentObject = factor(allfiles$CurrentObject, levels = c("FirstItem", "SecondItem", "ThirdItems", "FourthItem", "Fixation"))
  
  ## Keep only necessry columns
  keep_columns <- c("Subject", "ID", "TimestampSec", "TimestampMicrosec", "DiameterPupilLeftEye", "ValidityLeftEye", "DiameterPupilRightEye", "ValidityRightEye", "TrialId", "CRESP", "RESP", "ACC", "RT", "Category", "SETornoSET", "TrainorExp", "Position_false_shape", "CurrentObject")
  
  return(allfiles[,keep_columns])
}
