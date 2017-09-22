SET_trials = read.csv("C:/Users/Maria/Dropbox/SeqSETData/lateralizedSET/stimuli3set.csv")
SET_trials = SET_trials[1:160,1:11]

file_dir = "C:/Users/maria/MEGAsync/Berkeley/R scripts/sequentialset/subj_files/data2016/raw_data"

filenames = list.files(file_dir, pattern = "*.csv")

### Set up the loop over all files
for (filename in filenames) {
  subj_file = vector()
  subj_file = read.csv(file = file.path(file_dir, filename), header = T, na.strings = c("", "NA", "-1"),
                       colClasses = c(rep("numeric", 24), rep("factor", 2), rep("numeric", 2), rep("factor", 8)))
  
  subj_file_m = merge(subj_file, subset(SET_trials, select = c("Shape1", "Shape2", "Shape3", "lateralization")), by = c("Shape1", "Shape2", "Shape3"))
  
  subj_file_shapes = subset(subj_file_m, select = c("Shape1", "Shape2", "Shape3"))
  subj_file_rest   = subset(subj_file_m, select = -(1:3))
  
  subj_file_complete = cbind(subj_file_rest, subj_file_shapes)
  
#   subj_file$block = 4
#   subj_file$block[subj_file$TrialId <= 125] = 3
#   subj_file$block[subj_file$TrialId <=  83] = 2
#   subj_file$block[subj_file$TrialId <=  41] = 1
  
#   for (lat in c("MM", "LL", "RR", "RL", "LR")) {
#     for (blocki in 1:4) {
#       shapes = subset(SET_trials, lateralization == lat & BLOCK == block, select = c(shape1, shape2, shape3))
#       for (row in 1:nrow(shapes)) {
#         shapes_r = shapes[row,]
#         subj_file$lateralization[as.character(subj_file$Shape1) == as.character(shapes_r[[1]]) &
#                                  as.character(subj_file$Shape2) == as.character(shapes_r[[2]]) &
#                                  as.character(subj_file$Shape3) == as.character(shapes_r[[3]])] = lat#& subj_file$block == blocki] 
#                                  
#       }
#     }
#   }
  write.csv(subj_file_complete, paste(file_dir, '/', strsplit(filename, '.csv'), '_withlat.csv', sep = ''), row.names = F)
}
    
