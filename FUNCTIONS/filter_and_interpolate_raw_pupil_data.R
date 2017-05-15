filter_and_interpolate_raw_pupil_data = function(file, output_dir, se = 5, numPoints = 80, max.interp = 12, min.percent.of.valid.data = 30) {
  
  # file.directory indicates the folder that contains all the files you want to filter and interpolate (must be .csv files) (put the directory into " " and use '/' instead of '\')
  # se indicates number of standard errors around loess regression function where pupil values are still accepted
  # numPoints indicates number of data points on both sides of the regressed point that are used to calculate the loess regression
  # max.interp: only gaps that are shorter than max.interp rows will be interpolated; gaps that are longer (where more consecutive rows of data are missing) will not be interpolated. With a sampling rate of 120 Hz (1 measurement each 8ms), 25 rows correspond to 200ms, our decided limit for interpolation; with a sampling rate of 60 Hz, 12 rows correspond to 200ms.
  # min.percent.of.valid.data indicates how many percent of data points in the raw file need to be valid in order for the script to execute; errors are likely to occur if less than 30% of the data are valid; good interpolation needs more than 50% (but decision may be made on trial-by-trial basis)
  
  
    ### START ###
    
  # Create new picture folder to put plots
  # dir.create(file.path(output_dir, "Plots"))
  
  # Prepare filter_stats (stats about how much data has been interpolated and filtered for each subject)
  filter_stats = data.frame(Subject = NA, initial_valid_percent = NA, filtered_valid_percent = NA, interpolated_valid_percent = NA)
  file_interpolated = cbind(file[1,], interp_av_pupil = NA, filtered_av_pupil = NA)
                                
  # Loop over subjects (uncomment corresponding lines to read in individual subject files)
  # filenames <- list.files(output_dir, pattern = "*.csv")
  for (subj in unique(file$Subject)) {
    raw.dat <- vector()
    raw.dat <- subset(file, Subject == subj)
    
    # filename.nocsv <- unlist(strsplit(file.name, split = '.', fixed = TRUE))[1]   #only the name of the file, without ending '.csv'
  
    # Replace invalid pupil data with "NA" (invalid: DiameterPupilXXXEye == -1 or ValidityXXXEye == 4)
    raw.dat$DiameterPupilLeftEye[raw.dat$DiameterPupilLeftEye == -1] <- NA
    raw.dat$DiameterPupilLeftEye[raw.dat$ValidityLeftEye == 4] <- NA
    raw.dat$DiameterPupilRightEye[raw.dat$DiameterPupilRightEye == -1] <- NA
    raw.dat$DiameterPupilRightEye[raw.dat$ValidityRightEye == 4] <- NA
    
    numb.of.val <- sum(!is.na(raw.dat$DiameterPupilLeftEye)) + sum(!is.na(raw.dat$DiameterPupilRightEye))    # number of valid measurements BEFORE interpolation
    valid.data <- numb.of.val / 2
    row.numb <- dim(raw.dat)[1]
    perc.of.val <- valid.data*100 / row.numb    # percent of valid measurements BEFORE interpolation
    
    # cat('In subject ', subj, ', ', numb.of.val, ' out of ', row.numb*2, ' data points ', '(', round(perc.of.val, 2), '%)', ' have valid values.\n', sep = '')
  
    # if less than 'min.percent.of.valid.data' data points have valid values, a copy of the file is made that indicates how many data points have valid values, but the two additional columns are not added; that means, the rest of the script does not execute
    if (perc.of.val < min.percent.of.valid.data) {
      
      print(paste("data only has ", perc.of.val, "% of valid data and will therefore not be preprocessed (minimum percentage for preprocessing: ", min.percent.of.valid.data, ")."), sep = "")
      # write.csv(raw.dat, file = paste(output_dir, "/", file.name, '.filtered_and_interpolated.', round(perc.of.val), '%.csv', sep = ''))
      
    } else {
    
      # This function takes the average of diameters or whichever eye has data if only one eye has data
      get.values <- function(dat) {
        rVal <- (dat$DiameterPupilLeftEye + dat$DiameterPupilRightEye) / 2
        rVal[is.na(rVal) & !is.na(dat$DiameterPupilLeftEye)] <- dat$DiameterPupilLeftEye[is.na(rVal) & !is.na(dat$DiameterPupilLeftEye)]
        rVal[is.na(rVal) & !is.na(dat$DiameterPupilRightEye)] <- dat$DiameterPupilRightEye[is.na(rVal) & !is.na(dat$DiameterPupilRightEye)]
        return(rVal)
      }
      
      # collector for cleaned pupil data
      filtered_av_pupil <- vector()
      interp_av_pupil <- vector()
      
      # split file into pieces of 8010 rows (blocksize); with overlap of 80 rows (numPoints)
      blocksize <- 8000
      
      for (block in seq(1, (nrow(raw.dat)), blocksize)) {
        block.dat <- data.frame()
  
        if ((block + blocksize) < nrow(raw.dat)) {
          block.dat <- raw.dat[block:(block + blocksize + 2 * numPoints),]
        } else {
          block.dat <- raw.dat[block:nrow(raw.dat),]
        }
  
        span <- numPoints / nrow(block.dat)     # parameter for loess function
        
        # get data values (x = ID, y = pupildiameter)
        dat <- data.frame()
        dat <- data.frame(X = block.dat$ID, Y = get.values(block.dat))
        
        val.percent <- sum(!is.na(dat$Y))/nrow(dat)
        # cat('In subject ', subj, ', block ', block, ': ', sum(!is.na(dat$Y)), ' out of ', dim(dat)[1], ' points (', round(val.percent * 100,2), '%) have valid values.\n', sep = '')
        
        # fit loess to data
        dat.lo <- loess(Y ~ X, dat, span = span)
        dat.fit <- predict(dat.lo, se = 1)
        
        # store fit values as well as upper and lower bounds
        fit <- rep(NA, dim(dat)[1])
        fit[!is.na(dat$Y)] <- dat.fit$fit
        lb <- rep(NA, dim(dat)[1])
        lb[!is.na(dat$Y)] <- dat.fit$fit - (dat.fit$se.fit * se)
        ub <- rep(NA, dim(dat)[1])
        ub[!is.na(dat$Y)] <- dat.fit$fit + (dat.fit$se.fit * se)
        
        # diagnostic plots and message
#         png(filename = paste(output_dir, "/Plots/subj", subj, "loess.rows", block, ".png", sep=''))
#         plot(dat$X, dat$Y, pch = '.', type = 'l')
#         lines(dat$X, fit, pch = '.', type = 'l', col = 'blue')
#         lines(dat$X, lb, pch = '.', type = 'l', col = 'red')
#         lines(dat$X, ub, pch = '.', type = 'l', col = 'red')
#         dev.off()
        
        passFilter <- vector()
        passFilter <- ifelse(is.na(dat$Y) | dat$Y < lb | dat$Y > ub, 0, 1)
        
#         png(filename = paste(output_dir, "/Plots/subj", subj, "filtered.rows", block, ".png", sep = ''))      
#         plot(dat$X, dat$Y, pch = 20, col = ifelse(passFilter == 0, 'red', 'black'),
#              main = paste('Subject', raw.dat[1, 'Subject'], ', data points', dat$X[1], ' - ', dat$X[length(dat$X)], sep = ' '))
#         dev.off()
        
        perc <- (sum(!is.na(dat$Y)) - sum(passFilter))/sum(!is.na(dat$Y))
        filtered_numb <- (sum(!is.na(dat$Y)) - sum(passFilter))
        # cat('In subject ', subj, ', block ', block, ': ', filtered_numb, ' out of ', sum(!is.na(dat$Y)), ' points (', round(perc * 100, 2), '%) have been filtered.\n', sep = '')
        
        # replace filtered values with NA (passFilter is 0 when points should be filtered); safe filtered pupil data to filtered_av_pupil
        dat$Y[!passFilter] <- NA
        
        if (block == 1) {
          if (blocksize > dim(raw.dat)[1]) {
            filtered_av_pupil <- c(filtered_av_pupil, dat$Y[1:dim(raw.dat)[1]])
          } else {
            filtered_av_pupil <- c(filtered_av_pupil, dat$Y[1:(blocksize + numPoints)])
        }}
        else if ((block + blocksize) < dim(raw.dat)[1] & block > 1) {
          filtered_av_pupil <- c(filtered_av_pupil, dat$Y[(numPoints + 1):(blocksize + numPoints)])
        }
        else if ((block + blocksize) > dim(raw.dat)[1]) {
          filtered_av_pupil <- c(filtered_av_pupil, dat$Y[(numPoints + 1):(dim(block.dat)[1])])
        }
        
        # replace missing values with 'loess' fit points
        Missinglist <- vector()
        Missinglist <- dat$X[is.na(dat$Y) & !is.na(dat$X)]
        dat.lo <- loess(Y ~ X, dat, span = span)
        missing.predict <- vector()
        missing.predict <- predict(dat.lo, data.frame(X = Missinglist))
        dat$Y[is.na(dat$Y) & !is.na(dat$X)] <- missing.predict
        
        # replace interpolated data points for missing sequences > 200ms (max.interp = 13 or 25, depending on sampling rate) with NA - if Missinglist contains more than 13 or 25 values at all
        if (length(Missinglist) > max.interp) {
          dat$drop <- rep(0, length(dat$X))           #vector collecting info about length of missing data
          for (i in 1:(length(Missinglist) - max.interp)) {
            if ((Missinglist[i] + max.interp) == (Missinglist[i + max.interp])) {
              for (j in 0:max.interp) {
                dat$drop[dat$X == Missinglist[i+j]] <- 1
              }
            }
          }
          dat$Y[dat$drop == 1] <- NA         #replace pupildata with NAs if to many missing values in a row
        }
  
        # plot interpolated points and message
        prozent <- (length(Missinglist) - sum(is.na(dat$Y)))/length(Missinglist)
        interpolated_numb <- (length(Missinglist) - sum(is.na(dat$Y)))
#         cat('In subject ', subj, ', block ', block, ': ', interpolated_numb, ' out of ', length(Missinglist), ' missing data points (', round(prozent*100, 2), '%) have been interpolated.\n', sep = '')
        
        
#         png(filename = paste(output_dir, "/Plots/subj", subj, "interpolated.rows", block, ".png", sep=''))      
#         plot(dat$X, dat$Y, pch = 20, col = ifelse(dat$X[!is.na(dat$X)] %in% Missinglist, 'forestgreen', 'black'),
#              main = paste('Subject', raw.dat[1, 'Subject'], ' (data points', dat$X[1], ' - ', dat$X[length(dat$X)],
#                           '): interpolated data points', sep=' '))
#         dev.off()
        
        # safe interpolated data into variable interp_av_pupil; notice overlap of 50 rows (numPoints)
        if (block == 1) {
          if (blocksize > dim(raw.dat)[1]) {
            interp_av_pupil <- c(interp_av_pupil, dat$Y[1:dim(raw.dat)[1]])
          } else {
            interp_av_pupil <- c(interp_av_pupil, dat$Y[1:(blocksize + numPoints)])
          }}
        else if ((block + blocksize) < dim(raw.dat)[1] & block > 1) {
          interp_av_pupil <- c(interp_av_pupil, dat$Y[(numPoints + 1):(blocksize + numPoints)])
        }
        else if ((block + blocksize) > dim(raw.dat)[1]) {
          interp_av_pupil <- c(interp_av_pupil, dat$Y[(numPoints + 1):(dim(block.dat)[1])])
        }
      }
      
      # attach cleaned data as new column to data
      raw.dat$interp_av_pupil <- interp_av_pupil
      raw.dat$filtered_av_pupil <- filtered_av_pupil
      
      file_interpolated = rbind(file_interpolated, raw.dat)
      
      # Create filter stats for each subject and add them together
      filter_stats_row = data.frame(Subject = subj,
                                    initial_valid_percent = perc.of.val,
                                    filtered_valid_percent = sum(!is.na(filtered_av_pupil))*100 / row.numb,
                                    interpolated_valid_percent = sum(!is.na(interp_av_pupil))*100 / row.numb)
      filter_stats = rbind(filter_stats, filter_stats_row)
      
      # cat('Subject', raw.dat$Subject[1], 'has an average resting pupil diameter of', mean(interp_av_pupil, na.rm = T), 'cm.\n')
    }
  }
  
  ## Finalize file and filter_stats (remove first row, which was initialized to NA's)
  file_interpolated = file_interpolated[2:nrow(file_interpolated),]
  filter_stats <- filter_stats[2:nrow(filter_stats),]
  
  ## Return file and filter_stats
  return(list(file_interpolated, filter_stats))
}
