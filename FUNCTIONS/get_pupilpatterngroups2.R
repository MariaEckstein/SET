get_pupilpatterngroups2 = function(pupildat, perf_data) {
  
  ### Get mean pupil dilation during items 2 & 3 for each subject (in SETs only; separately for each block)
  # Add column "item" (2, 3, or 4 for items 2, 3, and 4)
  allfiles$item = NA
  allfiles$item[allfiles$bin_time >= 2.5 & allfiles$bin_time < 3] = 2
  allfiles$item[allfiles$bin_time >= 4 & allfiles$bin_time < 4.5] = 3
  allfiles$item[allfiles$bin_time >= 5.5 & allfiles$bin_time < 6] = 4
  # Add column "block" (1 or 2)
  allfiles$block = 1
  allfiles$block[allfiles$TrialId >= 44] = 2
  # Add column "bigspan"
  allfiles$bigspan = as.character(allfiles$span)
  allfiles$bigspan[allfiles$span %in% c("1span", "2span")] = "12span"
  allfiles$bigspan = factor(allfiles$bigspan)
  # ddply
  pupil_items234 = subset(allfiles, !is.na(item) & set == "SET" & ACC == 1)
#   pupil_items234 = ddply(pupil_items234, .(block, Subject, TrialId, set, bigspan, item), summarize,
#                         rel.pupil = mean(rel_pupil, na.rm = T),
#                         abs.pupil = mean(roll_pupil, na.rm = T))
  
  ### Put subjects into the two groups based on their pupil dilation during items 2, 3, and 4
  # feature-encoding: 1&2span < 3span & sig. t-tests 1&2 vs 3 (for all 3 items)
  # relation-encoding: 1&2span > 3span & sig. t-tests 1&2 vs 3 (for all 3 items)
  # ttests = data.frame(Subject = NA, block = NA, mean0 = NA, mean12 = NA, mean3 = NA, p0_12 = NA, p0_3 = NA, p12_3 = NA)
  ttests = array(NA, c(length(unique(allfiles$Subject)),    # rows (dim1): subjects; 
                       6,                                   # columns (dim2): results of means & ttests; 
                       2,                                   # depth (dim3): block (1, 2);
                       3))                                  # depth (dim4): items (2, 3, 4)
  strategy_groups = expand.grid(item = 2:4, block = c(1, 2), Subject = unique(allfiles$Subject))
  strategy_groups$strategy = NA
  subj_counter = 1
  for (subj in unique(pupil_items234$Subject)) {
    for (blo in c(1, 2)) {
      ite_counter  = 1
      for (ite in 2:4) {
        if (!(subj == 15 & blo == 2)) {       # (Block 2 by subject 15 is missing)
          # Get needed chunk of the data
          subj_items234 = subset(pupil_items234, Subject == subj & block == blo & item == ite & bigspan %in% c("12span", "3span"))
          
          # Calculate t-tests on the pupil dilation between 1&2span and 3span
          subj_ttest = t.test(subj_items234$rel_pupil ~ subj_items234$bigspan)
          mean_12 = subj_ttest$"estimate"[[1]]
          mean_3  = subj_ttest$"estimate"[[2]]
          p12_3   = subj_ttest$"p.value"
          
          # Determine subject's strategy
          subj_strat = 0      # "neither"
          if (mean_12 > mean_3 & p12_3 < 0.05) {
            subj_strat = 1    # "relation-encoding"
          } else if (mean_12 < mean_3 & p12_3 < 0.05) {
            subj_strat = -1   # "feature-encoding"
          }
          
          # Add strategy to strategy_groups dataframe
          strategy_groups[strategy_groups$Subject == subj & strategy_groups$block == blo & strategy_groups$item == ite,]$strategy = subj_strat
          
          # Insert means and results of the t-test into the array
          ttests[subj_counter, , blo, ite_counter] = c(Subject = subj, block = blo,
                                                       mean12 = mean_12, mean3 = mean_3,
                                                       p12_3 = p12_3, sig = F)
        }
        ite_counter = ite_counter + 1
      }
    }
    subj_counter = subj_counter + 1
  }
  
  strategy_groups_raw = strategy_groups
  strategy_groups = ddply(strategy_groups, .(Subject, block), summarize,
                          strategy = mean(strategy))
  strategy_groups$strategy_fac = "Neither"
  strategy_groups$strategy_fac[strategy_groups$strategy < 0] = "Feature-encoding"
  strategy_groups$strategy_fac[strategy_groups$strategy > 0] = "Relation-encoding"
  
  ### Get subject IDs for each block that fulfill feature-encoding requirements and relation-encoding requirements
  feat_subj_b1 = subset(strategy_groups, block == 1 & strategy_fac == "Feature-encoding")$Subject
  feat_subj_b2 = subset(strategy_groups, block == 2 & strategy_fac == "Feature-encoding")$Subject
  rel_subj_b1  = subset(strategy_groups, block == 1 & strategy_fac == "Relation-encoding")$Subject
  rel_subj_b2  = subset(strategy_groups, block == 2 & strategy_fac == "Relation-encoding")$Subject
  
  ### Add the found pupilpatterngroup to the pupildat data
  pupildat$pupilpatterngroup = "Neither"
  pupildat$pupilpatterngroup[pupildat$block == 1 & pupildat$Subject %in% feat_subj_b1] = "Feature-encoding"
  pupildat$pupilpatterngroup[pupildat$block == 2 & pupildat$Subject %in% feat_subj_b2] = "Feature-encoding"
  pupildat$pupilpatterngroup[pupildat$block == 1 & pupildat$Subject %in% rel_subj_b1] = "Relation-encoding"
  pupildat$pupilpatterngroup[pupildat$block == 2 & pupildat$Subject %in% rel_subj_b2] = "Relation-encoding"
  
  big_span_data_byblock$pupilpatterngroup = "Neither"
  big_span_data_byblock$pupilpatterngroup[big_span_data_byblock$block == 1 & big_span_data_byblock$Subject %in% feat_subj_b1] = "Feature-encoding"
  big_span_data_byblock$pupilpatterngroup[big_span_data_byblock$block == 2 & big_span_data_byblock$Subject %in% feat_subj_b2] = "Feature-encoding"
  big_span_data_byblock$pupilpatterngroup[big_span_data_byblock$block == 1 & big_span_data_byblock$Subject %in% rel_subj_b1] = "Relation-encoding"
  big_span_data_byblock$pupilpatterngroup[big_span_data_byblock$block == 2 & big_span_data_byblock$Subject %in% rel_subj_b2] = "Relation-encoding"
  
#   perf_data$pupilpatterngroup = "Neither"
#   perf_data$pupilpatterngroup[perf_data$block == 1 & perf_data$Subject %in% feat_subj_b1] = "Feature-encoding"
#   perf_data$pupilpatterngroup[perf_data$block == 2 & perf_data$Subject %in% feat_subj_b2] = "Feature-encoding"
#   perf_data$pupilpatterngroup[perf_data$block == 1 & perf_data$Subject %in% rel_subj_b1] = "Relation-encoding"
#   perf_data$pupilpatterngroup[perf_data$block == 2 & perf_data$Subject %in% rel_subj_b2] = "Relation-encoding"
  
  return(list(puildat, ttests, strategy_groups))
}
