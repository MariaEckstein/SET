get_pupilpatterngroups = function(pupildat, perf_data) {
  
  ### Get mean pupil dilation during items 2 & 3 for each subject (in SETs only; separately for each block)
  pupil_items23 = subset(pupildat, rel.time >= 2.5 & rel.time < 6 & set == "SET" & ACC == 1)  # USED TO BE 1.5 - 4.5!!!!!
  pupil_items23 = ddply(pupil_items23, .(block, Subject, set, bigspan, rel.time), summarize,
                        rel.pupil = mean(rel.pupil, na.rm = T))
  
  ### Put subjects into the two groups based on their pupil dilation during items 2 & 3
  # feature-encoding: 0span < 1&2span < 3span & sig. t-tests 1&2 vs 3 & 0 vs 3 (why the latter??)
  # relation-encoding: 1&2span > 0span + 3span & sig. t-tests 1&2 vs 3 & 0 vs 1&2
  ttests = data.frame(Subject = NA, block = NA, mean0 = NA, mean12 = NA, mean3 = NA, p0_12 = NA, p0_3 = NA, p12_3 = NA)
  for (subj in unique(pupil_items23$Subject)) {
    for (blo in c(1, 2)) {
      if (!(subj == 15 & blo == 2) & !(subj == 36)) {                            # Block 2 by subject 15 is excluded because there is almost no data
        subj_items23 = subset(pupil_items23, Subject == subj & block == blo)
        
        # Get mean pupil dilation for 0span items2&3, 1&2span items2&3, and 3span items2&3
        means = ddply(subj_items23, .(bigspan), summarize, item_pupil = mean(rel.pupil, na.rm = T))
        
        # Calculate pairwise t-tests on the pupil dilation for items2&3 between 0span, 1&2span, and 3span
        ps = data.frame(pairwise.t.test(subj_items23$rel.pupil, subj_items23$bigspan, p.adjust.method = "none", pool.sd = F, paired = T)$p.value)
        # t.test(subset(subj_items23, bigspan == "3span")$rel.pupil, subset(subj_items23, bigspan == "12span")$rel.pupil, paired = T)
        
        # Add means and p-values to the ttests dataframe
        subj_ttests = data.frame(Subject = subj, block = blo,
                                 mean0 = means$item_pupil[1], mean12 = means$item_pupil[2], mean3 = means$item_pupil[3],
                                 p0_12 = ps$X0span[1], p0_3 = ps$X0span[2], p12_3 = ps$X12span[2])
        ttests = rbind(ttests, subj_ttests)
      }
    }
  }
  
  ttests = ttests[-1,]
  
  ### Get subject IDs for each block that fulfill feature-encoding requirements and relation-encoding requirements
  feat_subj_b1 = subset(ttests, block == 1 & mean0 < mean12 & mean12 < mean3 & p12_3 < 0.05 & p0_3 < 0.05)$Subject
  rel_subj_b1  = subset(ttests, block == 1 & mean12 > mean0 & mean12 > mean3 & p12_3 < 0.05 & p0_12 < 0.05)$Subject
  feat_subj_b2 = subset(ttests, block == 2 & mean0 < mean12 & mean12 < mean3 & p12_3 < 0.05 & p0_3 < 0.05)$Subject
  rel_subj_b2  = subset(ttests, block == 2 & mean12 > mean0 & mean12 > mean3 & p12_3 < 0.05 & p0_12 < 0.05)$Subject
  
  ### Add the found pupilpatterngroup to the pupildat data
  pupildat$pupilpatterngroup = "Neither"
  pupildat$pupilpatterngroup[pupildat$block == 1 & pupildat$Subject %in% feat_subj_b1] = "Feature-encoding"
  pupildat$pupilpatterngroup[pupildat$block == 2 & pupildat$Subject %in% feat_subj_b2] = "Feature-encoding"
  pupildat$pupilpatterngroup[pupildat$block == 1 & pupildat$Subject %in% rel_subj_b1] = "Relation-encoding"
  pupildat$pupilpatterngroup[pupildat$block == 2 & pupildat$Subject %in% rel_subj_b2] = "Relation-encoding"
  
  perf_data$pupilpatterngroup = "Neither"
  perf_data$pupilpatterngroup[perf_data$block == 1 & perf_data$Subject %in% feat_subj_b1] = "Feature-encoding"
  perf_data$pupilpatterngroup[perf_data$block == 2 & perf_data$Subject %in% feat_subj_b2] = "Feature-encoding"
  perf_data$pupilpatterngroup[perf_data$block == 1 & perf_data$Subject %in% rel_subj_b1] = "Relation-encoding"
  perf_data$pupilpatterngroup[perf_data$block == 2 & perf_data$Subject %in% rel_subj_b2] = "Relation-encoding"
  
  return(list(pupildat,
              pupil_items23,
              ttests,
              perf_data,
              data.frame(feat_subj_b1[1:30], feat_subj_b2[1:30], rel_subj_b1[1:30], rel_subj_b2[1:30])))
}
