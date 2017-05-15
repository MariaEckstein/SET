bootlme = function(formula, random, data, indices) {
  
  # fit = lme(as.formula(formula), as.formula(random), data = data[indices,], method = "ML")
  # return(fit$coefficients$fixed)
  fit = try(lme(as.formula(formula), as.formula(random), data = data[indices,], method = "ML"), silent = T)
  if (class(fit) == "fit-error") {
    return(fit)
  } else {
    return(fit$coefficients$fixed)
  }
  
  # object <- try(glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay +
  #                       Experience + (1 | NewID), data = bigdata, subset = Replicate == i, family = binomial,
  #                     nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  # if (class(object) == "try-error")
  #   return(object)
  # c(fixef(object), getME(object, "theta"))
  
}
