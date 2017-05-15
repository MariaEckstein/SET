calculate_r_from_t = function(t, df) {
  r <- sqrt((t^2) / (t^2 + df))
  d <- (t * 2) / sqrt(df)
  return(r)
}
