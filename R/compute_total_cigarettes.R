compute_total_cigarettes <- function(cig_per_day,
                                     age_started,
                                     age_quit){
  years_smoked <- age_quit - age_started
  total_cigs_yr <- cig_per_day * 365
  total_smoked <- total_cigs_yr * years_smoked
  return(total_smoked)
}
