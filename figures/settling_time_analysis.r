settling_time_analysis <- function(data_location){
  force_posture_timestamps <- get_timestamps_for_all_forces(data_location) #returns list(c(rownumber_start, rownumber_finish), ...)
  settling_time_df <- compute_settling_times_across_all_force_trials(data_location, force_posture_timestamps)
  plot(settling_time_df$delta_tension, settling_time_df$settling_time, xlab="Change in tension requested (N)", ylab="Settling Time (ms)")
}


compute_settling_times_across_all_force_trials <- function(data_location, force_posture_timestamps){
  lapply(force_posture_timestamps, function(row_range){
    force_timeseries <- read.csv(data_location, nrows=row_range[2] - row_range[1], skip=row_range[1])
    
  })
  settling_time_df$delta_tension <- settling_time_df$final_tension - settling_time_df$initial_tension
  return(settling_time_df)
}

get_timestamps_for_all_forces <- function(data_location){
  browser()
}