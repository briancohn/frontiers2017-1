library(ggplot2)

plotting_specifics_for_force_control <- function(forces,
                                                 ylim,
                                                 ticks = c(0, 400, 800, 1200, 1600,2000,2400,2800,3200)){
  plot.new()
  plot.window(xlim=range(ticks),ylim=ylim)
  axis(1, at = ticks)
  axis(2);
  box()
  adept_x_val <- forces[[5]][1, 'adept_x']
  adept_y_val <- forces[[5]][1, 'adept_y']
  title(main="Forces 5-8 in Posture #1", sub=paste("Adept x:",adept_x_val, "Adept y:", adept_y_val), 
        xlab="Time (ms)", ylab="Force (N)")
  
}

plot_muscle_forces_over_time <- function(forces, minimum_tendon_force, maximum_tendon_force, indices_of_interest) {
  plotting_specifics_for_force_control(forces, ylim=c(minimum_tendon_force, maximum_tendon_force))
  lapply(list(do.call('rbind',forces[indices_of_interest])),
         function(force_ts) {
           plot_tendon_rise_time_curves(force_ts, tendon_of_interest_string=c("M0", "M1", "M2","M3", "M4", "M5", "M6"), ylim=c(minimum_tendon_force,maximum_tendon_force))
         }
  )
  return(0)
}

plot_JR3_forces_over_time <- function(forces, minimum_tendon_force, maximum_tendon_force) {
  #TODO get ts data from data_path instead of economics_long. Color by force + dotted for torques

  sample_of_forces <- forces[5:8]
  plotting_specifics_for_force_control(forces, ylim=c(-3.75,1), ticks = seq(0,4*800, by=400))
  smoothed_curves <- lapply(list(do.call('rbind', sample_of_forces)), plot_force_smoothed_curves)
  return(0)
}

wrench_vector_to_labeled_vals <- function(wrench_vector){
  return(paste(
        "FX =",
        wrench_vector[1],
        ", ",
        "FY =",
        wrench_vector[2],
        ", ",
        "FZ =",
        wrench_vector[3],
        ", ",
        "MX =",
        wrench_vector[4],
        ", ",
        "MY =",
        wrench_vector[5],
        ", ",
        "MZ =",
        wrench_vector[6]

                ))
}


compose_dataframe_of_first_three_muscle_activation_patterns <- function(forces){
  muscle_activation_patterns <- rbind(
    unique(forces$reference_M0),
    unique(forces$reference_M1),
    unique(forces$reference_M2),
    unique(forces$reference_M3),
    unique(forces$reference_M4),
    unique(forces$reference_M5),
    unique(forces$reference_M6)
  )
  row.names(muscle_activation_patterns) <- muscle_names
  colnames(muscle_activation_patterns) <- LETTERS[1:length(unique(forces$reference_M6))]
  # transpose so the rows are A B and C.
  return(t(muscle_activation_patterns))
}


plot_wrench_text <- function(wrench, x=0,y=-10, add_SD=FALSE){
  wrench_text <- wrench_vector_to_labeled_vals(wrench)
  if (add_SD){
    wrench_text <- paste("SD :", wrench_text)
  }
  text(x,y, wrench_text, cex=0.70)
}

plot_wrench_SD_text <- function(wrench_SD_vector, x=0,y=10){
  plot_wrench_text(wrench_SD_vector, x, y, add_SD=TRUE)
}


library(MASS)
generate_parcoord_plot <- function(dataframe_of_observations){
  require(GGally)
  require(ggplot2)
  p_raw <- ggparcoord(dataframe_of_observations, scale='globalminmax', alpha=1, boxplot=FALSE, mapping=ggplot2::aes(colour="midnightblue"))
  p <- p_raw + theme_bw() + theme(
    panel.grid.major.x = element_line(color = "black", size = 0.5),
    panel.grid.major = element_blank(),
    legend.position = "none"
  ) + ylab("Tendon Force (N)") + xlab("Tendon") + ylim(c(0,20))
  return(p)
}














norm_vec <- function(x) sqrt(sum(x^2))
plot_JR3_endpoint_force_vectors <- function(list_of_4_wrenches, list_of_SD_for_4_wrenches, xlim=c(-5,5), ylim=c(-5,5)) {
  wrench_a <- list_of_4_wrenches[[1]]
  wrench_b <- list_of_4_wrenches[[2]]
  wrench_c <- list_of_4_wrenches[[3]]
  wrench_d <- list_of_4_wrenches[[4]]
  
  wrench_a_SD <- list_of_SD_for_4_wrenches[[1]]
  wrench_b_SD <- list_of_SD_for_4_wrenches[[2]]
  wrench_c_SD <- list_of_SD_for_4_wrenches[[3]]
  wrench_d_SD <- list_of_SD_for_4_wrenches[[4]]
  
  
  par(mfrow=c(1,4))
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_a = ", norm_vec(wrench_a)), xlab=paste("Fx is ", wrench_a[1]), ylab=paste("Fy is ", wrench_a[2]), asp=1)
  segments(0,0,wrench_a[1], wrench_a[2])
  plot_wrench_text(wrench_a)
  plot_wrench_SD_text(wrench_a_SD)
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_b = ", norm_vec(wrench_b)), xlab=paste("Fx is ", wrench_b[1]), ylab=paste("Fy is ", wrench_b[2]), asp=1)
  segments(0,0,wrench_b[1], wrench_b[2]) 
  plot_wrench_text(wrench_b)
  plot_wrench_SD_text(wrench_b_SD)
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_b = ", norm_vec(wrench_c)), xlab=paste("Fx is ", wrench_c[1]), ylab=paste("Fy is ", wrench_c[2]), asp=1)
  segments(0,0,wrench_c[1], wrench_c[2]) 
  plot_wrench_text(wrench_c)
  plot_wrench_SD_text(wrench_c_SD)
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_b = ", norm_vec(wrench_c)), xlab=paste("Fx is ", wrench_c[1]), ylab=paste("Fy is ", wrench_c[2]), asp=1)
  segments(0,0,wrench_d[1], wrench_d[2]) 
  plot_wrench_text(wrench_d)
  plot_wrench_SD_text(wrench_d_SD)
}




data_description_analysis <- function(first_data_chunk, minimum_tendon_force, maximum_tendon_force){
  indices_of_interest <- 5:8
  
  # Prep data for parcoord
  force_samples <- first_data_chunk[first_data_chunk$reference_M0==unique(first_data_chunk$reference_M0)[indices_of_interest],]
  plot(generate_parcoord_plot(compose_dataframe_of_first_three_muscle_activation_patterns(force_samples)))
  
  postures <- split_by_position(first_data_chunk$adept_x, first_data_chunk)
  forces <- unlist(lapply(postures, split_by_reference_force), recursive=FALSE)
  
  plot(plot_muscle_forces_over_time(forces, minimum_tendon_force, maximum_tendon_force, indices_of_interest))
  plot(plot_JR3_forces_over_time(forces, minimum_tendon_force, maximum_tendon_force))
  
  #Get the mean of the last 100 force values for each of the force signals, for the 5th through 8th muscle activation patterns.
  list_of_tail_wrench_mean <- lapply(forces[indices_of_interest], function(x){
                                                                colMeans(
                                                                  tail(x[force_column_names],100)
                                                                  )
                                                              })
  list_of_tail_wrench_SD <- lapply(forces[indices_of_interest], function(x) {
    apply(
      tail(x[force_column_names],100)
      , 2, sd)
  })
  
  
  list_of_4_wrenches <- lapply(list_of_tail_wrench_mean, as.numeric)
  list_of_SD_for_4_wrenches <- lapply(list_of_tail_wrench_SD, as.numeric)
  plot_JR3_endpoint_force_vectors(list_of_4_wrenches, list_of_SD_for_4_wrenches)

}



