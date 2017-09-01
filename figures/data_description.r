library(ggplot2)
plot_muscle_forces_over_time <- function(data_path) {
  browser()
  #TODO get ts data from data_path instead of economics_long. Color by muscle number.
  p <- ggplot(economics_long, aes(date, value01, colour = variable), xlab="time (ms)") +
    geom_line()
  return(p)
}

plot_JR3_forces_over_time <- function(data_path) {
  #TODO get ts data from data_path instead of economics_long. Color by force + dotted for torques
  p <- ggplot(economics_long, aes(date, value01, colour = variable), xlab="time (ms)") +
    geom_line()
  return(p)
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


compose_dataframe_of_first_three_muscle_activation_patterns <- function(first_three_forces){
  first_three_muscle_activation_patterns <- rbind(
    unique(first_three_forces$reference_M0),
    unique(first_three_forces$reference_M1),
    unique(first_three_forces$reference_M2),
    unique(first_three_forces$reference_M3),
    unique(first_three_forces$reference_M4),
    unique(first_three_forces$reference_M5),
    unique(first_three_forces$reference_M6)
  )
  row.names(first_three_muscle_activation_patterns) <- muscle_names
  colnames(first_three_muscle_activation_patterns) <- c("A","B","C")
  return(t(first_three_muscle_activation_patterns))
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

norm_vec <- function(x) sqrt(sum(x^2))
plot_JR3_endpoint_force_vectors <- function(list_of_3_wrenches, list_of_SD_for_3_wrenches, xlim=c(-10,10), ylim=c(-10,10)) {
  wrench_a <- list_of_3_wrenches[[1]]
  wrench_b <- list_of_3_wrenches[[2]]
  wrench_c <- list_of_3_wrenches[[3]]
  
  wrench_a_SD <- list_of_SD_for_3_wrenches[[1]]
  wrench_b_SD <- list_of_SD_for_3_wrenches[[2]]
  wrench_c_SD <- list_of_SD_for_3_wrenches[[3]]
  
  
  par(mfrow=c(1,3))
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_a = ", norm_vec(wrench_a)), xlab=paste("Fx is ", wrench_a[1]), ylab=paste("Fy is ", wrench_a[2]), asp=1)
  #1 and 2 are Fx and Fy in wrench vector
  segments(0,0,wrench_a[1], wrench_a[2])
  plot_wrench_text(wrench_a)
  plot_wrench_SD_text(wrench_a_SD)
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_b = ", norm_vec(wrench_b)), xlab=paste("Fx is ", wrench_b[1]), ylab=paste("Fy is ", wrench_b[2]), asp=1)
  #1 and 2 are Fx and Fy in wrench vector
  segments(0,0,wrench_b[1], wrench_b[2]) 
  plot_wrench_text(wrench_b)
  plot_wrench_SD_text(wrench_b_SD)
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste("Norm of w_b = ", norm_vec(wrench_c)), xlab=paste("Fx is ", wrench_c[1]), ylab=paste("Fy is ", wrench_c[2]), asp=1)
  #1 and 2 are Fx and Fy in wrench vector
  segments(0,0,wrench_c[1], wrench_c[2]) 
  plot_wrench_text(wrench_c)
  plot_wrench_SD_text(wrench_c_SD)
  
}




data_description_analysis <- function(data_path){
  plot(plot_muscle_forces_over_time(data_path))
  plot(plot_JR3_forces_over_time(data_path))
  #TODO get list of wrenches from data
  list_of_3_wrenches <- list(c(4.10,2.24,1,0,0,0), c(-7.10,2.24,1,0,0,0), c(-6.10,-7.7,1,0,0,0))
  list_of_SD_for_3_wrenches <- list(c(0.5,0.5,0.5,0.5,0.5), c(0.5,0.5,0.5,0.5,0.5), c(0.5,0.5,0.5,0.5,0.5))
  plot_JR3_endpoint_force_vectors(list_of_3_wrenches, list_of_SD_for_3_wrenches)
}



