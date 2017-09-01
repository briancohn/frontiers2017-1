setwd('~/Documents/GitHub/bc/frontiers2017/figures')
source('data_description.r')
muscle_names = c("M0", "M1", "M2","M3","M4","M5","M6")
data_location <- "/Users/briancohn/Resilio Sync/data/realTimeData2017_08_16_13_23_42.txt"

#Only take first part of data to create data description figure
first_data_chunk <- read.csv(data_location, nrows=60e3, header=TRUE)
first_three_forces_df <- first_data_chunk[first_data_chunk$reference_M0==unique(first_data_chunk$reference_M0)[2:4],]
first_three_muscle_activation_patterns <- compose_dataframe_of_first_three_muscle_activation_patterns(first_three_forces_df)

data_description_analysis(first_data_chunk)