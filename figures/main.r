setwd('~/Documents/GitHub/bc/frontiers2017/figures')
source('data_description.r')

data_location <- "/Users/briancohn/Resilio Sync/data/realTimeData2017_08_16_13_23_42.txt"
run_all_figures <- function(data_path){
	message('Running all figures')
	data_description_analysis(data_path)
}
run_all_figures(data_location)
