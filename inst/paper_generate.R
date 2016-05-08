rm(list=ls(pos='.GlobalEnv',all=TRUE),pos='.GlobalEnv')
unloadNamespace('aira')

bootstrap_iterations <<- 200
bust <<- TRUE
# Set the seed everytime, so we can rerun each of the tests individually
set.seed(12345)
source('inst/paper_generate_example_plot.R')
set.seed(12345)
source('inst/paper_sine_wave.R')
set.seed(12345)
source('inst/paper_comparison_rosmalen_aira.R')
set.seed(12345)
source('inst/paper_generate_advice.R')
set.seed(12345)
source('inst/paper_generate_example_effects_HGI.R')
set.seed(12345)
source('inst/paper_generate_percentage_effects_HGI.R')
set.seed(12345)
source('inst/paper_table_of_functions.R')

