rm(list=ls(pos='.GlobalEnv',all=TRUE),pos='.GlobalEnv')
set.seed(12345)
bootstrap_iterations <<- 2000
source('inst/paper_generate_example_plot.R')
source('inst/paper_sine_wave.R')
source('inst/paper_comparison_rosmalen_aira.R')
source('inst/paper_generate_advice.R')
