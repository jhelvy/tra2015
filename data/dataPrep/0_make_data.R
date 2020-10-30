# Author: John P. Helveston
# First Written: October 29, 2020
# Last Updated: October 29, 2020

# Description: This code runs all the subsequent scripts to build the final data used in the models. 

source("1_clean.R")
source("2_fix.R")
# source("3a_compute_weights.R") # Only run these once to compute the weights
# source("3b_run_weights_optim.R") # Only run these once to compute the weights
source("4_make_demo_summaries.R")
source("5_impute.R")
source("6_aer_coding.R")
source("7_interaction_prep.R")
source("8_make_final_choice_data.R")
