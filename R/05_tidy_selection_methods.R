# Title: Selection methods for landscape metrics - EO4SDG --------------

### load the libs
library(landscapemetrics)
library(tidyverse)
library(corrr)

# Load data
# df <- read_csv('data/hessen/metrics.csv')

# function automatically reads metrics.csv in the tempdir folder

# calculate beta and rank by (1) beta and in case of ties by (2) minimum absolute correlation
s

# try function

# basic minimal output
foo <- calc_beta_rank(df)
foo$ranked_data
foo$ranked_data$beta[[1]]
foo$ranked_data$beta[[27]]

# with all other options returned
foo_cor <- calc_beta_rank(df,
                         correlation = TRUE,
                         landscape = TRUE,
                         df_with_intermediate_steps = TRUE)

# correlations
foo_cor$correlations # all the correlation output
foo_cor$correlations$data[[1]] # raw data - long format
foo_cor$correlations$pivoted[[1]] # data for correlations
foo_cor$correlations$corr[[1]] # raw correlation matrix
foo_cor$correlations$corr_up[[1]] # lower triangular matrix
foo_cor$correlations$corr_up2[[1]] # averaged with the mean

# landscape object
foo_cor$landsc_

# data is now with all intermediate steps (for debugging)
foo_cor$ranked_data






