# Title: Selection methods for landscape metrics - EO4SDG --------------

### load the libs
library(landscapemetrics)
library(tidyverse)
library(corrr)

# Load data
df <- read_csv('data/hessen/metrics.csv')

# calculate beta and rank by (1) beta and in case of ties by (2) minimum absolute correlation
calc_beta_rank <- function(df,
                          correlation = FALSE,
                          landscape = FALSE,
                          df_with_intermediate_steps = FALSE){
    # 0. function input checks
    required_names <- c("layer", "level", "class", "id", "metric", "value",
                        "plot_id", "percentage_inside")
    if(!inherits(df, "data.frame")) stop("input must be a data.frame")
    if(!all(required_names %in% names(df)))
        stop(paste0("input must contain columns with all of the following names: ",
                    paste0(required_names, collapse = ", ")))

    # 1. Beta calculation
    message("calculating beta")
    beta <- df |>
        group_by(level, plot_id, metric) |>
        summarise(min  = min(value),
                  max  = max(value),
                  beta = (max-min)/max) |>
        mutate(rank = min_rank(desc(beta)))

    # 2. Correlation matrices (for tiebreaks)
    my_stretch <- function(z){
        z |>
            stretch() |>
            group_by(x) |>
            summarise(mean = abs(mean(r, na.rm = TRUE)))
    }
    message("calculating correlations")
    correlations <- df |>
        group_by(level, plot_id) |>
        select(id, class, metric, value) |>
        nest() |>
        mutate(pivoted = map(data, pivot_wider, names_from=metric, values_from=value),
               pivoted = map(pivoted, select, -id, -class),
               corr    = map(pivoted, correlate)) |>
        mutate(corr_up  = map(corr, shave),
               corr_up2 = map(corr_up, my_stretch))

    # 2.1 create landsc_ object
    if(landscape) {
        landsc_ <- correlations |>
            filter(level == "landscape") |>
            pull(pivoted) |>
            bind_rows()
    } else{
        landsc_ <- NULL
    }

    # 2.2 make the tiebreaks from here
    message("starting tiebreaks")
    my_comb_rank <- function(y){
        y |>
            arrange(desc(beta), mean) |>
            rowid_to_column("rank_no_ties") |>
            relocate(rank_no_ties, .after = last_col())
    }

    out <-
        beta |>
        group_by(level, plot_id) |>
        nest(.key = "beta") |>
        left_join(correlations) |>
        mutate(beta2 = map2(beta, corr_up2, left_join, by = c(metric = "x")),
               beta3 = map(beta2, my_comb_rank))

    # 3. output formatting
    message("formatting output")
    if(!df_with_intermediate_steps){
        out <-
            out |>
            select(level, plot_id, beta3) |>
            rename(beta = beta3)
    }

    if(!correlation) correlations <- NULL

    return(list(ranked_data = out,
                correlations = correlations,
                landsc_ = landsc_))
}

# try function

# basic minimal output
foo <- calc_beta_rank(df)
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






