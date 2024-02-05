# Title: Selection methods for landscape metrics - EO4SDG --------------

### load the libs
library(landscapemetrics)
library(sf)
library(gdfR)
library(tidyverse)
library(terra)
library(GGally)
library(ggstatsplot)
library(lares)
library(Hmisc)
source("R/correlation_matrix.R")



# All metrics in "landscapemetrics" according by types
types <-
    list_lsm() |>
    select(type, metric) |>
    distinct(metric, .keep_all = TRUE)


# Load data
df <-
    read_csv('data/hessen/metrics.csv') |>
    left_join(types)

# Nest the whole dataset according to the level column
m2 <-
    df |>
    group_by(level) |>
    nest()


# see the unique landscape names
unique_landscapes <-
    df |>
    select(plot_id) |>
    unique()

# calculate beta and rank by (1) beta and in case of ties by (2) minimum absolute correlation
calc_beta_rank<- function(df){
    # 1. Beta calculation
    beta<- df |>
        group_by(level, plot_id, metric) |>
        summarise(min = min(value),
                  max = max(value),
                  beta = (max-min)/max) |>
        mutate(rank = min_rank(desc(beta)))

    # 2. Correlation matrices (for tiebreaks)
    my_stretch<- function(z){
        z |>
            stretch() |>
            group_by(x) |>
            summarise(mean = abs(mean(r, na.rm = TRUE)))
    }

    foo<- df |>
        group_by(level, plot_id) |>
        select(id, class, metric, value) |>
        nest() |>
        mutate(pivoted = map(data, pivot_wider, names_from=metric, values_from=value),
               pivoted = map(pivoted, select, -id, -class),
               corr = map(pivoted, correlate)) |>
        mutate(corr_up = map(corr, shave),
               corr_up2 = map(corr_up, my_stretch))

    #  make a tiebreaker from here
    my_comb_rank<- function(y){
        y |>
            arrange(desc(beta), mean) |>
            rowid_to_column("rank_no_ties") |>
            relocate(rank_no_ties, .after = last_col())
    }

    tmp<-
        beta |>
        group_by(level, plot_id) |>
        nest(.key = "beta") |>
        left_join(foo) |>
        mutate(beta2 = map2(beta, corr_up2, left_join, by = c(metric = "x"))) |>
        mutate(beta3 = map(beta2, my_comb_rank))

    # output
    tmp |>
        select(level, plot_id, beta3) |>
        rename(beta = beta3)

}

# try function
foo<- calc_beta_rank(df)
foo$beta[[1]]
foo$beta[[27]]


# create landsc_ object
landsc_ <- foo |>
    filter(level == "landscape") |>
    pull(pivoted) |>
    bind_rows()



