# Mon Nov 13 10:10:02 2023 --------
# D. Emin -------------------------


library(landscapemetrics)
library(sf)
library(gdfR)
library(tidyverse)
library(terra)
library(GGally)


df <- read_csv("data/area_metrics.csv")

df




df |>
    filter(plot_id == "Alheim") |>
    select(layer, level, class, id, metric, value) |>
    filter(!is.na(id)) |>
    show_correlation()



df |>
    filter(plot_id == "Alheim", !is.na(id)) |>
    select(layer, level, class, id, metric, value) |>
    pivot_wider(names_from = metric, values_from = value)  |>
    ggpairs(aes(fill = class), columns = 5:7)



landscape |>
    rast() |>
    calculate_lsm(type = "area and edge metric") |>
    filter(!is.na(id))
    # show_correlation()




