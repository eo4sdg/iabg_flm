
# Exploratory data analysis
# Area and edge metrics
# Note: we consider each administrative unit (9 in total) as a landscape
#
# Overview of landscape metrics grouped by type
# lm_by_type <- list_lsm() |> group_by(type) |> nest()
#

#Functions--------------------------------------------------------------------

my_ggplot <- function(data, x, y, title, subtitle=NULL){
    ggplot(data) +
        ggtitle(title, subtitle) +
        geom_bar(aes({{x}},
                     {{y}},
                     fill=class),
                 stat="identity")
}


#------------Percentage of landscape area -------------------------------
# It is the percentage of the landscape belonging to class i.
# It is a measure of composition and because of the relative character directly comparable
# among landscapes with different total areas.

##
##NOTE: We include two pland calculations in this chapter. One with background ("manual" calculation)
##      and one without (built-in metric)


#Copy the sample_lsm output as xx and group by landscape extent name
xx <- x[[1]] |>
    group_by(plot_id)

#Also possible to nest the dataframe so that we have all sample_lsm results categorised by landscape extent
# xx <- x[[1]] |>
#     group_by(plot_id) |>
#     nest()

# Calculate the area for each "landscape" out of the aoi data
aoi_w_area <- aoi |>
    mutate(plot_id = NAME_4) |>
    select(plot_id) |>
    mutate(area = units::set_units(st_area(geometry), "ha")) |>
    st_drop_geometry()


# Do a left join to the dataset with total landscape area and define it as a new variable
xx2 <- xx |>
    left_join(aoi_w_area) |>
    mutate(landscape_extent = plot_id,
           area             = as.numeric(area),
           relative_area    = value/area,
           class            = factor(class))


# Use the total landscape area to calculate class area percentage (background considered)
xx_pland_w_backg <- xx2 |>
    filter(metric == "ca") |>
    my_ggplot(landscape_extent,
              relative_area,
              "Class area relative to landscape area",
              "With background" )



# Calculate percentage of landscape per class using the built-in metric pland (background not considered)
xx_pland_wout_backg  <- xx2 |>
    filter(metric=="pland",
           level == "class") |>
    mutate(percentage_of_land = value) |>
    my_ggplot(landscape_extent,
              percentage_of_land,
              "Percentage of Land",
              "No background")



#------------Radius of gyration (class level)---------------------------------------------
# Radius of gyration is a measure of patch extent (i.e., how far-reaching it is);
# thus, it is effected by both patch size and patch compaction.
# Note that the choice of the 4-neighbor or 8-neighbor rule for delineating patches
# will have an impact on this metric.
# Behaviour: Approaches GYRATE_MN = 0 if every patch is a single cell. Increases, without limit, when only one patch is present.


xx_gyrate <- xx2 |>
    filter(metric=="gyrate_mn",
           level == "class") |>
    mutate(gyration_value = value) |>
    my_ggplot(landscape_extent,
              gyration_value,
              "Gyration value")




#------------Largest patch index---------------------------------------------------------
# It is the percentage of the landscape covered by the corresponding largest patch of each class i.
# It is a simple measure of dominance.

xx_lpi <- xx2 |>
    filter(metric=="lpi",
           level == "class") |>
    mutate(lpi = value) |>
    my_ggplot(landscape_extent,
              lpi,
              "Largest Patch Index")



#------------Edge density----------------------------------------------------------------
# The edge density equals the sum of all edges of class i in relation to the landscape area.
# The boundary of the landscape is only included in the corresponding total class edge length
# if count_boundary = TRUE. The metric describes the configuration of the landscape,
# e.g. because an aggregation of the same class will result in a low edge density.
# The metric is standardized to the total landscape area, and therefore comparisons among
# landscapes with different total areas are possible.


xx_ed <- xx2 |>
    filter(metric=="ed",
           level == "class") |>
    mutate(ed = value) |>
    my_ggplot(landscape_extent,
              ed,
              "Edge density")








