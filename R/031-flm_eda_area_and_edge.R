
# Exploratory data analysis
# Area and edge metrics
# Note: we consider each administrative unit (9 in total) as a landscape
#
# Overview of landscape metrics grouped by type
# lm_by_type <- list_lsm() |> group_by(type) |> nest()
#

# functions
my_ggplot <- function(data, x, y, title){
    ggplot(data) +
        ggtitle(title) +
        geom_bar(aes({{x}},
                     {{y}},
                     fill=class),
                 stat="identity")
}



##---------------------------------------------------------------------------
# Class area and its percentage of total landscape


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


# Use the total landscape area to calculate class area percentage and plot
xx2 <- xx |>
    left_join(aoi_w_area) |>
    mutate(landscape_extent = plot_id,
           area             = as.numeric(area),
           relative_area    = value/area,
           class            = factor(class))


# Use the total landscape area to calculate class area percentage and plot
xx_area_plot <- xx |>
    filter(metric == "ca") |>
    ggplot() + ggtitle("Class area relative to landscape area") + geom_bar(aes(landscape_extent, relative_area,  fill = class), stat = "identity")



# Calculate percentage of landscape (per class) using the built-in metric pland
xx_pland <- xx2 |>
    filter(metric=="pland", level == "class") |>
    mutate(percentage_of_land = value) |>
    my_ggplot(landscape_extent, percentage_of_land, "title")



#----------------------------------------------------------------------------
# Radius of gyration (class level)
    # Radius of gyration is a measure of patch extent (i.e., how far-reaching it is);
    # thus, it is effected by both patch size and patch compaction.
    # Note that the choice of the 4-neighbor or 8-neighbor rule for delineating patches
    # will have an impact on this metric.
    # Behaviour: Approaches GYRATE_MN = 0 if every patch is a single cell. Increases, without limit, when only one patch is present.


xx_gyrate <- xx |>
    filter(metric=="gyrate_mn", level == "class") |>
    mutate(gyration_value = value) |>
    ggplot() +
    ggtitle("Radius of Gyration") +
    geom_bar(aes(landscape_extent,
                 gyration_value,
                 fill=class),
             stat="identity")




#----------------------------------------------------------------------------
# Largest patch index
    # It is the percentage of the landscape covered by the corresponding largest patch of each class i.
    # It is a simple measure of dominance.

xx_lpi <- xx |>
    filter(metric=="lpi", level == "class") |>
    mutate(plot_id, lpi = value, class=factor(class)) |>
    ggplot() + ggtitle("Largest Patch Index") + geom_bar(aes(plot_id, lpi, fill=class), stat="identity")




#----------------------------------------------------------------------------
# Edge density
    # The edge density equals the sum of all edges of class i in relation to the landscape area.
    # The boundary of the landscape is only included in the corresponding total class edge length
    # if count_boundary = TRUE. The metric describes the configuration of the landscape,
    # e.g. because an aggregation of the same class will result in a low edge density.
    # The metric is standardized to the total landscape area, and therefore comparisons among
    # landscapes with different total areas are possible.


xx_ed <- xx |>
    filter(metric=="ed", level == "class") |>
    mutate(landscape_extent = plot_id, ed = value, class=factor(class)) |>
    ggplot() + ggtitle("Edge density") + geom_bar(aes(landscape_extent, ed, fill=class), stat="identity")








