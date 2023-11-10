# Exploratory data analysis
# Area and edge metrics
# Note: we consider each administrative unit (9 in total) as a landscape
#
# Overview of landscape metrics grouped by type
#lm_by_type <- list_lsm() |> group_by(type) |> nest()
#

# Functions --------------------------------------------------------------------

my_ggplot <- function(data, x, y, title, subtitle=NULL){
    ggplot(data) +
        ggtitle(title, subtitle) +
        geom_bar(aes({{x}},
                     {{y}},
                     fill=class),
                 stat="identity",
                 width=.5,
                 #position = "dodge"
                 ) +
        scale_x_discrete(guide = guide_axis(angle = 30))
}


my_boxplot <- function(data, means, .metric, title, subtitle = NULL){
    ggplot(data = filter(data,
                         metric == .metric),
           aes(x    = class,
               y    = value,
               fill = class)) +
        geom_boxplot() +
        geom_line(data  = filter(means, metric == .metric),
                  aes(x = class,
                      y = value,
                      group = interaction(plot_id, metric))) +
        facet_wrap(~plot_id,
                   scales = "free") +
        ggtitle(str_to_title(title), subtitle) +
        theme(legend.position = "none")
}









# Area and edge metrics --------------------------------------------------

#Copy the sample_lsm output as xx and group by landscape extent name
xx_area <- x_area[[1]] |>
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
xx_area <- xx_area |>
    left_join(aoi_w_area) |>
    mutate(plot_id,
           area             = as.numeric(area),
           relative_area    = (value/area)*100,
           class            = factor(class))

#------------ # Mean patch size for a class --------------------------------
# The metric summarises each class as the mean of all patch areas belonging to class i.
# The metric is a simple way to describe the composition of the landscape.
# Especially together with the total class area (lsm_c_ca),
# it can also give an an idea of patch structure (e.g. many small patches vs. few larges patches).


# Tried boxplots on patch level, but too many outliers.
# Boxplots are very squished

xx_mean_area_class <-
    xx_area |>
    filter(level == "class", str_detect(metric, "_mn")) |>
    mutate(metric = str_replace(metric, "_mn", ""))


xx_mean_patch_size <-
    xx_area |>
    filter(level  == "patch") |>
    my_boxplot(means = xx_mean_area_class,
               .metric = "area",
              title = "Mean size",
              subtitle = "Patch level" )

# Bar plots on class level
xx_size_class <-
    xx_area |>
    filter(metric == "area_mn",
           level  == "class") |>
    mutate(mean_patch_area = value) |>
    my_ggplot(plot_id,
              mean_patch_area,
              "Mean size",
              "Class level - unclassified area not included" )

#
#------------ # Total class area --------------------------------
# xx_totalarea_class <-
#     xx_area |>
#     filter(metric == "ca") |>
#     mutate(total_area = value) |>
#     my_ggplot(plot_id,
#               total_area,
#               "Total area",
#               "Class level - unclassified area not included" )



#------------ # Percentage of landscape area -------------------------------
# It is the percentage of the landscape belonging to class i.
# It is a measure of composition and because of the relative character directly comparable
# among landscapes with different total areas.

##
##NOTE: We include two pland calculations in this chapter. One with background ("manual" calculation)
##      and one without (built-in metric)

# Use the total landscape area to calculate class area percentage (background considered)
xx_pland_w_backg <-
    xx_area |>
    filter(metric == "ca") |>
    my_ggplot(plot_id,
              relative_area,
              "Percentage of landscape",
              "Class level - unclassified area included" )



# Calculate percentage of landscape per class using the built-in metric pland (background not considered)
# xx_pland_wout_backg  <-
#     xx_area |>
#     filter(metric == "pland",
#            level  == "class") |>
#     mutate(percentage_of_land = value) |>
#     my_ggplot(landscape_extent,
#               percentage_of_land,
#               "Percentage of Landscape",
#               "Class level - unclassified area not included")



#------------ # Radius of gyration (class level) #---------------------------------------------
# Radius of gyration is a measure of patch extent (i.e., how far-reaching it is);
# thus, it is effected by both patch size and patch compaction.
# Note that the choice of the 4-neighbor or 8-neighbor rule for delineating patches
# will have an impact on this metric.
# Behaviour: Approaches GYRATE_MN = 0 if every patch is a single cell. Increases, without limit, when only one patch is present.


xx_gyrate <-
    xx_area |>
    filter(metric=="gyrate_mn",
           level == "class") |>
    mutate(gyration_value = value) |>
    my_ggplot(plot_id,
              gyration_value,
              "Gyration value",
              "Class level")




#------------ # Largest patch index #----------------------------------
# It is the percentage of the landscape covered by the corresponding largest patch of each class i.
# It is a simple measure of dominance.

xx_lpi <- xx_area |>
    filter(metric=="lpi",
           level == "class") |>
    mutate(lpi = value) |>
    my_ggplot(plot_id,
              lpi,
              "Largest Patch Index",
              "Class level")



#------------ # Edge density #----------------------------------------------------------------
# The edge density equals the sum of all edges of class i in relation to the landscape area.
# The boundary of the landscape is only included in the corresponding total class edge length
# if count_boundary = TRUE. The metric describes the configuration of the landscape,
# e.g. because an aggregation of the same class will result in a low edge density.
# The metric is standardized to the total landscape area, and therefore comparisons among
# landscapes with different total areas are possible.


xx_ed <-
    xx_area |>
    filter(metric=="ed",
           level == "class") |>
    mutate(ed = value) |>
    my_ggplot(plot_id,
              ed,
              "Edge density",
              "Class level")









# Shape metrics ----------------------------------------------------------

#Copy the sample_lsm output as xx and group by landscape extent name
xx_shape <- x_shape[[1]] |>
    group_by(plot_id)

# Calculate the area for each "landscape" out of the aoi data
aoi_w_area <- aoi |>
    mutate(plot_id = NAME_4) |>
    select(plot_id) |>
    mutate(area = units::set_units(st_area(geometry), "ha")) |>
    st_drop_geometry()


# Do a left join to the dataset with total landscape area and define it as a new variable
xx_shape <- xx_shape |>
    left_join(aoi_w_area) |>
    mutate(plot_id,
           area             = as.numeric(area),
           relative_area    = (value/area)*100,
           class            = factor(class))




#-------------# Shape index #-------------------------------------------------
# It describes the ratio between the actual perimeter of the patch and the
# square root of patch area. It is a simple measure of shape complexity.
#
# Behavior: Equals SHAPE_MN = 1 if all patches are squares. Increases,
# without limit, as the shapes of patches become more complex.
#

# Creating boxplots to visualize metrics per class and compare them between landscapes


# To plot a line connecting all mean values between classes (regardless of metric and landscape)
# Extract the mean values of each metric per class and delete the "_mn" suffix
xx_mean_class <-
    xx_shape |>
    filter(level == "class",
           str_detect(metric, "_mn")) |>
    mutate(metric = str_replace(metric, "_mn", ""))

    #Check for correct replacement using
    #unique(xx_mean_class$metric)



xx_shapeindex <-
    xx_shape |>
    filter(level  == "patch") |>
    my_boxplot(means = xx_mean_class,
               .metric = "shape",
               title = "shape index",
               subtitle = "Patch level")



#-------------# Related circumscribing circle #-------------------------------------------------
# The metric characterises the compactness of the patch and is comparable among patches with different area.
# Behaviour: CIRCLE = 0 for a circular patch and approaches CIRCLE = 1 for a linear patch.


xx_circle <-
    xx_shape |>
    filter(level  == "patch") |>
    my_boxplot(means = xx_mean_class,
               .metric = "circle",
               title= "Related circumscribing circle",
               subtitle = "Patch level")


#-------------# Contiguity index #-------------------------------------------------
# The metric characterises the compactness of the patch and is comparable among patches with different area.
# Behaviour: CIRCLE = 0 for a circular patch and approaches CIRCLE = 1 for a linear patch.


xx_contig <-
    xx_shape |>
    filter(level  == "patch") |>
    my_boxplot(means = xx_mean_class,
               .metric = "contig",
               title= "Contiguity index",
               subtitle = "Patch level")








# Core metrics ----------------------------------------------------------


#Copy the sample_lsm output as xx and group by landscape extent name
xx_core <- x_core[[1]] |>
    group_by(plot_id)

# Calculate the area for each "landscape" out of the aoi data
aoi_w_area <- aoi |>
    mutate(plot_id = NAME_4) |>
    select(plot_id) |>
    mutate(area = units::set_units(st_area(geometry), "ha")) |>
    st_drop_geometry()


# Do a left join to the dataset with total landscape area and define it as a new variable
xx_core <- xx_core |>
    left_join(aoi_w_area) |>
    mutate(plot_id,
           area             = as.numeric(area),
           relative_area    = (value/area)*100,
           class            = factor(class))


#-------------# Core area index #-------------
# Range: 0 <= CAI_MN <= 100
# Behaviour: CAI_MN = 0 when all patches have no core area and approaches
#            CAI_MN = 100 with increasing percentage of core area within patches.


# Creating boxplots to visualize metrics per class and compare them between landscapes


# To plot a line connecting all mean values between classes (regardless of metric and landscape)
# Extract the mean values of each metric per class and delete the "_mn" suffix
xx_mean_class_core <-
    xx_core |>
    filter(level == "class",
           str_detect(metric, "_mn")) |>
    mutate(metric = str_replace(metric, "_mn", ""))

#Check for correct replacement using
unique(xx_mean_class_core$metric)

xx_coreindex <-
    xx_core |>
    filter(level  == "patch") |>
    my_boxplot(means = xx_mean_class_core,
               .metric = "cai",
               title = "core area index",
               subtitle = "Patch level")



#-------------# Core percentage of land (not yet fully done) #-------------
# It is the percentage of core area of class i in relation to the total landscape area.
# A cell is defined as core area if the cell has no neighbour with a different value than
# itself (rook's case). Because CPLAND is a relative measure,
# it is comparable among landscapes with different total areas.

# Units; Percentage
# Range: 0 <= CPLAND < 100
# Behaviour: Approaches CPLAND = 0 if CORE = 0 for all patches.
#            Increases as the amount of core area increases, i.e. patches become
#            larger while being rather simple in shape.

xx_cpland_w_backg <-
    xx_core |>
    filter(metric == "cpland") |>
    my_ggplot(plot_id,
              relative_area,
              "Core area percentage of landscape",
              "Class level - unclassified area included" )



# # Calculate core area percentage of landscape per class using the built-in metric cpland (background not considered)
xx_cpland_wout_backg  <-
    xx_core |>
    filter(metric == "cpland",
           level  == "class")|>
    my_ggplot(plot_id,
              value,
              "Core area percentage of landscape",
              "Class level - unclassified area not included")









# TODO
# --------------------------------# Visualisation # -------------------------------
# This section takes the output from our calculated *shape metrics* as an example.
# You need to run the section "Shape metrics" in this file before continuing with the visualisation


# Create a single boxplot per metric per landscape (region)
my_single_boxplot <- function(data, land, .metric, title, subtitle = NULL){
        data |>
        filter(level  == "patch",
               plot_id == land,
               metric == .metric) |>
        ggplot(aes(x = class,
                   y = value,
                   fill = class)) +
        geom_boxplot() +
        facet_wrap(~plot_id,
                   scales = "free") +
        ggtitle(str_to_title(title), subtitle) +
        theme(legend.position = "none")
}



xx_shape_alheim <-
    my_single_boxplot(data = xx_shape,
                  land = "Alheim",
                  #means = xx_mean_class,
                  .metric = "shape",
                  title = "shape index",
                  subtitle = "Alheim")

#
# xx_shape_al <-
#     xx2 |>
#     filter(level  == "patch",
#            plot_id == "Alheim",
#            metric == "shape") |>
#     ggplot(aes(x = class,
#                y = value,
#                fill = class)) +
#     geom_boxplot() +
#     geom_line(data = filter(means, metric == .metric),
#               aes(x = class,
#                   y = value,
#                   group = interaction(plot_id, metric))) +
#     facet_wrap(~plot_id,
#                scales = "free") +
#     ggtitle(str_to_title("shape index"), "Alheim") +
#     theme(legend.position = "none")



aoi |>
    st_transform(crs = 4326) |>
    leaflet() |>
    addTiles() |>
    addPolygons(color = "#444444",
                weight = 1,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                popup = popupGraph(xx_shape_alheim, width = 300, height = 300)
                )



# Source for adding graphs to maps with leaflet:
# https://github.com/r-spatial/leafpop
    # alternative to what I used:
    # addPopupGraphs(list(xx_shape_alheim), group = "xxx", width = 300, height = 400)











