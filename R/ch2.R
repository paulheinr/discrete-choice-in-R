library(discrtr) # A companion package for the book Introduction to Discrete Choice Analysis with `R` library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(gplots) # Various R Programming Tools for Plotting Data
library(ggmosaic) # Mosaic Plots in the 'ggplot2' Framework

# Load the data set
data("mc_commute_wide",
     package = "discrtr")

# Pipe table `mc_commute_wide`
mc_commute_wide <- mc_commute_wide %>%
  # Function `mutate()` creates new columns in the table; mutate
  # the table to convert variables `child` and `vehind` to factors
  # with more informative labels
  mutate(child = factor(child,
                        levels=c("Yes",
                                 "No"),
                        # Give the factor categories more descriptive labels
                        labels=c("Living with a child",
                                "Not living with a child")),
                        # Relabel `vehind` variable
                        vehind = factor(vehind,
                        levels=c("No",
                                  "Yes"),
                        # Give the factor categories more descriptive labels
                        labels=c("No ind. vehicle access",
                                  "Ind. vehicle access")))

summary(mc_commute_wide)

# Pipe data frame `mc_mode_wide` to next function
mc_commute_wide <- mc_commute_wide %>%
  # Use mutate to create a new variable
  # Function `case_when()` is a vectorized form of if-else statements
  mutate(Shelters = case_when(Shelters_SD == 1 ~-2,
                              Shelters_D == 1 ~-1,
                              Shelters_A == 1 ~ 1,
                              Shelters_SA == 1 ~ 2,
                              TRUE ~ 0))

mc_commute_wide <- mc_commute_wide %>%
  # Use `mutate()` to modify the content of an existing variable
  mutate(Shelters = factor(Shelters,
  levels = c(-2, -1, 0, 1, 2),
  labels = c("Strongly Disagree",
              "Disagree",
              "Neutral",
              "Agree",
              "Strongly Agree"),
  # The factor is an ordinal variable
  ordered = TRUE))

summary(mc_commute_wide$Shelters)

# Pipe table `mc_Commute_wide to `ggplot()`
mc_commute_wide %>%
  # Create a ggplot object with the table that was piped
  # and map the variable `Shelters` to the x-axis
  ggplot(aes(x=Shelters)) +
  # Add a geometric object of type bar; we do not need
  # to specify the y-axis because the height of the bar
  # will be the statistic for the corresponding categorical
  # outcome
  geom_bar(color = "black",
           fill = "white") +
  # The function `labs()` adds labels to part of the plot, for instance the x and y axes
  labs(x="Public transport facilities of good quality",
        y="Number of respondents")

# Pipe table `mc_commute_wide`
mc_commute_wide %>%
  # Use `group_by()` to group the table by the values
  # of variable `Shelters`
  group_by(Shelters) %>%
  # Summarize: calculate the number n() of cases by
  # category of `Shelters`
  summarize(n=n()) %>%
  # Pipe the result to `ggplot()`; map `Shelters` to the x-axis # and map the number of cases to y; to create segments map
  # the end of the segment to y = 0 and keep it constant on x, # this will create vertical line
  ggplot(aes(x=Shelters,
             xend = Shelters,
             y=n,
             yend = 0)) +
  # Add geometric featues of type point
  geom_point(color = "black",
             fill = "white",
             size = 6) +
  # Add geometric features of type segment (line segments)
  geom_segment(size = 1) +
  # Label the axes
    labs(x="Public transport facilities of good quality",
         y="Number of respondents")

library(treemapify)
# Pipe table
mc_commute_wide %>%
  # Group table based on `choice`
  group_by(choice) %>%
  # Count the number of cases by `choice` and pipe to `ggplot()`
  summarize(n=n()) %>%
  # Map the color of the rectangles to the variable `choice` and # their area to the number of cases
  ggplot(aes(fill = choice,
             area = n)) +
  # Layer geometric object of type treemap
  geom_treemap() +
  # Add labels
  labs(title = "Trips by mode",
       fill="Mode")

mc_commute_wide <- mc_commute_wide %>%
  # Use `mutate()` to convert variable `housing` to a factor
  mutate(housing = case_when(shared != "No" ~ "shared",
                              family != "No" ~ "family",
                              TRUE ~ "solo"),
  housing = factor(housing))
  summary(mc_commute_wide$housing)

##### Bivariate Analysis #####
# Pipe table `mc_commute_wide` to `select()`
mc_commute_wide %>%
  # Select variables `choice` and `sidewalk_density`
  select(choice,
          sidewalk_density) %>%
summary()

mc_commute_wide %>%
# Map `choice` to the x-axis and `sidewalk_density` to the y-axis
ggplot(aes(x=choice,
y=sidewalk_density)) +
# Boxplots are useful for visualizing the relationship between
# one categorical and one quantitative variable
geom_boxplot() +
labs(x="Mode",
# The expression function allows us to include superscripts # and subscripts in labels and titles
y=expression("Sidewalk density (km/km"^2*")"))

mc_commute_wide %>%
  ggplot(aes(x=choice,
             y=sidewalk_density,
             # Map the color of the polygons to `choice`
             fill = choice)) +
  # Add a geometric object of type violin
  geom_violin(trim = FALSE) +
  # Add geometric object of type boxplot
  geom_boxplot(width = 0.1,
              fill = "white") +
  labs(x="Mode",
      y=expression("Sidewalk density (km/km"^2*")"), # Add a label for the fill
      fill = "Mode")

library(ggridges)
mc_commute_wide %>%
  ggplot(aes(x=sidewalk_density,
             y=choice,
             # Map the color of the polygons to `choice`
             fill = choice)) +
  # Add geometric object of type ridges with jittered points
  geom_density_ridges(jittered_points = TRUE,
                      bandwidth = 3.5,
                      position = position_points_jitter(width = 0.05, height = 0), point_shape = '|',
                      point_size = 3,
                      point_alpha = 1,
                      alpha = 0.7) +
  labs(y="Mode",
       x=expression("Sidewalk density (km/km"^2*")"),
       # Add a label for the fill
       fill = "Mode")

# Create a table with the two variables of interest
tableau <- table(mc_commute_wide$choice,
                 mc_commute_wide$child)
balloonplot(as.table(tableau),
            # The parameters below control the aspect of the table
            # Labels
            xlab = "Mode",
            ylab = "Dependent minor(s)",
            # Adjust maximum dot size
            dotsize = 3/max(strwidth(19),
                            strheight(19)),
            # Symbol used for the dots
            dotcolor = "skyblue",
            text.size = 0.65,
            # Title of plot
            main = "Mode as a function of dependent minors in household", # Display the values in the cells
            label = TRUE,
            label.size = 0.80,
            # Scale balloons by volume (or diameter)
            scale.method = c("volume"),
            # Scale balloons relative to zero
            scale.range = c("absolute"),
            # Space for column/row labels
            colmar = 1,
            rowmar = 2,
            # Display zeros if present
            show.zeros = TRUE,
            # Display row and column sums
            show.margins = TRUE,
            # Display cumulative margins as cascade plots
            cum.margins = TRUE)

tableau <- table(mc_commute_wide$choice,
                 mc_commute_wide$housing)
balloonplot(as.table(tableau),
            # The parameters below control the aspect of the table # Labels
            xlab = "Mode",
            ylab = "Living arrangement",
            # Adjust maximum dot size
            dotsize = 3/max(strwidth(19),
                            strheight(19)),
            # Symbol used for the dots
            dotcolor = "skyblue",
            text.size = 0.65,
            # Title of plot
            main = "Mode as a function of living arrangement",
            # Display the values in the cells
            label = TRUE,
            label.size = 0.80,
            # Scale balloons by volume (or diameter)
            scale.method = c("volume"),
            # Scale balloons relative to zero
            scale.range = c("absolute"),
            # Space for column/row labels
            colmar = 1,
            rowmar = 2,
            # Display zeros if present
            show.zeros = TRUE,
            # Display row and column sums
            show.margins = TRUE,
            # Display cumulative margins as cascade plots
            cum.margins = TRUE)

tableau <- table(mc_commute_wide$child,
                 mc_commute_wide$housing)
balloonplot(as.table(tableau),
            # The parameters below control the aspect of the table
            # Labels
            xlab = "Living arrangement",
            ylab = "Dependent minor(s)",
            # Adjust maximum dot size
            dotsize = 3/max(strwidth(19),
                            strheight(19)),
            # Symbol used for the dots
            dotcolor = "skyblue",
            text.size = 0.65,
            # Title of plot
            main = "Dependent minors in household and living arrangement", # Display the values in the cells
            label = TRUE,
            label.size = 0.80,
            # Scale balloons by volume (or diameter)
            scale.method = c("volume"),
            # Scale balloons relative to zero
            scale.range = c("absolute"),
            # Space for column/row labels
            colmar = 1,
            rowmar = 2,
            # Display zeros if present
            show.zeros = TRUE,
            # Display row and column sums
            show.margins = TRUE,
            # Display cumulative margins as cascade plots
            cum.margins = TRUE)

mc_commute_wide %>%
  ggplot() +
  # Add geometric object of type mosaic
  # Map the interaction between `choice` and `child` to the x-axis
  geom_mosaic(aes(x=product(choice,
                            child),
                  fill = choice)) +
  # Add labels
  labs(x="Dependent minor(s)",
       y="Mode",
       fill = "Mode")

ggplot(data = mc_commute_wide) +
  # Add geometric object of type mosaic
  # Map the interaction between `choice` and `numna` to the x-axis
  geom_mosaic(aes(x=product(choice,
                            numna),
                  # Color rectangles based on `choice`
                  fill = choice)) +
  # Add labels
  labs(x="Number of alternatives",
       y="Mode",
       fill = "Mode")

# Pipe table to next function
mc_commute_wide %>%
  # Group observations by `choice` and `housing`
  group_by(choice,
           housing) %>%
  # Calculate number of cases by combination of `choice` and `housing`
  summarize(n=n(),
            .groups = "drop") %>%
  ggplot(aes(x=choice,
             y=housing)) +
  # Add geometric object of type tile, map the color
  # of tiles to `n`, the number of cases
  geom_tile(aes(fill = n)) +
  # Add labels
  labs(x="Mode",
        y="Living arrangement",
        fill = "Number of respondents")

mc_commute_wide %>%
  ggplot(aes(x=like_active_neighborhood, fill = choice)) +
  geom_bar(position = "fill") +
  labs(y="Proportion",
       x="Like active neighborhood",
       fill="Mode")

##### Multivariate Analysis #####
mc_commute_wide %>%
  ggplot(aes(x=sidewalk_density,
             # choice is mapped to the y axis
             y=choice,
             # choice is also mapped to the fill color!
             fill = choice)) +
  # Add geometric object of type density ridges
  geom_density_ridges(jittered_points = TRUE,
                      bandwidth = 3.5,
                      position = position_points_jitter(width = 0.05, height = 0), point_shape = '|',
                      point_size = 3,
                      point_alpha = 1,
                      alpha = 0.7)+
  # Add labels
  labs(y="Mode",
       x=expression("Sidewalk density (km/km"^2*")"),
       fill = "Mode")

ggplot(data = mc_commute_wide,
       aes(x=sidewalk_density,
           y=choice,
           # By mapping the fill color to `vehind`
           # we introduce an additional data dimension to the plot
           ill = vehind)) +
  geom_density_ridges(jittered_points = TRUE,
                      bandwidth = 3.5,
                      position = position_points_jitter(width = 0.05, height = 0), point_shape = '|',
                      point_size = 3,
                      point_alpha = 1,
                      alpha = 0.7) +
  labs(y="Mode",
       x=expression("Sidewalk density (km/km"^2*")"),
       fill = "Individual access to a vehicle")

ggplot(data = mc_commute_wide %>%
  # Group by choice and gender
  group_by(choice,
           gender) %>%
  # Summarize to obtain the number of responses by choice-gender combination # and the mean of sidewalk density for each group
  summarize(n=n(),
            sidewalk_density = mean(sidewalk_density),
            .groups = "drop"),
       # Map the area of the rectangles to the number of responses, the fill color # to mean sidewalk density, and group rectangles by choice
       aes(area = n,
           fill = sidewalk_density,
           label = gender,
           subgroup = choice)) +
  # Create main treemap
  geom_treemap() +
  # Plot borders of subgroups
  geom_treemap_subgroup_border(size = 5)+
  # Add labels
  geom_treemap_subgroup_text(fontface = "bold",
                             colour = "white",
                             place = "topleft",
                             size = 13,
                             grow = FALSE) +
  geom_treemap_text(fontface = "italic",
                    colour = "lightgray",
                    place = "centre",
                    size = 10,
                    grow = FALSE) +
  labs(title = "Trips by Mode-Gender and sidewalk density", fill = expression("Sidewalk density (km/km"^2*")"))

mc_commute_wide %>%
  ggplot(aes(x=sidewalk_density,
             y=choice,
             fill = vehind)) +
  geom_density_ridges(jittered_points = TRUE,
                      bandwidth = 3.5,
                      position = position_points_jitter(width = 0.05,
                                                        height = 0), point_shape = '|',
                      point_size = 3,
                      point_alpha = 1,
                      alpha = 0.7) +
  labs(y="Mode",
       x=expression("Sidewalk density (km/km"^2*")"),
       fill = "Individual access to a vehicle")  +
  # `facet_wrap()` creates subplots after partitioning the data set # by the variable(s) specified, in this case `vehind`
  facet_wrap(~ vehind)

mc_commute_wide %>%
  ggplot(aes(x=child,
             fill = choice)) +
  geom_bar(position = "fill") +
  labs(y="Proportion",
       x="",
       fill="Mode") +
  # Facet the plots based on `gender`
  facet_wrap(~ gender)

mc_commute_wide %>%
  ggplot(aes(x=gender,
             fill = choice)) +
  geom_bar(position = "fill") +
  labs(y="Proportion",
       x="Gender",
       fill="Mode") +
  # Facet the plots based on `child`
  facet_wrap(~ child)

mc_commute_wide %>%
  ggplot(aes(x=gender,
             fill = choice)) +
  geom_bar(position = "fill") +
  labs(y="Proportion",
       x="Gender",
       fill="Mode") +
  # `facet_grid()` creates a "matrix" of subplots # with the variable on the left spread on the
  # x-axis and the one on the right on the y-axis
  facet_grid(vehind ~ child)

mc_commute_wide %>%
  ggplot() +
  (aes(x=gender,
       fill = choice)) +
  geom_bar(position = "fill") +
  labs(y="Proportion",
       x="Getting there is fun", fill = "Mode") +
  facet_wrap(~ getting_there_fun)

mc_commute_wide %>%
  ggplot(aes(x=getting_there_fun, fill = choice)) +
  geom_bar(position = "fill") +
  labs(y="Proportion",
       x="Getting there is fun", fill = "Mode") +
  facet_wrap(~ child)

mc_commute_wide %>%
  ggplot() +
  geom_mosaic(aes(x=product(choice,
                            getting_there_fun), fill = choice)) +
  facet_wrap(~ child)+
  labs(y="Proportion",
       x="Getting there is fun",
       fill = "Mode")

names(mc_commute_wide)[names(mc_commute_wide) == "numna"]<-"Alternatives" # Renaming variable
mc_commute_wide %>%
ggplot(aes(x=available.Walk)) +
  labs(y="Proportion",
       x="Walk is available") +
  geom_bar(color="black",
           fill="white") +
  facet_wrap(~ Alternatives,
             labeller = label_both)

mc_commute_wide %>%
  ggplot(aes(x=Alternatives)) +
  labs(y="Proportion",
       x="Number of alternatives")+ geom_bar(color = "black",
                                             fill = "white") +
  facet_wrap(~ available.Walk,
             labeller = label_both)