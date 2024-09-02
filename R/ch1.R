library(discrtr) # A companion package for the book Introduction to Discrete Choice Analysis with `R` library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(mlogit) # Multinomial Logit Models
library(readr) # Read Rectangular Text Data
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables

# Load the data
mc_mode_choice <- read_csv(system.file("extdata", "mc_commute.csv", package = "discrtr"))

stargazer(as.data.frame(mc_mode_choice[,1:5]),
          # change the type to text, html, or latex depending on the desired output type = "latex",
          header = FALSE, # do not print package version info in the output
          title = "Example of a table with summary statistics", # Title of table
          omit.summary.stat = c("N",
                                "median"), # summary statistics to omit from output
          font.size = "small") # font size can be changed

# Function `factor()` is used to convert a variable (which could be character or numeric)
# into a factor, that is, a label or category; when we want a factor to be ordered (treated
# as an ordinal variable) we specify argument ordered = TRUE. Non-ordinal variables by default # are displayed alphabetically, but changing their order when  specifying the labels changes
# the order they are displayed _without necessarily making them ordinal_
mc_mode_choice$choice <- factor(mc_mode_choice$choice,
                                labels = c("Cycle",
                                           "Walk",
                                           "HSR",
                                           "Car"))

summary(mc_mode_choice$choice)

# Find the class of an object
class(mc_mode_choice$choice)

# All these mehods refer to the same object, but have different types
a <- mc_mode_choice[2, 2]
class(a)
b <- mc_mode_choice$choice[2]
class(b)
c <- mc_mode_choice[["choice"]][2]
class(c)

# Selecting specific rows and columns: rows 2 to 5 and column 7 to 8
mc_mode_choice[2:5, 7:8]

time.Cycle.clean <- mc_mode_choice$timecycle[mc_mode_choice$timecycle != 100000]
class(time.Cycle.clean)
summary(time.Cycle.clean)

# using the pipe operator from package dplyr
time.Active.clean <- mc_mode_choice %>% # Pipe data frame `mc_mode_choice`
  select(c("timecycle", # Select columns from the data frame that was piped
           "timewalk")) %>%
  filter(timecycle != 100000 & timewalk != 100000) # Filter observations that are _not_ 100000

# plot the data
ggplot(data = time.Active.clean) +
  geom_area(aes(x=timecycle),
             stat = "bin",
             binwidth = 5,
             fill = "blue",
             color = "blue",
             alpha = 0.6) +
  geom_area(aes(x=timewalk),
            stat = "bin",
            binwidth = 5,
            fill = "yellow",
            color = "yellow",
            alpha = 0.6) +
  xlab("Time (in minutes)")

# The pipe operator `%>%` takes an object and passes it on
# to the next function where it is used as the first argument
mc_mode_choice %>%
# `select()` retrieves columns from a data frame
select(c("choice", "side_den")) %>%
  summary()


# Pipe the table to `ggplot()` where it is assumed to be the # first argument of the function, i.e., data
mc_mode_choice %>%
  # Map `choice` to the x-axis and `side_den` to the y-axis
  ggplot(aes(x=choice,
             y=side_den)) +
    # Add a geometric object of type boxplot
    geom_boxplot()


#################### EXERCISES ####################
data("Mode")

# 4
summary(Mode)
head(Mode)

# 5
summary(Mode$choice)

# 6
mean(Mode$cost.car)
mean(Mode$cost.carpool)
mean(Mode$cost.bus)
mean(Mode$cost.rail)

# 7
ggplot(data = Mode) +
  geom_area(aes(x=time.car),
            stat = "bin",
            binwidth = 2,
            fill = "blue",
            color = "blue",
            alpha = 0.6) +
  geom_area(aes(x=time.bus),
            stat = "bin",
            binwidth = 2,
            fill = "yellow",
            color = "yellow",
            alpha = 0.6) +
  xlab("Time (in minutes)")

# 8
# create new column cost. take the value of cost.car if choice is car, cost.carpool if choice is carpool, cost.bus if choice is bus, and cost.rail if choice is rail
Mode$cost <- ifelse(Mode$choice == "car", Mode$cost.car,
                    ifelse(Mode$choice == "carpool", Mode$cost.carpool,
                           ifelse(Mode$choice == "bus", Mode$cost.bus,
                                  ifelse(Mode$choice == "rail", Mode$cost.rail, NA))))

Mode %>%
  ggplot(aes(x=choice,
             y=cost)) +
  geom_boxplot()