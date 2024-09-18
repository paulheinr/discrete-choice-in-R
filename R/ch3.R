library(dplyr) # A Grammar of Data Manipulation
library(evd) # Functions for Extreme Value Distributions
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

# Define a function to return a value of one if
#-L>x<=Landzero otherwise
uniform <- function(x, L) {
  # Logical condition: x greater than minus L and less than L
  ifelse(x > -L & x <= L,
         # Value if true
         1 / (2 * L),
         # Value if false
         0)
}

# Define parameter L for the distribution
L <- 2
# Define an upper limit for calculating the probability
X <- 2
# Create a data frame for plotting the full distribution
df <- data.frame(x = seq(from = -(L + 1),
                         to = L + 1,
                         by = 0.01)) %>%
  # Mutate the data frame to add a new column with the
  # value of the function
  mutate(f = uniform(x, L))
# Create a data frame for plotting the portio of the
# distribution that is less than X
df_p <- data.frame(x = seq(from = -(L + 1),
                           to = X,
                           by = 0.01)) %>%
  mutate(f = uniform(x, L))
# Plot
ggplot() +
  # Use data frame `df` to plot the full distribution
  geom_area(data = df,
            # Map column x in the data frame to the x-axis # and column f to the y-axis
            aes(x = x,
                y = f),
            fill = "orange",
            alpha = 0.5) +
  # Use data frame `df_p` to plot area under the curve to X
  geom_area(data = df_p,
            # Map column x in the data frame to the x-axis # and column f to the y-axis
            aes(x = x,
                y = f),
            fill = "orange",
            alpha = 1) +
  # Set the limits of the y axis
  ylim(c(0,
         1 / (2 * L) + 0.2 * 1 / (2 * L))) +
  # Draw a horizontal line for the x axis
  geom_hline(yintercept = 0) +
  # Draw a horizontal line for the y axis
  geom_vline(xintercept = 0) +
  ylab("f(x)")

# Define the cumulative distribution function
punif <- function(x, L) {
  # Logical statement: x less or equal to -L
  ifelse(x <= -L,
         # Value if true
         0,
         # If false, check whether x is between -L and L
         ifelse(x > -L & x <= L,
                # Value if true
                (x + L) / (2 * L),
                # Value if false
                1))
}

# Define L
L <- 2
# Create a data frame for plotting the cumulative distribution function
df <- data.frame(x = seq(from = -(L + 1),
                         to = L + 1,
                         by = 0.01)) %>%
  mutate(f = punif(x, L))
# Plot
ggplot(data = df,
       # Map column x in the data frame to the x-axis
       # and column f to the y-axis
       aes(x = x,
           y = f)) +
  # Add geometric object of type step to the plot to
  # plot cumulative distribution function
  geom_step(color = "orange") +
  ylim(c(0, 1)) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("F(x)") # Label the y axis

# Define a function for the linear distribution function
linear <- function(x) {
  # Logical statement: x between 0 and 1
  ifelse(x > 0 & x <= 1,
         # Value if true
         2 * x,
         # Value if false
         0)
}

# Create a data frame for plotting
df <- data.frame(x = seq(from = 0,
                         to = 1,
                         by = 0.01)) %>%
  mutate(f = linear(x))
# Plot
ggplot(data = df,
       aes(x = x,
           y = f)) +
  geom_area(fill = "orange",
            alpha = 0.5) +
  ylim(c(0, 2)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ylab("f(x)")

# Define a function for the cumulative distribution
plinear <- function(x) {
  ifelse(x <= 0,
         0,
         ifelse(x > 0 & x <= 1,
                x^2,
                1))
}

# Create a data frame for plotting
df <- data.frame(x = seq(from = -0.2,
                         to = 1.2,
                         by = 0.001)) %>% mutate(f = plinear(x))
# Plot
ggplot(data = df,
       aes(x = x,
           y = f)) +
  # Add geometric object of type step to the plot to # to plot cumulative distribution function
  geom_step(color = "orange") +
  ylim(c(0, 1)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ylab("F(x)")