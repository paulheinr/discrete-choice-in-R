library(discrtr) # A companion package for the book Introduction to Discrete Choice Analysis with `R`
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(htmlwidgets) # HTML Widgets for R
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(mlogit) # Multinomial Logit Models
library(plotly) # Create Interactive Web Graphics via 'plotly.js'
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables
library(tidyr) # Tidy Messy Data
library(webshot2) # Take Screenshots of Web Pages

data("mc_commute_wide",
     package = "discrtr")

example_wide <- mc_commute_wide %>%
  # Select columns from the table
  select(id, choice, starts_with("time")) %>%
  # Filter three decision-makers by their `id`
  # Here the symbol `|` is for "or", so this reads
  # filter rows with id == 566910139 OR id == 566873140 OR id == 566872636
  filter(id == 566910139 |
           id == 566873140 |
           id == 566872636)
example_wide

example_wide %>%
  # `pivot_longer()` takes a wide table and makes it long
  # Here we pivot the columns with the `time` variable
  pivot_longer(cols = starts_with("time."),
               # There is a pattern to the names: time.ALTERNATIVE
               # The prefix is the name of the variable
               names_prefix = "time.",
               # The alternatives are placed in a new column called
               # "alternative"
               names_to = "alternative",
               # The values of the variables are consolidated in
               # a single column called "time"
               values_to = "time")

example_long <- mc_commute_wide %>%
  # Filter three decision-makers by their `id`
  # Here the symbol `|` is for "or", so this reads
  # filter rows with id == 566910139 OR id == 566873140 OR id == 566872636
  filter(id == 566910139 |
           id == 566873140 |
           id == 566872636) %>%
  mlogit.data(shape = "wide",
              # Name of column with the choices
              choice = "choice",
              # Numbers of columns with attributes that vary by alternative
              varying = 3:22)

data.frame(example_long) %>%
  # Select columns
  select(id,
         choice,
         alt,
         starts_with("time"),
         idx)

print(data.frame(mc_commute_long$idx))

colnames(example_long)

?mc_commute_wide

# Function `mFormula()` is used to define multi-part formulas of the form:
# y ~ x | z | w, which in the notation used for the anatomy of utility functions is
# choice ~ alternative vars. with generic coefficients |
#         individual vars. with specific coefficients |
#         alternative vars. with specific coefficients
# In this formula time is one of x variables
f1 <- mFormula(choice ~ time)

# Pipe `f1` to next function
f1 %>%
  # Build the model matrix with data set `example_long`
  model.matrix(example_long)

# Function `mFormula()` is used to define multi-part formulas of the form:
# y ~ x | z | w, which in the notation used for the anatomy of utility functions is
# choice ~ alternative vars. with generic coefficients |
#         individual vars. with specific coefficients |
#         alternative vars. with specific coefficients
# In this formula `time` is one of x variables and `sidewalk_density` is one of z variables
f2 <- mFormula(choice ~ time | sidewalk_density)

# Pipe `f2` to next function
f2 %>%
  # Build the model matrix with data set `example_long`
  model.matrix(example_long)

f3 <- mFormula(choice ~ 0 | sidewalk_density | time)
# Pipe `f3` to next function
f3 %>%
  # Build the model matrix with data set `example_long`
  model.matrix(example_long)

mc_commute_long <- mc_commute_wide %>%
  mlogit.data(shape = "wide",
              # Name of column with the choices
              choice = "choice",
              # Numbers of columns with attributes that vary by alternative
              varying = 3:22)
class(f1)
class(f2)
class(f3)

# Function `mlogit()` is used to estimate logit models
# It needs a multi-part formula and a data set in long form
model1 <- mlogit(f1,
                 mc_commute_long)
# Function `summary()` give the summary of data objects,
# including the output of model estimation algorithms
summary(model1)

# Note: use chunk option results="asis" to display latex output in pdf
stargazer::stargazer(model1,
                     # Use type = "text", "latex". or "html" depending
                     # on the desired output
                     type = "text",
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Model 1")

# Note: use chunk option results="asis" to display latex output in pdf
model2 <- mlogit(f2,
                 mc_commute_long)
stargazer::stargazer(model2,
                     # Use type = "text", "latex". or "html" depending
                     # on the desired output
                     type = "text",
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Model 2")

##### Use walk as reference #####
# Note: use chunk option results="asis" to display latex output in pdf
model2 <- mlogit(f2,
                 mc_commute_long,
                 # Specify the alternative that acts as reference
                 reflevel = "Walk")
stargazer::stargazer(model2,
                     # Use type = "text", "latex". or "html" depending
                     # on the desired output
                     type = "latex",
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Model 2 (alternative: Walk)")

summary(mc_commute_long$sidewalk_density)
mc_commute_predict <- mc_commute_long[1:52,]

# Function `rep()` repeats the values in the argument a designated
# number of times; here, the values in the sequence 0 to 60 in intervals
# of 5 are repeated four times each (once for each alternative)
mc_commute_predict$sidewalk_density <- rep(seq(from = 0,
                                               to = 60,
                                               by = 5),
                                           each = 4)

mc_commute_predict %>%
  data.frame() %>%
  select(sidewalk_density) %>%
  slice_head(n = 8)

median(mc_commute_predict$time,
       na.rm = TRUE)
mc_commute_predict$time <- 10
mc_commute_predict %>%
  data.frame() %>%
  select(time, sidewalk_density) %>%
  summary()

mc_commute_predict %>%
  data.frame() %>%
  select(time, sidewalk_density) %>%
  slice_head(n = 8)

probs <- predict(model2,
                 newdata = mc_commute_predict)
print(probs)

probs <- data.frame(sidewalk_density = seq(from = 0,
                                           to = 60,
                                           by = 5),
                    probs) %>%
  # Pivot longer all columns _except_ `sidewalk_density`
  pivot_longer(cols = -sidewalk_density,
               # The column names become a new column called "Mode"
               names_to = "Mode",
               # The values are gathered into a single column called
               # "Probability"
               values_to = "Probability")

probs %>%
  slice_head(n = 8)

probs %>%
  # Create ggplot object; map `sidewalk_density` to the y-axis
  # `Probability` to the x-axis, and the color of geometric
  # objects to `Mode`
  ggplot(aes(x = sidewalk_density,
             y = Probability,
             color = Mode)) +
  # Add geometric object of type line with size = 1
  geom_line(size = 1) +
  labs(y = "Probability",
       x = expression("Sidewalk density (km/km"^2 * ")"))

f0 <- mFormula(choice ~ 1)
model0 <- mlogit(f0,
                 mc_commute_long)
stargazer::stargazer(model0,
                     # Use type = "text", "latex". or "html" depending
                     # on the desired output
                     type = "text",
                     header = FALSE,
                     single.row = TRUE,
                     title = "Estimation results: Market Shares Model (Null Model)")

1 - as.numeric(model2$logLik) / as.numeric(model0$logLik)

lrtest(model1,
       model2)

########## Estimation ##########
ts <- data.frame(xA = c(5, 2, 5, 1, 4, 3),
                 xB = c(4, 5, 2, 6, 1, 4),
                 yA = c(1, 1, 0, 1, 0, 0),
                 yB = c(0, 0, 1, 0, 1, 1))

# Set the parameters:
mu <- 0.1
beta <- -0.65
# Calculate probabilities. Notice that these are the logit probabilities
# Individual 1
P1A <- (exp(beta * ts$xA[1]) /
  (exp(beta * ts$xA[1]) + exp(mu + beta * ts$xB[1])))
P1B <- 1 - P1A

# Individual 2
P2A <- (exp(beta * ts$xA[2]) /
  (exp(beta * ts$xA[2]) + exp(mu + beta * ts$xB[2])))
P2B <- 1 - P2A

# Individual 3
P3A <- (exp(beta * ts$xA[3]) /
  (exp(beta * ts$xA[3]) + exp(mu + beta * ts$xB[3])))
P3B <- 1 - P3A

# Individual 4
P4A <- (exp(beta * ts$xA[4]) /
  (exp(beta * ts$xA[4]) + exp(mu + beta * ts$xB[4])))
P4B <- 1 - P4A

# Individual 5
P5A <- (exp(beta * ts$xA[5]) /
  (exp(beta * ts$xA[5]) + exp(mu + beta * ts$xB[5])))
P5B <- 1 - P5A

# Individual 6
P6A <- (exp(beta * ts$xA[6]) /
  (exp(beta * ts$xA[6]) + exp(mu + beta * ts$xB[6])))
P6B <- 1 - P6A

# Calculate likelihood function as the product of all the probabilities
# Each probability is raised to ynj
L <- P1A^ts$yA[1] *
  P1B^ts$yB[1] *
  P2A^ts$yA[2] *
  P2B^ts$yB[2] *
  P3A^ts$yA[3] *
  P3B^ts$yB[3] *
  P4A^ts$yA[4] *
  P4B^ts$yB[4] *
  P5A^ts$yA[5] *
  P5B^ts$yB[5] *
  P6A^ts$yA[6] *
  P6B^ts$yB[6]

# Create data frame to tabulate results:
df_experiment_1 <- data.frame(Individual = c(1, 2, 3, 4, 5, 6),
                              Choice = c("A", "A", "B", "A", "B", "B"),
                              PA = c(P1A, P2A, P3A, P4A, P5A, P6A),
                              PB = c(P1B, P2B, P3B, P4B, P5B, P6B))
# Display table
kable(df_experiment_1, "latex", digits = 4,
      booktabs = TRUE,
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  footnote(general = paste("The value of the likelihood function in Example 1 is: ",
                           round(L, digits = 4)))

# Create a grid to plot the likelihood function
mu = seq(from = -1, to = 1, by = 0.05)
beta = seq(from = -2, to = 0, by = 0.05)
coeffs <- expand.grid(mu, beta)

# Define the likelihood function
lkh <- function(mu = 0, beta = 0) {
  ts <- data.frame(Individual = c(1, 2, 3, 4, 5, 6),
                   Choice = c("A", "A", "B", "A", "B", "B"),
                   xA = c(5, 2, 5, 1, 4, 3),
                   xB = c(4, 5, 2, 6, 1, 4),
                   yA = c(1, 1, 0, 1, 0, 0),
                   yB = c(0, 0, 1, 0, 1, 1))
  P1A <- (exp(beta * ts$xA[1]) / (exp(beta * ts$xA[1]) + exp(mu + beta * ts$xB[1])))
  P1B <- 1 - P1A
  P2A <- (exp(beta * ts$xA[2]) / (exp(beta * ts$xA[2]) + exp(mu + beta * ts$xB[2])))
  P2B <- 1 - P2A
  P3A <- (exp(beta * ts$xA[3]) / (exp(beta * ts$xA[3]) + exp(mu + beta * ts$xB[3])))
  P3B <- 1 - P3A
  P4A <- (exp(beta * ts$xA[4]) / (exp(beta * ts$xA[4]) + exp(mu + beta * ts$xB[4])))
  P4B <- 1 - P4A
  P5A <- (exp(beta * ts$xA[5]) / (exp(beta * ts$xA[5]) + exp(mu + beta * ts$xB[5])))
  P5B <- 1 - P5A
  P6A <- (exp(beta * ts$xA[6]) / (exp(beta * ts$xA[6]) + exp(mu + beta * ts$xB[6])))
  P6B <- 1 - P6A

  P1A^ts$yA[1] *
    P1B^ts$yB[1] *
    P2A^ts$yA[2] *
    P2B^ts$yB[2] *
    P3A^ts$yA[3] *
    P3B^ts$yB[3] *
    P4A^ts$yA[4] *
    P4B^ts$yB[4] *
    P5A^ts$yA[5] *
    P5B^ts$yB[5] *
    P6A^ts$yA[6] *
    P6B^ts$yB[6]
}

# Evaluate the likelihood function on the grid
L <- lkh(mu = coeffs$Var1, beta = coeffs$Var2)
print(L) # Debugging line to check the output of lkh

L <- data.frame(mu = coeffs$Var1, beta = coeffs$Var2, L)
L <- xtabs(L ~ beta + mu, L) %>% # Convert to cross-tabulation matrix
  unclass() # Drop the xtabs class (plotly does not like it)

likelihood_plot <- plot_ly(z = ~L, x = ~mu, y = ~beta) %>%
  add_surface() %>%
  layout(scene = list(
    xaxis = list(title = "x-axis (mu)"), yaxis = list(title = "y-axis (beta)"), zaxis = list(title = "$z$-axis (L)")))
# This code displays the figure in the Rmd document
# but is not run for knitting to pdf
likelihood_plot

