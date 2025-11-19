# Load required library
library(plotly)

### Define the function
my.fun <- function(x, y) {
  (1/125) * x * exp(-x/5) * exp(-y/5)
}

# Create a grid of x and y values
x <- seq(0, 20, length.out = 50)
y <- seq(0, 20, length.out = 50)

# Compute z values
z <- outer(x, y, my.fun)

# Create the 3D surface plot
fig <- plot_ly(
  x = ~x,
  y = ~y,
  z = ~z,
  type = "surface"
)

# Show the plot
fig


### Define the function
my.fun <- function(x, y) {
  ifelse(x <= y, 24*x*y, 0)
}

# Create a grid of x and y values
x <- seq(0, 1, length.out = 50)
y <- seq(0, 1, length.out = 50)

# Compute z values
z <- outer(x, y, my.fun)

# Create the 3D surface plot
fig2 <- plot_ly(
  x = ~x,
  y = ~y,
  z = ~z,
  type = "surface"
)

# Show the plot
fig2
