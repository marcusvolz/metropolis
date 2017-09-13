# Metropolis: Generative city visualisations

# Packages
library(ggart)
library(tidyverse)
library(tweenr)
library(viridis)

# Make reproducible
set.seed(10000)

# Parameters
n <- 1000 # iterations
r <- 75 # neighbourhood
width <- 10000 # canvas width
height <- 10000 # canvas height
delta <- 2 * pi / 180 # angle direction noise
p_branch <- 0.01 # probability of branching
initial_pts <- 10 # number of initial points
nframes <- 200 # number of tweenr frames

# Function for generating a building
# generate_building <- function(p, q) {
#   p <- runif(2, 0, 10000)
#   q <- runif(2, 0, 10000)
#   midpoint <- (p + q)/2
#   grad <- (q[2] - p[2]) / (q[1] - p[1])
#   grad2 <- -1 / grad
# }

# Initialise data frames
points <- data.frame(x = numeric(n), y = numeric(n), dir = numeric(n), level = integer(n))
edges <-  data.frame(x = numeric(n), y = numeric(n), xend = numeric(n), yend = numeric(n), level = integer(n))

# points[1:initial_pts, ] <- c(runif(initial_pts, 0, width),
#                              runif(initial_pts, 0, height), runif(initial_pts, -2*pi, 2*pi), 1)

if(initial_pts > 1) {
  i <- 2
  while(i <= initial_pts) {
    points[i, ] <- c(runif(1, 0, width), runif(1, 0, height), runif(1, -2*pi, 2*pi), 1)
    i <- i + 1
  }
}

t0 <- Sys.time()

# Main loop ----
i <- initial_pts + 1
while (i <= n) {
  valid <- FALSE
  while (!valid) {
    random_point <- sample_n(points[seq(1:(i-1)), ], 1) # Pick a point at random
    branch <- ifelse(runif(1, 0, 1) <= p_branch, TRUE, FALSE)
    alpha <- random_point$dir[1] + runif(1, -(delta), delta) + (branch * (ifelse(runif(1, 0, 1) < 0.5, -1, 1) * pi/2))
    v <- c(cos(alpha), sin(alpha)) * r * (1 + 1 / ifelse(branch, random_point$level[1]+1, random_point$level[1])) # Create directional vector
    xj <- random_point$x[1] + v[1]
    yj <- random_point$y[1] + v[2]
    lvl <- random_point$level[1]
    lvl_new <- ifelse(branch, lvl+1, lvl)
    if(xj < 0 | xj > width | yj < 0 | yj > height) {
      next
    }
    points_dist <- points %>% mutate(d = sqrt((xj - x)^2 + (yj - y)^2))
    if (min(points_dist$d) >= 1 * r) {
      points[i, ] <- c(xj, yj, alpha, lvl_new)
      edges[i, ] <- c(xj, yj, random_point$x[1], random_point$y[1], lvl_new)
      # Add a building if possible
      buiding <- 
      valid <- TRUE
    }
  }
  i <- i + 1
  print(i)
}

edges <- edges %>% filter(level > 0)

sand <- data.frame(alpha = numeric(0), x = numeric(0), y = numeric(0))
perp <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))

make_instance <- function(a, b) {
  # Function for adding points along a line with endpoints a and b
  sandpaint_line <- function(a, b, n) {
    result <- data.frame(alpha = runif(n, 0, 1)) %>%
      mutate(x = (1 - alpha) * a[1] + alpha * b[1],
             y = (1 - alpha) * a[2] + alpha * b[2])
    result %>% select(x, y)
  }
  
  # Function for interpolating a line in a specified direction
  interpolate_line <- function(a, b, theta, delta) {
    dx <- delta * cos(theta)
    dy <- delta * sin(theta)
    df1 <- data.frame(x = a[1], y = a[2], xend = b[1], yend = b[2])
    df2 <- data.frame(x = a[1] + dx, y = a[2] + dy, xend = b[1] + dx, yend = b[2] + dy)
    df <- list(df1, df2)
    tf <- tween_states(df, tweenlength = 2, statelength = 0, ease = "linear", nframes = nframes)
    tf
  }
  
  # a <- runif(2)
  # b <- runif(2)
  theta <- atan((b[2] - a[2]) / (b[1] - a[1])) + pi/2
  delta <- sqrt(sum((b - a)^2)) * runif(1, 0.25, 5)
  test <- interpolate_line(a, b, theta, delta)
  
  points <- data.frame(x = numeric(0), y = numeric(0), id = integer(0))
  
  for(i in 1:nrow(test)) {
    a <- as.numeric(test[i, c("x", "y")])
    b <- as.numeric(test[i, c("xend", "yend")])
    points <- points %>% rbind(sandpaint_line(a, b, 1000 / i) %>% mutate(id = i))
  }
  points
}

for(i in 1:nrow(edges)) {
  a <- c(edges$x[i], edges$y[i])
  b <- c(edges$xend[i], edges$yend[i])
  temp <- make_instance(a, b)
  sand <- rbind(sand, temp)
}

# Create plot
p <- ggplot() +
  #geom_segment(aes(x, y, xend = xend, yend = yend, size = -level), edges, lineend = "round", colour = "red") +
  #geom_segment(aes(x, y, xend = xend, yend = yend), perp, lineend = "round", alpha = 0.15) +
  #geom_point(aes(x, y), points) +
  geom_point(aes(x, y), sand, size = 0.05, alpha = 0.05) +
  xlim(0, 10000) +
  ylim(0, 10000) +
  coord_equal() +
  #scale_size_continuous(range = c(0.5, 2)) +
  #scale_color_viridis() +
  theme_blankcanvas(margin_cm = 0)

# Save plot
ggsave("plots/plot003.png", p, width = 20, height = 20, units = "in", dpi = 300)
