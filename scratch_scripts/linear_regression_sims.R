
library(tidyverse)


# Basic data frame
basic.data <- data.frame(x = rnorm(50, 70, 3))

# High-correlation data
high.cor <- basic.data %>%
  mutate(err = rnorm(50, 0, 1),
         y = 10 + x*3 + err)

# Summary
summary(lm(high.cor$y ~ high.cor$x))

# Plot
high.cor %>%
  ggplot(aes(x = x, y = y)) + 
  geom_smooth(method = 'lm', se = F, colour = "black", linewidth = 1.5) +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black'))

# Medium-correlation data
med.cor <- basic.data %>%
  mutate(err = rnorm(50, 0, 5),
         y = 10 + x*3 + err)

# Summary
summary(lm(med.cor$y ~ med.cor$x))

# Plot
med.cor %>%
  ggplot(aes(x = x, y = y)) + 
  geom_smooth(method = 'lm', se = F, colour = "black", linewidth = 1.5) +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black'))

# Low-correlation data
low.cor <- basic.data %>%
  mutate(err = rnorm(50, 0, 14),
         y = 10 + x*3 + err)

# Summary
summary(lm(low.cor$y ~ low.cor$x))

# Plot
low.cor %>%
  ggplot(aes(x = x, y = y)) + 
  geom_smooth(method = 'lm', se = F, colour = "black", linewidth = 1.5) +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black'))

# Q-Q plot

# Generate sample data for s-shaped plot with light/heavy tails
s_shaped_data <- data.frame(x = (rt(n=100, df=2)))

# Generate QQ plot against Normal Distribution
s_shaped_data %>%
  ggplot(aes(sample = x)) + 
  stat_qq_line(colour = 'black', linewidth = 1.1, linetype = "dashed") +
  stat_qq(size = 3, colour = "#ED4865") +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Generate sample data for right-skewed concave-up
concave_up_data <- data.frame(x = (rexp(150, rate = .5)))

# Generate QQ plot against Normal Distribution
concave_up_data %>%
  ggplot(aes(sample = x)) + 
  stat_qq_line(colour = 'black', linewidth = 1.1, linetype = "dashed") +
  stat_qq(size = 3, colour = "#ED4865") +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Generate sample data for right-skewed concave-down
concave_down_data <- data.frame(x = -rgamma(n = 500, shape = 2, rate = 1))

# Generate QQ plot against Normal Distribution
concave_down_data %>%
  ggplot(aes(sample = x)) + 
  stat_qq_line(colour = 'black', linewidth = 1.1, linetype = "dashed") +
  stat_qq(size = 3, colour = "#ED4865") +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Generate sample data for normal Q-Q
norm_data <- data.frame(x = rnorm(150, 0, 1))

# Generate QQ plot against Normal Distribution
norm_data %>%
  ggplot(aes(sample = x)) + 
  stat_qq_line(colour = 'black', linewidth = 1.1, linetype = "dashed") +
  stat_qq(size = 3, colour = "#ED4865") +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Simulate fan-shaped residual plot
x <- seq(1, 100, length.out = 100)

# 2. Generate an error term where variance increases with x
# The standard deviation of the error is made proportional to x
error_sd <- 0.5 + 0.1 * x 
errors <- rnorm(100, mean = 0, sd = error_sd)

# 3. Generate the response variable (y)
# A linear relationship is used: y = 2 + 0.5*x + errors
y <- 2 + 0.5 * x + errors

# 4. Fit a linear model to the data
model <- lm(y ~ x)

# Data frame for residuals
fan_resids <- data.frame(x = fitted(model),
                         y = residuals(model))

# Plot
fan_resids %>%
  ggplot(aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linewidth = 1.2, linetype = "dashed") +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Fitted Values", y = "Residuals")


# Simulate normal residual plot
x <- 1:100
# Generate 'y' with a linear trend (2*x + 5) and normal random noise (mean 0, sd 10)
y <- 5 + 2 * x + rnorm(100, mean = 0, sd = 10)
sim_data <- data.frame(x, y)

# 2. Fit a linear model
model <- lm(y ~ x, data = sim_data)

# Data frame for residuals
norm_resids <- data.frame(x = fitted(model),
                          y = residuals(model))

# Plot
norm_resids %>%
  ggplot(aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linewidth = 1.2, linetype = "dashed") +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Fitted Values", y = "Residuals")

# Simulate wave-shaped residual plot
n <- 100
x <- seq(1, 10, length.out = n)

# Generate a true, non-linear relationship (e.g., linear + sine wave)
# True model: Y = 2 + 0.5X + 2*sin(X) + error
y_true <- 2 + 0.5 * x + 2 * sin(x)
error <- rnorm(n, mean = 0, sd = 0.5)
y <- y_true + error

# 2. Fit a Linear Model (incorrectly assuming linear relationship)
model <- lm(y ~ x)

# Data frame for residuals
wave_resids <- data.frame(x = fitted(model),
                          y = residuals(model))

# Plot
wave_resids %>%
  ggplot(aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linewidth = 1.2, linetype = "dashed") +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Fitted Values", y = "Residuals")

#  Simulate quadratic residual plot
x <- seq(1, 100, length.out = 100)
# True relationship is y = x^2, with added noise
y <- 5 + 0.5 * x + 0.05 * x^2 + rnorm(100, sd = 10)

# 2. Fit a linear model (mis-specified)
model <- lm(y ~ x)

# Data frame for residuals
curved_resids <- data.frame(x = fitted(model),
                            y = residuals(model))

# Plot
curved_resids %>%
  ggplot(aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linewidth = 1.2, linetype = "dashed") +
  geom_point(colour = "#ED4865", size = 3) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(x = "Fitted Values", y = "Residuals")
