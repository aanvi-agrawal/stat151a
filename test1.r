# Load libraries
library(ggplot2)

# Create data
x <- 1:10
y <- x^2

# Plot
plot(x, y, type = "b", col = "blue")

# Linear model
model <- lm(y ~ x)
summary(model)

