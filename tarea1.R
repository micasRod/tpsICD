library(tidyverse)
library(modelr)
options(na.action = na.warn)
ggplot(sim1, aes(x, y)) +
  geom_point()

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)



grid <- sim1 %>%
  data_grid(x)


grid <- grid %>%
  add_predictions(sim1_mod)

grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_freqpoly(binwidth = 0.5)