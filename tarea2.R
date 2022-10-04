library(tidyverse)
library(modelr)

view(sim3)

ggplot(sim3, aes(x1)) +
  geom_point(aes(y = y))

sim3_mod1 <- lm(y ~ x1 +x2, data = sim3)
sim3_mod2 <- lm(y ~ x2*x1, data = sim3)

grid <- sim3 %>%
  data_grid(x1,x2)


grid <- grid %>%
  add_predictions(sim3_mod1)



grid2 <- grid %>%
  add_predictions(sim3_mod2)

ggplot(sim3, aes(x=x1)) +
  geom_point(aes(y = y,color=x2)) +
  geom_line(aes(y = pred, color=x2), data = grid, size = 1)

ggplot(sim3, aes(x=x1)) +
  geom_point(aes(y = y,color=x2)) +
  geom_line(aes(y = pred,color=x2), data = grid2, size = 1)