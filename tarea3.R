library(tidyverse)
library(modelr)
library(ggplot2)

view(diamonds)



mod <- lm(price~ carat, data = diamonds)



grid <- diamonds %>%
  data_grid(carat)

grid <- grid %>%
  add_predictions(mod)

ggplot(diamonds, aes(x=carat)) +
  geom_point(aes(y = price,color=cut))+
  facet_wrap(~cut)+
  geom_line(aes(y = pred), data = grid, size = 1)

diamonds <- diamonds %>%
  add_residuals(mod)

ggplot(diamonds, aes(x=carat)) +
  geom_point(aes(y = price,color=clarity))+
  facet_wrap(~clarity)+
  geom_line(aes(y = pred), data = grid, size = 1)

mod2  <- lm(log2(price) ~ log2(carat), data = diamonds)

diamonds <- diamonds %>%
  add_residuals(mod2)

ggplot(diamonds,aes(x=log2(carat))) +
  geom_point(aes(y =log2(price)))



