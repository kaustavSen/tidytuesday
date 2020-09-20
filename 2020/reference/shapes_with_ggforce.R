library(tidyverse)
library(ggforce)

rounded_rec <- data.frame(
  x = c(0, 30, 30, 29, 28, 0),
  y = c(0, 0, 18.5, 19.5, 20, 20)
)

handle <- data.frame(
  x = c(-35, 5, 5, -35),
  y = c(16, 16, 20, 20)
)

ggplot(rounded_rec, aes(x, y)) +
  geom_shape(data = handle, radius = unit(0.5, "cm"),
               fill = "grey20", color = NA) +
  geom_polygon(fill = "grey80", color = NA) +
  annotate("tile", x = 15, y = 0, width = 30, 
           height = 3, fill = "grey45") +
  annotate("tile", x = 22.5, y = 0, width = 15, 
           height = 3, fill = "grey55") +
  annotate("point", x = -31, y = 18, size = 10, color = "grey75") +
  theme_void()

df <- data.frame(
  x = c(-5, 2.5, -2.5, 5, 2.5, -2.5),
  y = c(0, 4, 4, 0, -4, -4),
  angle = c(0, 60, 120, 180, 300, 240)
)

ggplot(df) +
  geom_ellipse(aes(x0 = x, y0 = y, a = 5, b = 2.5, m1 = 1.5, 
                   angle = angle * pi / 180),
               color = NA, fill = "red", alpha = 1/10) +
  geom_ellipse(aes(x0 = x + 21, y0 = y, a = 5, b = 2.5, m1 = 1.5, 
                   angle = angle * pi / 180),
               color = NA, fill = "red", alpha = 1/10) +
  annotate("curve", x = 0, y = 0,
           xend = 0, yend = -25, curvature = -0.3, 
           size = 1.5, color = "brown") +
  annotate("point", x = 21, y = 0, size = 8, 
           color = "red", alpha = 0.65) +
  annotate("curve", x = 0 + 21, y = 0,
           xend = 0 + 21, yend = -25, curvature = 0.3, 
           size = 1.5, color = "brown") +
  annotate("point", x = 0, y = 0, size = 8, 
           color = "red", alpha = 0.65) +
  coord_equal() +
  theme_minimal()
