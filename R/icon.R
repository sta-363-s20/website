library(hexSticker)
library(tidyverse)
library(ElemStatLearn)
library(class)
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod15 <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod15, "prob")
prob <- ifelse(mod15 == "1", prob, 1 - prob) 
px1 <- mixture.example$px1
px2 <- mixture.example$px2


m <- tibble(
  x = rep(px1, length(px2)),
  y = rep(px2, each = length(px1)),
  z = prob
)

d <- tibble(
  a = x[,1],
  b = x[,2],
  g = ifelse(g == 1, "cornflower blue", "orange")
)

p <- ggplot(d, aes(x = a, y = b)) +
  geom_point(aes(color = g), size = 0.1) +
  scale_color_manual(values = c("cornflower blue" = "cornflower blue", "orange" = "orange")) + 
  geom_contour(data = m, binwidth = 0.55, aes(x = x, y = y, z = z), color = "#798EA4") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

sticker(p,
        package = "STA 363", 
        p_size = 6,
        s_x = 1,
        s_y = .75,
        s_width = 2,
        s_height = 1.2,
        filename = "favicon.png",
        h_color = "#798EA4",
        h_fill = "white",
        p_color = "#798EA4")
