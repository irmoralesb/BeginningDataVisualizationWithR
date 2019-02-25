
library(ggplot2)
movies = read.csv("../data/Movies.csv")
main_title <- "Distribution of Movies Runtimes"
x_lab = "Runtime in minutes"

ggplot(
  data = movies,
  aes(x = Runtime, stat = "count")) +
  geom_dotplot(
    binwidth = 1,
    stackdir = "center") +
  ggtitle(main_title) +
  xlab(x_lab)

ggplot(
  data = movies,
  aes(x= Runtime, y=Runtime)) +
  geom_boxplot() + 
  coord_flip() +
  ggtitle(main_title) + 
  xlab("") +
  ylab(x_lab) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

ggplot(
  data = movies,
  aes(x = Runtime)) +
  geom_histogram(binwidth = 10) +
  ggtitle(main_title) + 
  xlab(x_lab)

ggplot(
  data = movies,
  aes(x=Runtime)) +
  geom_density() +
  ggtitle(main_title) +
  xlab(x_lab)


#Plot small multiples of all four
# This was not working on R notebooks, so testing out for standard R script file
while (!is.null(dev.list()))  dev.off()

library(grid)

viewport <- viewport(
  layout = grid.layout(4,1))

pushViewport(viewport)

dot <-ggplot(
  data = movies,
  aes(x = Runtime, ..count..)) +
  geom_dotplot(
    binwidth = 0.25) +
  scale_x_continuous(
    limits = c(0,250)) +
  ggtitle(main_title) +
  xlab("")

print(
  x = dot,
  vp = viewport(
    layout.pos.row = 1,
    layout.pos.col = 1))

box <- ggplot(
  data = movies,
  aes(x = Runtime, y = Runtime)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(
    limits = c(0,250)) + 
  xlab("")

print(
  x = box,
  vp = viewport(
    layout.pos.row = 2,
    layout.pos.col = 1))

hist <- ggplot(
  data = movies, 
  aes(x = Runtime)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(
    limits = c(0, 250)) +
  xlab("")

print(
  x = hist,
  vp = viewport(
    layout.pos.col = 1,
    layout.pos.row = 3)) 

density <- ggplot(
  data = movies, 
  aes(x = Runtime)) +
  geom_density() +
  scale_x_continuous(
    limits = c(0, 250)) +
  xlab(x_lab)

print(
  x = density,
  vp = viewport(
    layout.pos.col = 1,
    layout.pos.row = 4))