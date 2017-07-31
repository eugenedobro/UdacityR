# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.
library(ggplot2)
library(dplyr)

ggplot(aes(x = price, color = I('black'), fill = I('blue')), data = diamonds) +
  geom_histogram(binwidth = 2500) +
  facet_grid(~color)

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.
ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(color = cut))

# Typical table range for the majority of diamonds of Ideal & Premium cut
summary(subset(diamonds, cut == "Ideal")$table)
summary(subset(diamonds, cut == "Premium")$table)

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = subset(diamonds, diamonds$volume < quantile(diamonds$volume, .99))) +
  geom_point(aes(color = clarity)) +
  scale_y_log10() +
  scale_color_brewer(type = 'div')

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.
pf <- read.delim('/datasets/ud651/pseudo_facebook.tsv')

pf$prop_initiated <- pf$friendships_initiated / pf$friend_count
summary(pf$prop_initiated)


# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

ggplot(aes(x = tenure, y = prop_initiated),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.
ggplot(aes(x = 7 * round(tenure / 30), y = prop_initiated),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)

summary(pf[pf$year_joined.bucket == "(2012,2014]",]$prop_initiated)

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.
diamonds$pcr <- diamonds$price / diamonds$carat

ggplot(aes(x = cut, y = price/carat), data = diamonds) +
  geom_point(aes(color = color)) +
  facet_grid(~clarity) +
  scale_color_brewer(type = 'div')


