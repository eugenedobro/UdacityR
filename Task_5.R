# Two variables assigment.
# Problem set exploration


# First task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.
data("diamonds")
str(diamonds)
ggplot(aes(x = price, y = x), data = diamonds) +
  geom_point()
# Correlation between price and x,y,z
cor.test(diamonds$price, diamonds$x)
cor.test(diamonds$price, diamonds$y)
cor.test(diamonds$price, diamonds$z)


# Create a simple scatter plot of price vs depth.
ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point()
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units.
ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point(alpha = 1/100) + 
  scale_x_continuous() 

summary(diamonds$depth)

cor.test(diamonds$price, diamonds$depth)


# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
ggplot(aes(x = price, y = carat), data = diamonds) +
  geom_point() +
  xlim(0, quantile(diamonds$price, .99)) +
  ylim(0, quantile(diamonds$carat, .99))


# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.
# Create a new variable for volume in the diamonds data frame.
diamonds$volume <- with(diamonds, x*y*z)
ggplot(aes(x = price, y = volume), data = diamonds) +
  geom_point()

# Outliers
sum(ifelse(diamonds$volume == 0 | diamonds$volume >= 800, 1, 0))

# Correlation between price and volume for diamonds with volume greater that 0 and less than 800
with(subset(diamonds, diamonds$volume > 0 & diamonds$volume < 800),cor.test(price, volume))


# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot.
ggplot(aes(x = price, y = volume), data = subset(diamonds, diamonds$volume > 0 & diamonds$volume < 800)) +
  geom_point(alpha = 0.025) +
  geom_smooth(method = "lm")


# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
data(diamonds)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)
head(diamondsByClarity)

# Create summary data frames with the mean price
# by clarity and color.
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

ggplot(aes(x = mean_price, color = I('black'), fill = I('blue')), data = diamonds_mp_by_color) +
  geom_histogram() +
  facet_grid(~color)

ggplot(aes(x = mean_price, color = I('black'), fill = I('blue')), data = diamonds_mp_by_clarity) +
  geom_histogram() +
  facet_grid(~clarity)

