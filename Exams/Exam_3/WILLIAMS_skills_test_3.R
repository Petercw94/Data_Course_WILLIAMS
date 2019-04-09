# Load packages
library(tidyverse)
library(gapminder)

# creating a color palette for plot 1
pal = c("#6b5456","#ec8d1b","#6abf2a","#8b53b7","#70acbe","#01c95b","#c00014","#31332f","#f7d000","#abba00")

# generate data for plot 1
plot1.data = gapminder

glimpse(plot1.data)

# make plot 1
ggplot(plot1.data, aes(x = year, y = lifeExp, color = continent)) +
  geom_point(alpha = 0.3) + geom_smooth(se = FALSE) +
  scale_color_manual(values = pal) +
  labs(title = "Life Exectancy Over Time", 
       x = "Year", 
       y = "Life Expectancy", 
       color = "Continent",
       subtitle = "Colored by Continent") +
  theme(panel.background = element_rect(fill = "White"),
        panel.grid = element_line(color = "Light Grey", size = 0.2),
        legend.key = element_rect(fill = "White", color = "White"),
        axis.ticks = element_blank()) +
  ggsave("WILLIAMS_plot1.png", dpi = 300, width = 5, height = 5)



# make plot 2
set.seed(123)
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
plot2.data <- rbind(a,b,c)

# Just in case the set.seed gets ran again and changes the values
write.csv(plot2.data, "plot2.data.csv")

ggplot(plot2.data, aes(x=x, y=y) ) +
  geom_bin2d() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  ggsave("WILLIAMS_plot2.png", dpi = 300, width = 5, height = 5)
