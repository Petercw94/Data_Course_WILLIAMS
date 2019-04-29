edata("mtcars")
df = mtcars

# fix factors in the data set
df$am = factor(df$am)

# Make a color pallete
pal = c("#417a1f", "#fff838")

# Make the y axis title
my_y_title <- expression(paste(italic("Miles "),"per ", italic("Gallon")))

p1 = ggplot(df, aes(x=hp, y=mpg, color = am)) +
  geom_point(size = 3, alpha = .5) +
  labs(x = "Horsepower", y = my_y_title, color = "Auto/Man")

p2 = p1 + scale_color_manual(labels = c("Auto", "Man"), values = pal) +
  scale_x_discrete(limits = c(100, 150, 200,250,300)) +
  theme(panel.background = element_rect(fill = "Black"), 
        panel.grid = element_line(color = "Black"),
        legend.background = element_rect(fill = "Black"),
        legend.text = element_text(color = "White"),
        legend.title = element_text(color = "White"),
        legend.key = element_rect(fill = "Red", color = "Black"))


