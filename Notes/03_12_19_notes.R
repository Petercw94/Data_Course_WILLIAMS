library(tidyverse)


# load the data
data(mtcars)
df = mtcars

# clean data
df$am = factor(df$am)

# custom palette
pal = c("#417a1f", "#fff838")

# initial plot
p1 = ggplot(df, aes(x = hp, y = mpg, color = am)) +
  geom_point(size = 5, alpha = 0.5)
p1 

# making ggplot prettier
# p1 = p1 + labs(title = "MPG vs HP", x = "Horsepower", y = "Miles per Gallon", color = "Auto/Man") 
# Italicizing certain words in the axis
p1 = p1 + labs(x="Horsepower", y= expression(paste(italic("Miles"), "  per " ,italic("Gallon"))), color = "Auto/Man")

# google color picker to pick a color and get the code
# More makeover on the plot
p2 = p1 + scale_color_manual(labels = c("Auto", "Man"), values = pal) + 
  theme(plot.title = element_text(face = "italic", hjust = 0.5), 
        panel.background = element_rect(fill = "Black"), 
        axis.ticks = element_line(colour = NULL), 
        panel.grid = element_line(color = "Black"),
        legend.background = element_rect(fill = "Black"),
        legend.key = element_rect(fill = "Red", color = "Black"), 
        legend.text = element_text(color = "White"),
        legend.title = element_text(color = "White")) 

jpeg("../Replicate_this_plot.jpeg")
p2
dev.off()


# play around with the themes
p1 + theme_minimal()

# lets play some more
p2 + theme(axis.title = element_text(face = "italic"))




p3 = ggplot(df, aes(x=am, y = mpg)) 
p3

p3 + geom_boxplot()
p3 + geom_violin() + geom_point(alpha = 0.5)



# load another data set

sal = read.csv("./Exams/Exam_2/salaries.csv")
long = gather(sal, Rank, Salary, 5:7)

ggplot(long, aes(x = Rank, y = Salary, fill = Tier, color = Tier)) +
  geom_violin() + geom_point(alpha = 0.5, position = "jitter") +
  scale_color_manual(values = wesanderson::wes_palette(3)) + 
  scale_fill_manual(values = wesanderson::wes_palette(3))


#installing colorblindr

devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")

devtools::install_github("clauswilke/colorblindr")


