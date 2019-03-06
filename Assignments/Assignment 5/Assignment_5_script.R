library(tidyverse)

data("mtcars")
mtcars
glimpse(mtcars)
head(mtcars)
?mtcars

# am Transmission (0 = automatic, 1 = manual)
# subsets the mtcars dataframe to include only **automatic transmissions**
# saves this new subset as a new file called "automatic_mtcars.csv" in 
# your Assignment_5 directory

df1 = mtcars %>%
  filter(am == "0")
write.csv(df1, "automatic_mtcars.csv")

#  plots the effect of horsepower on miles-per-gallon (update plot to have meaningful labels and title)
#  saves this plot as a png image called "mpg_vs_hp_auto.png" in your Assignment_5 directory
png("mpg_vs_hp_auto.png")
ggplot(df.aut, aes(x=hp, y=mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle( "Miles per Gallon ~ Horsepower") +
  labs(x = "Horsepower", y = "Miles per Gallon") 
dev.off()

#  plots the effect of weight on miles-per-gallon (with improved labels, again)
#  saves this second plot as a **tiff** image called "mpg_vs_wt_auto.tiff" in your Assignment_5 directory
tiff("mpg_vs_wt_auto.tiff")
ggplot(df.aut, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle( "Miles per Gallon ~ Weight") +
  labs(x = "Weight", y = "Miles per Gallon") 
dev.off()

#  subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
#  saves that new subset as a csv file called mtcars_max200_displ.csv

df %>%
  filter(disp <= 200) %>%
  write.csv("mtcars_max200_displ.csv")

# includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
# prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt
mtcars %>%
  summarise(max(hp))

df1 %>%
  summarise(max(hp))

df2  %>%
  summarise(max(hp))




