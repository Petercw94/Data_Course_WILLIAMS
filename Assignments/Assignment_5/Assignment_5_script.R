library(tidyverse)

# load the mtcars dataset
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

ggplot(df1, aes(x=hp, y=mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle( "Miles per Gallon ~ Horsepower") +
  labs(x = "Horsepower", y = "Miles per Gallon") +
  ggsave("./mpg_vs_hp_auto.png")


#  plots the effect of weight on miles-per-gallon (with improved labels, again)
#  saves this second plot as a **tiff** image called "mpg_vs_wt_auto.tiff" in your Assignment_5 directory
tiff("mpg_vs_wt_auto.tiff")
ggplot(df1, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle( "Miles per Gallon ~ Weight") +
  labs(x = "Weight", y = "Miles per Gallon") 
dev.off()

#  subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
#  saves that new subset as a csv file called mtcars_max200_displ.csv

df2 = mtcars %>%
  filter(disp <= 200)

write.csv(df2, "mtcars_max200_displ.csv")

# includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
# prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt
sum1 = mtcars %>%
  summarise(max(hp))

sum2 = df1 %>%
  summarise(max(hp))

sum3 = df2  %>%
  summarise(max(hp))

sink("./hp_maximums.txt")
paste("Original =", sum1, "max hp")
paste("Automatic =", sum2, "max hp")
paste("Max200 =", sum3, "max hp")
sink(NULL)



