library(tidyverse)
library(ggplot2)
library(readr)

# For my skill test 4 I chose to re-do my first Skill Test

read.delim("DNA_Conc_by_Extraction_Date.csv", sep = "\t") -> df

head(df, n=10)
tail(df, n=10)
glimpse(df)

# Task 1: Make a histogram for Katy and Ben's data

hist(df$DNA_Concentration_Katy, 
     main="Histogram for Katy's data", 
     xlab="DNA Concentration", 
     border="black", 
     col="pink", 
     breaks = 5)

hist(df$DNA_Concentration_Ben, 
     main="Histogram for Ben's data", 
     xlab="DNA Concentration", 
     border="black", 
     col="light blue",
     breaks = 5)

# I made the breaks equal just to be fair... and yes, pink for a girl and blue for a boy, it's not sexist I promise.

# Your second task is to look at DNA concentrations from the different extraction years.
df$Year_Collected = as.factor(df$Year_Collected)

#Katy's plot
jpeg("WILLIAMS_Plot_1.png")
p <- ggplot(df, aes(x=Year_Collected, y=DNA_Concentration_Katy)) +
  geom_boxplot() + theme_minimal()

p + ggtitle("Katy's Extractions") +
  xlab("YEAR") + ylab("DNA Concentration")
dev.off()


#Ben's Plot
jpeg("WILLIAMS_Plot_2.png")
p2 <- ggplot(df, aes(x=Year_Collected, y=DNA_Concentration_Ben)) +
  geom_boxplot() + theme_minimal()

p2 + ggtitle("Ben's Extractions") +
  xlab("YEAR") + ylab("DNA Concentration")
dev.off()



# Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... 
# with a plot or with summary stats or both.
# It looks like Ben had consistently higher DNA yields than Katy did..
# .but surely it wasn't uniformly better, right? With some samples, he only had a marginal improvement over Katy.
# With other samples, he had a relatively massive improvement over her.
# Your task here is to write some code that tells us: in which extraction YEAR, 
# was Ben's performance the lowest RELATIVE TO Katy's performance?
df$Year_Collected = as.factor(df$Year_Collected)

modK = summary(df$DNA_Concentration_Katy)
modB = summary(df$DNA_Concentration_Ben)
modK
modB
modK > modB

# This will give us the average difference per year
df_difference = df %>%
  group_by(Year_Collected) %>%
  summarise(avg.diff = abs(mean(DNA_Concentration_Ben) - mean(DNA_Concentration_Katy)))
# This will give us the year the smallest average difference occured
df_difference %>%
  filter(avg.diff == min(df_difference$avg.diff))


#For this final problem, let's just look at Ben's DNA concentration values. 
#I think Katy messed up her PCRs, and at any rate, we can't use them for sequencing.
#Besides, our original purpose for this experiment was to see if DNA extractions sitting 
#in a freezer degraded over time.
#To that end, I want you to make a new data frame (just using Ben's values) 
#that has one column containing the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  
#Just to be clear, this data frame should have only 12 rows (one for each year)! 
#You will need to find a way to take the average of Ben's DNA values in each separate year. 
#A for-loop, or repeated subsetting, or some other way... 
#Once you have this new data frame of averages by year, 
#write some code that shows which extraction year has the highest average DNA concentration 
#(and what that concentration is)
df2 = df %>%
  group_by(Year_Collected) %>%
  summarise(Bens_Average_con = mean(DNA_Concentration_Ben))


write.csv(df2, "Ben_average_by_year.csv", quote = FALSE)


#Here is the code to show the highest average value and its accompanying year
df2[which(df2$Bens_Average_con == max(df2$Bens_Average_con)), ]


