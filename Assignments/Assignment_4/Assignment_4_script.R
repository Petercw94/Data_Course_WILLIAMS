# Now, for the rest of the assignment...

# 1.  Create a new R script as part of your Assignment 4 R-project. Name it "Assignment_4.R"
# 2.  That script should do the following:
#  + Read in the file: "/Data/ITS_mapping.csv"   ...good luck with that, hahaha!
#  + Somehow summarize all of the columns and do a bit of additional exploration (play with some functions)
# + Make a boxplot where "Ecosystem"" is on the x-axis and "Lat" is on the y-axis
# + Write code to export this boxplot to a new file in your Assignment_4 directory called "silly_boxplot.png"
# Hints on below ...
# 3.  Make sure to save your completed script and Rproject and make sure your png file is saved correctly
# 4.  Push all these saved changes and new files onto your GitHub repository so I can grade them
# 5.  Don't forget the plaintext file with answers to bolded questions needs to go to Canvas as well!    


library(readr)
library(ggplot2)
library(tidyverse)


read.delim("ITS_mapping.csv", sep = "\t", stringsAsFactors = FALSE) -> df2 #ITS_mapping.csv is really a tab separated file, so running it as a tsv makes it 
# readable .... How would I run this same code with read_tsv? I want to change the argument stringsAsFactors, but it 
# doesnt seem to be an available function.
head(df2, n=10)
tail(df2, n=10)
glimpse(df2)
dim(df2)

summary(df2) 

png("silly_boxplot.png")
ggplot(df2, aes(x=Ecosystem, y=Lat)) +
  geom_boxplot()
dev.off()
