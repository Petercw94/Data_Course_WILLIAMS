library(tidyverse)
df = read.csv("./Exercises/ugly_data1.csv")

long = gather(df, Group, Value, 2:9)


malerows = grep("^m", long$Group)
femalerows = grep("^f", long$Group)

long$Gender[malerows] <- "Male"
long$Gender[femalerows] <- "Female"

long$Group = str_remove(long$Group, "^m") 
long$Group = str_remove(long$Group, "^f") 

long$Group = str_replace(long$Group, "014", "0014")

# Try doing the following code but differently using the sep_replace() and separate function. ######

group1 = substr(long$Group, start = 1, stop = 1)
group2 = substr(long$Group, start = 2, stop = 5)
groups = paste0(group1, "-", group2)
long$Group = groups
long = separate(long, Group, into = c("Gender", "AgeRange"), sep = "-")

# This stretch of code accomplishes a similar task as above but much shorter #####

# This is a handy way to insert a dash between our group ages to make them prettier! 

ages1 = substr(long$Group, start = 1, stop = 2)
ages2 = substr(long$Group, start = 2, stop = 4)
ages = paste0(ages1, "-", ages2) # paster0() allows the items to be pasted together without any spaces between it,
# this is the same idea as sep = ""
long$Group = ages

names(long)[names(long) == "Group"] <- "AgeRange"
# is the same as:
names(long)[2] <- "AgeRange"


#Now that we have clean data, it is time to write it out...
write.csv(long, "./Exercises/ugly_data1_cleaned.csv", row.names = FALSE, quote = FALSE)



#ugly data 2 

df = read.csv("./Exercises/ugly_data2.csv")
df1 = df[c(1:9),]
df2 = df[c(20:28), ]
df1$Gender = "Male"
df2$Gender = "Female"
df = full_join(df1, df2)
df = gather(df, key = "AgeGroup", value = "Value", c(2:5))


# Finish cleaning this! 
