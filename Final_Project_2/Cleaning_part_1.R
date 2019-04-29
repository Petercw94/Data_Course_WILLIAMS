##################################################
#   This script loads and cleans the raw data    #
#   for part 1 and saves the clean data set.     #
#                                                #
#   Author: Peter Williams                       #
#                                                #
##################################################

# Load the libraries
library(tidyverse)

# Load the data and the demographics ####
df = read.csv("./Raw_data/SR_stress_response_without_NAs.csv")
demi = read.csv("./Raw_data/SR_Demographics - Sheet1.csv")

# Clean the demographics ####

# make each subject ID just a number that can be filtered
demi$Research.ID = str_remove(demi$Research.ID, "SR_")
demi$Research.ID = str_remove(demi$Research.ID, "^0")
demi$Research.ID = as.numeric(demi$Research.ID)
names(demi)[1] = "Subject.ID"

# filter the IDs to just 41-60 and the IDs that correlate to data collected
demi = demi %>%
  filter(Subject.ID < 61) %>%
  filter(Subject.ID != 43) %>%
  filter(Subject.ID != 44) %>%
  filter(Subject.ID != 49) %>%
  filter(Subject.ID != 50) %>%
  filter(Subject.ID != 52) %>%
  filter(Subject.ID != 53) %>%
  arrange(Subject.ID)

# filter the demi data set for gender and yoga experience 
colnames(demi)
demi = demi[,c(1,3,4,13,14)]
demi$Yoga.Practice...................1.never..2.sometimes..................3.regularly = str_replace(demi$Yoga.Practice...................1.never..2.sometimes..................3.regularly, "always--most days of the week", "3")
demi$Subject.ID = as.numeric(demi$Subject.ID)

# change the subject_ID to match the demigraphics
df$Subject.ID = str_remove(df$Subject.ID, "^SR")
df$Subject.ID = as.numeric(df$Subject.ID)
unique(demi$Subject.ID)
unique(df$Subject.ID)

# clean the data set ####
long = gather(df, Measurement, Reading, c(2:length(df)))
long = separate(long, Measurement, into = c("Time", "Measurement"))

# change the time values 
long$Time = plyr::mapvalues(long$Time, from = "BL", to = "Pre_Stressor")
long$Time = plyr::mapvalues(long$Time, from = "Stress", to = "Stressor")
long$Time = plyr::mapvalues(long$Time, 
                            from = c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"), 
                            to = rep("Post_Stressor", times = 15))

# combine the two data sets ####
stress = full_join(long, demi)

# make the column names prettier
names(stress)[8] = "Yoga.Practice..1.never..2.sometimes..3.regularly"

# make the values numeric/factors
stress$Reading = as.numeric(stress$Reading)
stress$Time = factor(stress$Time, ordered = TRUE, c("Pre_Stressor", "Stressor", "Post_Stressor"))
stress$Measurement = factor(stress$Measurement)
stress$Gender..1.male..2.female = factor(stress$Gender..1.male..2.female) 
stress$Yoga.Instruction.1.No..2.Yes = factor(stress$Yoga.Instruction.1.No..2.Yes )
stress$Yoga.Practice..1.never..2.sometimes..3.regularly = factor(stress$Yoga.Practice..1.never..2.sometimes..3.regularly)

# write your new tidy data sets so you don't have to re-clean ####
write.csv(stress, "./Stress_&_Yoga_clean_04_2019.csv", row.names = FALSE)
write.csv(demi, "./demographics.cleaned.csv", row.names = FALSE)




      



  
               
