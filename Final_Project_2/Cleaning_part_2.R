##################################################
#   This script loads and cleans the raw data    #
#   for part 2 and saves the clean data set.     #
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

# change the Time value to a vector of 1:16
long$Time = plyr::mapvalues(long$Time, from = unique(long$Time), to = 1:17)

# combine the two data sets
stress = full_join(long, demi)

# make the column names prettier
names(stress)[8] = "Yoga.Practice..1.never..2.sometimes..3.regularly"

# make the values numeric/factors ####
stress$Reading = as.numeric(stress$Reading)
stress$Time = as.numeric(stress$Time)
stress$Measurement = factor(stress$Measurement)
stress$Gender..1.male..2.female = factor(stress$Gender..1.male..2.female) 
stress$Yoga.Instruction.1.No..2.Yes = factor(stress$Yoga.Instruction.1.No..2.Yes )
stress$Yoga.Practice..1.never..2.sometimes..3.regularly = factor(stress$Yoga.Practice..1.never..2.sometimes..3.regularly)


# Make a time vector 
now <- Sys.time()
tseq <- seq(from = now, length.out = 16, by = "mins")
length(tseq)
tseq
# make the new sequence a character to work with it easier
tseq = as.character(tseq)
# determine how many obs. of each time there was
time.length = stress%>%
  filter(Time == 2)
unique(stress$Time)
# make the new time vector to match the arranged time vector
new.time = c(rep(tseq[1], 70), 
             rep(tseq[2], 70), 
             rep(tseq[3], 70),
             rep(tseq[4], 70),
             rep(tseq[5], 70),
             rep(tseq[6], 70),
             rep(tseq[7], 70),
             rep(tseq[8], 70),
             rep(tseq[9], 70),
             rep(tseq[10], 70),
             rep(tseq[11], 70),
             rep(tseq[12], 70),
             rep(tseq[13], 70),
             rep(tseq[14], 70),
             rep(tseq[15], 70),
             rep(tseq[16], 70),
             rep(tseq[17], 70))
# add the new vector into the arranged data frame
stress = stress %>%
  arrange(Time)
stress$New.Time = new.time

stress = stress[-2]
colnames(stress)[8] = "Time"
# write the new clean data set as a csv ####
write.csv(stress, "./Stress_&_Yoga_time_clean_04_2019.csv", row.names = FALSE)
