library(tidyverse)


# Load the data and change the appropriate classes
stress = read.csv("./Stress_&_Yoga_time_clean_04_2019.csv", stringsAsFactors = TRUE)
stress$Reading = as.numeric(stress$Reading)
stress$Time = as.numeric(stress$Time)
stress$Measurement = factor(stress$Measurement)
stress$Gender..1.male..2.female = factor(stress$Gender..1.male..2.female) 
stress$Yoga.Instruction.1.No..2.Yes = factor(stress$Yoga.Instruction.1.No..2.Yes )
stress$Yoga.Practice..1.never..2.sometimes..3.regularly = factor(stress$Yoga.Practice..1.never..2.sometimes..3.regularly)
stress = stress %>%
  filter(Measurement != "NA")

# Make a time vector 
now <- Sys.time()
tseq <- seq(from = now, length.out = 16, by = "mins")
length(tseq)
tseq
# make the new sequence a character to work with it easier
tseq = as.character(tseq)
# determine how many obs. of each time there was
time.length = stress%>%
  filter(Time == 1)
# make the new time vector to match the arranged time vector
new.time = c(rep(tseq[1], 350), 
             rep(tseq[2], 350), 
             rep(tseq[3], 350),
             rep(tseq[4], 350),
             rep(tseq[5], 350),
             rep(tseq[6], 350),
             rep(tseq[7], 350),
             rep(tseq[8], 350),
             rep(tseq[9], 350),
             rep(tseq[10], 350),
             rep(tseq[11], 350),
             rep(tseq[12], 350),
             rep(tseq[13], 350),
             rep(tseq[14], 350),
             rep(tseq[15], 350),
             rep(tseq[16], 350))
# add the new vector into the arranged data frame
stress = stress %>%
  arrange(Time)
stress$New.Time = new.time


tseq = as.character(tseq)
stress$Time = as.character(stress$Time)
stress$New.Time = as.POSIXct(stress$New.Time)

stress = stress[,-2]
colnames(stress)[8] = "Time"

write.csv(stress, "time_analysis_stress_df.csv", row.names = FALSE)






