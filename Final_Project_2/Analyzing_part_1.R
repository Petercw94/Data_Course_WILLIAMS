##################################################
#   This script loads and analyzes the cleaned   #
#   data for part 1                              #
#                                                #
#   Author: Peter Williams                       #
#                                                #
##################################################

# Load the libraries ####
library(tidyverse)
library(modelr)

# Load the data and change the appropriate classes
stress = read.csv("./Stress_&_Yoga_clean_04_2019.csv", stringsAsFactors = TRUE)
stress$Reading = as.numeric(stress$Reading)
stress$Time = factor(stress$Time, ordered = TRUE, c("Pre_Stressor", "Stressor", "Post_Stressor"))
stress$Measurement = factor(stress$Measurement)
stress$Gender..1.male..2.female = factor(stress$Gender..1.male..2.female) 
stress$Yoga.Instruction.1.No..2.Yes = factor(stress$Yoga.Instruction.1.No..2.Yes )
stress$Yoga.Practice..1.never..2.sometimes..3.regularly = factor(stress$Yoga.Practice..1.never..2.sometimes..3.regularly)

# Make each variable a data frame to analyze####
# HF 
HF = stress %>%
  filter(Measurement == "HF")
# HR
HR = stress %>%
  filter(Measurement == "HR")
# SV 
SV = stress %>%
  filter(Measurement == "SV")
# LF
LF = stress %>%
  filter(Measurement == "LF")
# VLF 
VLF = stress %>%
  filter(Measurement == "VLF")

# Build a model for each data frame ####

# HR 
modHR = aov(Reading ~ Time + Gender..1.male..2.female * 
              Yoga.Practice..1.never..2.sometimes..3.regularly * 
              Yoga.Instruction.1.No..2.Yes, 
            data = HR)
sumHR = summary(modHR)

# HF 
modHF = aov(Reading ~ Time + Gender..1.male..2.female * 
              Yoga.Practice..1.never..2.sometimes..3.regularly * 
              Yoga.Instruction.1.No..2.Yes, 
            data = HF)
sumHF = summary(modHF)

# LF 
modLF = aov(Reading ~ Time + Gender..1.male..2.female * 
              Yoga.Practice..1.never..2.sometimes..3.regularly * 
              Yoga.Instruction.1.No..2.Yes, 
            data = LF)
sumLF = summary(modLF)

# SV 
modSV = aov(Reading ~ Time + Gender..1.male..2.female * 
              Yoga.Practice..1.never..2.sometimes..3.regularly * 
              Yoga.Instruction.1.No..2.Yes, 
            data = SV)
sumSV = summary(modSV)

# VLF 
modVLF = aov(Reading ~ Time + Gender..1.male..2.female * 
              Yoga.Practice..1.never..2.sometimes..3.regularly * 
              Yoga.Instruction.1.No..2.Yes, 
            data = VLF)
sumVLF = summary(modVLF)

# Write the model summaries as a CSV ####
sink("model_summaries.txt")
print(sumHR)
print(sumHF)
print(sumSV)
print(sumLF)
print(sumVLF)
sink(NULL)

# Write the data frames as their own csv ####
write.csv(HR, "./HR_only.csv", row.names = FALSE)
write.csv(SV, "./SV_only.csv", row.names = FALSE)
write.csv(HF, "./HF_only.csv", row.names = FALSE)
write.csv(LF, "./LF_only.csv", row.names = FALSE)
write.csv(VLF, "./VLF_only.csv", row.names = FALSE)
