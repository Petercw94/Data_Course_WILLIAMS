# Load the libraries ####
library(tidyverse)
library(modelr)

# Load the data and change the appropriate classes
stress = read.csv("./Stress_&_Yoga_time_clean_04_2019.csv", stringsAsFactors = TRUE)
stress$Reading = as.numeric(stress$Reading)
stress$Time = as.POSIXct(stress$Time)
stress$Measurement = factor(stress$Measurement)
stress$Gender..1.male..2.female = factor(stress$Gender..1.male..2.female) 
stress$Yoga.Instruction.1.No..2.Yes = factor(stress$Yoga.Instruction.1.No..2.Yes )
stress$Yoga.Practice..1.never..2.sometimes..3.regularly = factor(stress$Yoga.Practice..1.never..2.sometimes..3.regularly)
stress = stress %>%
  filter(Measurement != "NA")

# Create a Palette
pal = c("#2176ff","#4cc401", "#ff4c4c")

# Graphing part 2
ggplot(stress, aes(x = Time, 
                   y = Reading, 
                   color = Yoga.Practice..1.never..2.sometimes..3.regularly)) +
  geom_smooth(se = FALSE) + 
  geom_point(alpha = 0.3) +
  facet_wrap(~Measurement, scales = "free") +
  theme_minimal() + 
  scale_color_manual(labels = c("Never", "Sometimes", "Regularly"), values = pal) +
  labs(color = "Yoga Practice", title = "Reading over Time described by Yoga Practice", 
       subtitle = "Separated by Measurement")
