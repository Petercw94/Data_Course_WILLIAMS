library(tidyverse)
library(modelr)
library(MASS)
library(GGally)

# I. 	Simple tidying exercise ####

# Read in salaries.csv
# This is faculty salary information from 1995 - Split up by university, state, faculty rank, and university 
# tier

salaries = read.csv("./salaries.csv")
glimpse(salaries)

# Convert to usable tidy format so we can look at "Salary" as a dependent variable (10 points)
long.salaries = gather(salaries, "Rank", "Salary", c(5:7))

#  Create boxplot of salary by University Tier, colored by Faculty Rank (10 points)
# x-axis = Tier
# y-axis = Salary
# Boxplot fill color = Rank
# Title = "Faculty Salaries - 1995"

ggplot(long.salaries, aes(x=Tier, y = Salary, fill = Rank)) +
  geom_boxplot() + 
  ggtitle("Faculty Salaries - 1995")


# Export this delightful boxplot to a file named "LASTNAME_exam2_plot1.jpeg" (10 points)
jpeg("./WILLIAMS_exam2_plot1.jpeg")
ggplot(long.salaries, aes(x=Tier, y = Salary, fill = Rank)) +
  geom_boxplot() + 
  ggtitle("Faculty Salaries - 1995")
dev.off()



# II. Linear modeling and predictions ####


# Read in atmosphere.csv (pretty clean data set)
# These are observations of fungal diversity (number of different species) found 
# in air samples along a time series
# SampleID - The unique sample ID for the observation (dd-mm-YYYY)
# Year - What do you think?
#  Quarter - Q1 = Jan/Feb/Mar, Q2 = Apr/May/Jun, etc
# Month - This stands for "Magpie ovulation number..." no, it's just Month
       #  Mday - Day of the month
       # BarcodeSequence - Not important
       #  Aerosol_Density - Number of detectable particles in the air sample per cubic cm
       #  CO2_Concentration - CO2 ppm on the day the sample was taken
       # Diversity - Number of different fungal species found in the air sample
       # Precip - Precipitation on the sampling day (mm)

atmos = read.csv("./atmosphere.csv")

# Create two different linear models with Diversity as the dependent variable. 
# The second model should have the same terms as the first, but an additional 
# one or two terms as well. (10 points)
pairs(atmos)
names(atmos)

mod1 = lm(Diversity~Aerosol_Density + CO2_Concentration + Precip, data = atmos)
summary(mod1)

mod2 = lm(Diversity~Aerosol_Density + CO2_Concentration + Precip + Year, data = atmos)
summary(mod2)


# Compare the residuals of the two models 
# and document which has better explanatory power for the data (10 points)

# comparing the two models similarity 
anova(mod1, mod2)

#RMSE
sqrt(sum(residuals(mod1)^2)/df.residual(mod1))
sqrt(sum(residuals(mod2)^2)/df.residual(mod2))

# comparing means of residuals squared
mean(residuals(mod1)^2)
mean(residuals(mod2)^2)

# It appears that the models are significantly different and mod2 is a better fit for our data than mod1 ####

# Use these both models to predict Diversity values in the data set (10 points)
atmos = add_predictions(atmos, mod1, var = "mod1pred")
atmos = add_predictions(atmos, mod2, var = "mod2pred")
atmos = mutate(atmos, DIFF.mod1 = abs(mod1pred - Diversity))
atmos = mutate(atmos, DIFF.mod2 = abs(mod2pred - Diversity))

#Restesting the models:
mean(atmos$DIFF.mod1)
mean(atmos$DIFF.mod2)

# Make a plot showing actual Diversity values, along with the two models' predicted Diversity values.
# Use color or some other aesthetic to differentiate the actual values and both predictions (10 points)

ggplot(atmos, aes(x = rownames(atmos), y = Diversity)) +
  geom_jitter() + 
  geom_jitter(aes(y = atmos$mod1pred, color = "Red")) +
  geom_jitter(aes(y = atmos$mod2pred, color = "Blue")) +
  ggtitle("Predicted V. Actual Diversity values based on Models") +
  xlab("Value") + 
  scale_color_discrete(labels = c("Mod2","Mod1"))
 

# Export this plot
jpeg("./WILLIAMS_exam2_plot2.jpeg")
ggplot(atmos, aes(x = rownames(atmos), y = Diversity)) +
  geom_jitter() + 
  geom_jitter(aes(y = atmos$mod1pred, color = "Red")) +
  geom_jitter(aes(y = atmos$mod2pred, color = "Blue")) +
  ggtitle("Predicted V. Actual Diversity values based on Models") +
  xlab("Value") + 
  scale_color_discrete(labels = c("Mod2","Mod1"))
dev.off()
# Write code to show the predicted values of Diversity for each model using the hypothetical data 
# found in hyp_data.csv (10 points)
new.data = read.csv("hyp_data.csv")
mod1.predictions = predict(mod1, newdata = new.data)
mod2.predictions = predict(mod2, newdata = new.data)

# Export a text file that contains the summary output from *both* your 
# models to "model_summaries.txt" (10 points)
write.table(c(mod1.predictions, mod2.predictions), "./model_summaries.txt", row.names = FALSE)

# BONUS: ####
#Add these predicted values (from hypothetical data - Part II, Step 6) to a plot of actual data 
# and differentiate them by color. (10 bonus points possible for a pretty graph) 

# This graph is probably wrong, but it is showing the predicted Diversity values at these Aerosol Densities
ggplot(new.data, aes(x = row.names(new.data), y = Aerosol_Density)) +
  geom_point() +
  geom_point(aes(y= mod1.predictions), color = "Red") +
  geom_point(aes(y = mod2.predictions), color = "Blue") +
  ggtitle("Predicted Diversity Based on Aerosol Density") +
  labs(x = "Sample Number", y = "Aerosol Density")



# BONUS BONUS, clean the bird data ####

# Clean this sh*t up 
bird = read.csv("./Bird_Measurements.csv")
# find columms with mass (except egg mass) 
masscols = c(5,7,9)
# Make a col with all the important information 
impt.cols = c(1:4)
# subset to mass only 
bird.mass = bird[, c(impt.cols,masscols)]
# turn long into wide for mass 
mass.long = gather(bird.mass, Sex, Mass, c(5:7))
mass.long$Sex = str_remove(mass.long$Sex, "_mass")
unique(mass.long$Sex)
# Make an N df 
N_mass = c(6,8,10)
bird.mass.N = bird[, c(impt.cols, N_mass)]
bird.mass.N = gather(bird.mass.N, "Sex", "Mass.N", c(5,6,7))
bird.mass.N$Sex = str_remove(bird.mass.N$Sex, "_mass_N")
unique(bird.mass.N$Sex)
# Join them!  
mass.final = full_join(x = mass.long, y = bird.mass.N)
unique(mass.final$Sex)
# subset to wings only 
wingcols = c(11, 13, 15)
bird.wing = bird[, c(impt.cols,wingcols)]
# turn long into wide for wing
wing.long = gather(bird.wing, Sex, Wing, c(5:7))
wing.long$Sex = str_remove(wing.long$Sex, "_wing")
unique(wing.long$Sex)
# Make an N df
N_wing = c(12,14,16)
bird.wing.N = bird[, c(impt.cols, N_wing)]
bird.wing.N = gather(bird.wing.N, "Sex", "Wing.N", c(5,6,7))
bird.wing.N$Sex = str_remove(bird.wing.N$Sex, "_wing_N")
unique(bird.wing.N$Sex)
# Join them!  
wing.final = full_join(x = wing.long, y = bird.wing.N)
unique(wing.final$Sex)
# subset to tarsus only 
tarsuscols = c(17, 19, 21)
bird.tarsus = bird[, c(impt.cols,tarsuscols)]
# turn long into wide for wing 
tarsus.long = gather(bird.tarsus, Sex, Tarsus, c(5:7))
tarsus.long$Sex = str_remove(tarsus.long$Sex, "_tarsus")
unique(tarsus.long$Sex)
# Make an N df 
N_tarsus = c(18,20,22)
bird.tarsus.N = bird[, c(impt.cols, N_tarsus)]
bird.tarsus.N = gather(bird.tarsus.N, "Sex", "Tarsus.N", c(5,6,7))
bird.tarsus.N$Sex = str_remove(bird.tarsus.N$Sex, "_tarsus_N")
unique(bird.tarsus.N$Sex)
# Join them!  
tarsus.final = full_join(x = tarsus.long, y = bird.tarsus.N)
unique(tarsus.final$Sex)
unique(tarsus.final$Tarsus.N)
# subset to bill only 
billcols = c(23, 25, 27)
bird.bill = bird[, c(impt.cols,billcols)]
# turn long into wide for wing 
bill.long = gather(bird.bill, Sex, Bill, c(5:7))
bill.long$Sex = str_remove(bill.long$Sex, "_bill")
unique(bill.long$Sex)
# Make an N df 
N_bill = c(24,26,28)
bird.bill.N = bird[, c(impt.cols, N_bill)]
bird.bill.N = gather(bird.bill.N, "Sex", "Bill.N", c(5,6,7))
bird.bill.N$Sex = str_remove(bird.bill.N$Sex, "_bill_N")
unique(bird.bill.N$Sex)
# Join them!  
bill.final = full_join(x = bill.long, y = bird.bill.N)
unique(bill.final$Sex)
unique(bill.final$Bill.N)
# subset to tail only 
tailcols = c(29, 31, 33)
bird.tail = bird[, c(impt.cols,tailcols)]
# turn long into wide for wing 
tail.long = gather(bird.tail, Sex, Tail, c(5:7))
tail.long$Sex = str_remove(tail.long$Sex, "_tail")
unique(tail.long$Sex)
# Make an N df 
N_tail = c(30,32,34)
bird.tail.N = bird[, c(impt.cols, N_tail)]
bird.tail.N = gather(bird.tail.N, "Sex", "Tail.N", c(5,6,7))
bird.tail.N$Sex = str_remove(bird.tail.N$Sex, "_tail_N")
unique(bird.tail.N$Sex)
# Join them!  
tail.final = full_join(x = tail.long, y = bird.tail.N)
unique(tail.final$Sex)
unique(tail.final$Tail.N)
# Account for the last two columns 
lastcols = c(35, 36, 37)
bird.last = bird[, c(impt.cols,lastcols)]
# Now join them all!  
df2 = full_join(x = mass.final, y = wing.final)
df2 = full_join(x = df2, y = tarsus.final)
df2 = full_join(x = df2, y = bill.final)
df2 = full_join(x = df2, y = tail.final)
df2 = full_join(x = df2, y = bird.last)
# I omitted the NAs 
bird.clean = na.omit(df2)
bird.cleaner = gather(df2, "Characteristic", "Value", c(6:18))
bird.cleaner = na.omit(bird.cleaner)
write.csv(bird.cleaner, "./Bird_Measurements_clean.csv")



