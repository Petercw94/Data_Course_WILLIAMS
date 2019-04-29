library(tidyverse)
library(modelr)

# Clean this sh*t up ####
bird = read.csv("./Data/Bird_Measurements.csv")

names(bird)
# find columms with mass (except egg mass) ####
masscols = c(5,7,9)
# Make a col with all the important information ####
impt.cols = c(1:4)

# subset to mass only ####
bird.mass = bird[, c(impt.cols,masscols)]

# turn long into wide for mass ####
mass.long = gather(bird.mass, Sex, Mass, c(5:7))
mass.long$Sex = str_remove(mass.long$Sex, "_mass")
unique(mass.long$Sex)

# Make an N df ####
N_mass = c(6,8,10)
bird.mass.N = bird[, c(impt.cols, N_mass)]
bird.mass.N = gather(bird.mass.N, "Sex", "Mass.N", c(5,6,7))
bird.mass.N$Sex = str_remove(bird.mass.N$Sex, "_mass_N")
unique(bird.mass.N$Sex)
# Join them!  ####
mass.final = full_join(x = mass.long, y = bird.mass.N)
unique(mass.final$Sex)



names(bird)
# subset to wings only ####
wingcols = c(11, 13, 15)
bird.wing = bird[, c(impt.cols,wingcols)]

# turn long into wide for wing####
wing.long = gather(bird.wing, Sex, Wing, c(5:7))
wing.long$Sex = str_remove(wing.long$Sex, "_wing")
unique(wing.long$Sex)

# Make an N df####
N_wing = c(12,14,16)
bird.wing.N = bird[, c(impt.cols, N_wing)]
bird.wing.N = gather(bird.wing.N, "Sex", "Wing.N", c(5,6,7))
bird.wing.N$Sex = str_remove(bird.wing.N$Sex, "_wing_N")
unique(bird.wing.N$Sex)

# Join them!  ####
wing.final = full_join(x = wing.long, y = bird.wing.N)
unique(wing.final$Sex)



names(bird)
# subset to tarsus only ####
tarsuscols = c(17, 19, 21)
bird.tarsus = bird[, c(impt.cols,tarsuscols)]

# turn long into wide for wing ####
tarsus.long = gather(bird.tarsus, Sex, Tarsus, c(5:7))
tarsus.long$Sex = str_remove(tarsus.long$Sex, "_tarsus")
unique(tarsus.long$Sex)

# Make an N df ####
N_tarsus = c(18,20,22)
bird.tarsus.N = bird[, c(impt.cols, N_tarsus)]
bird.tarsus.N = gather(bird.tarsus.N, "Sex", "Tarsus.N", c(5,6,7))
bird.tarsus.N$Sex = str_remove(bird.tarsus.N$Sex, "_tarsus_N")
unique(bird.tarsus.N$Sex)

# Join them!  ####
tarsus.final = full_join(x = tarsus.long, y = bird.tarsus.N)
unique(tarsus.final$Sex)
unique(tarsus.final$Tarsus.N)



names(bird)
# subset to bill only ####
billcols = c(23, 25, 27)
bird.bill = bird[, c(impt.cols,billcols)]

# turn long into wide for wing ####
bill.long = gather(bird.bill, Sex, Bill, c(5:7))
bill.long$Sex = str_remove(bill.long$Sex, "_bill")
unique(bill.long$Sex)

# Make an N df ####
N_bill = c(24,26,28)
bird.bill.N = bird[, c(impt.cols, N_bill)]
bird.bill.N = gather(bird.bill.N, "Sex", "Bill.N", c(5,6,7))
bird.bill.N$Sex = str_remove(bird.bill.N$Sex, "_bill_N")
unique(bird.bill.N$Sex)

# Join them!  ####
bill.final = full_join(x = bill.long, y = bird.bill.N)
unique(bill.final$Sex)
unique(bill.final$Bill.N)


names(bird)
# subset to tail only ####
tailcols = c(29, 31, 33)
bird.tail = bird[, c(impt.cols,tailcols)]

# turn long into wide for wing ####
tail.long = gather(bird.tail, Sex, Tail, c(5:7))
tail.long$Sex = str_remove(tail.long$Sex, "_tail")
unique(tail.long$Sex)

# Make an N df ####
N_tail = c(30,32,34)
bird.tail.N = bird[, c(impt.cols, N_tail)]
bird.tail.N = gather(bird.tail.N, "Sex", "Tail.N", c(5,6,7))
bird.tail.N$Sex = str_remove(bird.tail.N$Sex, "_tail_N")
unique(bird.tail.N$Sex)

# Join them!  ####
tail.final = full_join(x = tail.long, y = bird.tail.N)
unique(tail.final$Sex)
unique(tail.final$Tail.N)

names(bird)
# Account for the last two columns ####
lastcols = c(35, 36, 37)
bird.last = bird[, c(impt.cols,lastcols)]

names(bird)
# Now join them all!  ####
df2 = full_join(x = mass.final, y = wing.final)
df2 = full_join(x = df2, y = tarsus.final)
df2 = full_join(x = df2, y = bill.final)
df2 = full_join(x = df2, y = tail.final)
df2 = full_join(x = df2, y = bird.last)

# I omitted the NAs 
bird.clean = na.omit(df2)

bird.cleaner = gather(df2, "Characteristic", "Value", c(6:18))

bird.cleaner = na.omit(bird.cleaner)



