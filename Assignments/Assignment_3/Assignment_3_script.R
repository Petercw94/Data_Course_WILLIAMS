# Assignment Week 3



# topics:   type conversions, factors, plot(), making a data frame from "scratch",
#           reordering, 


# vector operations!
vector1 = 1:10
vector2=c(5,6,7,8,4,3,2,1,3,10)


vector1*vector2


# move the thatch ant csv to this assignment folder


list.files(recursive = TRUE, pattern = "thatch_ant.csv" ) -> path #This allows the file to be found and called even if we decide to change the directory of that file
# load the data and look at it
dat = read.csv(path)
names(dat)
summary(dat)
str(dat)

#why are these plots different??? ####
plot(x=dat$Headwidth..mm., y=dat$Mass)
plot(x=dat$Size.class, y=dat$Mass)

# The first graph has continuous data along the x axis whereas the second plot has
# categorical data


#check the classes of these vectors ####
class(dat$Headwidth..mm.)
class(dat$Size.class)

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat) ####
str(dat)

# Two of them are "Factor" ....why is the column "Headwidth" a factor? It looks numeric!
print(dat$Headwidth)
levels(dat$Headwidth)
# It is a factor because it has both missing data, and incorrectly 
# entered data! 

#tidy up the data
dat[which(dat$Headwidth =="41mm"), "Headwidth"] <- "41.000" # The which commands allows you to search within the data set for a TRUE FALSE statement.
#Wrapping this into the [ ] allows you to use that whole command as a row, then add the column that you would like to add that into.
dat[which(dat$Headwidth == ""), "Headwidth"] <- NA # Same as before * it is important to keep your data as characters to keep the factor a chr factor
dat$Headwidth = as.numeric(dat$Headwidth) # Now we can convert to numerics

plot(dat$Mass, dat$Headwidth) # Now we can replot the data


# we can try to coerce one format into another with a family of functions
# as.factor, as.matrix, as.data.frame, as.numeric, as.character, as.POSIXct, etc....

#make a numeric vector to play with:
nums = c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric


# convert to a factor
as.factor(nums) # show in console
nums_factor = as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it

#check it out
plot(nums) 
plot(nums_factor)
# take note of how numeric vectors and factors behave differently in plot()

# Let's modify and save these plots. Why not!?
?plot()
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label")


?jpeg()


dev.off()



# back to our ant data...
dat$Headwidth
levels(dat$Headwidth) # levels gives all the "options" of a factor you feed it
summary(dat$Headwidth)
class(dat$Headwidth)
# I notice a couple weird ones in there: "" and "41mm"
# The "" means a missing value, basically. The "41mm" sure looks like a data entry error.
# It should probably be "41.000"

# FIND WHICH ONES HAVE "41mm"



# CONVERT THOSE TO "41.000"



# DO THE SAME FOR "", BUT CONVERT THOSE TO "NA"



# NOW, REMOVE ALL THE ROWS OF "dat" THAT HAVE AN "NA" VALUE
na.omit(dat)


# NOW, CONVERT THAT PESKY "Headwidth" COLUMN INTO A NUMERIC VECTOR WITHIN "dat"

levels(dat$Size.class)

# LET'S LEARN HOW TO MAKE A DATA FRAME FROM SCRATCH... WE JUST FEED IT VECTORS WITH NAMES!

# make some vectors *of equal length* (or you can pull these from existing vectors)
col1 = c("hat", "tie", "shoes", "bandana")
col2 = c(1,2,3,4)
col3 = factor(c(1,2,3,4)) # see how we can designate something as a factor             



# here's the data frame command:
data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # colname = vector, colname = vector....
df1 = data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # assign to df1
df1 # look at it...note column names are what we gave it.



# Make a data frame from the first 20 rows of the ant data that only has "Colony" and "Mass"
# save it into an object called "dat3"





###### WRITING OUT FILES FROM R #######
?write.csv()
write.csv(dat, "./clean_thatchant.csv", quote=FALSE)

# Write your new object "dat3" to a file named "LASTNAME_first_file.csv" in your PERSONAL git repository
write.csv(dat, "./WILLIAMS_first_file.csv", quote=FALSE)



### for loops in R ###

#simplest example:
for(i in 1:10){
  print(i)
}

#another easy one
for(i in levels(dat$Size.class)){
  print(i)
}

# can calculate something for each value of i ...can use to subset to groups of interest
for(i in levels(dat$Size.class)){
  print(mean(dat[dat$Size.class == i,"Mass"]))
}

# more complex:
# define a new vector or data frame outside the for loop first
new_vector = c() # it's empty
# also define a counter
x = 1

for(i in levels(dat$Size.class)){
  new_vector[x] = mean(dat[dat$Size.class == i,"Mass"])
  x = x+1 # add 1 to the counter (this will change the element of new_vector we access each loop)
}

#check it
new_vector



# PUT THIS TOGETHER WITH THE LEVELS OF OUR FACTOR SO WE HAVE A NEW DATA FRAME:
# FIRST COLUMN WILL BE THE FACTOR LEVELS....
# SECOND COLUMN WILL BE NAMED "MEAN" AND WILL BE VALUES FROM  new_vector

#fill it in
size_class_mean_mass = data.frame(size_class=levels(dat$Size.class), mean_mass=new_vector)
plot(size_class_mean_mass)




############ YOUR HOMEWORK ASSIGNMENT ##############

# 1.  Make a scatterplot of headwidth vs mass. See if you can get the points to be colored by "Colony"

ggplot(dat, aes(x=Headwidth, y=Mass, colour=factor(Colony))) +
  geom_point() 

# 2.  Write the code to save it (with meaningful labels) as a jpeg file
ggplot(dat, aes(x=Headwidth, y=Mass, colour=factor(Colony))) +
  geom_point() +
  ggsave("./Colony_plot.jpeg")


# 3.  Subset the thatch ant data set to only include ants from colony 1 and colony 2
dat2 = dat %>%
  filter(Colony < 3)

# plotted and saved 
ggplot(dat2, aes(x=Headwidth, y=Mass, colour=factor(Colony))) +
  geom_point() +
  ggsave("./Colony_plot_2.jpeg")
# 4.  Write code to save this new subset as a .csv file
write.csv(dat2, "edited_dat.csv")


# 5.  Upload this R script (with all answers filled in and tasks completed) to canvas
# I should be able to run your R script and get all the plots created and saved, etc.
