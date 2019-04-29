library(tidyverse)
library(vegan)
library(zoo)

# load data
meta = read.csv("../../../Data_Course/Data/MLO_Metadata.csv")
otu = read.csv("../../../Data_Course/Data/MLO_OTU_Table.csv")


# fix row names 
colnames(otu)[1] <- "sample"
row.names(otu) <- otu$sample

otu = otu[,-1]




a = c("a", "b", "c")
b = c("c", "b", "f")

# does this element in "a" appear in "b" as well?
a %in% b


# remove empty OTUs 
otu = otu[colSums(otu) > 0]


# remove empty samples
otu = otu[rowSums(otu) > 0, ]



good.samples = as.character(meta$SampleID) %in% row.names(otu)
# false_goods = which(good.samples == FALSE)

# new_meta = meta[-false_goods,]

# an easier way to do this is to do:
#### meta = meta[good.samples, ]

meta = meta[good.samples, ]


# arranging with tidyverse
meta = arrange(meta, SampleID)
row.names(otu)

# sanity test
identical(row.names(otu), as.character(meta$SampleID))



# plotting  ####

# quick heatmap
heatmap(as.matrix(otu))


# build a model .... community structure as function of Year
?dist
?vegdist
dist = vegdist(otu, method = "jaccard", binary = TRUE)
adonis(dist ~ meta$Year + meta$Quarter)


# visualize which year makes a difference
# This can be useful for determining similarities between male and female (or yoga experience or not) groups 

nmds = metaMDS(otu)
x = nmds$points[,1]
y = nmds$points[,2]

df = data.frame(Quarter = meta$Quarter, Year = meta$Year, x=x,y=y)

ggplot(df, aes(x=x, y=y, color = factor(Year))) +
  geom_point() + 
  # set the x and y limits of what to show (a way to zoom in on the plot)
  lims(x = c(-.1,.1))




# dealing with TRUE FALSE responses:  ####




