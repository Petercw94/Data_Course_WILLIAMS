library(tidyverse)
library(modelr)
# Load the data
grad = read.csv("GradSchool_Admissions.csv")


# Explore the data
glimpse(grad)
grad %>%
  group_by(admit) %>%
  summarise(Avg.GRE = mean(gre), Avg.GPA = mean(gpa), Avg.Rank = mean(rank))

# factor the appropriate classes
grad$rank = factor(grad$rank)

# create a model
mod1 = glm(admit ~ gre+gpa+rank, data = grad, family = "binomial")
summary(mod1)

mod2 = glm(admit ~ gre+gpa, data = grad, family = "binomial")
summary(mod2)


# Adding predictions into the data
grad$binom.pred <- predict(mod2, type = "response")
grad$binom.resid <- residuals(mod2, type = "response")


exp(coefficients(mod2))


# Graphing your findings

ggplot(grad, aes(x=gpa,y=admit)) +
  geom_segment(aes(xend=gpa,yend=binom.pred), alpha=.5) +
  geom_point() +
  geom_point(aes(y=binom.pred), shape=24, color="Blue")
