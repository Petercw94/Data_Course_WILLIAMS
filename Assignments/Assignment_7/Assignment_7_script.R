library(tidyverse)
library(modelr)


#  loads the "/Data/mushroom_growth.csv" data set
df = read.csv("./mushroom_growth.csv")

# creates several plots exploring relationships between the response and predictors
jpeg("Assignment_7_plots.jpeg")
GGally::ggpairs(df)
dev.off()

# from the graphs created by ggpairs, it looks like light, nitrogen, and humidity could all possibly
#have some kind of relationship with GrowthRate. It does look like light is positively correlated with GrowthRate
# and Nitrogen is negatively correlated.


# defines at least 2 models that explain the **dependent variable "GrowthRate"**
# One must be a lm() and 
# one must be an aov()

mod1 = glm(GrowthRate~Light, data = df)
summary(mod1)
mod2 = aov(GrowthRate~Light+Nitrogen+Humidity, data = df)
summary(mod2)


#  calculates the mean sq. error of each model

mean(residuals(mod1)^2)
mean(residuals(mod2)^2)


# RMSE for each model:
sqrt(sum(residuals(mod1)^2)/df.residual(mod1))
sqrt(sum(residuals(mod2)^2)/df.residual(mod2))

# adds predictions based on new values for the independent variables used in your model####
df = add_predictions(df, mod2, var = "Predictions")
df = mutate(df, DIFF = abs(Predictions - GrowthRate))
mean(df$DIFF)

#  plots these predictions alongside the real data ####
p1 = ggplot(df, aes(x = Nitrogen)) +
  geom_point(aes(y = GrowthRate)) +
  geom_point(aes(y=df$Predictions), color = "Red") 
p1
p1 + ggsave("Nitrogen_predictions.png")


p2 = ggplot(df, aes(x = Light)) +
  geom_point(aes(y = GrowthRate)) +
  geom_point(aes(y=df$Predictions), color = "Blue")
p2
p2 + ggsave("Light_predictions.png")

