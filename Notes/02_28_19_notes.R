library(tidyverse)
library(modelr)
df = read.csv("./Data/mushroom_growth.csv")

glimpse(df)



pairs(df)
ggplot(df, aes(x = Light, y= GrowthRate, color = Species)) +
  geom_jitter() + geom_smooth(method = "lm", se=FALSE)

# Light as the independent variable ####
# glm(DV ~ IV + IV2, data = df, family = ...)

mod1 = lm(GrowthRate~Light, data = df)
summary(mod1)

df2 = add_predictions(df, mod1)
df2 = mutate(df2, DIFF2 = abs(pred - GrowthRate))
mean(df2$DIFF2)

ggplot(df2, aes(x = Light, shape = Species)) +
  geom_jitter(aes(y = GrowthRate)) +
  geom_jitter(aes(y=pred, color = "Red"))

# Nitrogen as independent variable ####

ggplot(df, aes(x = Nitrogen, y= GrowthRate)) +
  geom_jitter() + geom_smooth(method  = "loess", se=FALSE)

# gaussian family is another way to say normal distribution
mod2 =  glm(GrowthRate~Nitrogen, data = df, family = "loess")
summary(mod2)
?glm

#using the quadratic equation to define nitrogen because it is not part of the linear model:
# the nls() function will help us do this
# a refers to a constant in our data, x refers to nitrogen
# This was soo confusing we all gave up on it! ####
?nls()
nls(((a*x^2) + (b*x) + c), df, start = )


# making predictions using the loess method ####
predict(loess(GrowthRate ~ Nitrogen, df))
df$pred = predict(loess(GrowthRate ~ Nitrogen, df))
df = mutate(df, DIFF = abs(pred - GrowthRate))
mean(df$DIFF)

mod.loess = loess(GrowthRate ~ Nitrogen, df)
summary(mod.loess)


ggplot(df, aes(x=Nitrogen,)) +
  geom_point(aes(y = df$GrowthRate)) +
  geom_smooth(se=FALSE,aes(y=GrowthRate)) +
  geom_point(aes(y=df$pred, color = "Red", size=4))

# This is a way to make predictions about numbers that aren't in the data.
new_data = data.frame(Nitrogen = 15)
predict(mod.loess, newdata = new_data)
add_predictions(new_data, mod.loess)
