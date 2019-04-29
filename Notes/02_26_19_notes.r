library(tidyverse)
library(modelr)
library(MASS)

data(mtcars)
df = mtcars
glimpse(df)
df$am = factor(df$am)

ggplot(df, aes(x=cyl, y=mpg)) +
  geom_point() + geom_smooth(method = "lm")

mod1 = lm(mpg ~ cyl, data = df)
summary(mod1)
mod1

df2 = add_predictions(df, mod1)
df2$am = factor(df$am)
ggplot(df2, aes(x=cyl)) + 
  geom_point(aes(y=mpg)) +
  geom_point(aes(y=pred), color = "Red")


ggplot(df2, aes(x=cyl)) + 
  geom_point(aes(y=mpg)) +
  geom_jitter(aes(y=pred), color = "Red") +
  labs(x = "Cylinder", y = "MPG")

df2 = mutate(df2, DIFF = abs(pred-mpg))
# Is the same as as add_residuals ####
add_residuals(df2, mod1)

# What is the average difference between the predictions and the actual results
mean(df2$DIFF)


ggplot(df, aes(x = cyl, y = mpg, color = am) + geom_point() + geom_smooth(method = "lm", se = FALSE)

mod2 = lm(mpg ~ cyl * am, data = df)
summary(mod2)
# Use the * when you want to look for relationships between our two independent variables, + does not do this ####

df2 = add_predictions(df2, mod2, "pred2")
df2 = mutate(df2, DIFF2 = abs(pred2 - mpg))

ggplot(df2, aes(x=cyl)) +
  geom_point(aes(y=mpg), alpha = .5) +
  geom_jitter(aes(y=pred), color = "Red") +
  geom_jitter(aes(y=pred2), color = "Blue")

mean(df2$DIFF2)
mean(df2$DIFF)


mod3 = lm(mpg ~ cyl * wt, data = df)
summary(mod3)

df2 = add_predictions(df2, mod3, "pred3")
df2 = mutate(df2, DIFF3 = abs(pred3 - mpg))

mean(df2$DIFF3)


mod4 = lm(mpg ~ cyl * wt * am * hp, data = df)
summary(mod4)

df2 = add_predictions(df2, mod4, "pred4")
df2 = mutate(df2, DIFF4 = abs(pred4 - mpg))

mean(df2$DIFF4)



df.mod = sample_n(df, nrow(df)/2)
df.cross = anti_join(df, df.mod)


# Pretend this is a model of every variable ####
mod.full.cross = glm(mpg ~ cyl * wt * am * hp, data = df.mod)
df3 = add_predictions(df.cross, mod.full.cross)


# Run a cross validation on the models you created ####



mod.AIC = glm(mpg ~ cyl + wt + am + hp, data = df)
# aic model tester ####
?stepAIC
stepAIC(mod4)

# The lower the AIC, the better
mod.best 




ggplot(df, aes(x=disp, y=mpg, color = carb)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~am)
