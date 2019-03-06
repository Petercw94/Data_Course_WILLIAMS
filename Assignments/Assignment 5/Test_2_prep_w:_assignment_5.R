library(tidyverse)
library(modelr)


data(mtcars)
df = mtcars

# subset to only automatic (0)
df.aut = df %>%
  filter(am == 0)

df.aut %>%
  write.csv("automatic_mtcars.csv")

png("mpg_vs_hp_auto.png")
ggplot(df.aut, aes(x=hp, y=mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle( "Miles per Gallon ~ Horsepower") +
  labs(x = "Horsepower", y = "Miles per Gallon") 
dev.off()

tiff("mpg_vs_wt_auto.tiff")
ggplot(df.aut, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle( "Miles per Gallon ~ Weight") +
  labs(x = "Weight", y = "Miles per Gallon") 
dev.off()


df %>%
  filter(disp <= 200) %>%
  write.csv("mtcars_max200_displ.csv")



# Practice building and comparing models of the data
mod1 = lm(mpg ~ factor(cyl) + hp + drat, data = df)
summary(mod1)

mod2 = lm(mpg ~ hp + drat, data = df)
summary(mod2)

anova(mod1,mod2)

sqrt(sum(residuals(mod1)^2)/df.residual(mod1))
sqrt(sum(residuals(mod2)^2)/df.residual(mod2))

df2 = add_predictions(df, mod1, var = "mod1")
df2 = add_predictions(df2, mod2, var = "mod2")

ggplot(df2, aes(x = row.names(df2))) +
  geom_point(aes(y=mpg)) +
  geom_point(aes(y=df2$mod1), color = "Red") +
  geom_point(aes(y=df2$mod2), color = "Blue")


df2 = mutate(df2, DIFF = abs(mod1 - mpg))  
df2 = mutate(df2, DIFF2 = abs(mod2 - mpg)) 
mean(df2$DIFF)
mean(df2$DIFF2)
