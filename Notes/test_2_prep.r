library(tidyverse)
library(GGally)
library(modelr)


df = read.csv("./Data/1620_scores.csv")

# Make some variables that are easier to work with ####
Exam1 = df$Exam.1..4245260.
Exam2 = df$Exam.2..4245261.
df$SumExams = Exam1 + Exam2

names(df)
df$meanQuiz = rowMeans(df[,c(17:23)])

# Build some possible models to describe the data ####
mod1 = lm(SumExams ~ meanQuiz, data = df)
summary(mod1)
mod2 = lm(SumExams ~ meanQuiz + Actual.PreTest.Score...not.what.your.grade.is...4122452., data = df)
summary(mod2)
mod3 = lm(SumExams ~ meanQuiz + Actual.PreTest.Score...not.what.your.grade.is...4122452. + YOUR.CHOICE....Midterm..4122464., data = df)
summary(mod3)


# Test your models against each other to see which is a better fit ####
anova(mod1,mod2)
anova(mod2,mod3)
anova(mod1, mod3)

# It looks like model 1 and 2 are not significantly different from each other but they are
# significantly different from model 3
# Compare the sum of squares:

sum(residuals(mod1)^2)
sum(residuals(mod2)^2)
sum(residuals(mod3)^2)

# It looks like mod3 is the best fit for our data, lets make some predictions and plot them with this data
df.pred = add_predictions(df, mod3, var = "Predictions")


ggplot(df.pred, aes(x=meanQuiz)) +
  geom_point(aes(y=df.pred$SumExams)) + 
  geom_point(aes(y = df.pred$Predictions), color = "Red")

ggplot(df.pred, aes(x=meanQuiz)) +
  geom_point(aes(y=df.pred$SumExams)) + 
  geom_smooth(aes(y = df.pred$Predictions), color = "Red", se=FALSE, method = "lm")

ggplot(df.pred, aes(x=row.names(df.pred))) +
  geom_point(aes(y=df.pred$SumExams), ) + 
  geom_point(aes(y = df.pred$Predictions), color = "Red")



# Now we can make predictions for the future using the predict function ####
new.data = data.frame(meanQuiz = c(8, 2, 7), 
                       Actual.PreTest.Score...not.what.your.grade.is...4122452. = c(40, 20, 35),
                      YOUR.CHOICE....Midterm..4122464. = c(50, 20, 45))
new.data
predict(mod3,newdata = new.data)
