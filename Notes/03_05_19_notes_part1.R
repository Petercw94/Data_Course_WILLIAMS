library(tidyverse)
library(GGally)
library(modelr)


df = read.csv("./Data/1620_scores.csv")


df$MeanQuiz = rowMeans(df[,17:23])

Exam1 = df$Exam.1..4245260.
Exam2 = df$Exam.2..4245261.
df$SumExams = Exam1 + Exam2

mod1 = lm(SumExams ~ Actual.PreTest.Score...not.what.your.grade.is...4122452., data = df)
summary(mod1)

mod2 = lm(SumExams ~ Actual.PreTest.Score...not.what.your.grade.is...4122452. + MeanQuiz, data = df)
summary(mod2)

mod3 = lm(SumExams ~ Actual.PreTest.Score...not.what.your.grade.is...4122452. + MeanQuiz + YOUR.CHOICE....Midterm..4122464., data = df)
summary(mod3)



anova(mod1, mod2)
anova(mod2, mod3)
anova(mod1, mod3)

sum(residuals(mod1)^2)/df.residual(mod1)
sum(residuals(mod3)^2)/df.residual(mod3)
mean(residuals(mod1)^2)
mean(residuals(mod3)^2)





