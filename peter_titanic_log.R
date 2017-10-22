titanic <- read.csv('Train.csv')
names(titanic)
attach(titanic)
titanic <- titanic[complete.cases(titanic), ]
titanic <- na.omit(titanic)
dropPred <- c("PassengerId","Name","Ticket","Cabin","Embarked")
titanic <- titanic[, !(names(titanic) %in% dropPred)]
glm.fit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare,
               data = titanic, family = "binomial")
# Pclass, Sexmale, Age, SibSp significant
# Parch, Fare not significant
summary(glm.fit)
glm.fit.Pclass <- glm(Survived ~ Pclass, data = titanic, family = "binomial")
summary(glm.fit.Pclass)
glm.fit.Sexmale <- glm(Survived ~ titanic$Sex, data = titanic, family = "binomial")
summary(glm.fit.Sexmale)
glm.fit.Age <- glm(Survived ~ Age, data = titanic, family = "binomial")
summary(glm.fit.Age) # note that alone, p-value of 0.0397, whereas model with all preds gives p-value of 0
glm.fit.SibSp <- glm(Survived ~ SibSp, data = titanic, family = "binomial")
summary(glm.fit.SibSp) # alone, not significant at all, but p-value of 0 with other preds

test <- read.csv('test.csv')
test.Survived <- read.csv('gender_submission_.csv')
test <- data.frame(test, test.Survived)
attach(test)
test <- test[complete.cases(test), ]
test <- na.omit(test)
dropPred2 <- c("PassengerId","Name","Ticket","Cabin","Embarked","PassengerId.1")
test <- test[, !(names(test) %in% dropPred2)]
test.Survived <- test$Survived

glm.probs <- predict(glm.fit, newdata=test, type="response")
glm.preds <- rep(0, length(glm.probs))
glm.preds[glm.probs>.5] = 1
mean(glm.preds == test.Survived) # 91.8429% accuracy

glm.fit2 <- glm(Survived ~ Pclass + Sex, data = titanic, family = "binomial")
glm.probs <- predict(glm.fit2, newdata=test, type="response")
glm.preds <- rep(0, length(glm.probs))
glm.preds[glm.probs > .5] = 1
mean(glm.preds == test.Survived) # 100% accuracy 
