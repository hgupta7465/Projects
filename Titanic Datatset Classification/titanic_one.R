#we have set the working directory
setwd("C:/Users/hp/Desktop/kaggle")

#the following commands loads the train and the test dataset
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#we have added an additional coloumn in both the datasets named as IsTrainSet
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#since Survived coloumn was missing in the test dataset so we have added that to the test dataset
#after adding the new coloumn the no. of coloumns in the train and the test dataset are equal
titanic.test$Survived <- NA

#now we can join the test and the train datasets
titanic.full <- rbind(titanic.train,titanic.test)

#now there are two rows in Embarked coloumn that are missing in the titanic.full dataset
#so in the following step we put 'S' in place of vacant rows in Embarked coloumn
titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'

#here we are finding the median age so that we can replace the empty rows in age coloumn
#with the median age of titanic.full dataset
#age.median <- median(titanic.full$Age, na.rm = TRUE)

#replacing all empty age rows with the median age
#titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

upper.whisker1 <- boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter1 <- titanic.full$Age < upper.whisker1

age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
age.model <- lm(
  formula = age.equation,
  data = titanic.full[outlier.filter1,]
)

age.row <- titanic.full[is.na(titanic.full$Age), c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
age.predictions <- predict(age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.predictions

#here we are finding the median Fare so that we can replace the empty rows in Fare coloumn
#with the median Fare of titanic.full dataset
#fare.median <- median(titanic.full$Fare, na.rm = TRUE)

#replacing all empty Fare rows with the median Fare
#titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

upper.whisker2 <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter2 <- titanic.full$Fare < upper.whisker2

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter2,]
)

fare.row <- titanic.full[is.na(titanic.full$Fare), c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")]
fare.predictions <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions

#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#now since our data is cleaned so we will separate our test and train dataset 
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]

#now we will categorically cast Survived coloumn also
titanic.train$Survived <- as.factor(titanic.train$Survived)

#now we are going to predict the survival
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

#building the predictive model
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)






 