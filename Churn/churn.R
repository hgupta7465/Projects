#The first thing that we need to do here is to load all the 
#required libraries. These are not the only libraries that we are 
#going to use. Further, we would add more libraries as and when required.
install.packages('plyr')
library(plyr)
install.packages('corrplot')
library(corrplot)
install.packages('ggplot2')
library(ggplot2)
install.packages('gridExtra')
library(gridExtra)
install.packages('ggthemes')
library(ggthemes)
install.packages('caret')
library(caret)
install.packages('MASS')
library(MASS)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)
install.packages('modeest')
library(modeest)
install.packages('CatEncoders')
library(CatEncoders)

#Now after loading the libraries we need to set the working directory.
#My working directory was my desktop since my dataset was kept there.
#For further running in any other machine we can manually set by the command
#mentioned below or we can go to the session tab in the menu bar.
#setting up the working directory
setwd("C:/Users/hp/Desktop")

#Now we need to load the dataset from the destop. The name of the dataset
#is churn.csv so read.csv(). stringAsFactors = FALSE ensures that R studio
#does not unnecessarily converts features as factors.
#loading the dataset
dataset <- read.csv(file = "churn.csv", stringsAsFactors = FALSE, header = TRUE)

#Trimming off the first column from the dataset since it was not useful
#to us.
dataset <- dataset[2:22]

#Eliminating the phone number column since if we don't consider this 
#column as a categorical variable then it would have large values
#and if we would cast them as categorical variables then there would
#be a large number of categories generated in than column which would 
#lead to inaccurate results.
dataset = subset(dataset, select = -c(Phone))

#Reviewing the dataset
View(dataset)

#Now comes the part to fill the missing values in the dataset
#Here in the dataset we see that instead of indicating missing values
#with "NA", missing values are marked with "?". This would create a trouble 
#for is to use some functionalities so we need to replace all the "?"
#with "NA".
dataset$State[dataset$State == "?"] <- NA
dataset$Account.Length[dataset$Account.Length == "?"] <- NA
dataset$Area.Code[dataset$Area.Code == "?"] <- NA
dataset$Int.l.Plan[dataset$Int.l.Plan == "?"] <- NA
dataset$VMail.Plan[dataset$VMail.Plan == "?"] <- NA
dataset$VMail.Message[dataset$VMail.Message == "?"] <- NA
dataset$Day.Mins[dataset$Day.Mins == "?"] <- NA
dataset$Day.Calls[dataset$Day.Calls == "?"] <- NA
dataset$Day.Charge[dataset$Day.Charge == "?"] <- NA
dataset$Eve.Mins[dataset$Eve.Mins == "?"] <- NA
dataset$Eve.Calls[dataset$Eve.Calls == "?"] <- NA
dataset$Eve.Charge[dataset$Eve.Charge == "?"] <- NA
dataset$Night.Mins[dataset$Night.Mins == "?"] <- NA
dataset$Night.Calls[dataset$Night.Calls == "?"] <- NA
dataset$Night.Charge[dataset$Night.Charge == "?"] <- NA
dataset$Intl.Mins[dataset$Intl.Mins == "?"] <- NA
dataset$Intl.Calls[dataset$Intl.Calls == "?"] <- NA
dataset$Intl.Charge[dataset$Intl.Charge == "?"] <- NA
dataset$CustServ.Calls[dataset$CustServ.Calls == "?"] <- NA

#now we need to find out that which columns have missing vales in them.
#Since all columns don't have missing values so we have to selectively
#remove missing values from each column
#finding out the distribution of null and non-null values in each column
table(is.na(dataset))
table(is.na(dataset$State))
table(is.na(dataset$Account.Length))
table(is.na(dataset$Area.Code)) #has missing values
table(is.na(dataset$Int.l.Plan)) #has missing values
table(is.na(dataset$VMail.Plan)) #has missing values
table(is.na(dataset$VMail.Message))
table(is.na(dataset$Day.Mins))
table(is.na(dataset$Day.Calls))
table(is.na(dataset$Day.Charge)) #has missing values
table(is.na(dataset$Eve.Mins)) #has missing values
table(is.na(dataset$Eve.Calls)) #has missing values
table(is.na(dataset$Eve.Charge))
table(is.na(dataset$Night.Mins))
table(is.na(dataset$Night.Calls))
table(is.na(dataset$Night.Charge)) #has missing values
table(is.na(dataset$Intl.Mins))
table(is.na(dataset$Intl.Calls)) #has missing values
table(is.na(dataset$Intl.Charge)) #has missing values
table(is.na(dataset$CustServ.Calls)) 

#so the columns which have missing values have been marked above

#Now, I would like to remove missing values from those columns which
#contains categorical vaiables. Here, I have replaced the missing term 
#with the most frequently occurning term in that column ie the mode value.

#since Area.Code is a categorical variable so we can replace it with most value.
#frequemtly occuring value
p1 <- ggplot(dataset, aes(x=Area.Code)) + ggtitle("Area.Code Analysis") + xlab("Area.Code") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1)
#replacing all NA values with 415 since it is in majority
dataset[is.na(dataset$Area.Code), "Area.Code"] <- 415
#again cross checking the distribution
p1 <- ggplot(dataset, aes(x=Area.Code)) + ggtitle("Area.Code Analysis") + xlab("Area.Code") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1)
table(is.na(dataset$Area.Code)) #no missing values now


#since Int.l.Plan is a categorical variable so we can replace it with most value.
#frequemtly occuring value
p3 <- ggplot(dataset, aes(x=Int.l.Plan)) + ggtitle("Int.l.Plan Analysis") + xlab("Int.l.Plan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p3)
#replacing all NA values with no since it is in majority
dataset[is.na(dataset$Int.l.Plan), "Int.l.Plan"] <- "no"
#again cross checking the distribution
p3 <- ggplot(dataset, aes(x=Int.l.Plan)) + ggtitle("Int.l.Plan Analysis") + xlab("Int.l.Plan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p3)
table(is.na(dataset$Int.l.Plan)) #no missing values now


#since VMail.Plan is a categorical variable so we can replace it with most value.
#frequemtly occuring value
p2 <- ggplot(dataset, aes(x=VMail.Plan)) + ggtitle("VMail.Plan Analysis") + xlab("VMail.Plan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p2)
#replacing all NA values with no since it is in majority
dataset[is.na(dataset$VMail.Plan), "VMail.Plan"] <- "no"
#again cross checking the distribution
p2 <- ggplot(dataset, aes(x=VMail.Plan)) + ggtitle("VMail.Plan Analysis") + xlab("VMail.Plan") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p2)
table(is.na(dataset$VMail.Plan)) #no missing values now


#Now, I have found an issue that the columns in the dataset are 
#character vectors and we need to change then to numeric vectors before
#doing and further calculations.
#so, converting columns into numeric vector from character vector
dataset$Account.Length <- as.numeric(dataset$Account.Length)
dataset$VMail.Message <- as.numeric(dataset$VMail.Message)
dataset$Day.Mins <- as.numeric(dataset$Day.Mins)
dataset$Day.Calls <- as.numeric(dataset$Day.Calls)
dataset$Day.Charge <- as.numeric(dataset$Day.Charge)
dataset$Eve.Mins <- as.numeric(dataset$Eve.Mins)
dataset$Eve.Calls <- as.numeric(dataset$Eve.Calls)
dataset$Eve.Charge <- as.numeric(dataset$Eve.Charge)
dataset$Night.Mins <- as.numeric(dataset$Night.Mins)
dataset$Night.Calls <- as.numeric(dataset$Night.Calls)
dataset$Night.Charge <- as.numeric(dataset$Night.Charge)
dataset$Intl.Mins <- as.numeric(dataset$Intl.Mins)
dataset$Intl.Calls <- as.numeric(dataset$Intl.Calls)
dataset$Intl.Charge <- as.numeric(dataset$Intl.Charge)

#now, we have to fill the missing values in rest of the columns. Now, 
#for doing this we are going to apply the regression for filling the
#missing values

#filling missing values in Day.Charge column
upper.whisker1 <- boxplot.stats(dataset$Day.Charge)$stats[5]
outlier.filter1 <- dataset$Day.Charge < upper.whisker1

daycharge.equation = "Day.Charge ~ State + Account.Length + Area.Code + Int.l.Plan + VMail.Plan + VMail.Message + Day.Mins + Day.Calls + Eve.Charge + Night.Mins + Night.Calls + Intl.Mins + CustServ.Calls"
daycharge.model <- lm(
  formula = daycharge.equation,
  data = dataset[outlier.filter1,]
)

daycharge.row <- dataset[is.na(dataset$Day.Charge), c("State", "Account.Length", "Area.Code", "Int.l.Plan", "VMail.Plan", "VMail.Message", "Day.Mins", "Day.Calls", "Eve.Charge", "Night.Mins", "Night.Calls", "Intl.Mins", "CustServ.Calls")]
daycharge.predictions <- predict(daycharge.model, newdata = daycharge.row)
dataset[is.na(dataset$Day.Charge), "Day.Charge"] <- daycharge.predictions

#filling missing values in Eve.Mins column
upper.whisker2 <- boxplot.stats(dataset$Eve.Mins)$stats[5]
outlier.filter2 <- dataset$Eve.Mins < upper.whisker2

evemins.equation = "Eve.Mins ~ State + Account.Length + Area.Code + Int.l.Plan + VMail.Plan + VMail.Message + Day.Mins + Day.Calls + Day.Charge + Eve.Charge + Night.Mins + Night.Calls + Intl.Mins + CustServ.Calls"
evemins.model <- lm(
  formula = evemins.equation,
  data = dataset[outlier.filter2,]
)

evemins.row <- dataset[is.na(dataset$Eve.Mins), c("State", "Account.Length", "Area.Code", "Int.l.Plan", "VMail.Plan", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge", "Eve.Charge", "Night.Mins", "Night.Calls", "Intl.Mins", "CustServ.Calls")]
evemins.predictions <- predict(evemins.model, newdata = evemins.row)
dataset[is.na(dataset$Eve.Mins), "Eve.Mins"] <- evemins.predictions


#filling missing values in Eve.Calls column
upper.whisker3 <- boxplot.stats(dataset$Eve.Calls)$stats[5]
outlier.filter3 <- dataset$Eve.Calls < upper.whisker3

evecalls.equation = "Eve.Calls ~ State + Account.Length + Area.Code + Int.l.Plan + VMail.Plan + VMail.Message + Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Charge + Night.Mins + Night.Calls + Intl.Mins + CustServ.Calls"
evecalls.model <- lm(
  formula = evecalls.equation,
  data = dataset[outlier.filter3,]
)

evecalls.row <- dataset[is.na(dataset$Eve.Calls), c("State", "Account.Length", "Area.Code", "Int.l.Plan", "VMail.Plan", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge", "Eve.Mins", "Eve.Charge", "Night.Mins", "Night.Calls", "Intl.Mins", "CustServ.Calls")]
evecalls.predictions <- predict(evecalls.model, newdata = evecalls.row)
dataset[is.na(dataset$Eve.Calls), "Eve.Calls"] <- evecalls.predictions

#filling missing values in Night.Charge column
upper.whisker4 <- boxplot.stats(dataset$Night.Charge)$stats[5]
outlier.filter4 <- dataset$Night.Charge < upper.whisker4

nightcharge.equation = "Night.Charge ~ Account.Length + Area.Code + Int.l.Plan + VMail.Plan + VMail.Message + Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Calls + Eve.Charge + Night.Mins + Night.Calls + Intl.Mins + CustServ.Calls"
nightcharge.model <- lm(
  formula = nightcharge.equation,
  data = dataset[outlier.filter4,]
)

nightcharge.row <- dataset[is.na(dataset$Night.Charge), c("Account.Length", "Area.Code", "Int.l.Plan", "VMail.Plan", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge", "Eve.Mins", "Eve.Calls", "Eve.Charge", "Night.Mins", "Night.Calls", "Intl.Mins", "CustServ.Calls")]
nightcharge.predictions <- predict(nightcharge.model, newdata = nightcharge.row)
dataset[is.na(dataset$Night.Charge), "Night.Charge"] <- nightcharge.predictions


#filling missing values in Intl.Calls column
upper.whisker5 <- boxplot.stats(dataset$Intl.Calls)$stats[5]
outlier.filter5 <- dataset$Intl.Calls < upper.whisker5

intlcalls.equation = "Intl.Calls ~ State + Account.Length + Area.Code + Int.l.Plan + VMail.Plan + VMail.Message + Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Charge + Night.Mins + Night.Calls + Night.Charge + Intl.Mins + CustServ.Calls"
intlcalls.model <- lm(
  formula = intlcalls.equation,
  data = dataset[outlier.filter5,]
)

intlcalls.row <- dataset[is.na(dataset$Intl.Calls), c("State", "Account.Length", "Area.Code", "Int.l.Plan", "VMail.Plan", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge", "Eve.Mins", "Eve.Charge", "Night.Mins", "Night.Calls", "Night.Charge", "Intl.Mins", "CustServ.Calls")]
intlcalls.predictions <- predict(intlcalls.model, newdata = intlcalls.row)
dataset[is.na(dataset$Intl.Calls), "Intl.Calls"] <- intlcalls.predictions


#filling missing values in Intl.Charge column
upper.whisker6 <- boxplot.stats(dataset$Eve.Calls)$stats[5]
outlier.filter6 <- dataset$Intl.Charge < upper.whisker6

intlcharge.equation = "Intl.Charge ~ State + Account.Length + Area.Code + Int.l.Plan + VMail.Plan + VMail.Message + Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Charge + Night.Mins + Night.Calls + Night.Charge + Intl.Mins + Intl.Calls + CustServ.Calls"
intlcharge.model <- lm(
  formula = intlcharge.equation,
  data = dataset[outlier.filter6,]
)

intlcharge.row <- dataset[is.na(dataset$Intl.Charge), c("State", "Account.Length", "Area.Code", "Int.l.Plan", "VMail.Plan", "VMail.Message", "Day.Mins", "Day.Calls", "Day.Charge", "Eve.Mins", "Eve.Charge", "Night.Mins", "Night.Calls", "Night.Charge", "Intl.Mins", "Intl.Calls", "CustServ.Calls")]
intlcharge.predictions <- predict(intlcharge.model, newdata = intlcharge.row)
dataset[is.na(dataset$Intl.Charge), "Intl.Charge"] <- intlcharge.predictions

#Now, all the missing values have been removed from our dataset
#The next thing wee need to do is to convert all the columns with 
#categorical variables as factors
dataset$State <- as.factor(dataset$State)
dataset$Area.Code <- as.factor(dataset$Area.Code)
dataset$Int.l.Plan <- as.factor(dataset$Int.l.Plan)
dataset$VMail.Plan <- as.factor(dataset$VMail.Plan)
dataset$CustServ.Calls <- as.factor(dataset$CustServ.Calls)
dataset$Churn. <- as.factor(dataset$Churn.)

#Now, I just wanted to see the distribution of churn class in the 
#dataset. Clearly we can see that there is a class imbalance in the dataset.
#Churn distribution
ggplot(data=dataset)+geom_bar(mapping=aes(x=Churn.,fill=Churn.))
prop.table(table(dataset$Churn.))
barplot(prop.table(table(dataset$Churn.)))

# Splitting the dataset into the Training set and Test set. we are going
#to have a 80-20 split of training and test set respectively. 
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Churn., SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Churn distribution in training set
ggplot(data=training_set)+geom_bar(mapping=aes(x=Churn.,fill=Churn.))
prop.table(table(training_set$Churn.))
barplot(prop.table(table(training_set$Churn.)))
#We found an unequal distribution of churn in the training dataset also.

#Now, its the time to manage the class imbalances in the dataset.
#Upsampling
#This method works with minority class. It replicates the observations 
#from minority class to balance the data. An advantage of using this 
#method is that it leads to no information loss but disadvantage of 
#using this method is that, since oversampling simply adds replicated 
#observations in original data set, it ends up adding multiple 
#observations of several types, thus also may lead to overfitting.
set.seed(9560)
up_train <- upSample(x = training_set[, -ncol(training_set)],
                         y = training_set$Churn.)
colnames(up_train)[colnames(up_train)=="Class"] <- "Churn."
table(up_train$Churn.)

#cross validation by applying the randomForest
controlParameters <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

parameterGrid <- expand.grid(mtry = c(20, 40, 60))

modelRandom <- train(Churn.~.,
                     data = training_set,
                     method = "rf",
                     trControl = controlParameters,
                     tuneGrid = parameterGrid)

modelRandom

# Predicting the Test set results
y_pred = predict(modelRandom, newdata = test_set[-20])

# Making the Confusion Matrix
confusionMatrix(y_pred, test_set$Churn.)

#and that is how I have solved this dataset
