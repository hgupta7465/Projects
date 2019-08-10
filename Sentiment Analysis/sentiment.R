#Sentiment Analysis (pos Vs. neg)

#we need to set the working directory.
#My working directory was my desktop since my dataset was kept there.
#For further running in any other machine we can manually set by the command
#mentioned below or we can go to the session tab in the menu bar.
#setting up the working directory
setwd("C:/Users/hp/Desktop")

#Now we need to load the dataset from the destop. The name of the dataset
#is sentiment.tsv so read.delim() since it is a tab separated file. 
#stringAsFactors = FALSE ensures that R studio does not unnecessarily 
#converts strings as factors. Also, set header to false since there are
#no column names in this dataset so it makes the first row as a column name.
#Importing the dataset
dataset_original = read.delim('sentiment.tsv', quote = '', stringsAsFactors = FALSE, header = FALSE)

#Now, we can name the columns as "Label" which contains the categorical variables
#and "Text" which contains textual data.
names(dataset_original) <- c("Label", "Text")

# Convert our class label into a factor.
dataset_original$Label <- as.factor(dataset_original$Label)

# Encoding categorical data in the "Label" column
dataset_original$Label = factor(dataset_original$Label,
                       levels = c('neg', 'pos'),
                       labels = c(0, 1))

#Cleaning the texts
#installing the packages required
install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)

#firstly we will create a corpus of the dataset. This corpus has the same
#number of observations as the dataset.
corpus = VCorpus(VectorSource(dataset_original$Text))

#Now we can see that we should convert all the letters in the dataset to
#lower cases. R interprets differently for the same words if one is
#in upper case and another in upper case.
corpus = tm_map(corpus, content_transformer(tolower))
#We can see that now all letters are in lower case
as.character(corpus[[5]]) 

#Now we need to remove any numbers if they exists. Numbers become a hurdle
#while creating the classification model
corpus = tm_map(corpus, removeNumbers)
#so all the numerical values have been removed
as.character(corpus[[14]])

#Now, we need to remove the punctuation marks from the text. They are
#of no use to us.
corpus = tm_map(corpus, removePunctuation)
#So all the  punctuation marks are also removed
as.character(corpus[[8]])

#Now, we need to remove the stop word like "and", "the", "is", "are", etc.
corpus = tm_map(corpus, removeWords, stopwords())
#So, all the stop words are also removed
as.character(corpus[[5]])

#Now, we shold stem the words in the dataset because some words have the
#same root word but called differently at different situations. So same words
#with somewhat same meanings with different postfix are created.
#This should be avoided so we stem the words.
corpus = tm_map(corpus, stemDocument)
#So, the words are now stemmed
as.character(corpus[[7]])

#Now w need to remove the unnecessary whitespaces becauses whenever a punctuation 
#mark or a number is removed it creates a whitespace in place of it
#while removing it.
corpus = tm_map(corpus, stripWhitespace)
#So, all the unnecessary white spaces are removed
as.character(corpus[[5]])

 
# Creating the Bag of Words model
#First we need to create a document term matrix of the coupus.
dtm = DocumentTermMatrix(corpus)

#This document term matrix is a sparse matrix containing a lot of zeros.
#so we need to reduce the sparcity of this matrix and filter some 
#of the unnecessary columns from the dtm. This removes some of the 
#columns from the dataset.
dtm = removeSparseTerms(dtm, 0.999)

#now we need to cast this dtm as a datafram so that we can access it
#and apply some classification model on it.
dataset = as.data.frame(as.matrix(dtm))

#Now since this dataframe was created from the dtm so it does not has "Label"
#column attached to it. So, we sould add this label column to the dataframe now.
dataset$Label = dataset_original$Label

# deEncoding categorical data
#This was done because the cross validation does not finds 0, 1 as
#appropriate labels for categorical variables.
dataset$Label = factor(dataset$Label,
                       levels = c(0, 1),
                       labels = c('neg', 'pos'))

# Encoding the target feature as factor
dataset$Label = factor(dataset$Label)

# Splitting the dataset into the Training set and Test set. we are going
#to have a 80-20 split of training and test set respectively.
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Label, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-916],
                          y = training_set$Label,
                          ntree = 100)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-916])

# Making the Confusion Matrix
confusionMatrix(y_pred, test_set$Label)

#cross validation by applying the randomForest
controlParameters <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

parameterGrid <- expand.grid(mtry = c(5,10,15,20))

modelRandom <- train(Label~.,
                     data = training_set,
                     method = "rf",
                     trControl = controlParameters,
                     tuneGrid = parameterGrid)
modelRandom
# Predicting the Test set results
y_pred = predict(modelRandom, newdata = test_set[-916])

# Making the Confusion Matrix
confusionMatrix(y_pred, test_set$Label)
