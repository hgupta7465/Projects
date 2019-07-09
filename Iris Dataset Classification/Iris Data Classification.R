# Importing the dataset
setwd("C:/Users/hp/Desktop")
dataset = read.csv('Iris.csv')
dataset = dataset[,2:6]

install.packages('ggplot2')
install.packages('gridExtra')
install.packages('grid')
install.packages('plyr')
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)

#Visualizations using histogram
#Sepal length 
HisSl <- ggplot(data=iris, aes(x=dataset$SepalLengthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Length (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Sepal Length")+
  geom_vline(data=iris, aes(xintercept = mean(dataset$SepalLengthCm)),linetype="dashed",color="grey")


# Sepal width
HistSw <- ggplot(data=iris, aes(x=dataset$SepalWidthCm)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Width (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Sepal Width")+
  geom_vline(data=iris, aes(xintercept = mean(dataset$SepalWidthCm)),linetype="dashed",color="grey")

# Petal length
HistPl <- ggplot(data=iris, aes(x=dataset$PetalLengthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Petal Length (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Petal Length")+
  geom_vline(data=iris, aes(xintercept = mean(dataset$PetalLengthCm)),
             linetype="dashed",color="grey")

# Petal width
HistPw <- ggplot(data=iris, aes(x=dataset$PetalWidthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Petal Width (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="right" )+
  ggtitle("Histogram of Petal Width")+
  geom_vline(data=iris, aes(xintercept = mean(dataset$PetalWidthCm)),linetype="dashed",color="grey")

# Plot all visualizations
grid.arrange(HisSl + ggtitle(""),
             HistSw + ggtitle(""),
             HistPl + ggtitle(""),
             HistPw  + ggtitle(""),
             nrow = 2,
             top = textGrob("Iris Frequency Histogram", 
                            gp=gpar(fontsize=15)))

#Visualizing the density curves
DhistPl <-    ggplot(iris, aes(x=dataset$PetalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(dataset$PetalLengthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Petal Length (cm)") +  
  ylab("Density")+
  theme(legend.position="none")

DhistPw <- ggplot(iris, aes(x=dataset$PetalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(dataset$PetalWidthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Petal Width (cm)") +  
  ylab("Density")+
  theme(legend.position="none")

DhistSw <- ggplot(iris, aes(x=dataset$SepalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(dataset$SepalWidthCm),  colour=Species), linetype="dashed",color="grey", size=1)+
  xlab("Sepal Width (cm)") +  
  ylab("Density")+
  theme(legend.position="none")

DhistSl <- ggplot(iris, aes(x=dataset$SepalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(dataset$SepalLengthCm),  colour=Species),linetype="dashed", color="grey", size=1)+
  xlab("Sepal Length (cm)") +  
  ylab("Density")+
  theme(legend.position="none")


# Plot all density visualizations
grid.arrange(DhistSl + ggtitle(""),
             DhistSw  + ggtitle(""),
             DhistPl + ggtitle(""),
             DhistPw  + ggtitle(""),
             nrow = 2,
             top = textGrob("Iris Density Plot", 
                            gp=gpar(fontsize=15)))

# Let's plot all the variables in a single visualization that will contain all the boxplots
BpSl <- ggplot(iris, aes(dataset$Species, dataset$SepalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Length (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpSw <-  ggplot(iris, aes(dataset$Species, dataset$SepalWidthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Width (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPl <- ggplot(iris, aes(dataset$Species, dataset$PetalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPw <-  ggplot(iris, aes(dataset$Species, dataset$PetalWidthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Width (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Box Plot", x = "Species")

# Plot all visualizations
grid.arrange(BpSl  + ggtitle(""),
             BpSw  + ggtitle(""),
             BpPl + ggtitle(""),
             BpPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Sepal and Petal Box Plot", 
                            gp=gpar(fontsize=15)))

# You can also visualize the data using the violin plots. They are similar to the Box Plots but they 
# show the number of points at a particular value by the width of the shapes. 
# The can also include the marker for the median and a box for the interquartile range.

VpSl <-  ggplot(iris, aes(dataset$Species, dataset$SepalLengthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Sepal Length", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  theme(legend.position="none")

VpSw <-  ggplot(iris, aes(dataset$Species, dataset$SepalWidthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Sepal Width", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  theme(legend.position="none")

VpPl <-  ggplot(iris, aes(dataset$Species, dataset$PetalLengthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Petal Length", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  theme(legend.position="none")

VpPw <-  ggplot(iris, aes(dataset$Species, dataset$PetalWidthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Petal Width", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  labs(title = "Iris Box Plot", x = "Species")

# Plot all visualizations
grid.arrange(VpSl  + ggtitle(""),
             VpSw  + ggtitle(""),
             VpPl + ggtitle(""),
             VpPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Sepal and Petal Violin Plot", 
                            gp=gpar(fontsize=15)))

# Encoding the target feature as factor
dataset$Species = factor(dataset$Species, 
                         levels = c('Iris-setosa', 'Iris-versicolor', 'Iris-virginica'),
                         labels = c(0, 1, 2))

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Species, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-5],
                          y = training_set$Species,
                          ntree = 100)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-5])

# Making the Confusion Matrix
cm = table(test_set[, 5], y_pred)
cm
