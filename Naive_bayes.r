library(class) 
library(e1071) 

#Read the data
breastCancerDataWithId <- read.csv("C:/Academic/Data Mining/Assignments/Assignment_7/naive_bayes/breast-cancer-wisconsin.data",header = FALSE)

#Set column names
colnames(breastCancerDataWithId) <- c("Sample_code_number","Clump_Thickness","Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape","Marginal_Adhesion","Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Class")

#Remove the primary key
breastCancerData <- breastCancerDataWithId[-1]

#Create training data set
trainData <- breastCancerData[1:500,]

#Create test data set
testData <- breastCancerData[501:699,]

#Create the Naive Bayes model
NBclassifier <- naiveBayes(as.factor(Class) ~ ., data = trainData) 

#Predict values using this model
pred <- predict(NBclassifier, testData) 

#Form and print confusion matrix
tab <- table(pred, testData$Class) 
print(tab)

#Plot the confusion matrix
fourfoldplot(tab, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Four Fold Plot")
mosaicplot(tab, main = "Mosaic Plot", color = TRUE)