library(rpart)
library(rpart.plot)

#Read the data
foodExpDataWithId <- read.csv("C:/Academic/Data Mining/Assignments/Assignment_6/decision_tree_classifier/breast-cancer-wisconsin.data",header = FALSE)

#Set column names
colnames(foodExpDataWithId) <- c("Sample_code_number","Clump_Thickness","Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape","Marginal_Adhesion","Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Class")

#Remove the primary key
foodExpData <- foodExpDataWithId[-1]

#Create training data set
trainData <- foodExpData[1:500,]

#Create test data set
testData <- foodExpData[501:699,]

#Create decision tree model
dtree_model <- rpart(Class ~ Clump_Thickness + Uniformity_of_Cell_Size + Uniformity_of_Cell_Shape + Marginal_Adhesion + Single_Epithelial_Cell_Size + Bare_Nuclei + Bland_Chromatin + Normal_Nucleoli + Mitoses, data = trainData)

#Plot the tree
rpart.plot(dtree_model)

#Predict the class labels for test data using the decision tree model
pred <- predict(dtree_model, newdata = testData)

#Round the values as the class labels are discrete
pred <- round(pred, 0)

#Create a frame with the actual class labels and predicted class labels
comparisonFrame <- data.frame(pred, testData$Class)

#Compare the actual and predicted values
comparisonFrame$result <- ifelse(comparisonFrame$pred == comparisonFrame$testData.Class, TRUE, FALSE)

#Print the frame
print(comparisonFrame)
print(summary(comparisonFrame))

