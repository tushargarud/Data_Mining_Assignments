
#Read the data
foodExpData <- read.csv("C:/Academic/Data Mining/Assignments/Assignment_6/linear_regression/FoodExpenditures_table1.csv",header = FALSE,stringsAsFactors=FALSE)

#Set column headings
colnames(foodExpData) <- c("dt_year","food.home.sales","food.home.prod","food.home.total","food.away.sales","food.away.supp","food.away.total","food.all","alco.pckg","alco.drnk","alco.total")

#Divide data into training and testing data
trainData <- foodExpData[1:100,]
testData <- foodExpData[101:126,]

#Draw scatter plot for current data
plot(food.home.total~food.home.sales, data=trainData, main="Linear regression")

#Calculate linear model
lmodel=lm(food.home.total~food.home.sales, data=trainData)

#Draw line representing linear model
abline(lmodel,col="red")

#Plot different graphs for the linear model
plot(lmodel)

#Predict values for testing data
predicted_values <- predict(lmodel, testData)

#Create a frame with actual and predicted data
comparisonFrame <- data.frame(testData$food.home.total, predicted_values)

#Compare the actual and predicted data
comparisonFrame$difference <- as.numeric(comparisonFrame$testData.food.home.total) - comparisonFrame$predicted_values

#Print the frame
print(comparisonFrame)

#Print the differencr
print("Summary of difference :")
print(summary(comparisonFrame$difference))

#Plot the predicted data against actual data
plot(comparisonFrame$testData.food.home.total, comparisonFrame$predicted_values, main="Predicted Vs Actual values")
abline(a=0, b=1, col = "red")


