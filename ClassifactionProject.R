#Downloading train and test data
trainFile = "adult.data"; testFile = "adult.test"

if (!file.exists (trainFile))
  download.file (url = "<a href="http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data">http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data</a>",
                 destfile = trainFile)

if (!file.exists (testFile))
  download.file (url = "<a href="http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test">http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test</a>",
                 destfile = testFile)
#Assigning column names
colNames = c ("age", "workclass", "fnlwgt", "education",
              "educationnum", "maritalstatus", "occupation",
              "relationship", "race", "sex", "capitalgain",
              "capitalloss", "hoursperweek", "nativecountry",
              "incomelevel")

#Reading training data
training = read.table (trainFile, header = FALSE, sep = ",",
                       strip.white = TRUE, col.names = colNames,
                       na.strings = "?", stringsAsFactors = TRUE)
#Display structure of the data
str (training)
#Removing NAs
TrainSet = training [!is.na (training$workclass) & !is.na (training$occupation), ]
TrainSet = TrainSet [!is.na (TrainSet$nativecountry), ]
#Removing unnecessary variables

TrainSet$fnlwgt = NULL
#Data Exploration
#Exploring the age variable

summary (TrainSet$age)
#Boxplot for age variable
boxplot (age ~ incomelevel, data = TrainSet,
         main = "Income levels based on the Age of an individual",
         xlab = "Income Level", ylab = "Age", col = "salmon")
#Histogram for age variable
incomeBelow50K = (TrainSet$incomelevel == "<=50K")
xlimit = c (min (TrainSet$age), max (TrainSet$age))
ylimit = c (0, 1600)

hist1 = qplot (age, data = TrainSet[incomeBelow50K,], margins = TRUE,
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = incomelevel)

hist2 = qplot (age, data = TrainSet[!incomeBelow50K,], margins = TRUE,
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = incomelevel)
grid.arrange (hist1, hist2, nrow = 2)
summary (TrainSet$educationnum)
#Boxplot for education-num variable
boxplot (educationnum ~ incomelevel, data = TrainSet,
         main = "Years of Education distribution for different income levels",
         xlab = "Income Levels", ylab = "Years of Education", col = "green")
summary (TrainSet[ TrainSet$incomelevel == "<=50K", + c("capitalgain", "capitalloss")])
#Evaluate hours/week variable
summary (TrainSet$hoursperweek)
boxplot (hoursperweek ~ incomelevel, data = TrainSet,
         main = "Hours Per Week distribution for different income levels",
         xlab = "Income Levels", ylab = "Hours Per Week", col = "salmon")
#Evaluating work-class variable
qplot (incomelevel, data = TrainSet, fill = workclass) + facet_grid (. ~ workclass)
#Evaluating occupation variable
qplot (incomelevel, data = TrainSet, fill = occupation) + facet_grid (. ~ occupation)
#Evaluating marital-status variable
qplot (incomelevel, data = TrainSet, fill = maritalstatus) + facet_grid (. ~ maritalstatus)
#Evaluating relationship variable
qplot (incomelevel, data = TrainSet, fill = relationship) + facet_grid (. ~ relationship)
#Building the model
set.seed (32323)

trCtrl = trainControl(method = "cv", number = 10)

boostFit = train (incomelevel ~ age + workclass + education + educationnum +
                    maritalstatus + occupation + relationship +
                    race + capitalgain + capitalloss + hoursperweek +
                    nativecountry, trControl = trCtrl,
                  method = "gbm", data = TrainSet, verbose = FALSE)
#Checking the accuracy of the model
confusionMatrix (TrainSet$incomelevel, predict (boostFit, TrainSet))


#Load the testing data set
testing = read.table (testFile, header = FALSE, sep = ",",
                      strip.white = TRUE, col.names = colNames,
                      na.strings = "?", fill = TRUE, stringsAsFactors = TRUE)

#Display structure of the data
str(testing)
table (complete.cases (testing))
#Removing NAs
TestSet = testing [!is.na (testing$workclass) & !is.na (testing$occupation), ]
TestSet = TestSet [!is.na (TestSet$nativecountry), ]

#Removing unnecessary variables
TestSet$fnlwgt = NULL
#Testing model
TestSet$predicted = predict (boostFit, TestSet)
table(TestSet$incomelevel, TestSet$predicted)

actuals_preds <- data.frame(cbind(actuals=TestSet$incomelevel, predicted=TestSet$predicted)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

