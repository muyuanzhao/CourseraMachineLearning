
# Load Data
# Assumes data has already been downloaded
# Assumes caret, randomforest and e1071 packages have been installed
pmlTrain <- read.csv('pml-training.csv')
pmlTest <- read.csv('pml-testing.csv')

# Partition data sets
library(caret)
dataSet <- createDataPartition(pmlTrain$classe, p=.5, list=FALSE)
dataTraining <- pmlTrain[dataSet,]
dataTesting <- pmlTrain[-dataSet,]

# Histogram - Classe values
hist(as.numeric(dataTraining$classe), xlab="Classe", main="Training Data Set", axes=FALSE)
axis(1, at = c(1,2,3,4,5), labels = c("A","B","C","D","E"))
axis(2)

# Data Cleaning
# Remove timestamps, names, etc that won't help predict
r <- c(1:5)
dataTraining <- dataTraining[-r]

# Remove columns with more than 90% NAs
dataTraining <- dataTraining[,colSums(is.na(dataTraining)) < (nrow(dataTraining) * .9)]

# Remove attributes that won't help predict
nzv <- nearZeroVar(dataTraining)
dataTraining <- dataTraining[-nzv]

# Create model
# set.seed(32343)
# model <- train(classe ~ ., data = dataTraining, method = "rf", importance = TRUE)
# model

#.993 accuracy

# Model 2
model <- randomForest(classe~.,data=dataTraining)
model

# Evaluate testing data

p <- predict(model, dataTesting)
cm <- confusionMatrix(dataTesting$classe, p);
cm

# Evaluate 

p2 <- predict(model, pmlTest)
p2



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(p2)


