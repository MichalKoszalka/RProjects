diabs <- read.csv("https://inf.ug.edu.pl/~gmadejsk/IO2017/diabetes.csv")
diabetes <- diabs
diabetes[,9] = ifelse(diabetes[,9] == 'tested_positive', 1, 0)
diabetes.norm <- as.data.frame(lapply(diabetes[1:9], normalize))
set.seed(1234)
diabs_separated <- sample(2, nrow(diabetes.norm), replace=TRUE, prob=c(0.67, 0.33))
diabetes_train <- diabetes.norm[diabs_separated==1, 1:9]
diabetes_test <- diabetes.norm[diabs_separated==2, 1:9]
diabetes_testLabels <- diabs[diabs_separated==2, 9]
diabetes.trainLabels <- diabs[diabs_separated==1,9]

diabetes_neuralnet <- neuralnet(class ~ pregnant.times + glucose.concentr + blood.pressure + skin.thickness + insulin + mass.index + pedigree.func + age, diabetes_train, hidden=10, lifesign="full")
plot(diabetes_neuralnet)
predicate <- compute(diabetes_neuralnet, diabetes_test[1:8])
predicate$net.result

diabetes_result <- data.frame(pred=ifelse(predicate$net.result[,1] > 1.0, 'tested_positive', 'tested_negative'))
cf <- confusionMatrix(diabetes_result$pred, diabetes_testLabels)
cf$overall['Accuracy'] * 100