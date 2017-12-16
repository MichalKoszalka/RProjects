library("neuralnet")
library("caret")

extendIris = function (iris_input) {
  iris_data <- iris_input
  iris_data[6] <-iris_data[5]
  iris_data[7] <-iris_data[5]
  iris_data[8] <-iris_data[5]
  for(i in 1: length(iris_data[,1])) {
    if(iris_data[i,5] == 'setosa') {
      iris_data[i,9] = 1
    } else {
      iris_data[i,9] = 0
    }
    if(iris_data[i,6] == 'versicolor') {
      iris_data[i,10] = 1
    } else {
      iris_data[i,10] = 0
    }
    if(iris_data[i,7] == 'virginica') {
      iris_data[i,11]  = 1
    } else {
      iris_data[i,11]  = 0
    }
  }
  names(iris_data)[9] <- 'setosa'
  names(iris_data)[10] <- 'virginica'
  names(iris_data)[11] <- 'versicolor'
  iris_data[6] <- NULL
  iris_data[6] <- NULL
  iris_data[6] <- NULL
  iris_data[9] <- iris_data[5]
  iris_data[5] <- NULL
  
  return(iris_data)
}

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

iris_norm <- extendIris(iris_input = iris)
iris_norm <- as.data.frame(lapply(iris_norm[1:7], normalize))
set.seed(1234)
separated_iris <- sample(2, nrow(iris_norm), replace=TRUE, prob=c(0.67, 0.33))
iris_labels <- iris[5]
View(iris_labels)
iris_training <- iris_norm[separated_iris==1, 1:7]
iris_test <- iris_norm[separated_iris==2, 1:7]
iris_test_labels <- iris[separated_iris==2, 5]
iris_neuralnet <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris_training, hidden=4, lifesign="full")
iris_predicate <- compute(iris_neuralnet, iris_test[1:4])
iris_results <- data.frame(pred=
                            ifelse(iris_predicate$net.result[,1] > 0.5, "setosa",
                                   (ifelse(iris_predicate$net.result[,2] > 0.5, "versicolor",
                                           ifelse(iris_predicate$net.result[,3] > 0.5, "virginica", "NA")))))



cf <- confusionMatrix(iris_results$pred, iris_test_labels)
cf
