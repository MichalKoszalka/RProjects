iris.values = scale(log(iris[, 1:4]))
iris.prcomp = prcomp(iris.values)
iris.predicted = predict(iris.prcomp)[,1:2]
result = kmeans(iris.predicted, 3, iter.max = 1000, nstart = 3)
plot(iris.predicted, col = result$cluster)
points(result$centers, col = 1:2, pch = 3, cex = 3)
