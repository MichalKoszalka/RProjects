require(fields)

customkmean = function(data, cls, iterations) {
  cl = 0
  dist = 0
  x = runif(cls, c(min(data[,1])), max(data[,1]))
  y = runif(cls, min(data[,2]), max(data[,2]))
  cent = as.data.frame(matrix(c(x, y), nrow = cls, ncol = 2), stringsAsFactors = default.stringsAsFactors())
  colnames(cent) = c("X", "Y")
  for (i in 1:iterations) {
    for(j in 1:length(data[,1])) {
      for(k in 1:length(cent[,1])) {
        dist[k] = rdist(x1 = t(data[j,]), x2 = cent[k,])
      }
      cl[j] = which(dist == min(dist))
    }
    data.classes = as.data.frame(matrix(c(data, cl), ncol = 3), stringsAsFactors = default.stringsAsFactors())
    colnames(data.classes) = c("PC1", "PC2", "cl")
    
    for(j in 1:length(cent[,1])) {
      filtered = data.classes[which(data.classes$cl == j),]
      cent[j,]$X = mean(filtered$PC1)
      cent[j,]$Y = mean(filtered$PC2)
    }
    plot(data, col = cl)
    points(cent, col = 1:2, pch = 3, cex = 3)
    dev.copy(jpeg,filename=paste(i,".png" ,sep=''));
    dev.off ();
  }
  plot(data, col = cl)
  points(cent, col = 1:2, pch = 3, cex = 3)
  dev.copy(jpeg,filename="result.jpg");
  dev.off ();
}
