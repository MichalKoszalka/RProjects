geneticFitness = function(x) {
      A = dataset
       for(i in 1:length(x)) {
             A[A == i] <- x[i];
             A[A == -i] <- !x[i];
       }
      print(A)
       result = 0;
       for(r in 1 : nrow(A)) {
         rowSum1 <- FALSE;
         for(c in 1:(ncol(A)-1)) {
           if(c==1) {
             rowSum1 = A[r,c];
           } else {
             rowSum1 <- rowSum1 || A[r,c];
           }
           print(A[r,c]);
         }
         print(rowSum1)
        if(rowSum1 == TRUE) {
            result = result + 1;  
        }
           
  }
        return(-result)
}