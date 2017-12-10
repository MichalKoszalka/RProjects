calculateTPRFPR = function(confusionMatrix){
  cmt<- confusionMatrix$table;
  FPR = cmt[2,1]/(cmt[2,1] + cmt[1,1]);
  TPN = cmt[2,2]/(cmt[2,2] +cmt[1,2]);
  x <- c(FPR, TPN);
  return(x);
}



