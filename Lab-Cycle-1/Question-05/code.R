seriesSum <- function(term){
  sum <- 1
  j <-3
  if(term==1){
    return(1)
  }else{
    for(i in 2:term){
      sum <- sum + (((-1)^(i-1)) * i / (j))
      j<-j+2
    }
  }
  return(sum)
}
main <- function(){
  term <- as.integer(readline("Enter the number of term to calculate : "))
  cat("The Sum is : ",seriesSum(term))
}

main()
