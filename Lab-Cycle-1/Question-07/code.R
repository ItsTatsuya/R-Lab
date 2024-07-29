generateFibonacci <- function(){
  range <- as.integer(readline(prompt = "Enter the range of number to generate: "))
  fibonacci <- c(1,1,1)
  for(i in 4:range){
    nextNumber <- fibonacci[i-1]+fibonacci[i-2]+fibonacci[i-3]
    fibonacci <- append(fibonacci,nextNumber)
  }
  cat("The series is : ",fibonacci)
}

generateFibonacci()