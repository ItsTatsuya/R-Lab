runLengthEncoder <- function() {
  text <- readline(prompt = 'Enter the string: ')
  result <- ""
  n <- nchar(text)
  stext <- strsplit(text, split = "")[[1]]
  
  i <- 1
  while (i <= n) {
    count <- 1
    while (i < n && stext[i] == stext[i + 1]) {
      count <- count + 1
      i <- i + 1
    }
    result <- paste0(result, stext[i], count)
    i <- i + 1
  }
  
  return(result)
}

printstatement <- function() {
  result <- runLengthEncoder()
  cat("Encoded string is:", result, "\n")
}

printstatement()
