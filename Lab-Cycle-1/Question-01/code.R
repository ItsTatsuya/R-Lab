charLength <- function(words) {
  x <- numeric(length(words))
  for (i in seq_along(words)) {
    x[i] <- nchar(words[i])
  }
  return(x)
}

getMean <- function(numbers) {
  total_sum <- sum(numbers)  # Renamed variable to avoid conflict with sum() function
  return(total_sum / length(numbers))
}

getLongestChar <- function(words) {
  max_length <- 0
  longest_word <- NULL
  for (i in words) {
    if (nchar(i) > max_length) {
      max_length <- nchar(i)
      longest_word <- i
    }
  }
  return(longest_word)
}

replaceSpecificWord <- function(rword,nword, text) {
  word <- toupper(rword)
  for (i in seq_along(text)) {
    x <- toupper(text[i])
    if (word == x) {
      text[i] <- nword
    }
  }
  return(text)
}

process_text <- function() {
  text <- readline(prompt = "Enter the text : ")
  word <- unlist(strsplit(text, "\\s+"))
  
  flag <- TRUE
  while (flag) {
    cat("1 : Total number of words\n")
    cat("2 : Average word length\n")
    cat("3 : Longest word\n")
    cat("4 : Replace specific word\n")
    cat("5 : Change text\n")
    cat("6 : Quit\n")
    cat("Choose one : ")
    x <- scan(what = integer(), nmax = 1, quiet = TRUE)
    
    if (x == 1) {
      cat("Total number of words = ", length(word), "\n")
    } else if (x == 2) {
      world_length <- charLength(word)
      cat("Average word length = ", getMean(world_length), "\n")
    } else if (x == 3) {
      cat("Longest Word is = ", getLongestChar(word), "\n")
    } else if (x == 4) {
      sword <- readline(prompt = "Enter the word to replace : ")
      nword <- readline(prompt = "Enter the  new word to replace : ")
      ntext <- replaceSpecificWord(sword,nword, word)
      cat("After replacement, text is : ", paste(ntext, collapse = " "), "\n")
    } else if (x == 5) {
      text <- readline(prompt = "Enter the new text : ")
      word <- unlist(strsplit(text, "\\s+"))
    } else if (x == 6) {
      cat("Thank you\n")
      break
    } else {
      cat("Invalid choice\n")
    }
  }
}

process_text()
