passGenerator <- function(length){
  if(length < 8){
    return("The length should be at least 8 characters")
  }
  
  lower <- letters
  upper <- toupper(letters)
  number <- as.character(0:9)
  special <- strsplit("@#$&*!", "")[[1]]
  
  # Sample randomly selects an element from each character set
  password <- c(sample(lower, 1), sample(upper, 1), sample(number, 1), sample(special, 1))
  
  allcharacter <- c(lower, upper, number, special)
  password <- c(password, sample(allcharacter, length - 4, replace = TRUE))
  
  # Shuffle the password and collapse into a single string
  password <- sample(password)
  return(paste(password, collapse = ""))
}


repeat {
  length <- as.integer(readline(prompt = 'Enter the length of the password: '))
  
  if (!is.na(length) && length >= 8) {
    password <- passGenerator(length)
    cat("Generated password is: ", password, "\n")
    break
  } else {
    cat("Invalid input. The length should be at least 8 characters.\n")
  }
}

