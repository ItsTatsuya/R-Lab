checkPrime <- function(number) {
  if (number <= 1) {
    return(FALSE)
  }
  if (number == 2) {
    return(TRUE)
  }
  if (number %% 2 == 0) {
    return(FALSE)
  }
  
  sqrt_n <- floor(sqrt(number))
  for (i in 3:sqrt_n) {
    if (number %% i == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

generatePrime <- function(range) {
  prime <- c()  
  
  if (range >= 2) {
    for (i in 2:range) {
      if (checkPrime(i)) {
        prime <- append(prime, i)  
      }
    }
  }
  
  return(prime)
}

main <- function() {
  repeat {
    cat("1. Check Prime \n")
    cat("2. Generate a Series of Primes \n")
    cat("3. Quit \n")
    x <- as.integer(readline(prompt = "Choose an option: "))
    
    if (x == 1) {
      num <- as.integer(readline(prompt = "Enter a number to check: "))
      if (checkPrime(num)) {
        cat(num, "is a prime number.\n")
      } else {
        cat(num, "is not a prime number.\n")
      }
      
    } else if (x == 2) {
      range <- as.integer(readline(prompt = "Enter the range to generate primes up to: "))
      primes <- generatePrime(range)
      cat("Prime numbers up to", range, "are:", primes, "\n")
      
    } else if (x == 3) {
      cat("Quitting...\n")
      break
      
    } else {
      cat("Invalid option. Please choose 1, 2, or 3.\n")
    }
  }
}

main()