
is_age_valid <- function(age) {
  return(!is.na(as.integer(age)) && as.integer(age) >= 0)
}


is_grade_valid <- function(grade) {
  return(grade %in% c("A", "B", "C", "D", "F"))
}


studentRecord <- function() {
  student <- data.frame(Name = character(), Age = integer(), Grade = character(), stringsAsFactors = FALSE)
  
  while (TRUE) {
    cat("1. Enter Record \n")
    cat("2. Calculate Average Age \n")
    cat("3. View Details\n")
    cat("4. Quit \n")
    x <- as.integer(readline(prompt = 'Enter an option: '))
    
    if (x == 1) {
      name <- readline(prompt = 'Enter the name of the Student: ')
      flag1 <- TRUE
      while (flag1) {
        age <- as.integer(readline(prompt = 'Enter the age: '))
        if (is_age_valid(age)) {
          flag1 <- FALSE
        } else {
          cat("Invalid age. Age must be a positive number.\n")
        }
      }
      flag2 <- TRUE
      while (flag2) {
        grade <- readline(prompt = 'Enter the grade: ')
        if (is_grade_valid(grade)) {
          flag2 <- FALSE
        } else {
          cat("Invalid grade. Grade must be one of [A, B, C, D, F].\n")
        }
      }
      student <- rbind(student, data.frame(Name = name, Age = age, Grade = grade, stringsAsFactors = FALSE))
      cat("Record added successfully.\n")
      
    } else if (x == 2) {
      if (nrow(student) > 0) {
        avg_age <- mean(student$Age)
        cat("Average age of valid student records:", avg_age, "\n")
      } else {
        cat("No valid student records available to calculate the average age.\n")
      }
      
    } else if (x == 3) {
      if (nrow(student) > 0) {
        print(student)
        cat("\n")
      } else {
        cat("No student records to display.\n")
      }
      
    } else if (x == 4) {
      cat("Quitting the program.\n")
      break
      
    } else {
      cat("Invalid option. Please select a valid option (1, 2, 3, or 4).\n")
    }
  }
}


studentRecord()
