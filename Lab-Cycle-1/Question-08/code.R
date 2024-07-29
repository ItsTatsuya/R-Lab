checkPalindrome <- function(){
  string <- readline(prompt = "enter the string to text : ")
  string <- tolower(gsub("\\s+","",string))
  revstring <- tolower(paste(rev(strsplit(string,"")[[1]]),collapse = ""))
  if(string==revstring){
    cat("Given string is palindrome")
  }else{
    cat("Given String is not palindrome")
  }
}

checkPalindrome()