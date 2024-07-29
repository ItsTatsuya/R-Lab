caesarCipher <- function(text, shift) {
  result <- ""
  
  for (i in 1:nchar(text)) {
    char <- substr(text, i, i)
    if (grepl("[A-Za-z]", char)) {
      ascii_offset <- ifelse(char >= 'A' & char <= 'Z', 65, 97)
      shifted_char <- intToUtf8((utf8ToInt(char) - ascii_offset + shift) %% 26 + ascii_offset)
      result <- paste0(result, shifted_char)
    } else {
      result <- paste0(result, char)
    }
  }
  
  return(result)
}


while(TRUE){
  cat('1 CIPHER TEXT \n')
  cat("2 QUIT \n")
  
  x<-as.integer(readline(prompt = 'chosse one option : '))
  if(x==1){
    text<-readline(prompt = 'Enter the text : ')
    shiftValue<-as.integer(readline(prompt = 'Enter the shifted Value : '))
    cipher<-caesarCipher(text,shiftValue)
    cat('Ciphered text is : ',cipher,'\n')
  }else if(x==2){
    cat("Thank You \n")
    break
  }else{
    cat('Invalid')
  }
  
}