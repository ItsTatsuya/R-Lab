reverseList<-function(lists,lengths){
  if(lengths==0){
    return(lists[lengths])
  }
  return(c(lists[lengths],reverseList(lists,lengths-1)))
}

main <-function(){
  lists <- as.integer(strsplit(readline(prompt = "Enter the element seperated by comma : "),split = ",")[[1]])
  lengths <- length(lists)
  rlist <- reverseList(lists,lengths)
  cat("Reversed List is : ",rlist)
}

main()