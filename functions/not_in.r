#Function to ask "not in"
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0