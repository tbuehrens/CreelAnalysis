
# using ####
# Function to get packages
#
# ARGS: 
#   A character vector of the required packages
#
# RETURNS: loads required packages, or if not already installed, installs and loads 
using <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only=TRUE, quietly=TRUE))
    need <- libs[req==FALSE]
    if(length(need) > 0){ 
        install.packages(need)
        lapply(need, require, character.only=TRUE, quietly=TRUE)
    }
}  
  