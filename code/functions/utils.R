# helper functions

# order two words alphabetically, returning the word in the order (1 or 2) 
# specified
alpha_order <- function(name, match, order) {
    vec <- c(name, match)
    a1 <- sort(vec)[order]
    return(a1)
}

create_edge <- function(name, match) {
	edge <- c(name, match)
	return (edge)
}