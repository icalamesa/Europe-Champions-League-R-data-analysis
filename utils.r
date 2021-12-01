extract_results <- function(x)
{
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))  # Apply gregexpr & regmatches
  as.numeric(unlist(x_numbers)) 
}