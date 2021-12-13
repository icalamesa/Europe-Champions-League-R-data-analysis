

extract_results <- function(x, num)
{
  #print(x)
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))  # Apply gregexpr & regmatches
  ret = as.numeric(unlist(x_numbers))
  #this shall return an array of pairs.
  #if x is more than one row it shall still return pairs in a single array
}