

extract_results <- function(x, num)
{
  #print(x)
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))  # Apply gregexpr & regmatches
  ret = as.numeric(unlist(x_numbers))
  #this shall return an array of pairs.
  #if x is more than one row it shall still return pairs in a single array
}

process_year <- function(data)
{
  summary(data)
  proc_res = filter(dataset,
         grepl("ESP", Team.1) | grepl("ESP", Team.2)) | grepl("GER", Team.1) | grepl("GER", Team.2)
  proc_res = proc_res %>% mutate(FT = str_trim(str_remove(data$FT, "\\(\\*\\)")), # Quita (*) y espacio en blanco
                                 score_Team.1 = as.numeric(str_split(FT, "-", simplify = TRUE)[, 1]),
                                 score_Team.2 = as.numeric(str_split(FT, "-", simplify = TRUE)[, 2]))
  proc_res = data %>% transmute
  
}