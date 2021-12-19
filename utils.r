

extract_results <- function(x, num)
{
  #print(x)
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))  # Apply gregexpr & regmatches
  ret = as.numeric(unlist(x_numbers))
  #this shall return an array of pairs.
  #if x is more than one row it shall still return pairs in a single array
}

process_year <- function(dataset)
{
  proc_res = filter(dataset,
         grepl("ESP", Team.1) | grepl("ESP", Team.2) | grepl("GER", Team.1) | grepl("GER", Team.2))
  proc_res = proc_res %>% mutate(FT = str_trim(str_remove(FT, "\\(\\*\\)")), # Quita (*) y espacio en blanco
                                 score_Team.1 = as.numeric(str_split(FT, "-", simplify = TRUE)[, 1]),
                                 score_Team.2 = as.numeric(str_split(FT, "-", simplify = TRUE)[, 2]),
                                 old_Team.1 = Team.1,
                                 old_Team.2 = Team.2,
                                 Team.1 = str_trim(str_split(Team.1, "›", simplify = TRUE)[,1]),
                                 Team.2 = str_trim(str_split(Team.2, "›", simplify = TRUE)[,1]),
                                 Country_team.1 = substr(str_trim(str_split(old_Team.1, "›", simplify = TRUE)[, 2]), 1, 3),
                                 Country_team.2 = substr(str_trim(str_split(old_Team.2, "›", simplify = TRUE)[, 2]), 1, 3))
  
  scores_team.1 = aggregate(score_Team.1 ~ Team.1 + Country_team.1, data = proc_res, FUN=sum)
  names(scores_team.1)[names(scores_team.1) == 'score_Team.1'] <- 'Score'
  names(scores_team.1)[names(scores_team.1) == 'Team.1'] <- 'Team'
  names(scores_team.1)[names(scores_team.1) == 'Country_team.1'] <- 'Country'
  scores_team.2 = aggregate(score_Team.2 ~ Team.2 + Country_team.2, data = proc_res, FUN=sum)
  names(scores_team.2)[names(scores_team.2) == 'score_Team.2'] <- 'Score'
  names(scores_team.2)[names(scores_team.2) == 'Team.2'] <- 'Team'
  names(scores_team.2)[names(scores_team.2) == 'Country_team.2'] <- 'Country'
  scores = rbind(scores_team.1, scores_team.2)
  scores = aggregate(Score ~ Team + Country, data = scores, FUN=sum)
  
  #scores = mutate(scores, Country = unique(filter(proc_res, Team == proc_res$Team.1))[1])
    
  scores = filter(scores, Country == "ESP" | Country == "GER")
  
}