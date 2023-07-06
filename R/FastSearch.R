############################################################
####                                                    ####
####                 Fast Searching                     ####
####                                                    ####
############################################################

fast_grepl = function(pattern, string){

  ##1. perform search for first unique character in patterns

  #subset pattern to first character
  x = paste0(unique(substr(pattern,1,1)), collapse = "|")

  #search to unique first characters
  key = grepl(x, string)

  #create placeholder vector to identify strings with unique first characters
  answer = rep(FALSE, length(string))

  #create full pattern string
  x = paste0(pattern, collapse = "|")

  #search for full patterns in only strings with unique first characters found
  y = grepl(x, string[key])

  #give answer for full patterns
  answer[key] = y

  #return search
  answer

}
