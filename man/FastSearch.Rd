\name{fast_grepl}

\alias{FastSearch}

\title{
Fast Search
}

\description{

  This function searches for patterns in a string quicker than the base \code{grepl}
  function, expanding upon the base \code{grepl} function. The function is \strong{only} faster if 
  patterns are long and complex, containing similar first characters, while strings are also long and
  complex containing many different first characters. For example, searching for International   
  Classification of Disease codes of a particular chapter among patient records containing all 
  chapters.

}

\usage{
fast_grepl(pattern, string)
}

\arguments{

  \item{pattern}{
    A user supplied pattern in character format to search for in user supplied string. If multiple 
    patterns, \strong{DO NOT} collapse into one pattern, the function will do it for you.\cr 
  }

  \item{string}{
    A user supplied string or vector of strings to search for the user supplied pattern.
  }
  
}

\value{

  returns a single or vector of logical values indicating the presence (\code{TRUE}) or absence 
  (\code{FALSE}) of the pattern within the strings or strings. 

}

\references{

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole 
  (grep) \cr
  
  R Core Team (2021). R: A language and environment for statistical computing. R Foundation for 
  Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
  
  
}

\author{

  Shane J Sacco

}

\examples{

  #Install and load required packages
  ##if SuicideAttemptCodes is not installed, load R package 'devtools' and install from github
  if(require('SuicideAttemptCodes') == F){
    ##Load R package 'devtools' (and install if necessary) to install 'SuicideAttemptCodes' from github
    if(require('devtools') == F){
    
      install.packages('devtools')
      require('devtools')
    
    }
    
    install_github('saccosj/SuicideAttemptCodes')
    require('SuicideAttemptCodes')
    
  }
  
#Generate an example vector to test
string = c("T14.91XA",
      "R45.851, S01.331A",
      "F33.40, T41.204A",
      "L71.0",
      "T14.91XA, R45.851, S01.331A, F33.40, T41.204A",
      "R45.851, S01.331A, F33.40, T41.204A",
      "F33.40, T41.204A",
      "F13.250, S06.379A")

#repeat vector many times
string = rep(string, 10000000)

#define a multiple, complex patterns to search
pattern = c("T13.250", "T33.40", "T41.204A", "T14.91XA",
            "T21.204A", "T44.91XA", "T31.204A", "T15.91XA")

#get time spent for fast_grepl
system.time(fast_grepl(pattern = pattern,
           string = string))
## user  system elapsed 
## 72.00    0.59   72.60 

#get time spent for base grepl
system.time(grepl(pattern = paste0(pattern, collapse = "|"),
           x = string))
## user  system elapsed 
## 89.95    0.22   90.17

#check if results are identical
identical(fast_grepl(pattern = pattern,
                     string = string),
          grepl(pattern = paste0(pattern, collapse="|"),
                x = string))
##[1] TRUE

}


\keyword{ ~datagen }
\keyword{ ~methods }
\keyword{ ~utilities }
\keyword{ ~classif }

\concept{ ~suicide }
\concept{ ~search }