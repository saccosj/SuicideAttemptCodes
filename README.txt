###################SuicideAttemptCodes###################

###
Contents of main folder###

1. /data/ subfolder: contains data (attempt definitions) for use in the main function.

2. /man/ subfolder: contains R documentation for included functions.

3. /R/ subfolder: contains R scripts to execute functions.

4. DESCRIPTION.TXT: A basic description of the included R package.

5. README.TXT: a manifest of package contents, being read right now.


####Install SuicideAttemptCodes package###

#load (and install if needed) devtools package
if(require('devtools') == F){
  
  install.packages('devtools')
  require('devtools')
  
}

#install SuicideAttemptCodes package from github
install_github("saccosj/SuicideAttemptCodes")

#load SuicideAttemptCodes package
require("SuicideAttemptCodes")