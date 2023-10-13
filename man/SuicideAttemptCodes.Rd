\name{SuicideAttemptCodes (version 1.1.0)}

\alias{SuicideAttemptCodes}

\title{
SuicideAttemptCodes: Search for possible suicide attempts in International Classification of Diseases (ICD-10) codes
}

\description{
This function searches for the presence/absence of possible suicide attempts as determined by three combinations of International Classification of Diseases (ICD-10) diagnosis codes. The three combinations, or \emph{Groups}, are as follows:

\itemize{

  \item \emph{Group 1}: suicide code \strong{OR} intentional self-harm codes

  \item \emph{Group 2}: suicidal ideation code \strong{AND} open wounds \strong{OR} undetermined
  injuries/events \strong{IN THE SAME VISIT}

  \item \emph{Group 3}: suicide-related mental health disorder code \strong{AND} suicide-related open
  wounds \strong{OR} undetermined injuries/events \strong{IN THE SAME VISIT}


}


}

\usage{
SuicideAttemptCodes(DX_list = NULL,
                    full_results = F,
                    big_group_3 = F,
                    debug = F)
}

\arguments{

  \item{DX_list}{
    A user supplied vector of ICD-10 diagnosis codes for each patient or patient encounter in
    character format where each diagnosis code is separated by a comma or punctuations other than a
    period. \cr
  }

  \item{full_results}{
    An option to return only suicide attempt values (\code{FALSE}) or full results (\code{TRUE}).
    Default is \code{FALSE}.
  }

  \item{big_group_3}{
    An option to use a more conversative code list for \emph{Group 3} (\code{FALSE}) or an expanded
    code list (\code{TRUE}). See Details. Default is \code{FALSE}.
  }

  \item{debug}{
    An option to run some additional tests to determine if the code ran as expected (\code{TRUE}) or
    just run the algorithm (\code{FALSE}). Default is \code{FALSE}.
  }

}

\value{

  If \code{full=FALSE} returns a vector of logical values representing the presence/absence of a
  suicide attempt for each patient or patient encounter, respectively, or \cr

  If \code{full=TRUE} returns a data frame of logical values where rows represent each patient or
  patient encounter and columns for each component of the algorithm as such: \cr

  \enumerate{
    \item \code{ICD10_suicide_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of a suicide
    code used in \emph{Group 1}

    \item \code{ICD10_poison_self_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of
    poisoning and toxic effects, intentional self-harm codes used in \emph{Group 1}

    \item \code{ICD10_asphx_self_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of
    asphyxiation, intentional self-harm codes used in \emph{Group 1}

    \item \code{ICD10_event_self_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of event,
    intentional self-harm codes used in \emph{Group 1}

    \item \code{ICD10_ideation_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of a
    suicidal ideation code used in \emph{Group 2}

    \item \code{ICD10_any_wound_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of open
    wound codes used in \emph{Group 2}

    \item \code{ICD10_poison_undet_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of
    poisoning and toxic effects, undetermined intent codes used in \emph{Groups 2} and \emph{3}

    \item \code{ICD10_asphx_undet_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of
    asphyxiation, undetermined intent codes used in \emph{Groups 2} and \emph{3}

    \item \code{ICD10_event_undet_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of event
    , undetermined intent codes used in \emph{Groups 2} and \emph{3}

    \item \code{ICD10_mental_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of suicide
    -related mental health disorder codes used in \emph{Group 3}

    \item \code{ICD10_sui_wound_code} The presence (\code{TRUE}) or absence (\code{FALSE}) of suicide
    -related open wound codes used in \emph{Group 3}

    \item \code{SA_1_any} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon \emph{Group 1}

    \item \code{SA_2_any} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon \emph{Group 2}

    \item \code{SA_3_any} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon \emph{Group 3}

    \item \code{SA_1} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon \emph{Group 1} given hierarchical coding (idential to \code{SA_1_any})

    \item \code{SA_2} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon \emph{Group 2} given hierarchical coding (i.e., coded only if \code{SA_1
    =FALSE})

    \item \code{SA_3} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon \emph{Group 3} given hierarchical coding (i.e., coded only if \code{SA_1
    =FALSE & SA_2=FALSE})

    \item \code{SA} The presence (\code{TRUE}) or absence (\code{FALSE}) of a possible suicide
    attempt based upon the presence of coding of \emph{Groups 1}, \emph{2}, or \emph{3} given
    hierarchical coding (i.e., \code{SA_1=TRUE | SA_2=TRUE | SA_3=TRUE})

  }

}


\section{Details}{

  \subsection{\code{big_group_3}}{

  If \code{FALSE}, \emph{Group 3} is defined as the presence/absence of depressive disorder/symptoms
  \strong{AND} wrist, forearm, or neck open wound codes \strong{OR} undetermined injuries/events
  \strong{IN THE SAME VISIT}.\cr

  If \code{TRUE}, \emph{Group 3} is defined as the presence/absence of a wider net of suicide-related
  mental health disorders \strong{AND} a wider net of suicide-related open wound codes \strong{OR}
  undetermined injuries/events \strong{IN THE SAME VISIT}

  }
}

\references{
%% ~put references to the literature/web site here ~
}

\author{

  Shane J Sacco, Kun Chen, & Robert H Aseltine

}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
  x = c("T14.91XA",
        "R45.851, S01.331A",
        "F33.40, T41.204A",
        "L71.0",
        "T14.91XA, R45.851, S01.331A, F33.40, T41.204A",
        "R45.851, S01.331A, F33.40, T41.204A",
        "F33.40, T41.204A",
        "F13.250, S06.379A")

  # Run function only calling back final results
  SuicideAttemptCodes(DX_list = x,
                      full_results = F,
                      big_group_3 = F,
                      debug = F)

  ##returns
  [1]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE

  # Run function only calling back final results with wide-net group 3
  SuicideAttemptCodes(DX_list = x,
                      full_results = F,
                      big_group_3 = F,
                      debug = F)

  ##returns
  [1]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE

}


\keyword{ ~datagen }
\keyword{ ~methods }
\keyword{ ~utilities }
\keyword{ ~classif }

\concept{ ~suicide }
\concept{ ~search }
