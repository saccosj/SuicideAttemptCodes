############################################################
####                                                    ####
####          Suicide Attempt Classification            ####
####                                                    ####
############################################################


SuicideAttemptCodes = function(DX_list = NULL, full = F, group_3_big = F){

  if(is.null(DX_list) == F){

    #######import packages and functions########

    #import data.table for quick tables and install if needed
    if(require("data.table") == F){

      install.packages("data.table")
      require("data.table")

    }

    #import stringr for string manipulation and install if needed
    if(require("stringr") == F){

      install.packages("stringr")
      require("stringr")

    }

    ############################################

    #########import attempt defintions##########

    if(group_3_big == F){

    #group 3 mental health disorders only depressive
    ind = (ICD$Rule_Number == "Rule 3A" &
             grepl("depress|Depress|bipolar|Bipolar", ICD$Code_Description)) == F
    ICD = ICD[ind == F, ]

    #group 3 injuries only wrist, forearm, neck
    ind = (ICD$Rule_Number == "Rule 3B"&
             grepl("Wrist|wrist|Forearm|forearm|Neck|neck", ICD$Code_Description)) == F
    ICD = ICD[ind == F, ]

    }

    ############################################

    #########Format provided codes##############

    #remove periods from codes
    DX_list = str_replace_all(DX_list, "\\.", "")

    ############################################

    ######Search for definition components######

    #create labels for defintion components and attempt outcomes
    definition_labels = unique(ICD$Category)
    sa_labels = c("SA_1_any", "SA_2_any", "SA_3_any","SA_1", "SA_2", "SA_3", "SA")

    #create placeholder matrix for results
    results = matrix(NA,
                     nrow = length(DX_list),
                     ncol = length(definition_labels) + length(sa_labels)
    )

    #group 1 components
    results[,1]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Suicide attempt"], DX_list)
    results[,2]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Poisoning, intentional self-harm"], DX_list)
    results[,3]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Asphyxiation, intentional self-harm"], DX_list)
    results[,4]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Intentional self-harm event"], DX_list)

    #unique group 2 components
    results[,5]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Suicidal ideation"], DX_list)
    results[,6]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Open wounds"], DX_list)
    results[,7]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Poisoning, undetermined"], DX_list)
    results[,8]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Asphyxiation, undetermined"], DX_list)
    results[,9]  = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Undetermined event"], DX_list)

    #unique group 3 components
    results[,10] = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Related mental disorders"], DX_list)
    results[,11] = fast_grepl(ICD$ICD_10_Code[ICD$Category == "Related open wounds"], DX_list)

    ###calculate attempts
    #any group 1
    results[,12] = results[,1] | results[,2] | results[,3] | results[,4]
    #any group 2
    results[,13] = results[,5] &
                   (results[,6] | results[,7] | results[,8] | results[,9])
    #any group 3
    results[,14] = results[,10] &
                   (results[,11] | results[,7] | results[,8] | results[,9])

    #unique group 1
    results[,15] = results[,12]
    #unique group 2
    results[,16] = results[,13] & results[,12] == F
    #unique group 3
    results[,17] = results[,14] & results[,12] == F & results[,13] == F

    #any attempt present
    results[,18] = results[,15] | results[,16] | results[,17]

    if(full == T){

      #convert results to dataframe
      results = as.data.frame(results)

      #provide column names
      colnames(results) = c(definition_labels, sa_labels)

      #return results
      results

    }else{

      #return results
      results[,18]

    }

  }else{

    #throw error message if codes are null
    print("Please provide list of diagnosis codes!")

  }

}
