############################################################
####                                                    ####
####          Suicide Attempt Classification            ####
####                                                    ####
############################################################


SuicideAttemptCodes = function(DX_list      = NULL,
                               full_results = F,
                               big_group_3  = F,
                               debug        = F){

  if(is.null(DX_list)){

    #throw error message if codes are null
    stop("Please provide a list of diagnosis codes!")

  }else{

    #########import attempt definitions##########

    if(big_group_3 == F){

    #group 3 mental health disorders only depressive
    ind = which(ICD$Rule_Number == "Rule 3A" &
             grepl("depress|Depress|bipolar|Bipolar", ICD$Code_Description) == F)
    ICD = ICD[-ind, ]

    #group 3 injuries only wrist, forearm, neck
    ind = which(ICD$Rule_Number == "Rule 3B"&
             grepl("Wrist|wrist|Forearm|forearm|Neck|neck", ICD$Code_Description) == F)
    ICD = ICD[-ind, ]

    }

    ############################################

    #########Format provided codes##############

    #remove periods from codes
    DX_list = str_replace_all(DX_list, "\\.", "")

    ############################################

    ######Search for definition components######

    #create labels for definition components and attempt outcomes
    definition_labels = c("ICD10_suicide_code",
                          "ICD10_poison_self_code",
                          "ICD10_asphx_self_code",
                          "ICD10_event_self_code",
                          "ICD10_ideation_code",
                          "ICD10_ideation_code",
                          "ICD10_poison_undet_code",
                          "ICD10_asphx_undet_code",
                          "ICD10_event_undet_code",
                          "ICD10_mental_code",
                          "ICD10_sui_wound_code")

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

    if(debug == T){
      
      #check for zero attempts
      SA_sum = sum(results[,18])
      
      if(SA_sum == 0) warning("The code ran as expected, but no possible attempts were detected. This could be accurate, but you might want to double check the format of your data.")
      
      #check for length of codes
      n = length(DX_list)
      if(n > 10000) n = 10000
      DX_length = nchar(DX_list[1:n])
      if(sum(DX_length <= 8) == n) warning("The code ran as expected, but the data you provided might be incomplete codes or only one diagnosis code per entry. Consider double checking the format of your data. We only checked the first 10,000 entries.")
      
      
    }
    
    if(full_results == T){

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
    
  }

}

