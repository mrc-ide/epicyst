#' @title
#' Pre_pig_MDA
#' @description
#' Takes processed Tail states (following burn-in and a run) and selects specific age groups to implement pig MDA moves
#' @param Tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return Tail states targetted for age-structured pig MDA intervention 
#' @export

Pre_pig_MDA <- function(age_target, Tail_states){
  
  IPL0_age <- (unlist(Tail_states$IPL0[grep("IPL", names(Tail_states$IPL0))][age_target]))
  IPH0_age <- (unlist(Tail_states$IPH0[grep("IPH", names(Tail_states$IPH0))][age_target]))
  SP0_age  <- (unlist(Tail_states$SP0[grep("SP", names(Tail_states$SP0))][age_target]))
  RP0_age  <- (unlist(Tail_states$RP0[grep("RP", names(Tail_states$RP0))][age_target]))
  PP0_age  <- (unlist(Tail_states$PP0[grep("PP", names(Tail_states$PP0))][age_target]))
  
  tail_states_age <- list (
    SP0 = SP0_age,
    IPL0 = IPL0_age, 
    IPH0 = IPH0_age, 
    RP0 = RP0_age,
    PP0 = PP0_age
  )
  
  return(tail_states_age)
} # isolating relevant age classes


#' @title
#' Pre_pig_vaccine
#' @description
#' Takes processed Tail states (following burn-in and a run) and selects specific age groups to implement pig vaccine moves
#' @param Tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return Tail states targetted for age-structured pig vaccine intervention 
#' @export

Pre_pig_vaccine <- function(age_target, Tail_states){
  
  SP0_age  <- (unlist(Tail_states$SP0[grep("SP", names(Tail_states$SP0))][age_target]))
  VP0_age  <- (unlist(Tail_states$VP0[grep("VP", names(Tail_states$VP0))][age_target]))
  
  tail_states_age <- list (
    SP0 = SP0_age,
    VP0 = VP0_age
  )
  
  return(tail_states_age)
} # isolating relevant age classes


#' @title
#' Update_states
#' @description
#' Identifies age targetted states and updates these specific states in the overall Tail states from the initial model run
#' @param tail_states output from Inter_run_processing (to update specific age-classes)
#' @param states_move new age-specific states (updated following intervention moves) 
#'
#' @return overall Tail states list with new / updated specific age - classes following intervention moves 
#' @export

Update_states <- function(states_move, tail_states){
  
  # loop through our states that we are moving over (works for long list and nested)
  for (i in seq_len(length(states_move))) {
    
    # firstly are there any occurence of the names in states_move[[i]] that are
    # in tail_states, i.e. not nested within a named vector (e.g. tail_states_long) 
    # from long list
    mtchs <- match(names(states_move[[i]]), names(tail_states)) # should be NA if nested
    
    # if there are then move those across
    if (any(!is.na(mtchs))) {
      tail_states[na.omit(mtchs)] <- states_move[[i]][which(!is.na(mtchs))]
    } 
    
    # were there any occurrences that did not appear in tail_states (i.e. if NA)
    if (any(is.na(mtchs))) {
      
      # now let's check if those non appearing names are within a named vector 
      # within tail_states, (e.g. tail_states) i.e. nested
      
      # first is there a list element in tail_states that has the same name as
      # the states_move[i] i.e. names(states_move)[1] to check if SP0 occurs
      int_matches <- match(names(states_move)[i],names(tail_states))
      
      # if there are then look inside that tail_states element (i.e. SP0 nested = TRUE)
      if (!is.na(names(tail_states[int_matches]))) {
        
        # check for occurences within that tail_states element
        ins_matches <- match(names(states_move[[i]]), names(tail_states[[int_matches]])) ## identifies matching nested numbers i.e. SP[3]0
        
        # and if there were any matches within that then move the across. 
        if (any(!is.na(ins_matches))) {
          tail_states[[int_matches]][na.omit(ins_matches)] <- states_move[[i]][which(!is.na(ins_matches))]
        }
      }
    }
    
  }
  
  return(tail_states)
  
}

#' @title
#' age_struc_pig_vacc_func
#' @description
#' define age structure for pig vaccine if no age structure included (to account for vaccination from 2 months, and interval between 1st + 2nd dose which must be < 4 months)
#' @param oldest_age oldest_age - upper limit of age vector
#' @param Intervention_frequency and 2 months to this to define lowest limit for age vector
#'
#' @return age_target_pig_vaccine : vector defining lower and upper age limits for pig vaccination 
#' @export

age_struc_pig_vacc_func <-function(oldest_age, Intervention_frequency){
  
  # if(Intervention_frequency > 4){
  #   stop('If implementing pig vaccination, must have the frequency of vaccination rounds < 5 months as 2nd dose needed
  #        within 4 months (Lightowlers et al. 2016)')
  # }
  
  youngest <- Intervention_frequency + 2   # pigs vaccinated from 2 months
  
  oldest <- oldest_age
  
  age_target_pig_vaccine <- c(youngest:oldest)
  
  return(age_target_pig_vaccine)
}


#' @title
#' Pre_human_MDA
#' @description
#' Takes processed Tail states (following burn-in and a run) and selects specific age groups to implement human MDA moves
#' @param Tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return Tail states targetted for age-structured human MDA intervention 
#' @export

Pre_human_MDA <- function(age_target, Tail_states){
  
  IH0_age <- (unlist(Tail_states$IH0[grep("IH", names(Tail_states$IH0))][age_target]))
  SH0_age <- (unlist(Tail_states$SH0[grep("SH", names(Tail_states$SH0))][age_target]))
  SHC0_age  <- (unlist(Tail_states$SHC0[grep("SHC", names(Tail_states$SHC0))][age_target]))
  IHC0_age  <- (unlist(Tail_states$IHC0[grep("IHC", names(Tail_states$IHC0))][age_target]))
  
  tail_states_age <- list (
    IH0 = IH0_age,
    SH0 = SH0_age, 
    SHC0 = SHC0_age, 
    IHC0 = IHC0_age
  )
  
  return(tail_states_age)
} # isolating relevant age classes

#' @title
#' Pre_human_test_and_treat
#' @description
#' Takes processed Tail states (following burn-in and a run) and selects specific age groups to implement human T&T moves
#' @param Tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return Tail states targetted for age-structured human T&T intervention 
#' @export

Pre_human_test_and_treat <- function(age_target, Tail_states){
  
  IH0_age <- (unlist(Tail_states$IH0[grep("IH", names(Tail_states$IH0))][age_target]))
  SH0_age <- (unlist(Tail_states$SH0[grep("SH", names(Tail_states$SH0))][age_target]))
  
  tail_states_age <- list (
    IH0 = IH0_age,
    SH0 = SH0_age 
  )
  
  return(tail_states_age)
} # isolating relevant age classes