#' @title
#' pre_pig_MDA
#' @description
#' Takes processed tail states (following burn-in and a run) and selects specific age groups to implement pig MDA moves
#' @param tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return tail states targeted for age-structured pig MDA intervention 
#' @export

pre_pig_MDA <- function(age_target, tail_states) {
  IPL0_age <- (unlist(tail_states$IPL0[grep("IPL", names(tail_states$IPL0))][age_target]))
  IPH0_age <- (unlist(tail_states$IPH0[grep("IPH", names(tail_states$IPH0))][age_target]))
  SP0_age  <- (unlist(tail_states$SP0[grep("SP", names(tail_states$SP0))][age_target]))
  RP0_age  <- (unlist(tail_states$RP0[grep("RP", names(tail_states$RP0))][age_target]))
  PP0_age  <- (unlist(tail_states$PP0[grep("PP", names(tail_states$PP0))][age_target]))
  
  tail_states_age <- list (
    SP0 = SP0_age,
    IPL0 = IPL0_age, 
    IPH0 = IPH0_age, 
    RP0 = RP0_age,
    PP0 = PP0_age
  )
  
  return(tail_states_age)
} 


#' @title
#' pre_pig_vaccine
#' @description
#' Takes processed tail states (following burn-in and a run) and selects specific age groups to implement pig vaccine moves
#' @param tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return tail states targetted for age-structured pig vaccine intervention 
#' @export

pre_pig_vaccine <- function(age_target, tail_states) {
  SP0_age  <- (unlist(tail_states$SP0[grep("SP", names(tail_states$SP0))][age_target]))
  VP0_age  <- (unlist(tail_states$VP0[grep("VP", names(tail_states$VP0))][age_target]))
  
  tail_states_age <- list (
    SP0 = SP0_age,
    VP0 = VP0_age
  )
  
  return(tail_states_age)
} # isolating relevant age classes


#' @title
#' update_states
#' @description
#' Identifies age targetted states and updates these specific states in the overall tail states from the initial model run
#' @param tail_states output from Inter_run_processing (to update specific age-classes)
#' @param states_move new age-specific states (updated following intervention moves) 
#'
#' @return overall tail states list with new / updated specific age - classes following intervention moves 
#' @export

update_states <- function(states_move, tail_states){
  # loop through our states that we are moving over (works for long list and nested)
  for (i in seq_len(length(states_move))) {
    
    # firstly are there any occurrence of the names in states_move[[i]] that are
    # in tail_states, i.e. not nested within a named vector (e.g. tail_states_long) from long list
    mtchs <- match(names(states_move[[i]]), names(tail_states)) 
    
    # if there are then move those across
    if (any(!is.na(mtchs))) {
      tail_states[stats::na.omit(mtchs)] <- states_move[[i]][which(!is.na(mtchs))]
    } 
    
    # were there any occurrences that did not appear in tail_states (i.e. if NA)
    if (any(is.na(mtchs))) {
      
      # check if those non appearing names are within a named vector 
      # within tail_states, (e.g. tail_states) i.e. nested
      
      # first is there a list element in tail_states that has the same name as
      # the states_move[i] i.e. names(states_move)[1] to check if SP0 occurs
      int_matches <- match(names(states_move)[i],names(tail_states))
      
      # if there are then look inside that tail_states element (i.e. SP0 nested = TRUE)
      if (!is.na(names(tail_states[int_matches]))) {
        
        # check for occurrences within that tail_states element
        ins_matches <- match(names(states_move[[i]]), names(tail_states[[int_matches]])) ## identifies matching nested numbers 
        
        # and if there were any matches within that then move the across. 
        if (any(!is.na(ins_matches))) {
          tail_states[[int_matches]][stats::na.omit(ins_matches)] <- states_move[[i]][which(!is.na(ins_matches))]
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
#' @param intervention_frequency and 2 months to this to define lowest limit for age vector
#'
#' @return age_target_pig_vaccine : vector defining lower and upper age limits for pig vaccination 
#' @export

age_struc_pig_vacc_func <-function(oldest_age, intervention_frequency) {
  youngest <- intervention_frequency + 2   # pigs vaccinated from 2 months
  
  oldest <- oldest_age
  
  age_target_pig_vaccine <- c(youngest:oldest)
  
  return(age_target_pig_vaccine)
}


#' @title
#' pre_human_MDA
#' @description
#' Takes processed tail states (following burn-in and a run) and selects specific age groups to implement human MDA moves
#' @param tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return tail states targeted for age-structured human MDA intervention 
#' @export

pre_human_MDA <- function(age_target, tail_states) {
  IH0_age <- (unlist(tail_states$IH0[grep("IH", names(tail_states$IH0))][age_target]))
  SH0_age <- (unlist(tail_states$SH0[grep("SH", names(tail_states$SH0))][age_target]))
  SHC0_age  <- (unlist(tail_states$SHC0[grep("SHC", names(tail_states$SHC0))][age_target]))
  IHC0_age  <- (unlist(tail_states$IHC0[grep("IHC", names(tail_states$IHC0))][age_target]))
  
  tail_states_age <- list (
    IH0 = IH0_age,
    SH0 = SH0_age, 
    SHC0 = SHC0_age, 
    IHC0 = IHC0_age
  )
  
  return(tail_states_age)
} 

#' @title
#' pre_human_test_and_treat
#' @description
#' Takes processed tail states (following burn-in and a run) and selects specific age groups to implement human T&T moves
#' @param tail_states output from Inter_run_processing
#' @param age_target numeric of specific age groups to target/ select 
#'
#' @return tail states targeted for age-structured human T&T intervention 
#' @export
pre_human_test_and_treat <- function(age_target, tail_states){
  IH0_age <- (unlist(tail_states$IH0[grep("IH", names(tail_states$IH0))][age_target]))
  SH0_age <- (unlist(tail_states$SH0[grep("SH", names(tail_states$SH0))][age_target]))
  
  tail_states_age <- list (
    IH0 = IH0_age,
    SH0 = SH0_age 
  )
  
  return(tail_states_age)
} 