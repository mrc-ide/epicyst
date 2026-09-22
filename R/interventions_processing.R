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
  
  # isolating relevant age classes
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
  
  # isolating relevant age classes
  tail_states_age <- list (
    SP0 = SP0_age,
    VP0 = VP0_age
  )
  
  return(tail_states_age)
}


#' @title
#' pre_pig_combined
#' @description
#' Selects the pig compartments needed for a coupled round. Both the Pig_MDA
#' and the Pig_vaccine move are applied to the same sub-list, so it must carry
#' every compartment either touches: SP0, PP0, IPL0, IPH0, RP0 (Pig_MDA) and
#' VP0 (Pig_vaccine).
#'
#' @param age_target numeric vector of pig age classes
#' @param tail_states output from inter_run_setup()
#'
#' @return list of age-subsetted pig states
#' @export
pre_pig_combined <- function(age_target, tail_states) {
  
  need <- c("SP0", "PP0", "IPL0", "IPH0", "RP0", "VP0")
  miss <- need[!need %in% names(tail_states)]
  if (length(miss)) {
    stop("pre_pig_combined(): tail_states is missing ",
         paste(miss, collapse = ", "))
  }
  
  list(
    SP0  = unlist(tail_states$SP0 [grep("SP",  names(tail_states$SP0 ))][age_target]),
    PP0  = unlist(tail_states$PP0 [grep("PP",  names(tail_states$PP0 ))][age_target]),
    IPL0 = unlist(tail_states$IPL0[grep("IPL", names(tail_states$IPL0))][age_target]),
    IPH0 = unlist(tail_states$IPH0[grep("IPH", names(tail_states$IPH0))][age_target]),
    RP0  = unlist(tail_states$RP0 [grep("RP",  names(tail_states$RP0 ))][age_target]),
    VP0  = unlist(tail_states$VP0 [grep("VP",  names(tail_states$VP0 ))][age_target])
  )
}



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
#' pig_combined_round
#' @description
#' Applies a coupled round across its two age windows and writes the result
#' back into the full state list.
#'
#' The vaccine window is where the schedule completes and protection is
#' conferred - one age class per cohort, or the full eligible range for a
#' catch-up round. The OFZ window is the wider set of classes at which a drench
#' is given; classes in it but below the vaccine window receive oxfendazole
#' alone, at `ofz_cov`.
#'
#' @param tail_states full state list from inter_run_setup()
#' @param age_target_vac vaccine age window; if not numeric, taken from
#'   age_struc_pig_vacc_func()
#' @param age_target_ofz OFZ age window; if not numeric, taken as the vaccine
#'   window extended downwards by one round interval (floored at age class 1)
#' @param ofz_cov,vac_cov nested coverages, from resolve_combined_cov()
#' @param effects_full effect list at coverage 1
#' @param vaccinate_recovered see pig_combined_event()
#' @param na_pig number of pig age classes
#' @param intervention_frequency round interval, used to build defaults
#'
#' @return list with `states`, `age_target_vac` and `age_target_ofz`, the
#'   latter two so the caller can retain defaults filled in here
#' @export
pig_combined_round <- function(tail_states, age_target_vac, age_target_ofz,
                               ofz_cov, vac_cov, effects_full,
                               vaccinate_recovered, na_pig,
                               intervention_frequency) {
  
  # ---- Non age-structured pig model: no selection possible ----------------- #
  if (na_pig == 1) {
    sm <- pig_combined_event(
      states = tail_states[c("SP0", "PP0", "IPL0", "IPH0", "RP0", "VP0")],
      ofz_cov = ofz_cov, vac_cov = vac_cov, effects_full = effects_full,
      vaccinate_recovered = vaccinate_recovered)
    tail_states[names(sm)] <- sm
    return(list(states = tail_states,
                age_target_vac = age_target_vac,
                age_target_ofz = age_target_ofz))
  }
  
  # ---- Defaults ------------------------------------------------------------ #
  if (!is.numeric(age_target_vac)) {
    age_target_vac <- age_struc_pig_vacc_func(
      oldest_age = na_pig, intervention_frequency = intervention_frequency)
  }
  if (!is.numeric(age_target_ofz)) {
    age_target_ofz <- max(1, min(age_target_vac) - intervention_frequency):
      max(age_target_vac)
  }
  
  if (!all(age_target_vac %in% age_target_ofz)) {
    stop("The vaccine age window must sit inside the OFZ window: every ",
         "vaccinated animal is also medicated.")
  }
  
  # ---- Vaccine window: three-cell coupled round ---------------------------- #
  p  <- pre_pig_combined(age_target = age_target_vac, tail_states = tail_states)
  sm <- pig_combined_event(states = p, ofz_cov = ofz_cov, vac_cov = vac_cov,
                           effects_full = effects_full,
                           vaccinate_recovered = vaccinate_recovered)
  states <- update_states(states_move = sm, tail_states = tail_states)
  
  # ---- OFZ-only classes: drench at ofz_cov, no protection conferred -------- #
  ofz_only_ages <- setdiff(age_target_ofz, age_target_vac)
  
  if (length(ofz_only_ages) > 0 && ofz_cov > 0) {
    q    <- pre_pig_MDA(age_target = ofz_only_ages, tail_states = states)
    trt  <- intervention_event_state(states = q, intervention = "Pig_MDA",
                                     intervention_effect = effects_full)
    sm2  <- blend_states(list(q, trt), c(1 - ofz_cov, ofz_cov))
    states <- update_states(states_move = sm2, tail_states = states)
  }
  
  list(states = states,
       age_target_vac = age_target_vac,
       age_target_ofz = age_target_ofz)
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
  
  # isolating relevant age classes
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
  
  # isolating relevant age classes
  tail_states_age <- list (
    IH0 = IH0_age,
    SH0 = SH0_age 
  )
  
  return(tail_states_age)
} 