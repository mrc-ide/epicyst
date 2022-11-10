#' @title
#' Inter-run setup
#' @description
#' Converts output from odin into initial_states (all initial states and converts age-strcutured states & age_rates into vectors) to re-supply odin
#'
#' @param model_output large desolve element generated from odin run (ODE output)
#' @param na_pig number of age classes
#' @param na_human number of age classes (human)
#'
#' @return single lists of state variable values
#' @export
inter_run_setup <- function(model_output, na_pig, na_human) { 
  
  states_output <- as.list(utils::tail(model_output, 1))
  names(states_output) <- paste(colnames(model_output), '0', sep = '')
  
  # set number of age classes (pig compartments)
  SP_eq <- (unlist(states_output[grep("SP", names(states_output))][1:na_pig]))
  PP_eq <- (unlist(states_output[grep("PP", names(states_output))][1:na_pig]))
  IPL_eq <- (unlist(states_output[grep("IPL", names(states_output))][1:na_pig]))
  IPH_eq <- (unlist(states_output[grep("IPH", names(states_output))][1:na_pig]))
  RP_eq <- (unlist(states_output[grep("RP", names(states_output))][1:na_pig]))
  VP_eq <- (unlist(states_output[grep("VP", names(states_output))][1:na_pig]))
  
  # set number of age classes (human compartments)
  SH_eq <- (unlist(states_output[grep("SH", names(states_output))][1:na_human]))
  SHC_eq <- (unlist(states_output[grep("SHC", names(states_output))][1:na_human]))
  IH_eq <- (unlist(states_output[grep("IH", names(states_output))][1:na_human]))
  IHC_eq <- (unlist(states_output[grep("IHC", names(states_output))][1:na_human]))
  
  #=====================================================#
  #  Generate List of initial states for ODE's in ODIN #
  #====================================================#
  
  states_processed<-list(
    E0=states_output$E0,
    SH0 = SH_eq,
    SHC0 = SHC_eq,
    IH0 = IH_eq,
    IHC0 = IHC_eq,
    SP0 = SP_eq,
    PP0 = PP_eq,
    IPL0 = IPL_eq, 
    IPH0 = IPH_eq, 
    RP0 = RP_eq, 
    VP0 = VP_eq,
    CCC0 = states_output$CCC0,
    CTC0 = states_output$CTC0
   )
  
  return(states_processed)
}
