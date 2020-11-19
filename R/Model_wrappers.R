
#' @title
#' run Cysticercosis model ODE
#' @description
#' runs a single implementation of the ODE Cysticercosis model
#'
#' @param tt vector of times
#' @param params list of parameters
#' @param states list of states
#' @export
single_run <- function(tt, params, states) {
  mod <- cyst_generator(user = c(params, states))
  y <- mod$run(tt)
  return(y)
}


#' @title
#' run Cysticercosis model with interventions
#' @description
#' runs the ODE Cysticercosis model
#'
#' @param params List of model parameters
#' @param initial_states List of intitial state values
#' @param time The number of years to run the model for (from equilibrium). Default is at the halfway point.
#' @param intervention A vector of interventions to include from: Husbandry, Sanitatio, Inspection, Pig_MDA, Pig_vaccine and Human_test_and_treat
#' @param intervention_effect A list of intervention effect sizes, see \code{intervention_effect_size} for details
#' @param intervention_time Timing of intervention
#' @param step time step (months)
#' @param burn_in A burn in period run before model run (years)
#' @examples
#' # run the baseline model:
#' M1 <- run_model(time = 50, burn_in = 50)
#' plot(M1$t / 12, M1$Humans_Cysticercosis, t = "l", ylim = c(0, 1000), ylab = "Humans with Cysticercosis", xlab = "time (years)")
#'
#' # run the model with a single intervention:
#' M2 <- run_model(time = 50, intervention = "Sanitation", intervention_time = 20, burn_in = 50)
#' lines(M2$t / 12, M2$Humans_Cysticercosis, col = "deeppink")
#'
#' # run the model with multiple interventions:
#' M3 <- run_model(time = 50, intervention = c("Human_test_and_treat", "Pig_MDA"), intervention_time = 20, burn_in = 50)
#' lines(M3$t / 12, M3$Humans_Cysticercosis, col = "dodgerblue")
#' legend("topright", c("Baseline", "Sanitation", "Human test & treat and Pig MDA"), lty = c(1, 1, 1), col = c("black", "deeppink", "dodgerblue"))
#' @export
run_model <- function(params = NULL, initial_states = NULL, time, intervention = NULL, intervention_time = time / 2, intervention_effect = intervention_effect_size(), step = 1 / 30, burn_in = 0) {
  
  # Calculate parmaters and initial state variables (if not provided)
  initialise <- set_up()
  if (is.null(params)) {
    params <- initialise[[1]]
  }
  if (is.null(initial_states)) {
    initial_states <- initialise[[2]]
  }
  
  # run burn in period
  if (burn_in > 0) {
    tt_burn <- seq(0, (burn_in * 12), step)
    burn <- single_run(tt_burn, params = params, states = initial_states)
    initial_states <- as.list(utils::tail(burn, 1))
    names(initial_states) <- paste(colnames(burn), "0", sep = "")
    initial_states <- clean_initial_states(initial_states)
  }
  
  # run with no interventions (if none specified)
  if (is.null(intervention)) {
    run <- single_run(seq(0, time * 12, step), params, initial_states)
    run <- as.data.frame(run)
    return(run)
  }
  
  # Checks on inputs
  check_interventions(intervention)
  check_effect(intervention_effect)
  stopifnot(
    is.numeric(time), is.numeric(intervention_time), is.numeric(step),
    length(time) == 1, length(intervention_time) == 1, length(step) == 1,
    time > 0, intervention_time <= time, intervention_time > 0, is.list(params),
    is.list(initial_states), is.numeric(burn_in), burn_in >= 0
  )
  
  # Set time vectors for pre- and pos-intervention
  tt1 <- seq(0, (intervention_time * 12) - step, step)
  # Set yearly times for interention period
  splits <- seq((intervention_time * 12), time * 12, 12)
  tt2 <- list()
  for (i in 1:(length(splits) - 1)) {
    tt2[[i]] <- seq(splits[i] + step, splits[i + 1], step)
  }
  
  # run the pre-intervention period
  bl <- single_run(tt1, params = params, states = initial_states)
  
  runs <- list()
  runs[[1]] <- bl
  
  for (i in 1:length(tt2)) {
    # Pull the 'end' state values from previous run
    tail_states <- as.list(utils::tail(runs[[i]], 1))
    names(tail_states) <- paste(colnames(runs[[i]]), "0", sep = "")
    tail_states <- clean_initial_states(tail_states)
    
    # Alter states/params for single interventions
    if (i == 1) {
      params <- intervention_event_param(params = params, intervention, intervention_effect)
    }
    
    # Alter states/params for repeat interventions
    states <- intervention_event_state(states = tail_states, intervention, intervention_effect)
    
    
    # Do the next run
    runs[[i + 1]] <- single_run(tt2[[i]], params, states = states)
  }
  
  runs <- do.call("rbind", runs)
  runs <- as.data.frame(runs)
  
  return(runs)
}


#' Clean initial states
#'
#' @param x dataframe of model output states
#'
#' @return Initial states formatted for input (without summary columns)
clean_initial_states <- function(x){
  x <- x[!names(x) %in% c(
    "Humans_Taeniasis0", "Humans_Cysticercosis0",
    "Pigs_Cysticercosis0", "Human_Taeniasis_prev0",
    "Human_Cysticercosis_prev0", "Pig_Cysticercosis_prev0",
    "t0"
  )]
}
