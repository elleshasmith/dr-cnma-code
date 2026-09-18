################################################################################
# Project: CNMA Diminishing Returns
# Title: R code for running simulations
# Date: 26/06/26
################################################################################

#===============================================================================
# Set up for code
#===============================================================================

# Load packages
install.packages("R2WinBUGS")
install.packages("rje")
install.packages("stringr")
library(R2WinBUGS)
library(rje)
library(stringr)

# Set path to dataset directory
cnma_directory <- ".\\DRCNMA_SS\\"
datasets_directory <- paste0(cnma_directory, "SimData\\S1\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S2\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S3\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S4\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S5\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S6\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S7\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S8\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S9\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S10\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S11\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S12\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S13\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S14\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S15\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S16\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S17\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S18\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S19\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S20\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S21\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S22\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S23\\")
datasets_directory <- paste0(cnma_directory, "SimData\\S24\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S25\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S26\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S27\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S28\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S29\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S30\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S31\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S32\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S33\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S34\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S35\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S36\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S37\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S38\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S39\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S40\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S41\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S42\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S43\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S44\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S45\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S46\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S47\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S48\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S49\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S50\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S51\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S52\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S53\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S54\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S55\\")
datasets_directory <- paste0(cnma_directory, "SimData_5c\\S56\\")

# Load functions
source(paste0(cnma_directory, "SimData\\functions.R"))
source(paste0(cnma_directory, "SimData_5c\\functions5c.R"))

# Set analysis directory path (SimAnalysis for S1-24, SimAnalysis_5c for S25-56)
cnma_analysis_directory <- ".\\DRCNMA_SS\\SimAnalysis\\"
cnma_analysis_directory <- ".\\DRCNMA_SS\\SimAnalysis_5c\\"

# Set analysis directory
analysis_directory <- paste0(cnma_analysis_directory, "S1\\")
analysis_directory <- paste0(cnma_analysis_directory, "S2\\")
analysis_directory <- paste0(cnma_analysis_directory, "S3\\")
analysis_directory <- paste0(cnma_analysis_directory, "S4\\")
analysis_directory <- paste0(cnma_analysis_directory, "S5\\")
analysis_directory <- paste0(cnma_analysis_directory, "S6\\")
analysis_directory <- paste0(cnma_analysis_directory, "S7\\")
analysis_directory <- paste0(cnma_analysis_directory, "S8\\")
analysis_directory <- paste0(cnma_analysis_directory, "S9\\")
analysis_directory <- paste0(cnma_analysis_directory, "S10\\")
analysis_directory <- paste0(cnma_analysis_directory, "S11\\")
analysis_directory <- paste0(cnma_analysis_directory, "S12\\")
analysis_directory <- paste0(cnma_analysis_directory, "S13\\")
analysis_directory <- paste0(cnma_analysis_directory, "S14\\")
analysis_directory <- paste0(cnma_analysis_directory, "S15\\")
analysis_directory <- paste0(cnma_analysis_directory, "S16\\")
analysis_directory <- paste0(cnma_analysis_directory, "S17\\")
analysis_directory <- paste0(cnma_analysis_directory, "S18\\")
analysis_directory <- paste0(cnma_analysis_directory, "S19\\")
analysis_directory <- paste0(cnma_analysis_directory, "S20\\")
analysis_directory <- paste0(cnma_analysis_directory, "S21\\")
analysis_directory <- paste0(cnma_analysis_directory, "S22\\")
analysis_directory <- paste0(cnma_analysis_directory, "S23\\")
analysis_directory <- paste0(cnma_analysis_directory, "S24\\")
analysis_directory <- paste0(cnma_analysis_directory, "S25\\")
analysis_directory <- paste0(cnma_analysis_directory, "S26\\")
analysis_directory <- paste0(cnma_analysis_directory, "S27\\")
analysis_directory <- paste0(cnma_analysis_directory, "S28\\")
analysis_directory <- paste0(cnma_analysis_directory, "S29\\")
analysis_directory <- paste0(cnma_analysis_directory, "S30\\")
analysis_directory <- paste0(cnma_analysis_directory, "S31\\")
analysis_directory <- paste0(cnma_analysis_directory, "S32\\")
analysis_directory <- paste0(cnma_analysis_directory, "S33\\")
analysis_directory <- paste0(cnma_analysis_directory, "S34\\")
analysis_directory <- paste0(cnma_analysis_directory, "S35\\")
analysis_directory <- paste0(cnma_analysis_directory, "S36\\")
analysis_directory <- paste0(cnma_analysis_directory, "S37\\")
analysis_directory <- paste0(cnma_analysis_directory, "S38\\")
analysis_directory <- paste0(cnma_analysis_directory, "S39\\")
analysis_directory <- paste0(cnma_analysis_directory, "S40\\")
analysis_directory <- paste0(cnma_analysis_directory, "S41\\")
analysis_directory <- paste0(cnma_analysis_directory, "S42\\")
analysis_directory <- paste0(cnma_analysis_directory, "S43\\")
analysis_directory <- paste0(cnma_analysis_directory, "S44\\")
analysis_directory <- paste0(cnma_analysis_directory, "S45\\")
analysis_directory <- paste0(cnma_analysis_directory, "S46\\")
analysis_directory <- paste0(cnma_analysis_directory, "S47\\")
analysis_directory <- paste0(cnma_analysis_directory, "S48\\")
analysis_directory <- paste0(cnma_analysis_directory, "S49\\")
analysis_directory <- paste0(cnma_analysis_directory, "S50\\")
analysis_directory <- paste0(cnma_analysis_directory, "S51\\")
analysis_directory <- paste0(cnma_analysis_directory, "S52\\")
analysis_directory <- paste0(cnma_analysis_directory, "S53\\")
analysis_directory <- paste0(cnma_analysis_directory, "S54\\")
analysis_directory <- paste0(cnma_analysis_directory, "S55\\")
analysis_directory <- paste0(cnma_analysis_directory, "S56\\")

# Set bugs path
bugs_directory <- ".\\WinBUGS14"

# Load previously saved parameter estimates:
#  - cnma_statistics: list containing 3 matrices:
#    - median component parameter estimates from the cnma model, one row per component
#    - lower 95% CI component parameter estimates from the cnma model, one row per component
#    - upper 95% CI component parameter estimates from the cnma model, one row per component
#  - cumulative_linear_statistics
#  - multiplicative_effects_statistics
load(paste0(analysis_directory, "Parameter estimates.RData"))

# Set number of simulations
n_simulations <- 1000

#===============================================================================
# Create matrices to store the medians and credible intervals. 
#===============================================================================

# Load skeleton data
skeleton_file <- file.path(datasets_directory, "skeleton.RData")
load(skeleton_file)

# Create CNMA results matrix 
cnma_statistics_matrix <- matrix(NA, nrow = n_simulations, ncol = length(skeleton$component_effects))
rownames(cnma_statistics_matrix) <- paste0("sim", 1:n_simulations)
colnames(cnma_statistics_matrix) <- names(skeleton$component_effects)
cnma_statistics <- list(medians = cnma_statistics_matrix,
                        lower_cis = cnma_statistics_matrix,
                        upper_cis = cnma_statistics_matrix,
                        sd = cnma_statistics_matrix)

# Create cumulative linear CNMA results matrix
cumulative_linear_statistics_matrix <- matrix(NA, nrow = n_simulations, ncol = length(skeleton$component_effects) + 1)
rownames(cumulative_linear_statistics_matrix) <- paste0("sim", 1:n_simulations)
colnames(cumulative_linear_statistics_matrix) <- c(names(skeleton$component_effects), "beta")
cumulative_linear_statistics <- list(medians = cumulative_linear_statistics_matrix,
                                     lower_cis = cumulative_linear_statistics_matrix,
                                     upper_cis = cumulative_linear_statistics_matrix,
                                     sd = cumulative_linear_statistics_matrix)

# Create multiplicative effects CNMA results matrix
multiplicative_effects_statistics <- list(medians = cumulative_linear_statistics_matrix,
                                          lower_cis = cumulative_linear_statistics_matrix,
                                          upper_cis = cumulative_linear_statistics_matrix,
                                          sd = cumulative_linear_statistics_matrix)

#===============================================================================
# Set up things that don't change between iterations
#===============================================================================

# Create lists to store the results from each model and each simulation
cnma_results_list <- list()
cumulative_linear_results_list <- list()
multiplicative_effects_results_list <- list()

# Load the first data set
first_long_data <- read.csv(file = paste0(datasets_directory, "data_sim1.csv"), header = TRUE)

# Select only the simple component columns: c.A, c.B, c.C
component_columns <- grepl("^c\\.[A-E]$", names(first_long_data))

# Calculate the number of components (ensure numeric)
first_long_data$n_components <- rowSums(
  sapply(first_long_data[, component_columns, drop = FALSE], as.numeric)
)

# Caluculate the triangulated variable for the cumulative linear CNMA model
first_long_data$triangular_n_components <- 0.5 * first_long_data$n_components * (first_long_data$n_components - 1)

# Format data to wide format
first_data <- LongToWide(first_long_data)

# Calculate number of arms
n_arms <- as.vector(table(first_long_data$Study))
max_arms <- max(n_arms)

# Extract component names
component_names <- names(first_long_data)[grepl("^c\\.[A-E]$", names(first_long_data))]

# Calculate number of component
n_components <- length(component_names)

# Calculate number of trials
n_trials <- length(first_data$Study)

# Create n matrix for number of participants in each arm
n <- as.matrix(first_data[, grepl("^N\\.", names(first_data))])

# Create matrix for number of components in each arm
nc <- as.matrix(first_data[, grepl("^n_components\\.", names(first_data))])

# Create matrix for triangulated number of components in each arm
tnc <- as.matrix(first_data[, grepl("^triangular_n_components\\.", names(first_data))])

# Create component array
components <- array(dim = c(n_trials, max_arms, n_components),
                    dimnames = list(first_data$Study,
                                    1:max_arms,
                                    component_names
                    )
)

# Fill components array
for (comp in component_names) {
  components[, , comp] <- as.matrix(first_data[, grepl(paste0("^", comp, "\\.[0-9]+(\\.|$)"), names(first_data))])
}

#===============================================================================
# Set up BUGS characteristics
#===============================================================================

# MCMC characteristics
n_chains <- 2      #number of MCMC chains to run
n_its <- 150000  #number of samples for each chain     
n_bi <- 50000   #number of samples to discard as burn-in
n_thin <- 5  #thinning rate, increase this to reduce autocorrelation

# Initial values for delta - not needed for fixed effects model
# T_columns <- first_data[, grepl("^T.*", colnames(first_data))]
# delta_inits <- 1 * is.na(T_columns)
# delta_inits[, "T.1"] <- NA
# delta_inits[delta_inits == 1] <- NA

# Initial values for CNMA model
cnma_inits1 <- list(d = c(NA, rep(0, times = n_components)),
                    #delta = delta_inits,
                    mu = rep(0, times = n_trials)
                    #,sdbt = 0.5
)
cnma_inits2 <- list(d = c(NA, rep(1, times = n_components)),
                    #delta = delta_inits,
                    mu = rep(0, times = n_trials)
                    #,sdbt = 0.5
)
cnma_inits <- list(cnma_inits1, cnma_inits2)

# Initial values for cumulative linear model
cumulative_linear_inits1 <- list(d = c(NA, rep(0, times = n_components)),
                                 #delta = delta_inits,
                                 mu = rep(0, times = n_trials),
                                 #sdbt = 0.5, # Only for random effects
                                 beta = 0
)
cumulative_linear_inits2 <- list(d = c(NA, rep(1, times = n_components)),
                                 #delta = delta_inits,
                                 mu = rep(0, times = n_trials),
                                 #sdbt = 0.5, # Only for random effects
                                 beta = 0
)
cumulative_linear_inits <- list(cumulative_linear_inits1, cumulative_linear_inits2)

# Initial values for multiplicative effects model
multiplicative_effects_inits1 <- list(d = c(NA, rep(0, times = n_components)),
                                      #delta = delta_inits,
                                      mu = rep(0, times = n_trials),
                                      #sdbt = 0.5,
                                      beta = 1)
multiplicative_effects_inits2 <- list(d = c(NA, rep(1, times = n_components)),
                                      #delta = delta_inits,
                                      mu = rep(0, times = n_trials),
                                      #sdbt = 0.5,
                                      beta = 1)
multiplicative_effects_inits <- list(multiplicative_effects_inits1, multiplicative_effects_inits2)

#===============================================================================
# Set up Rhat matrices to store results for convergence checks
#===============================================================================

# Set up CNMA rhat matrix
cnma_rhat_matrix <- matrix(
  NA,
  nrow = n_simulations,
  ncol = length(component_names)
)

rownames(cnma_rhat_matrix) <- paste0("sim", 1:n_simulations)
colnames(cnma_rhat_matrix) <- component_names
cnma_rhat <- cnma_rhat_matrix

# Set up cumulative linear CNMA rhat matrix
cumulative_linear_rhat_matrix <- matrix(
  NA,
  nrow = n_simulations,
  ncol = length(component_names) + 1
)

rownames(cumulative_linear_rhat_matrix) <- paste0("sim", 1:n_simulations)
colnames(cumulative_linear_rhat_matrix) <- c(
  component_names,
  "beta"
)
cumulative_linear_rhat <- cumulative_linear_rhat_matrix

# Set up multiplicative effects CNMA rhat matrix
multiplicative_effects_rhat_matrix <- matrix(
  NA,
  nrow = n_simulations,
  ncol = length(component_names) + 1
)
rownames(multiplicative_effects_rhat_matrix) <- paste0("sim", 1:n_simulations)
colnames(multiplicative_effects_rhat_matrix) <- c(
  component_names,
  "beta"
)
multiplicative_effects_rhat <- multiplicative_effects_rhat_matrix

#===============================================================================
# The simulations
#===============================================================================
# Change the range each time new simulations are run
current_sim_range <- 42:1000

# Run the simulation
for (sim in current_sim_range) {
  
  long_data <- read.csv(file = paste0(datasets_directory, "data_sim", sim, ".csv"), header = TRUE)
  
  # Format data
  data <- LongToWide(long_data)
  
  y <- as.matrix(data[, grepl("^Mean\\.", names(data))])
  sd <- as.matrix(data[, grepl("^SD\\.", names(data))])
  
  #### CNMA MODEL ####
  winbugs_data <- list(y = y, sd = sd, n = n, components = components, 
                       na = n_arms, Ntrials = n_trials, nt = n_components + 1)
  
  cnma_bugs_out <- R2WinBUGS::bugs(
    data = winbugs_data,
    inits = cnma_inits,
    parameters.to.save = c("d", "dall"),
    model.file = paste0(cnma_analysis_directory, "cnma_model_fixed.txt"),
    n.chains = n_chains,
    clearWD = FALSE,
    n.iter = n_its,
    n.thin = n_thin,
    n.burnin = n_bi,
    bugs.directory = bugs_directory,
    working.directory = analysis_directory,
    codaPkg = FALSE,
    save.history = TRUE,
    debug = FALSE
  )
  
  # BUGS object now contains summary directly
  cnma_results_list[[sim]] <- cnma_bugs_out$summary
  
  # Extract R-hat directly
  d_rows <- grep("^d\\[", rownames(cnma_bugs_out$summary))
  cnma_rhat[paste0("sim", sim), ] <-
  cnma_results_list[[sim]][d_rows, "Rhat"]


  #### CUMULATIVE LINEAR MODEL ####
  winbugs_data <- list(y = y, sd = sd, n = n, components = components, 
                       na = n_arms, Ntrials = n_trials,
                       nt = n_components + 1, tnc = tnc)
  
  cumulative_linear_bugs_out <- R2WinBUGS::bugs(
    data = winbugs_data,
    inits = cumulative_linear_inits,
    parameters.to.save = c("d", "dall", "beta"),
    model.file = paste0(cnma_analysis_directory, "cumulative_linear_model_fixed.txt"),
    n.chains = n_chains,
    clearWD = FALSE,
    n.iter = n_its,
    n.thin = n_thin,
    n.burnin = n_bi,
    bugs.directory = bugs_directory,
    working.directory = analysis_directory,
    codaPkg = FALSE,
    save.history = TRUE,
    debug = FALSE
  )
  
  # BUGS object now contains summary directly
  cumulative_linear_results_list[[sim]] <- cumulative_linear_bugs_out$summary
  
  # Extract R-hat directly
  target_rows <- grep("^(d\\[|beta)", rownames(cumulative_linear_bugs_out$summary))
  cumulative_linear_rhat[paste0("sim", sim), ] <-
  cumulative_linear_results_list[[sim]][target_rows, "Rhat"]

  #### MULTIPLICATIVE EFFECTS MODEL ####
  winbugs_data <- list(y = y, sd = sd, n = n, components = components, 
                       na = n_arms, Ntrials = n_trials,
                       nt = n_components + 1, nc = nc)
  
  multiplicative_effects_bugs_out <- R2WinBUGS::bugs(
    data = winbugs_data,
    inits = multiplicative_effects_inits,
    parameters.to.save = c("d", "dall", "beta"),
    model.file = paste0(cnma_analysis_directory, "multiplicative_effects_model_fixed.txt"),
    n.chains = n_chains,
    clearWD = FALSE,
    n.iter = n_its,
    n.thin = n_thin,
    n.burnin = n_bi,
    bugs.directory = bugs_directory,
    working.directory = analysis_directory,
    codaPkg = FALSE,
    save.history = TRUE,
    debug = FALSE
  )
  
  # BUGS object now contains summary directly
  multiplicative_effects_results_list[[sim]] <- multiplicative_effects_bugs_out$summary
  
  # Extract R-hat directly
  target_rows <- grep("^(d\\[|beta)", rownames(multiplicative_effects_bugs_out$summary))
  multiplicative_effects_rhat[paste0("sim", sim), ] <-
  multiplicative_effects_results_list[[sim]][target_rows, "Rhat"]
}

#===============================================================================
# Post-simulation convergence checks
#===============================================================================

# Set threshold for rhat convergence
threshold <- 1.05

# CNMA convergence
# Per-simulation max rhat
cnma_max_rhat <- apply(cnma_rhat, 1, max, na.rm = TRUE)

# Fully converged runs (strict)
cnma_converged <- cnma_max_rhat <= threshold

# Proportion of converged runs
cnma_prop_converged <- mean(cnma_converged)

# Optional: per-parameter convergence rate
cnma_param_convergence <- colMeans(cnma_rhat <= threshold, na.rm = TRUE)

# Cumulative linear convergence
cumlin_max_rhat <- apply(cumulative_linear_rhat, 1, max, na.rm = TRUE)

cumlin_converged <- cumlin_max_rhat <= threshold

cumlin_prop_converged <- mean(cumlin_converged)

cumlin_param_convergence <- colMeans(cumulative_linear_rhat <= threshold, na.rm = TRUE)

# Multiplicative effects convergence
mult_max_rhat <- apply(multiplicative_effects_rhat, 1, max, na.rm = TRUE)

mult_converged <- mult_max_rhat <= threshold

mult_prop_converged <- mean(mult_converged)

mult_param_convergence <- colMeans(multiplicative_effects_rhat <= threshold, na.rm = TRUE)

#===============================================================================
# Extract the medians and credible intervals
#===============================================================================

# Set the simulation range to extract
current_sim_range <- 1:1000

# Match target_rows to skeleton-defined component names
component_names <- names(skeleton$component_effects)

# Create a named vector to map component names to dall[...] rows for 3 component simulations
component_map <- c(
  A       = "dall[2,1,1]",
  B       = "dall[1,2,1]",
  C       = "dall[1,1,2]",
  "A:B"   = "dall[2,2,1]",
  "A:C"   = "dall[2,1,2]",
  "B:C"   = "dall[1,2,2]",
  "A:B:C" = "dall[2,2,2]"
)

# Create a named vector to map component names to dall[...] rows for 5 component simulations
component_map <- c(
  A         = "dall[2,1,1,1,1]",
  B         = "dall[1,2,1,1,1]",
  C         = "dall[1,1,2,1,1]",
  D         = "dall[1,1,1,2,1]",
  E         = "dall[1,1,1,1,2]",
  
  "A:B"     = "dall[2,2,1,1,1]",
  "A:C"     = "dall[2,1,2,1,1]",
  "A:D"     = "dall[2,1,1,2,1]",
  "A:E"     = "dall[2,1,1,1,2]",
  "B:C"     = "dall[1,2,2,1,1]",
  "B:D"     = "dall[1,2,1,2,1]",
  "B:E"     = "dall[1,2,1,1,2]",
  "C:D"     = "dall[1,1,2,2,1]",
  "C:E"     = "dall[1,1,2,1,2]",
  "D:E"     = "dall[1,1,1,2,2]",
  
  "A:B:C"   = "dall[2,2,2,1,1]",
  "A:B:D"   = "dall[2,2,1,2,1]",
  "A:B:E"   = "dall[2,2,1,1,2]",
  "A:C:D"   = "dall[2,1,2,2,1]",
  "A:C:E"   = "dall[2,1,2,1,2]",
  "A:D:E"   = "dall[2,1,1,2,2]",
  "B:C:D"   = "dall[1,2,2,2,1]",
  "B:C:E"   = "dall[1,2,2,1,2]",
  "B:D:E"   = "dall[1,2,1,2,2]",
  "C:D:E"   = "dall[1,1,2,2,2]",
  
  "A:B:C:D" = "dall[2,2,2,2,1]",
  "A:B:C:E" = "dall[2,2,2,1,2]",
  "A:B:D:E" = "dall[2,2,1,2,2]",
  "A:C:D:E" = "dall[2,1,2,2,2]",
  "B:C:D:E" = "dall[1,2,2,2,2]",
  
  "A:B:C:D:E" = "dall[2,2,2,2,2]"
)

# Reorder target_rows to match the column order in your matrix
target_rows <- component_map[component_names]

# Extract results and store in data frame 
for (sim in current_sim_range) {
  # CNMA
  # d_rows <- grepl("^dall\\[", rownames(cnma_results_list[[sim]]$quantiles))
  cnma_statistics$medians[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "50%"]
  cnma_statistics$lower_cis[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "2.5%"]
  cnma_statistics$upper_cis[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "97.5%"]
  cnma_statistics$sd[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "sd"]

  # Cumulative linear
  # d_rows <- grepl("^dall\\[", rownames(cumulative_linear_results_list[[sim]]$quantiles))
  cumulative_linear_statistics$medians[paste0("sim", sim), ] <-
    c(cumulative_linear_results_list[[sim]][target_rows, "50%"],
      cumulative_linear_results_list[[sim]]["beta", "50%"])
  cumulative_linear_statistics$lower_cis[paste0("sim", sim), ] <-
    c(cumulative_linear_results_list[[sim]][target_rows, "2.5%"],
      cumulative_linear_results_list[[sim]]["beta", "2.5%"])
  cumulative_linear_statistics$upper_cis[paste0("sim", sim), ] <-
    c(cumulative_linear_results_list[[sim]][target_rows, "97.5%"],
      cumulative_linear_results_list[[sim]]["beta", "97.5%"])
  cumulative_linear_statistics$sd[paste0("sim", sim), ] <-
    c(cumulative_linear_results_list[[sim]][target_rows, "sd"],
      cumulative_linear_results_list[[sim]]["beta", "sd"])
  
  # Multiplicative effects
  # d_rows <- grepl("^dall\\[", rownames(multiplicative_effects_results_list[[sim]]$quantiles))
  multiplicative_effects_statistics$medians[paste0("sim", sim), ] <-
    c(multiplicative_effects_results_list[[sim]][target_rows, "50%"],
      multiplicative_effects_results_list[[sim]]["beta", "50%"])
  multiplicative_effects_statistics$lower_cis[paste0("sim", sim), ] <-
    c(multiplicative_effects_results_list[[sim]][target_rows, "2.5%"],
      multiplicative_effects_results_list[[sim]]["beta", "2.5%"])
  multiplicative_effects_statistics$upper_cis[paste0("sim", sim), ] <-
    c(multiplicative_effects_results_list[[sim]][target_rows, "97.5%"],
      multiplicative_effects_results_list[[sim]]["beta", "97.5%"])
  multiplicative_effects_statistics$sd[paste0("sim", sim), ] <-
    c(multiplicative_effects_results_list[[sim]][target_rows, "sd"],
      multiplicative_effects_results_list[[sim]]["beta", "sd"])
}


#===============================================================================
# Save the updated parameter estimates
#===============================================================================

# Save parameter estimates
save(file = paste0(analysis_directory, "Parameter estimates.RData"),
     list = c("cnma_statistics"
              ,"cumulative_linear_statistics"
              ,"multiplicative_effects_statistics"
              # "multiplicative_statistics",
              # "misterlich_statistics",
              # "michaelis_menten_statistics"
     )
)

# Save convergence statistics
save(file = paste0(analysis_directory, "Convergence.RData"),
     list = c("cnma_rhat"
              ,"cumulative_linear_rhat"
              ,"multiplicative_effects_rhat"))
     

