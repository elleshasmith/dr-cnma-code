################################################################################
## PROJECT: Diminishing Returns CNMA
## AUTHOR: Ellesha Smith
#  TITLE: Application of DR-CNMA models to LOS dataset
################################################################################

#===============================================================================
# Load packages and set library paths
#===============================================================================

# Load packages
.libPaths("Z:\\Rlib")
library("readxl")
library("rje")

# Set working directory
setwd(".")

# Load data
data<-read.csv("LengthOfStayDataWide2.csv")
data

# Bugs directory
bugs_directory <- "WinBUGS14"

# Load functions
source("functions.R")

# Define number of simulations as 1
n_simulations <- 1

# Load skeleton data
load("skeleton.RData")

#===============================================================================
# Create matrices to store the medians and credible intervals
#===============================================================================

# CNMA statistics matrix
cnma_statistics_matrix <- matrix(NA, nrow = n_simulations, ncol = length(skeleton$component_effects))
rownames(cnma_statistics_matrix) <- paste0("sim", 1:n_simulations)
colnames(cnma_statistics_matrix) <- names(skeleton$component_effects)
cnma_statistics <- list(medians = cnma_statistics_matrix,
                        lower_cis = cnma_statistics_matrix,
                        upper_cis = cnma_statistics_matrix,
                        sd = cnma_statistics_matrix)

# Cumulative linear CNMA statistics matrix
cumulative_linear_statistics_matrix <- matrix(NA, nrow = n_simulations, ncol = length(skeleton$component_effects) + 1)
rownames(cumulative_linear_statistics_matrix) <- paste0("sim", 1:n_simulations)
colnames(cumulative_linear_statistics_matrix) <- c(names(skeleton$component_effects), "beta")
cumulative_linear_statistics <- list(medians = cumulative_linear_statistics_matrix,
                                     lower_cis = cumulative_linear_statistics_matrix,
                                     upper_cis = cumulative_linear_statistics_matrix,
                                     sd = cumulative_linear_statistics_matrix)

# Multiplicative CNMA statistics matrix
multiplicative_effects_statistics <- list(medians = cumulative_linear_statistics_matrix,
                                          lower_cis = cumulative_linear_statistics_matrix,
                                          upper_cis = cumulative_linear_statistics_matrix,
                                          sd = cumulative_linear_statistics_matrix)

#===============================================================================
# Set up things that don't change between iterations
#===============================================================================

#Create lists to store the results from each model and each simulation
cnma_results_list <- list()
cumulative_linear_results_list <- list()
multiplicative_effects_results_list <- list()

# MCMC characteristics
n_chains <- 2      #number of MCMC chains to run
n_its <- 150000  #number of samples for each chain     
n_bi <- 50000   #number of samples to discard as burn-in
n_thin <- 5  #thinning rate, increase this to reduce autocorrelation

# Create data to send to WinBUGs
max_arms <- max(data$na)
y <- as.matrix(cbind(data$mean1,data$mean2, data$mean3, data$mean4))
sd <- as.matrix(cbind(data$sd1,data$sd2, data$sd3, data$sd4))
n <- as.matrix(cbind(data$n1,data$n2, data$n3, data$n4))
n_arms <- as.vector(data$na)
n_trials<- length(data$id)
component_names <- names(skeleton$component_effects)[1:6]
n_components <- length(component_names)
sd[sd == 0] <- 0.0001

#Create component array
components <- array(dim = c(n_trials, max_arms, n_components),
                    dimnames = list(data$id,
                                    1:max_arms,
                                    component_names
                    )
)


for (comp in component_names) {
  cols <- paste0("c.", comp, 1:max_arms)
  components[, , comp] <- as.matrix(data[, cols])
}

component_columns <- names(data)[grepl("^c\\.", names(data))]

# Calculate the number of components (ensure numeric)
nc <- matrix(0, nrow = nrow(data), ncol = max_arms)

for (k in 1:max_arms) {
  cols <- paste0("c.", component_names, k)
  nc[, k] <- rowSums(data[, cols])
}

tnc <- 0.5 * nc * (nc - 1)

#===============================================================================
# Set up initial values
#===============================================================================

# #Initial values for delta
# T_columns <- data[, grepl("mean*", colnames(data))]
# delta_inits <- 1 * is.na(T_columns)
# delta_inits[, "mean1"] <- NA
# delta_inits[delta_inits == 1] <- NA

#Initial values for CNMA model
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

#Initial values for cumulative linear model
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

#Initial values for multiplicative effects model
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
# Set up matrices for convergence measures
#===============================================================================

## Set up Rhat matrices

# CNMA
cnma_rhat_matrix <- matrix(
  NA,
  nrow = n_simulations,
  ncol = length(component_names)
)

rownames(cnma_rhat_matrix) <- paste0("sim", 1:n_simulations)
colnames(cnma_rhat_matrix) <- component_names

cnma_rhat <- cnma_rhat_matrix

# Cumulative linear 
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

# Multiplicative effects
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

# Simulation range
current_sim_range <- 1:1

for (sim in current_sim_range) {
  #### CNMA MODEL ####
  winbugs_data <- list(y = y, sd = sd, n = n, components = components, 
                       na = n_arms, Ntrials = n_trials, nt = n_components+1)
  
  cnma_bugs_out <- R2WinBUGS::bugs(
    data = winbugs_data,
    inits = cnma_inits,
    parameters.to.save = c("d", "dall"),
    model.file = paste0("cnma_model_fixed.txt"),
    n.chains = n_chains,
    clearWD = FALSE,
    n.iter = n_its,
    n.thin = n_thin,
    n.burnin = n_bi,
    bugs.directory = bugs_directory,
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
    model.file = paste0("cumulative_linear_model_fixed.txt"),
    n.chains = n_chains,
    clearWD = FALSE,
    n.iter = n_its,
    n.thin = n_thin,
    n.burnin = n_bi,
    bugs.directory = bugs_directory,
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
    model.file = paste0("multiplicative_effects_model_fixed.txt"),    
    n.chains = n_chains,
    clearWD = FALSE,
    n.iter = n_its,
    n.thin = n_thin,
    n.burnin = n_bi,
    bugs.directory = bugs_directory,
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
# Convergence checks
#===============================================================================

# Define rhat threshold
threshold <- 1.05

# CNMA convergence
# Per-simulation max R-hat
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

# Simulation range
current_sim_range <- 1:1

# Match target_rows to skeleton-defined component names
components <- c("p", "s", "b", "c", "r", "e")

make_code <- function(comb) {
  vec <- rep(1, length(components))
  idx <- match(comb, components)
  vec[idx] <- 2
  paste0("dall[", paste(vec, collapse=","), "]")
}

# Generate all non-empty combinations
all_combinations <- unlist(
  lapply(1:length(components), function(x) combn(components, x, simplify = FALSE)),
  recursive = FALSE
)

# Create names like "PI:PG", "PI:PG:TI", etc.
comb_names <- sapply(all_combinations, paste, collapse=":")

# Build component map
component_map <- setNames(
  sapply(all_combinations, make_code),
  comb_names
)

# Reorder target_rows to match the column order in your matrix
target_rows <- component_map

# Extract estimates
for (sim in current_sim_range) {
  #CNMA
  # d_rows <- grepl("^dall\\[", rownames(cnma_results_list[[sim]]$quantiles))
  cnma_statistics$medians[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "50%"]
  cnma_statistics$lower_cis[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "2.5%"]
  cnma_statistics$upper_cis[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "97.5%"]
  cnma_statistics$sd[paste0("sim", sim), ] <- cnma_results_list[[sim]][target_rows, "sd"]
  
  #Cumulative linear
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
  
  #Multiplicative effects
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

save(file = paste0("Parameter estimates.RData"),
     list = c("cnma_statistics"
              ,"cumulative_linear_statistics"
              ,"multiplicative_effects_statistics"
               )
      )

save(file = paste0("Convergence.RData"),
     list = c("cnma_rhat"
              ,"cumulative_linear_rhat"
              ,"multiplicative_effects_rhat"))
