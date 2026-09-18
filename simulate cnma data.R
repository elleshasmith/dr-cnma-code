################################################################################
## PROJECT: Diminishing Returns CNMA
## AUTHOR: Tom Morris, Ellesha Smith
#  TITLE: Simulating C1 data with 3 components
################################################################################

#Set the working directory to be the location of this file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Set cnma_directory
cnma_directory <- ".\\DRCNMA_SS"

#Load simulation functions
source(paste0(cnma_directory, "\\SimData\\functions.R"))


#===============================================================================
# Generate and save the skeleton dataset
#===============================================================================

# Set seed
seed<-64282
set.seed(seed)

# Create cnma_data
cnma_data <- data.frame(
  study = paste0("Study", sprintf(fmt = "%02i", rep(1:28, each = 2))),
  n_arms = rep(2, times = 56),
  treatment = c(
    "Reference", "A", "Reference", "A", "Reference", "A", "Reference", "A",
    "Reference", "B", "Reference", "B", "Reference", "B", "Reference", "B",
    "Reference", "C", "Reference", "C", "Reference", "C", "Reference", "C",
    "Reference", "A+B", "Reference", "A+B", "Reference", "A+B", "Reference", "A+B",
    "Reference", "A+C", "Reference", "A+C", "Reference", "A+C", "Reference", "A+C",
    "Reference", "B+C", "Reference", "B+C", "Reference", "B+C", "Reference", "B+C",
    "Reference", "A+B+C", "Reference", "A+B+C", "Reference", "A+B+C", "Reference", "A+B+C"
  ),
  control = rep("Reference", times = 56),
  n_patients = rep(100, each = 2)
)

# Add component variables
cnma_data <- CreateComponentVariables(
  cnma_data = cnma_data,
  components = c("A", "B", "C")
)

# Add mu variable
cnma_data$mu <- rep(runif(n = 28, min = -2, max = 2), each = 2)

#===============================================================================
# Choose scenario to simulate
#===============================================================================

# Scenario 1: C1M1P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 0, 0, 0,0)
)

# Scenario 2: C1M2P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 0, 0, 0, 0)
)

# Scenario 3: C1M3P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 0, 0, 0, 0)
)

# Scenario 4: C1M4P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 0, 0, 0, 0)
)

# Scenario 5: C1M1P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, -0.1, -0.1, -0.1, 0)
)

# Scenario 6: C1M2P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, -0.1, -0.1, -0.1, 0)
)

# Scenario 7: C1M3P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, -0.1, -0.1, -0.1, 0)
)

# Scenario 8: C1M4P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, -0.1, -0.1, -0.1, 0)
)

# Scenario 9: C1M1P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, -0.4, -0.4, -0.4, 0)
)

# Scenario 10: C1M2P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, -0.4, -0.4, -0.4, 0)
)

# Scenario 11: C1M3P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3,  -0.4, -0.4, -0.4, 0)
)

# Scenario 12: C1M4P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, -0.4, -0.4, -0.4, 0)
)

# Scenario 13: C1M1P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, -0.3, -0.1, -0.3, 0)
)

# Scenario 14: C1M2P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, -0.3, -0.1, -0.3, 0)
)

# Scenario 15: C1M3P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3,  -0.3, -0.1, -0.3, 0)
)

# Scenario 16: C1M4P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, -0.3, -0.1, -0.3, 0)
)

# Scenario 17: C1M1P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 0, 0.2, 0, 0)
)

# Scenario 18: C1M2P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 0, 0.2, 0, 0)
)

# Scenario 19: C1M3P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 0, 0.2, 0, 0)
)

# Scenario 20: C1M4P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 0, 0.2, 0, 0)
)

# Scenario 21: C1M1P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, -0.3, 0.2, -0.3, 0)
)

# Scenario 22: C1M2P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, -0.3, 0.2, -0.3, 0)
)

# Scenario 23: C1M3P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, -0.3, 0.2, -0.3, 0)
)

# Scenario 24: C1M4P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, -0.3, 0.2, -0.3, 0)
)

names(cnma_data$component_effects) <- c("A", "B", "C", "A:B", "A:C", "B:C", "A:B:C")
skeleton <-cnma_data

#===============================================================================
# Simulate data
#===============================================================================

#The number of simulations
n_simulations <- 1000

#-------------------------------------------------------------------------------
# S1: CNMA
#-------------------------------------------------------------------------------

# Set seed
set.seed(64282)

# Simulate data
for (sim in 1:n_simulations) {

  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S1\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S1\\skeleton.RData",
     list = "skeleton")


#-------------------------------------------------------------------------------
# S2 
#-------------------------------------------------------------------------------

# Set seed
set.seed(66282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S2\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S2\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S3 
#-------------------------------------------------------------------------------

# Set seed
set.seed(68282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S3\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S3\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S4 
#-------------------------------------------------------------------------------

# Set seed
set.seed(70282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S4\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S4\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S5 
#-------------------------------------------------------------------------------

# Set seed
set.seed(72282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S5\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S5\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S6 
#-------------------------------------------------------------------------------

# Set seed
set.seed(74282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S6\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S6\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S7
#-------------------------------------------------------------------------------

# Set seed
set.seed(76282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S7\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S7\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S8
#-------------------------------------------------------------------------------

# Set seed
set.seed(78282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S8\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S8\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S9
#-------------------------------------------------------------------------------

# Set seed
set.seed(80282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S9\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S9\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S10
#-------------------------------------------------------------------------------

# Set seed
set.seed(82282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S10\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S10\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S11
#-------------------------------------------------------------------------------

# Set seed
set.seed(84282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S11\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S11\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S12
#-------------------------------------------------------------------------------

# Set seed
set.seed(86282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S12\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S12\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S13
#-------------------------------------------------------------------------------

# Set seed
set.seed(88282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S13\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S13\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S14
#-------------------------------------------------------------------------------

# Set seed
set.seed(90282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S14\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S14\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S15
#-------------------------------------------------------------------------------

# Set seed
set.seed(92282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S15\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S15\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S16
#-------------------------------------------------------------------------------
set.seed(94282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S16\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S16\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S17
#-------------------------------------------------------------------------------

# Set seed
set.seed(96282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S17\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S17\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S18
#-------------------------------------------------------------------------------

# Set seed
set.seed(98282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S18\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S18\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S19
#-------------------------------------------------------------------------------

# Set seed
set.seed(100282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S19\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S19\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S20
#-------------------------------------------------------------------------------

# Set seed
set.seed(102282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S20\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S20\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S21
#-------------------------------------------------------------------------------

# Set seed
set.seed(104282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S21\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S21\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S22
#-------------------------------------------------------------------------------

# Set seed
set.seed(106282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S22\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S22\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S23
#-------------------------------------------------------------------------------

# Set seed
set.seed(108282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S23\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S23\\skeleton.RData",
     list = "skeleton")

# #-------------------------------------------------------------------------------
# # S24
# #-------------------------------------------------------------------------------

# Set seed
set.seed(110282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S24\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

# Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData\\S24\\skeleton.RData",
     list = "skeleton")
