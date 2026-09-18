################################################################################
## PROJECT: Diminishing Returns CNMA
## AUTHOR: Tom Morris, Ellesha Smith
#  TITLE: Simulating C2 data with 5 components
################################################################################

#Set the working directory to be the location of this file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Set cnma_directory
cnma_directory <- ".\\DRCNMA_SS"

#Load simulation functions
source(paste0(cnma_directory, "\\SimData_5c\\functions5c.R"))

#===============================================================================
# Generate and save the skeleton dataset
#===============================================================================

# Set seed
seed<-74282
set.seed(seed)

# All non-empty combinations of A–E
components <- c("A", "B", "C", "D", "E")

# Generate treatments
all_treatments <- unlist(
  lapply(1:length(components), function(k) {
    apply(combn(components, k), 2, paste, collapse = "+")
  })
)

# Repeat each treatment 4 times (4 studies per comparison)
treatments_rep <- rep(all_treatments, each = 4)

# Number of studies
n_studies <- length(treatments_rep)

# Create CNMA data
cnma_data <- data.frame(
  study = rep(sprintf("Study%03i", 1:n_studies), each = 2),
  n_arms = rep(2, times = n_studies * 2),
  treatment = as.vector(rbind(rep("Reference", n_studies), treatments_rep)),
  control = rep("Reference", times = n_studies * 2),
  n_patients = rep(100, times = n_studies * 2)
)

# Add component variables
cnma_data <- CreateComponentVariables(
  cnma_data = cnma_data,
  components = c("A", "B", "C", "D", "E")
)

# Add mu variable
cnma_data$mu <- rep(runif(n = 124, min = -2, max = 2), each = 2)

#===============================================================================
# Choose scenario to simulate
#===============================================================================

# Scenario 25: C2M1P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 26: C2M2P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 27: C2M3P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 28: C2M4P1
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 29: C2M1P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)

# Scenario 30: C2M2P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)

# Scenario 31: C2M3P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)

# Scenario 32: C2M4P2
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)


# Scenario 33: C2M1P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, c(rep(0, 16)))
)

# Scenario 34: C2M2P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, c(rep(0, 16)))
)

# Scenario 35: C2M3P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, c(rep(0, 16)))
)

# Scenario 36: C2M4P3
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, c(rep(0, 16)))
)

# Scenario 37: C2M1P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, -0.3, -0.1, -0.3, -0.1, -0.3, -0.6, -0.3, -0.3, -0.1, -0.3, c(rep(0, 16)))
)

# Scenario 38: C2M2P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, -0.3, -0.1, -0.3, -0.1, -0.3, -0.6, -0.3, -0.3, -0.1, -0.3, c(rep(0, 16)))
)

# Scenario 39: C2M3P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, -0.3, -0.1, -0.3, -0.1, -0.3, -0.6, -0.3, -0.3, -0.1, -0.3, c(rep(0, 16)))
)

# Scenario 40: C2M4P4
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, -0.3, -0.1, -0.3, -0.1, -0.3, -0.6, -0.3, -0.3, -0.1, -0.3, c(rep(0, 16)))
)

# Scenario 41: C2M1P5
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.3, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)

# Scenario 42: C2M2P5
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, -0.1, -0.1, -0.1, -0.1, -0.1, -0.3, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)

# Scenario 43: C2M3P5
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, -0.1, -0.1, -0.1, -0.1, -0.1, -0.3, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)

# Scenario 44: C2M4P5
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, -0.1, -0.1, -0.1, -0.1, -0.1, -0.3, -0.1, -0.1, -0.1, -0.1, c(rep(0, 16)))
)


# Scenario 45: C2M1P6
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, -0.3, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 46: C2M2P6
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, 0, 0, 0, 0, 0, -0.3, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 47: C2M3P6
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, 0, 0, 0, 0, 0, -0.3, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 48: C2M4P6
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, 0, 0, 0, 0, 0, -0.3, 0, 0, 0, 0, c(rep(0, 16)))
)

# Scenario 49: C2M1P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, 0, 0.2, 0, 0.2, 0, -0.3, 0, 0, 0.2, 0, c(rep(0, 16)))
)

# Scenario 50: C2M2P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, 0, 0.2, 0, 0.2, 0, -0.3, 0, 0, 0.2, 0, c(rep(0, 16)))
)

# Scenario 51: C2M3P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, 0, 0.2, 0, 0.2, 0, -0.3, 0, 0, 0.2, 0, c(rep(0, 16)))
)

# Scenario 52: C2M4P7
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, 0, 0.2, 0, 0.2, 0, -0.3, 0, 0, 0.2, 0, c(rep(0, 16)))
)


# Scenario 53: C2M1P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, -0.3, 0.2, -0.3, 0.2, -0.3, -0.6, -0.3, -0.3, 0.2, -0.3, c(rep(0, 16)))
)

# Scenario 54: C2M2P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0.3, 2.9, 0.4, -0.3, 0.2, -0.3, 0.2, -0.3, -0.6, -0.3, -0.3, 0.2, -0.3, c(rep(0, 16)))
)

# Scenario 55: C2M3P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, -0.3, 2.9, 0.4, -0.3, 0.2, -0.3, 0.2, -0.3, -0.6, -0.3, -0.3, 0.2, -0.3, c(rep(0, 16)))
)

# Scenario 56: C2M4P8
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(0.1, 1.5, 0, 2.9, 0.4, -0.3, 0.2, -0.3, 0.2, -0.3, -0.6, -0.3, -0.3, 0.2, -0.3, c(rep(0, 16)))
)

# Name component effects
names(cnma_data$component_effects) <- c(
  
  # Main effects
  "A", "B", "C", "D", "E",
  
  # 2-way interactions
  "A:B", "A:C", "A:D", "A:E",
  "B:C", "B:D", "B:E",
  "C:D", "C:E",
  "D:E",
  
  # 3-way interactions
  "A:B:C", "A:B:D", "A:B:E",
  "A:C:D", "A:C:E", "A:D:E",
  "B:C:D", "B:C:E", "B:D:E",
  "C:D:E",
  
  # 4-way interactions
  "A:B:C:D",
  "A:B:C:E",
  "A:B:D:E",
  "A:C:D:E",
  "B:C:D:E",
  
  # 5-way interaction
  "A:B:C:D:E"
)

skeleton <-cnma_data

#===============================================================================
# Simulate data
#===============================================================================

#The number of simulations
n_simulations <- 1000

#-------------------------------------------------------------------------------
# S25: CNMA
#-------------------------------------------------------------------------------

# Set seed
set.seed(124282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S25\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S25\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S26 
#-------------------------------------------------------------------------------

# Set seed
set.seed(126282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S26\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}


#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S26\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S27 
#-------------------------------------------------------------------------------

# Set seed
set.seed(128282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S27\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S27\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S28 
#-------------------------------------------------------------------------------

# Set seed
set.seed(130282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S28\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S28\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S29 
#-------------------------------------------------------------------------------

# Set seed
set.seed(132282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S29\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S29\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S30 
#-------------------------------------------------------------------------------

# Set seed
set.seed(134282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S30\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S30\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S31
#-------------------------------------------------------------------------------

# Set seed
set.seed(136282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S31\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S31\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S32
#-------------------------------------------------------------------------------

# Set seed
set.seed(138282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S32\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S32\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S33
#-------------------------------------------------------------------------------

# Set seed
set.seed(140282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S33\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S33\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S34
#-------------------------------------------------------------------------------

# Set seed
set.seed(142282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S34\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S34\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S35
#-------------------------------------------------------------------------------

# Set seed
set.seed(144282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S35\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S35\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S36
#-------------------------------------------------------------------------------

# Set seed
set.seed(146282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S36\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S36\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S37
#-------------------------------------------------------------------------------

# Set seed
set.seed(148282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S37\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S37\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S38
#-------------------------------------------------------------------------------

# Set seed
set.seed(150282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S38\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S38\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S39
#-------------------------------------------------------------------------------

# Set seed
set.seed(152282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S39\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S39\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S40
#-------------------------------------------------------------------------------

# Set seed
set.seed(154282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S40\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S40\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S41
#-------------------------------------------------------------------------------

# Set seed
set.seed(156282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S41\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S41\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S42
#-------------------------------------------------------------------------------

# Set seed
set.seed(158282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S42\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S42\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S43
#-------------------------------------------------------------------------------

# Set seed
set.seed(160282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S43\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S43\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S44
#-------------------------------------------------------------------------------

# Set seed
set.seed(162282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S44\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S44\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S45
#-------------------------------------------------------------------------------

# Set seed
set.seed(164282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S45\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S45\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S46
#-------------------------------------------------------------------------------

# Set seed
set.seed(166282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S46\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S46\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S47
#-------------------------------------------------------------------------------

# Set seed
set.seed(168282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S47\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S47\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S48
#-------------------------------------------------------------------------------

# Set seed
set.seed(170282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S48\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S48\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S49
#-------------------------------------------------------------------------------

# Set seed
set.seed(172282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S49\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S49\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S50
#-------------------------------------------------------------------------------

# Set seed
set.seed(174282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S50\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S50\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S51
#-------------------------------------------------------------------------------

# Set seed
set.seed(176282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S51\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S51\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S52
#-------------------------------------------------------------------------------

# Set seed
set.seed(178282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S52\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S52\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S53
#-------------------------------------------------------------------------------

# Set seed
set.seed(180282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S53\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S53\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S54
#-------------------------------------------------------------------------------

# Set seed
set.seed(182282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S54\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S54\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S55
#-------------------------------------------------------------------------------

# Set seed
set.seed(184282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S55\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset
save(file = ".\\DRCNMA_SS\\SimData_5c\\S55\\skeleton.RData",
     list = "skeleton")

#-------------------------------------------------------------------------------
# S56
#-------------------------------------------------------------------------------

# Set seed
set.seed(186282)

# Simulate data
for (sim in 1:n_simulations) {
  
  simulated_data <- SimulateCnmaWithInteractions(
    cnma_data = cnma_data$cnma_data,
    component_effects = cnma_data$component_effects,
    delta_sd = 0,
    outcome_type = "continuous",
    outcome_sd = 1.5,
    output_folder = ".\\S56\\",
    filename_suffix = paste0("_sim", sim),
    full_output = FALSE
  )
  
}

#Save the skeleton dataset?>
save(file = ".\\DRCNMA_SS\\SimData_5c\\S56\\skeleton.RData",
     list = "skeleton")
