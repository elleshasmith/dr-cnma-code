################################################################################
## PROJECT: Diminishing Returns CNMA
## AUTHOR: Ellesha Smith
#  TITLE: Generating skeleton data set for application to LOS
################################################################################

#===============================================================================
# Load packages and set library paths
#===============================================================================

# Load packages
.libPaths("Z:\\Rlib")

# Set the working directory
setwd(".")

# Load simulation functions
source("functions.R")

#===============================================================================
# Construction of the skeleton CNMA dataset
#===============================================================================

# Define components
components <- c("p", "s", "b", "c", "r", "e")

# Create function that generates dall array
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
all_treatments <- setNames(
  sapply(all_combinations, make_code),
  comb_names
)

# Repeat each treatment 4 times (4 studies per comparison)
treatments_rep <- rep(all_treatments, each = 4)

# Define number of studies
n_studies <- length(treatments_rep)

# Create data frame
cnma_data <- data.frame(
  study = rep(sprintf("Study%03i", 1:n_studies), each = 2),
  n_arms = rep(2, times = n_studies * 2),
  treatment = as.vector(rbind(rep("Reference", n_studies), treatments_rep)),
  control = rep("Reference", times = n_studies * 2),
  n_patients = rep(100, times = n_studies * 2)
)

# Create component variables
cnma_data <- CreateComponentVariables(
  cnma_data = cnma_data,
  components =  c("p", "s", "b", "c", "r", "e")
)

# Create mu column
cnma_data$mu <- rep(runif(n = 252, min = -2, max = 2), each = 2)

# Define component effects
cnma_data <- list(
  cnma_data = cnma_data,
  component_effects = c(1, 1, 1, 1, 1, 1, 
                        rep(0,57)
                        )
                  )

#===============================================================================
# Save skeleton data
#===============================================================================

# Rename component effect columns
names(cnma_data$component_effects) <- c(comb_names)

# Call this skeleton
skeleton <- cnma_data

# Save the skeleton dataset
save(file = "skeleton.RData",
     list = "skeleton")

