library(rje)
library(stringr)


#' Samples n integers from the range min:max, replacing or not.
#' The point of this function is that sample(a:b, ...) will sample from the range a:b, unless a == b, in which case it samples from the range 1:a. This function always samples from the range a:b.
#'   
#' @param min Minimum of the range to sample from.
#' @param max Maximum of the range to sample from.
#' @param n Number of samples to take.
#' @param replace TRUE to sample with replacement.
#' @return Vector of integers of size @param n from the given range and replacement.
SampleBetter <- function(min, max, n, replace) {
  if (min > max) {
    stop("min cannot be larger than max")
  }
  if (min == max) {
    if (!replace & n > 1) {
      stop("Set replace to TRUE")
    } else {
      return(rep(min, times = n))
    }
  } else {
    return(sample(min:max, size = n, replace = replace))
  }
}



#' Generate study names.
#'
#' @param n_arms Vector of the number of arms per study. Its length should be the number of studies.
#' @param studies Vector of study names to sample from. Its length must be at least length(@param n_arms).
#' @return Data frame with 'study' and 'n_arms' populated, and 'treatment', 'control' and 'n_patients' equal to NA.
GenerateStudies <- function(n_arms, studies){
  if (length(studies) < length(n_arms)) {
    stop("Not enough studies to sample from")
  }
  unique_studies <- sample(studies, size = length(n_arms), replace = FALSE)
  unique_studies <- unique_studies[order(unique_studies)]
  
  #Duplicate each study for its number of arms
  study <- rep(unique_studies, n_arms)
  number_of_arms <- rep(n_arms, times = n_arms)
  return(
    data.frame(
      study = study,
      n_arms = number_of_arms,
      treatment = NA,
      control = NA,
      n_patients = NA
    )
  )
}



#' Generate number of patients per arm in an NMA.
#' 
#' @param cnma_data Data frame containing 'study', 'n_arms' and 'treatment', typically created by GenerateStudies().
#' @param study_n_limits Pair of limits (l, u) to generate the number of patients per study from a Uniform(l, u) distribution.
#' @param imbalance The number of patients in the arms of a study is generated from a Uniform(@param study_n_limits - this, @param study_n_limits + this) distribution. So the maximum imbalance within a study is twice this. Defaults to 1.
#' @return @param cnma_data with 'n_patients' populated.
GenerateNumberOfPatients <- function(cnma_data, study_n_limits, imbalance = 1) {
  #The number of studies
  n_studies <- length(unique(cnma_data$study))
  #The studies and their number of arms
  study_with_n_arms <- unique(cnma_data[, c("study", "n_arms")])
  #Generate one n per study
  unique_arm_level_n <- SampleBetter(
    min = study_n_limits[1],
    max = study_n_limits[2],
    n = n_studies,
    replace = TRUE
  )
  #Sample from unique_arm_level_n +/- imbalance to get arm level n
  arm_level_n <- NULL
  for (i in 1:length(study_with_n_arms$n_arms)) {
    arm_level_n <- c(
      arm_level_n,
      SampleBetter(
        min = unique_arm_level_n[i] - imbalance,
        max = unique_arm_level_n[i] + imbalance,
        n = study_with_n_arms$n_arms[i],
        replace = TRUE
      )
    )
  }
  cnma_data$n_patients <- arm_level_n
  return(cnma_data)
}



#' Generate component and treatment names.
#'
#' @param cnma_data Data frame with 'study' and 'n_arms', typically created by GenerateStudies() and optionally GenerateNumberOfPatients().
#' @param n_components Number of components to sample.
#' @param components Vector of components to sample from.
#'        NOTE: The three arguments above must satisfy these inequalities:
#'          max(cnma_data$n_arms) <= 2^n_components, and n_components <= length(components)
#' @param reference_prob The probability that the reference is selected in each study, where applicable. Defaults to 0.5.
#' @return Data frame with 'study', 'n_arms', 'treatment', and 'control' populated, and 'n_patients' equal to NA or populated.
GenerateComponents <- function(cnma_data, n_components, components, reference_prob = 0.5) {
  #The studies and their number of arms
  study_with_n_arms <- unique(cnma_data[, c("study", "n_arms")])
  if (length(components) < n_components) {
    stop("Need more components to sample from")
  }
  if (2^n_components < max(cnma_data$n_arms)) {
    stop("Some studies have more arms than possible treatments")
  }
  #Create components and treatments
  component_order <- sample(
    components,
    size = n_components,
    replace = FALSE
  )
  combinations <- CreateTreatmentsFromComponents(
    components = component_order,
    return_type = "vector"
  )
  n_combinations <- length(combinations)
  #Create the treatments in each study.
  treatment <- NULL
  for (study in study_with_n_arms$study) {
    n_arms <- study_with_n_arms$n_arms[study_with_n_arms$study == study]
    contains_reference <- sample(
      c(FALSE, TRUE),
      size = 1,
      prob = c(1 - reference_prob, reference_prob)
    )
    #Include the reference treatment with probability reference_prob, unless there aren't enough treatments to not include the reference.
    if (contains_reference | n_arms == n_combinations) {
      treatment <- c(
        treatment,
        "Reference",
        sample(
          combinations[-1],
          size = n_arms - 1,
          replace = FALSE
        )
      )
    } else {
      treatment <- c(
        treatment,
        sample(
          combinations[-1],
          size = n_arms,
          replace = FALSE
        )
      )
    }
  }
  
  #Update output with components
  cnma_data$treatment <- treatment
  cnma_data$combination_order <- match(cnma_data$treatment, combinations)
  #Order by study, then combination_order
  cnma_data <- cnma_data[order(cnma_data$study, cnma_data$combination_order), ]
  cnma_data <- cnma_data[, -which(colnames(cnma_data) == "combination_order")]
  
  #Add the control treatment in each study to the output data frame
  cnma_data$control <- NA
  for (i in 1:length(cnma_data$study)) {
    cnma_data$control[i] <- combinations[min(match(cnma_data$treatment[cnma_data$study == cnma_data$study[i]], combinations))]
  }
  
  return(
    list(
      cnma_data = cnma_data,
      component_order = component_order
    )
  )
}



#' Creates all possible treatments formed by combinations of components.
#' 
#' @param components Vector of components in order.
#' @param return_type "vector" or "list".
#' @param separator The character to separate components if 'return_type' == "vector". Defaults to "+".
#' @param max_components An optional integer in the range 2,...,length('components') to limit the number of components that can form a treatment. Defaults to length('components').
#' @return All possible combinations of components, sorted by number of components then 'components'.
#'  - If @param return_type == "vector" then each element is a string of components separated by 'separator'.
#'  - If @param return_type == "list" then each element is a vector of components.
CreateTreatmentsFromComponents <- function(components, return_type, separator = "+", max_components = length(components)) {
  #Create a list of all possible combinations of components
  combinations_list <- rje::powerSet(x = components, m = max_components)
  #The first component in each combination
  first_components <- sapply(combinations_list, function(x){x[1]})
  #The position in 'components' of the first component in each combination
  first_components_order <- c(0, match(first_components[-1], components))
  #Sort the combinations by length then first component
  combinations_list <- combinations_list[order(lengths(combinations_list), first_components_order)]
  combinations_list <- combinations_list[-1]
  if (return_type == "list") {
    return(combinations_list)
  } else if (return_type == "vector") {
    #Turn combination list into a vector, and merge components by inserting a + between each one
    return(
      sapply(
        combinations_list,
        FUN = function(combin) {
          paste(combin, collapse = separator)
        }
      )
    )
  }
}



#' Add binary component variables indicating the presence of each component within each treatment. Also add the same for all possible interactions between components.
#'
#' @param cnma_data Data frame containing 'study' and 'treatment' where the treatments are "Reference" or consist of sums of components separated by "+".
#' @param components Vector of components.
#' @return 'cnma_data' with the additional columns "c.<component>" for each component and "c.<component_1>: ... :<component_k>" for all combinations of components.
CreateComponentVariables <- function(cnma_data, components) {
  #Vector of all combinations of components
  combinations_vector <- CreateTreatmentsFromComponents(
    components = components,
    return_type = "vector",
    separator = ":",
    max_components = 3
  )
  #List of all combinations of components
  combinations_list <- CreateTreatmentsFromComponents(
    components = components,
    return_type = "list",
    max_components = 3
  )
  for (comp in combinations_vector) {
    #Set component variable to 0 initially
    cnma_data[[paste0("c.", comp)]] <- 0
    for (row in 1:length(cnma_data$study)) {
      #Split the treatment string up by "+", check for the components in the resulting vector, set to 1 if present
      cnma_data[row, paste0("c.", comp)][all(combinations_list[[which(comp == combinations_vector)]] %in% stringr::str_split(cnma_data[row, "treatment"], "\\+")[[1]])] <- 1
    }
  }
  
  return(cnma_data)
}



#' Generate mu in each study, and mean component effects (d parameters) and interactions.
#' 
#' @param cnma_data Data frame containing 'study', 'n_arms', 'treatment', 'control' and 'n_patients'.
#' @param mu_limits Pair of limits (l, u) to generate mean control arm outcomes from a Uniform(l, u) distribution.
#' @param component_effect_limits Pair of limits (l, u) to generate component effects from a Uniform(l, u) distribution.
#' @return List with elements:
#'   - 'cnma_data': @param cnma_data with the additional column 'mu'.
#'   - 'component_effects' Named vector of mean component effects and interactions.
GenerateMuAndD <- function(cnma_data, mu_limits, component_effect_limits) {
  #The unique studies
  unique_studies <- unique(cnma_data$study)
  #The number of studies
  n_studies <- length(unique_studies)
  #The columns that contain components and interactions
  component_columns <- names(cnma_data)[grepl(pattern = "^c\\.", x = names(cnma_data))]
  #The component and interaction names (i.e. with "c." removed from the start)
  components <- substr(
    component_columns,
    start = 3,
    stop = nchar(component_columns)
  )
  #The number of components and interactions
  n_components = length(components)
  
  #Add mu
  mu <- data.frame(
    study = unique_studies,
    mu = runif(
      n = n_studies,
      min = mu_limits[1],
      max = mu_limits[2]
    )
  )
  cnma_data <- merge(cnma_data, mu, by="study")
  
  #Create d for each component and interaction
  component_effects <- runif(
    n = n_components,
    min = component_effect_limits[1],
    max = component_effect_limits[2]
  )
  names(component_effects) <- components
  
  #Set the reference effect to 0
  component_effects["Reference"] <- 0
  
  return(list(cnma_data = cnma_data, component_effects = component_effects))
}



#' Sort long CNMA data by study then treatment, with treatment sorted by component order.
#'   The order of individual components is determined by their column position.
#'   The order of combinations is determined first by the number of components and second by the individual component order.
#' 
#' @param long_data Long CNMA data.
#' @return Sorted long data.
SortByComponents <- function(long_data) {
  #The columns that contain components and interactions
  component_and_interaction_columns <- names(long_data)[grepl(pattern = "^c\\.", x = names(long_data))]
  #The columns that contain components
  component_columns <- component_and_interaction_columns[stringr::str_count(component_and_interaction_columns, pattern = ":") == 0]
  component_columns <- component_columns[component_columns != "c.Reference"]
  #The component names (i.e. with "c." removed from the start)
  components <- substr(component_columns, start = 3, stop = nchar(component_columns))
  #Create all possible combinations of components
  combinations_list <- rje::powerSet(components)
  #Order them according to number of components then individual component order
  first_components <- sapply(combinations_list, function(x){x[1]})
  first_components_order <- match(first_components, combinations_list)
  combinations_list <- combinations_list[order(lengths(combinations_list), first_components_order)]
  #Collapse the combinations into single strings with components separated by "+"
  combinations <- sapply(combinations_list, function(combin){paste(combin, collapse = "+")})
  #Add the reference to the start
  combinations[1] <- "Reference"
  
  treatment_sorting_order <- match(long_data$treatment, combinations)
  return(long_data[order(long_data$study, treatment_sorting_order), ])
}



#' Generate treatment effects in a single study in a CNMA.
#'
#' @param cnma_data Data frame containing 'study' and 'treatment'.
#' @param study A study from 'cnma_data'.
#' @param component_effects Named vector of mean component effects and interactions.
#' @param delta_sd Standard deviation of between-trial treatment effects.
#' @return 'cnma_data' with the new column 'delta'.
GenerateDeltas <- function(cnma_data, study, component_effects, delta_sd) {
  #The data for this study without the reference
  no_ref <- cnma_data[cnma_data$study == study & cnma_data$treatment != "Reference", ]
  #The number of non-reference treatments in this study
  n_treatments <- length(no_ref$study)
  
  #Derive the d for each treatment as the sum over all component combinations
  no_ref$d <- 0
  for (comp in names(component_effects)) {
    no_ref$d <- no_ref$d + component_effects[comp] * no_ref[, paste0("c.", comp)]
  }
  
  #Create the covariance matrix in two steps
  covariance <- matrix(rep(delta_sd^2 / 2, times = n_treatments^2), ncol = n_treatments)
  diag(covariance) <- delta_sd^2
  #Generate deltas for each component
  no_ref$delta <- as.vector(
    mvtnorm::rmvnorm(
      n = 1,
      mean = no_ref$d,
      sigma = covariance
    )
  )
  
  #Add reference treatment back if necessary
  if (is.element("Reference", cnma_data$treatment[cnma_data$study == study])) {
    delta_reference <- cnma_data[cnma_data$treatment == "Reference" & cnma_data$study == study, ]
    delta_reference[, c("d", "delta")] <- 0
    no_ref <- rbind(no_ref, delta_reference)
  }
  
  #Add delta1, the delta in the control group
  no_ref$delta1 <- no_ref$delta[no_ref$treatment == no_ref$control]
  
  return(no_ref)
}



#' Simulate CNMA data.
#'
#' @param cnma_data Data frame with 'study', 'n_arms', 'treatment', 'control', 'n_patients', 'mu', component columns named "c.<component>", and interaction columns named "c.<component_1>:...:<component_n>. Only two-way and three-way interactions are allowed. This dataset can be created using GenerateStudies() then GenerateNumberOfPatients() then GenerateComponents() then CreateComponentVariables() then GenerateMuAndD().
#' @param component_effects Named vector of component and interaction effects. Components in an interaction must be separated by ":".
#' @param delta_sd Standard deviation of between-trial treatment effects (defaults to 0, set to 0 to get fixed effects).
#' @param outcome_type "binary" or "continuous".
#' @param outcome_sd True standard deviation of the outcome, used to generate study SDs from a Chi-squared distribution. Required when 'outcome_type' == "continuous".
#' @param output_folder Directory to save output, preferably an empty folder. Must end in "\\". If NULL, no output is saved.
#' @param filename_suffix Appended to the filenames of the three files described in @return. Defaults to "".
#' @param full_output Set to TRUE to include the variables that were used to create the output, that the user would not have in a real data set. Defaults to FALSE.
#' @return The input data frame with generated outcomes.
#'  - if 'full_output' = TRUE then there are additional variables (only in what is returned to the R environment, not in what is saved)
#'  - Three CSV files are saved in 'output_folder':
#'    - data.csv contains the dataset in MetaInsight format.
#'    - data_full.csv contains the dataset with all additional variables.
#'    - parameters.csv contains the true values of the parameters.
SimulateCnmaWithInteractions <- function(cnma_data, component_effects, delta_sd = 0, outcome_type, outcome_sd = NULL, output_folder, filename_suffix = "", full_output = FALSE) {
  
  #The unique studies
  unique_studies <- unique(cnma_data$study)
  #The number of studies
  n_studies <- length(unique_studies)
  #The studies and their number of arms
  study_with_n_arms <- unique(cnma_data[, c("study", "n_arms")])
  #Name the random effects SD
  names(delta_sd) <- "delta_sd"

  #Get the deltas and store in a list with one study per element
  delta_list <- list()
  for (study in unique_studies) {
    delta_list[[study]] <- GenerateDeltas(
      cnma_data = cnma_data,
      study = study,
      component_effects = component_effects,
      delta_sd = delta_sd
    )
  }
  
  #Put the separated studies back into a single data frame
  cnma_output <- data.frame()
  for (i in 1:length(delta_list)) {
    cnma_output <- rbind(cnma_output, delta_list[[i]])
  }
  
  #-----Generate outcomes-----
  
  linear_predictor <- cnma_output$mu + (cnma_output$delta - cnma_output$delta1)
  
  if (outcome_type == "binary") {
    #Generate R
    cnma_output$R <- rbinom(
      n = length(cnma_output$study),
      size = cnma_output$n_patients,
      prob = plogis(linear_predictor)
    )
  } else if (outcome_type == "continuous") {
    #Generate SDs
    sd_chisq <- rchisq(
      n = length(cnma_output$study),
      df = cnma_output$n_patients - 1
    )
    cnma_output$SD <- round(
      outcome_sd / (cnma_output$n_patients - 1) * sd_chisq,
      digits = 2
    )
    #Generate Means
    cnma_output$Mean <- rnorm(
      n = length(cnma_output$study),
      mean = linear_predictor,
      sd = outcome_sd / sqrt(cnma_output$n_patients - 1)
    )
  }
  
  #Sort by Study then T, with T sorted by component order
  cnma_output <- SortByComponents(long_data = cnma_output)
  
  #Tidy up for exporting
  component_names <- paste0("c.", names(component_effects))
  names(cnma_output)[names(cnma_output) == "study"] <- "Study"
  names(cnma_output)[names(cnma_output) == "treatment"] <- "T"
  names(cnma_output)[names(cnma_output) == "n_patients"] <- "N"
  if (outcome_type == "binary") {
    cnma_output_small <- cnma_output[, c("Study", "T", "N", "R", component_names)]
  } else if (outcome_type == "continuous") {
    cnma_output_small <- cnma_output[, c("Study", "T", "N", "Mean", "SD", component_names)]
  }
  
  #Create a dataframe with all the parameters
  parameters <- data.frame(
    parameter = c(
      paste0("d-", names(component_effects)),
      paste0("delta-", paste0(cnma_output$Study, "-", cnma_output$T)),
      paste0("mu-", unique(cnma_output$Study))),
    value = c(
      component_effects,
      cnma_output[, "delta"],
      unique(cnma_output[, c("Study", "mu")])$mu
    )
  )
  if (delta_sd != 0) {
    parameters <- rbind(
      parameters,
      data.frame(
        parameter = "delta-sd",
        value = delta_sd
      )
    )
  }

  #Save output
  if (!is.null(output_folder)) {
    write.csv(
      x = cnma_output_small,
      file = paste0(output_folder, "data", filename_suffix, ".csv"),
      row.names = FALSE
    )
    write.csv(
      x = cnma_output,
      file = paste0(output_folder, "data_full", filename_suffix, ".csv"),
      row.names = FALSE
    )
    write.csv(
      x = parameters,
      file = paste0(output_folder, "parameters", filename_suffix, ".csv"),
      row.names = FALSE
    )
  }
  
  if (!full_output) {
    return(cnma_output_small)
  } else if (full_output) {
    return(cnma_output)
  }
}



#' Converts long CNMA data into wide.
#' 
#' @param long_data Long CNMA data.
#' @return Wide CNMA data.
LongToWide <- function(long_data) {
  #The columns that contain components
  component_columns <- names(long_data)[grepl(pattern = "^c\\.", x = names(long_data))]
  
  #Add a count of the arm number per study
  long_data$count <- 1
  for (row in 2:length(long_data$Study)) {
    if (long_data$Study[row] == long_data$Study[row - 1]) {
      long_data$count[row] <- long_data$count[row - 1] + 1
    }
  }
  
  #The maximum number of arms over all studies
  max_arms <- max(long_data$count)
  
  #Find the outcome type
  if (all(is.element(c("Mean", "SD"), names(long_data)))) {
    outcome_type <- "continuous"
  } else if (is.element("R", names(long_data))) {
    outcome_type <- "binary"
  }
  
  #Create a list whose n-th element contains the data for the n-th arm of each study, when such data exist
  #Rename all columns except Study by appending ".n"
  wide_data_list <- list()
  for (arm in 1:max_arms) {
    wide_data_list[[arm]] <- long_data[long_data$count == arm, -which(names(long_data) == "count")]
    names(wide_data_list[[arm]]) <- paste0(names(wide_data_list[[arm]]), ".", arm)
    names(wide_data_list[[arm]])[names(wide_data_list[[arm]]) == paste0("Study.", arm)] <- "Study"
  }
  
  #Merge the above list elements into one data frame
  wide_data <- wide_data_list[[1]]
  for (arm in 2:max_arms) {
    wide_data <- merge(wide_data, wide_data_list[[arm]], by = "Study", all = TRUE)
  }
  
  return(wide_data)
}
