likelihoods <- function(data, belief, observation) {
  if(is.null(data)) {
    stop("Dataframe is empty/invalid. Provide a valid dataframe as the first parameter.")
  } else if(TRUE) {
    ifelse(is.na(data),stop("Dataframe is empty/invalid. Provide a valid dataframe as the first parameter."),TRUE)
  } else if(nrow(data) <= 1) {
    stop("Dataframe must have more than one rows.")
  }
  
  if(is.null(belief) | is.na(belief) | trimws(belief) == '') {
    stop('Belief column is not specified.')
  } else if(!(belief %in% colnames(data))) {
    stop(paste('Belief column \'', belief, '\' is not present in the dataframe.', sep=''))
  }
  
  if(is.null(observation) | is.na(observation) | trimws(observation) == '') {
    stop('Observation column is not specified.')
  } else if(!(observation %in% colnames(data))) {
    stop(paste('Observation column \'', observation, '\' is not present in the dataframe.', sep=''))
  }
  
  likelihood_table <- .calc_likelihoods(data, belief, observation)
  likelihood_table
}

.calc_likelihoods <- function(data, belief, observation) {
  
  contingency_table <- table(data[,belief], data[,observation])
  
  belief_values <- sort(unique(data[,belief]))
  observation_values <- sort(unique(data[,observation]))
  
  belief_values_indices <- seq(length(belief_values))
  observation_values_indices <- seq(length(observation_values))
  
  zeroes <- rep(0, times=length(contingency_table))
  likelihood_matrix <- matrix(zeroes, nrow = length(belief_values), ncol = length(observation_values))
  
  for(i in belief_values_indices) {
    for(j in observation_values_indices) {
      true_positive <- contingency_table[i,j] / sum(contingency_table[i,])
      rows_f <- belief_values_indices[-i]
      false_positive <- sum(contingency_table[rows_f,j]) / sum(contingency_table[rows_f,])
      likelihood_ratio <- true_positive/false_positive
      likelihood_matrix[i,j] <- likelihood_ratio
    }
  }
  
  rownames(likelihood_matrix) <- belief_values
  colnames(likelihood_matrix) <- observation_values
  likelihood_table <- as.table(likelihood_matrix)
  
  likelihood_table
}
