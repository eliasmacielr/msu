### utils.R
###
### Copyright 2017 Gustavo Sosa
###
###
### This file is part of the `msu' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# rel_freq ------------------------------------------------------------------
#' Relative frequency of values of a categorical variable.
#'
#' @param variable A factor as a categorical variable
#' @return Relative frecuency distribution table for the values in
#'     \code{variable}.
#' @examples
#' rel_freq(factor(c(0,1)))
#' rel_freq(factor(c('a','a','b')))
#' \dontrun{
#' rel_freq(c(0,1))
#' }
#' @export
rel_freq <- function(variable) {
  if (!is.factor(variable)) {
    stop("value passed is not of type factor")
  }

  table <- table(variable)

  return ((table) / margin.table(table))
}
# ------------------------------------------------------------------------------


# sample_size ------------------------------------------------------------------
#' Estimate the sample size for a categorical variable.
#' 
#' @param max A number as the maximum value of the possible
#'     categories.
#' @param min A number as the minimum value of the possible
#'     categories.
#' @param z A number as the confidence coefficient.
#' @param error Admissible sampling error.
#' @return The sample size for a categorical variable based on a
#'     variance heuristic approximation.
#' @export
sample_size <- function(max, min = 1, z = 1.96, error = 0.05) {
  return (round(z^2 * ((max - min) / 4)^2 / error^2))
}
# ------------------------------------------------------------------------------


# categorical_sample_size ------------------------------------------------------
#' Estimate the sample size for a variable in function of its categories.
#' 
#' @param categories A vector containing the number of categories of
#'     each variable.
#' @param increment A number as a constant to which the sample size is
#'     incremented as a product.
#' @return The sample size for a categorical variable based on a
#'     ordered permutation heuristic approximation of its categories.
#' @export
categorical_sample_size <- function(categories, increment = 10) {
  return (increment * prod(categories))
}
# ------------------------------------------------------------------------------


# new_variable -----------------------------------------------------------------
#' Create a uniform categorical random variable.
#'
#' The sampling for the items of the created variable is done with replacement.
#' 
#' @param elements A vector with the elements from which to choose to
#'     create the variable.
#' @param n An integer indicating the number of items to be contained
#'     in the variable.
#' @return A factor that represents a uniform categorical variable.
#' @examples
#' new_variable(c(0,1), 4)
#' new_variable(c('a','b','c'), 10)
#' @export
new_variable <- function(elements, n) {
  if (length(elements) <= 1) {
    return (factor(elements))
  } else {
    return (factor(sample(elements, n, TRUE), levels = elements))
  }
}
# ------------------------------------------------------------------------------

# new_informative_variable -----------------------------------------------------
#' Create an informative uniform categorical random variable.
#'
#' The sampling for the items of the created variable is done with replacement.
#' 
#' @param variable_labels A factor as the labels for the new
#'     informative variable.
#' @param variable_class A factor as the class of the variable.
#' @param information_level A integer as the information level of the
#'     new variable.
#' @return A factor that represents an informative uniform categorical
#'     random variable created using the Kononenko method.
#' @export
new_informative_variable <- function(variable_labels, 
                                     variable_class, 
                                     information_level = 1) {
  c <- length(levels(variable_class))
  k <- information_level

  total_var_labels <- length(variable_labels)
  half_var_labels  <- total_var_labels %/% 2

  subset1 <- variable_labels[1:half_var_labels]
  subset2 <- variable_labels[(half_var_labels + 1):total_var_labels]

  cond_prob_vector <- vector(mode = "numeric", length = c)
  for(i in 1:c) {
    if (i %% 2 == 0)
      cond_prob_vector[i] <-     1 / (i + k * c)
    else
      cond_prob_vector[i] <- 1 - 1 / (i + k * c)
  }

  var_informative_vector <- as.character(variable_class)
  class_labels_vector    <- as.character(levels(variable_class))

  for (j in 1:length(variable_class)) {
    prob <- 0

    for(k in 1:c) {
      if (var_informative_vector[j] == class_labels_vector[k]) {
        prob <- cond_prob_vector[k]
        break
      }
    }

    if (stats::runif(1) <= prob) {
      if (length(subset1) == 1) {
        var_informative_vector[j] <- subset1
      } else {
        var_informative_vector[j] <- sample(subset1, 1, TRUE)
      }
    } else {
      if (length(subset2) == 1) {
        var_informative_vector[j] <- subset2
      } else {
        var_informative_vector[j] <- sample(subset2, 1, TRUE)
      }
    }
  }

  return (factor(var_informative_vector, levels = variable_labels))
}
# ------------------------------------------------------------------------------


# new_xor_variables ------------------------------------------------------------
#' Create a set of categorical variables using the logical XOR operator.
#' 
#' @param n_variables An integer as the number of variables to be
#'     created. It is the number of column variables of the table, an
#'     additional column is added as a result of the XOR operator over
#'     the instances.
#' @param n_instances An integer as the number of instances to be
#'     created. It is the number of rows of the table.
#' @param noise A float number as the noise level for the variables.
#' @return A set of random variables constructed using the logical XOR
#'     operator.
#' @examples
#' new_xor_variables(2, 4, 0)
#' new_xor_variables(5, 10, 0.5)
#' @export
new_xor_variables <- function(n_variables = 2,
                              n_instances = 1000,
                              noise = 0) {
  variable_matrix <- matrix(nrow = n_instances, 
                            ncol = n_variables + 1, 
                            byrow = TRUE)

  for (c in 1:n_variables) {
    variable_matrix[, c] <- sample(0:1, n_instances, TRUE)
  }

  variable_matrix[, (n_variables + 1)] <- 
    ifelse(rowSums(variable_matrix[, 1:n_variables]) == 1, 1, 0)

  variable_matrix[, (n_variables + 1)] <- 
    ifelse(stats::runif(n_instances) <= noise,
           1 - variable_matrix[, (n_variables + 1)],
           variable_matrix[, (n_variables + 1)])

  variable_group <- as.data.frame(matrix(nrow = n_instances, 
                                         ncol = (n_variables + 1), 
                                         byrow = TRUE))

  for(c in 1:(n_variables + 1)) {
    variable_group[[c]] <- factor(variable_matrix[, c], levels = c(0, 1))
  }

  colnames(variable_group)[(n_variables + 1)] <- "Y"

  return (variable_group)
}
# ------------------------------------------------------------------------------
