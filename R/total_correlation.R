### total_correlation.R
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


#' @param table_variables A list of factors as categorical variables.
#' @param table_class A factor representing the class of the case.
#' @return Total correlation estimation for the variable set
#'     {\code{table.variables}, \code{table.class}}.
#' @name total_correlation
NULL

# total_correlation ------------------------------------------------------------
#' Estimation of total correlation for a set of categorical random variables.
#'
#' Total Correlation is a generalization of information gain
#' (\code{\link[=information_gain]{IG}}) to measure the dependency of
#' a set of categorical random variables (see 
#' \url{https://en.wikipedia.org/wiki/Total_correlation}).
#' @rdname total_correlation
#' @examples
#' total_correlation(list(factor(c(0,1)), factor(c(1,0))), factor(c(0,0)))
#' total_correlation(list(factor(c('a','b')), factor(c('a','b'))),
#'     factor(c('a','b')))
#' \dontrun{
#' total_correlation(list(factor(c(0,1)), factor(c(1,0))), c(0,0))
#' total_correlation(c(factor(c(0,1)), factor(c(1,0))), c(0,0))
#' }
#' @export
total_correlation <- function(table_variables, table_class) {
  independent_entropy <- msu::H(table_class)
  for (i in 1:length(table_variables)) {
    independent_entropy <- independent_entropy + msu::H(table_variables[[i]])
  }

  dependent_entropy <- msu::multivar_joint_H(table_variables, table_class)
  if (independent_entropy < dependent_entropy) {
    stop('ERROR: total correlation value is incorrect.')
  }

  return (independent_entropy - dependent_entropy)
}
# ------------------------------------------------------------------------------


# C ----------------------------------------------------------------------------
#' @rdname total_correlation
#' @export
C <- total_correlation
# ------------------------------------------------------------------------------
