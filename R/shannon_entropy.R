### shannon_entropy.R
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


#' @param x A factor as the represented categorical variable.
#' @return Shannon entropy estimation of the categorical variable.
#' @name shannon_entropy
NULL

# shannon_entropy --------------------------------------------------------------
#' Estimation of Shannon entropy for a categorical variable.
#'
#' The Shannon entropy estimates the average minimum number of bits
#' needed to encode a string of symbols, based on the frequency of the
#' symbols (see
#' \url{http://www.bearcave.com/misl/misl_tech/wavelets/compression/shannon.html}).
#' @rdname shannon_entropy
#' @examples
#' shannon_entropy(factor(c(1,0)))
#' shannon_entropy(factor(c('a','b','c')))
#' \dontrun{
#' shannon_entropy(1)
#' shannon_entropy(c('a','b','c'))
#' }
#' @export
shannon_entropy <- function(x) {
  if (!is.factor(x)) {
    stop("value passed is not of type factor")
  }

  return (entropy::entropy.plugin(table(x), unit = "log2"))
}
# ------------------------------------------------------------------------------


# H ----------------------------------------------------------------------------
#' @rdname shannon_entropy
#' @export
H <- shannon_entropy
# ------------------------------------------------------------------------------


#' @param x A factor as the represented categorical variable.
#' @param y A factor as the represented categorical variable.
#' @return Joint Shannon entropy estimation for variables \code{x} and \code{y}.
#' @name joint_shannon_entropy
NULL

# joint_shannon_entropy --------------------------------------------------------
#' Estimation of the Joint Shannon entropy for two categorical variables.
#'
#' The joint Shannon entropy provides an estimation of the measure of
#' uncertainty between two random variables (see 
#' \url{https://en.wikipedia.org/wiki/Joint_entropy}).
#' @rdname joint_shannon_entropy
#' @examples
#' joint_shannon_entropy(factor(c(0,0,1,1)), factor(c(0,1,0,1)))
#' joint_shannon_entropy(factor(c('a','b','c')), factor(c('c','b','a')))
#' \dontrun{
#' joint_shannon_entropy(1)
#' joint_shannon_entropy(c('a','b'), c('d','e'))
#' }
#' @export
#' @seealso \code{\link{shannon_entropy}} for the entropy for a
#'     single variable and
#'     \code{\link{multivar_joint_shannon_entropy}} for the entropy
#'     associated with more than two random variables.
joint_shannon_entropy <- function(x, y) {
  if (!is.factor(x) || !is.factor(y)) {
    stop("one or both values passed are not of type factor")
  }

  return (entropy::entropy.plugin(table(x, y), unit = "log2"))
}
# ------------------------------------------------------------------------------


# joint_H ----------------------------------------------------------------------
#' @rdname joint_shannon_entropy
#' @export
joint_H <- joint_shannon_entropy
# ------------------------------------------------------------------------------


#' @param table_variables A list of factors as categorical variables.
#' @param table_class A factor representing the class of the case.
#' @return Joint Shannon entropy estimation for the variable set
#'     {\code{table.variables}, \code{table.class}}.
#' @name multivar_joint_shannon_entropy
NULL

# multivar_joint_shannon_entropy -----------------------------------------------
#' Estimation of joint Shannon entropy for a set of categorical variables.
#'
#' The multivariate joint Shannon entropy provides an estimation of
#' the measure of the uncertainty associated with a set of variables (see 
#' \url{https://en.wikipedia.org/wiki/Joint_entropy}).
#'
#' @rdname multivar_joint_shannon_entropy
#' @examples
#' multivar_joint_shannon_entropy(list(factor(c(0,1)), factor(c(1,0))),
#'     factor(c(1,1)))
#' @export
#' @seealso \code{\link{shannon_entropy}} for the entropy for a
#'     single variable and
#'     \code{\link{joint_shannon_entropy}} for the entropy
#'     associated with two random variables.
multivar_joint_shannon_entropy <- function(table_variables, table_class) {
  return (entropy::entropy.plugin(table(data.frame(table_variables,
                                                   table_class)),
                                  unit = 'log2'))
}
# ------------------------------------------------------------------------------


# multivar_joint_H -------------------------------------------------------------
#' @rdname multivar_joint_shannon_entropy
#' @export
multivar_joint_H <- multivar_joint_shannon_entropy
# ------------------------------------------------------------------------------
