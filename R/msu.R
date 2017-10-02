### msu.R
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


# msu --------------------------------------------------------------------------
#' Estimating Multivariate Symmetrical Uncertainty.
#'
#' MSU is a generalization of symmetrical uncertainty
#' (\code{\link[=symmetrical_uncertainty]{SU}}) where it is considered
#' the interaction between two or more variables, whereas SU can only
#' consider the interaction between two variables.  For instance,
#' consider a table with two variables X1 and X2 and a third variable,
#' Y (the class of the case), that results from the logical XOR
#' operator applied to X1 and X2 \tabular{ccc}{ X1 \tab X2 \tab Y\cr 0
#' \tab 0 \tab 0\cr 0 \tab 1 \tab 1\cr 1 \tab 0 \tab 1\cr 1 \tab 1
#' \tab 0\cr } For this case \deqn{MSU(X1, X2, Y) = 0.5.}  This, in
#' contrast to the measurements obtained by SU of the variables X1 and
#' X2 against Y, \deqn{SU(X1, Y) = 0} and \deqn{SU(X2, Y) = 0.}
#'
#' @param table_variables A list of factors as categorical variables.
#' @param table_class A factor representing the class of the case.
#' @return Multivariate symmetrical uncertainty estimation for the
#'     variable set \{\code{table_variables},
#'     \code{table_class}\}. The result is \code{round}ed to 7 decimal
#'     places.
#' @examples
#' # completely predictable
#' msu(list(factor(c(0,0,1,1))), factor(c(0,0,1,1)))
#' # XOR
#' msu(list(factor(c(0,0,1,1)), factor(c(0,1,0,1))), factor(c(0,1,1,0)))
#' \dontrun{
#' msu(c(factor(c(0,0,1,1)), factor(c(0,1,0,1))), factor(c(0,1,1,0)))
#' msu(list(factor(c(0,0,1,1)), factor(c(0,1,0,1))), c(0,1,1,0))
#' }
#' @export
#' @seealso \code{\link{symmetrical_uncertainty}}
msu <- function(table_variables, table_class) {
  n <- length(table_variables) + 1

  independent_entropy <- msu::H(table_class)
  for (i in 1:length(table_variables)) {
    independent_entropy <- independent_entropy + msu::H(table_variables[[i]])
  }

  dependent_entropy <- msu::multivar_joint_H(table_variables, table_class)
  if (round(independent_entropy) < round(dependent_entropy)) {
    stop('ERROR: total correlation value is incorrect.')
  }

  total_correlation <- independent_entropy - dependent_entropy

  msu <- n / (n - 1) * (total_correlation / independent_entropy)
  msu <- ifelse(is.nan(msu), 0, round(msu, digits = 7)) # if C/H(X_i) = 0/0
  if (!(msu >= 0 && msu <= 1)) {
    stop(paste('ERROR: estimated MSU out of range [0,1]. MSU = ', msu))
  }

  return (msu)
}
# ------------------------------------------------------------------------------


#' @param x A factor as the represented categorical variable.
#' @param y A factor as the represented categorical variable.
#' @return Symmetrical uncertainty estimation based on Sannon
#'     entropy. The result is \code{round}ed to 7 decimal places.
#' @name symmetrical_uncertainty
NULL

# symmetrical_uncertainty ------------------------------------------------------
#' Estimating Symmetrical Uncertainty of two categorical variables.
#'
#' Symmetrical uncertainty (SU) is the product of a normalization of
#' the information gain (\code{\link[=information_gain]{IG}}) with
#' respect to entropy. SU(X,Y) is a value in the range [0,1], where
#' \eqn{SU(X,Y) = 0} if X and Y are totally independent and
#' \eqn{SU(X,Y) = 1} if X and Y are totally dependent.
#'
#' @rdname symmetrical_uncertainty
#' @examples
#' # completely predictable
#' symmetrical_uncertainty(factor(c(0,1,0,1)), factor(c(0,1,0,1)))
#' # XOR factor variables
#' symmetrical_uncertainty(factor(c(0,0,1,1)), factor(c(0,1,1,0)))
#' symmetrical_uncertainty(factor(c(0,1,0,1)), factor(c(0,1,1,0)))
#' \dontrun{
#' symmetrical_uncertainty(c(0,1,0,1), c(0,1,1,0))
#' }
#' @export
#' @seealso \code{\link{msu}}
symmetrical_uncertainty <- function(x, y) {
  if (!is.factor(x) || !is.factor(y)) {
    stop("one or both values passed are not of type factor")
  }

  su <- 2 * (msu::IG(x,y) / (msu::H(x) + msu::H(y)))
  su <- ifelse(is.nan(su), 0, round(su, digits = 7))
  if (!(su >= 0 && su <= 1)) {
    stop("ERROR: estimated SU out of range [0,1]. SU = ", su)
  }

  return (su)
}
# ------------------------------------------------------------------------------


# SU ---------------------------------------------------------------------------
#' @rdname symmetrical_uncertainty
#' @export
SU <- symmetrical_uncertainty
# ------------------------------------------------------------------------------
