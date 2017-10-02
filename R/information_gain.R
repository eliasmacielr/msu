### information_gain.R
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


#' @param x A factor representing a categorical variable.
#' @param y A factor representing a categorical variable.
#' @return Information gain estimation based on Sannon entropy for
#'     variables \code{x} and \code{y}.
#' @name information_gain
NULL

# information_gain -------------------------------------------------------------
#' Estimating information gain between two categorical variables.
#'
#' Information gain (also called mutual information) is a measure of
#' the mutual dependence between two variables (see
#' \url{https://en.wikipedia.org/wiki/Mutual_information}).
#'
#' @rdname information_gain
#' @examples
#' information_gain(factor(c(0,1)), factor(c(1,0)))
#' information_gain(factor(c(0,0,1,1)), factor(c(0,1,1,1)))
#' information_gain(factor(c(0,0,1,1)), factor(c(0,1,0,1)))
#' \dontrun{
#' information_gain(c(0,1), c(1,0))
#' }
#' @export
information_gain <- function(x, y) {
  if (!is.factor(x) || !is.factor(y)) {
    stop("one or both values passed are not of type factor")
  }

  h_row <- entropy::entropy.plugin(rowSums(table(x, y)), unit = "log2")
  h_col <- entropy::entropy.plugin(colSums(table(x, y)), unit = "log2")
  h_tab <- entropy::entropy.plugin(table(x, y), unit = "log2")

  return (h_row + h_col - h_tab)
}
# ------------------------------------------------------------------------------


# IG ---------------------------------------------------------------------------
#' @rdname information_gain
#' @export
IG <- information_gain
# ------------------------------------------------------------------------------
