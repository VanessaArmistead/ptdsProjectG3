#' @title Retrieving the best wine in the World
#'
#' @description
#' @param B A \code{numeric} (integer) used to denote the number of sample
#' coordinates used to approximate pi
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{estimated_pi}{Estimated value of pi }
#'      \item{points}{Dataframe of the generated coordinates with an indication
#'      whether or not the coordinate is inside the circle}
#' }
#' @author Vanessa Armistead
#' @author Bart Roes
#' @author Luisa Pricken
#' @author Ameni Rouatbi
#' @importFrom stats runif
#' @export
#' @examples

