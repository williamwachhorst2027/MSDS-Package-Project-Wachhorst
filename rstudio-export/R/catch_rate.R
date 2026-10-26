#' Calculate Catch Rate
#'
#' Calculates the percentage of targets caught by a receiver.
#'
#' @param receptions Total number of receptions.
#' @param targets Total number of targets.
#' @return Catch rate as a percentage.
#' @export
#'
#' @examples
#' catch_rate(50, 75)

catch_rate <- function(receptions, targets) {
  if (targets <= 0) {
    stop("Targets must be greater than zero.")
  }
  
  if (receptions < 0 || receptions > targets) {
    stop("Receptions must be between zero and targets.")
  }
  
  (receptions / targets) * 100
}