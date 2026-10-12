#' Calculate Yards Per Reception
#'
#' Calculates the average receiving yards gained per reception.
#'
#' @param yards Total receiving yards.
#' @param receptions Total number of receptions.
#' @return Yards per reception.
#' @export
#'
#' @examples
#' yards_per_reception(850, 50)

yards_per_reception <- function(yards, receptions) {
  if (receptions <= 0) {
    stop("Receptions must be greater than zero.")
  }
  
  yards / receptions
}