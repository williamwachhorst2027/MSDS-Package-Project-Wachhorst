#' find wr comparisons
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param grade Input for the receiver evaluation.
#' @param data Input for the receiver evaluation.
#' @param tolerance Input for the receiver evaluation.
#' @param n Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
find_wr_comparisons <- function (grade, data = model_data, tolerance = 5, n = 10) 
{
    similar <- data[!is.na(data$g6_grade) & !is.na(data$p4_target_yards) & 
        abs(data$g6_grade - grade) <= tolerance, ]
    similar$grade_difference <- abs(similar$g6_grade - grade)
    similar <- similar[order(similar$grade_difference), ]
    comparisons <- head(similar[, c("player_name", "transfer_year", 
        "g6_team", "p4_team", "g6_grade", "g6_yards_per_snap", 
        "p4_target_yards", "grade_difference")], n)
    rownames(comparisons) <- NULL
    return(comparisons)
}
