#' wr translation report
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param data Input for the receiver evaluation.
#' @param grade Input for the receiver evaluation.
#' @param tolerance Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
wr_translation_report <- function (data, grade, tolerance = 5) 
{
    similar <- data[!is.na(data$g6_grade) & !is.na(data$p4_target_yards) & 
        abs(data$g6_grade - grade) <= tolerance, ]
    if (nrow(similar) == 0) {
        stop("No historical comparisons found.")
    }
    yards <- similar$p4_target_yards
    data.frame(target_grade = grade, comparisons = length(yards), 
        average_p4_yards = round(mean(yards), 1), median_p4_yards = round(median(yards), 
            1), p10_yards = round(as.numeric(quantile(yards, 
            0.1)), 1), p25_yards = round(as.numeric(quantile(yards, 
            0.25)), 1), p75_yards = round(as.numeric(quantile(yards, 
            0.75)), 1), p90_yards = round(as.numeric(quantile(yards, 
            0.9)), 1), pct_500_yards = round(mean(yards >= 500) * 
            100, 1), pct_750_yards = round(mean(yards >= 750) * 
            100, 1), pct_1000_yards = round(mean(yards >= 1000) * 
            100, 1))
}
