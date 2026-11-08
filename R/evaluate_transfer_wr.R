#' evaluate transfer wr
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param player_name Input for the receiver evaluation.
#' @param grade Input for the receiver evaluation.
#' @param yards_per_snap Input for the receiver evaluation.
#' @param receiving_snaps Input for the receiver evaluation.
#' @param data Input for the receiver evaluation.
#' @param tolerance Input for the receiver evaluation.
#' @param n_comparisons Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
evaluate_transfer_wr <- function (player_name, grade, yards_per_snap = NA, receiving_snaps = NA, 
    data = model_data, tolerance = 5, n_comparisons = 10) 
{
    report <- wr_translation_report(data = data, grade = grade, 
        tolerance = tolerance)
    comparisons <- find_wr_comparisons(grade = grade, data = data, 
        tolerance = tolerance, n = n_comparisons)
    cat("\n====================================\n")
    cat("G6 TO P4 WR TRANSLATION REPORT\n")
    cat("====================================\n")
    cat("Player:", player_name, "\n")
    cat("G6 Receiving Grade:", grade, "\n")
    if (!is.na(yards_per_snap)) {
        cat("G6 Yards Per Snap:", yards_per_snap, "\n")
    }
    if (!is.na(receiving_snaps)) {
        cat("G6 Receiving Snaps:", receiving_snaps, "\n")
    }
    cat("\nHISTORICAL P4 PRODUCTION\n")
    cat("Comparisons:", report$comparisons, "\n")
    cat("Average Yards:", report$average_p4_yards, "\n")
    cat("Median Yards:", report$median_p4_yards, "\n")
    cat("Middle 50%:", report$p25_yards, "-", report$p75_yards, 
        "yards\n")
    cat("10th-90th Percentile:", report$p10_yards, "-", report$p90_yards, 
        "yards\n\n")
    cat("HISTORICAL SUCCESS RATES\n")
    cat("500+ Yards:", report$pct_500_yards, "%\n")
    cat("750+ Yards:", report$pct_750_yards, "%\n")
    cat("1000+ Yards:", report$pct_1000_yards, "%\n\n")
    cat("CLOSEST HISTORICAL COMPARISONS\n")
    print(comparisons, row.names = FALSE)
    cat("\nNote: Historical benchmarks, not forecasts.\n")
    invisible(list(summary = report, comparisons = comparisons))
}
