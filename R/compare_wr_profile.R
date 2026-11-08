#' compare wr profile
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param grade Input for the receiver evaluation.
#' @param yards_per_snap Input for the receiver evaluation.
#' @param receiving_snaps Input for the receiver evaluation.
#' @param data Input for the receiver evaluation.
#' @param tolerance Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
compare_wr_profile <- function (grade, yards_per_snap, receiving_snaps, data = model_data, 
    tolerance = 5) 
{
    peers <- data[!is.na(data$g6_grade) & abs(data$g6_grade - 
        grade) <= tolerance, ]
    if (nrow(peers) == 0) {
        stop("No historical peers found.")
    }
    metrics <- data.frame(metric = c("Yards per Receiving Snap", 
        "Receiving Snaps"), prospect = c(yards_per_snap, receiving_snaps), 
        peer_average = c(mean(peers$g6_yards_per_snap, na.rm = TRUE), 
            mean(peers$g6_snaps, na.rm = TRUE)), percentile = c(mean(peers$g6_yards_per_snap <= 
            yards_per_snap, na.rm = TRUE) * 100, mean(peers$g6_snaps <= 
            receiving_snaps, na.rm = TRUE) * 100))
    metrics$prospect <- round(metrics$prospect, 2)
    metrics$peer_average <- round(metrics$peer_average, 2)
    metrics$percentile <- round(metrics$percentile, 1)
    print(metrics, row.names = FALSE)
    invisible(metrics)
}
