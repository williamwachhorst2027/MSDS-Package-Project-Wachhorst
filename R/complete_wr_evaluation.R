#' complete wr evaluation
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
complete_wr_evaluation <- function (player_name, grade, yards_per_snap = NA, receiving_snaps = NA, 
    data = model_data, tolerance = 5, n_comparisons = 10) 
{
    report <- evaluate_transfer_wr(player_name = player_name, 
        grade = grade, yards_per_snap = yards_per_snap, receiving_snaps = receiving_snaps, 
        data = data, tolerance = tolerance, n_comparisons = n_comparisons)
    if (!is.na(yards_per_snap) && !is.na(receiving_snaps)) {
        cat("\n====================================\n")
        cat("G6 STATISTICAL PEER COMPARISON\n")
        cat("====================================\n")
        peer_comparison <- compare_wr_profile(grade = grade, 
            yards_per_snap = yards_per_snap, receiving_snaps = receiving_snaps, 
            data = data, tolerance = tolerance)
    }
    else {
        peer_comparison <- NULL
    }
    invisible(list(translation = report, peer_comparison = peer_comparison))
}
