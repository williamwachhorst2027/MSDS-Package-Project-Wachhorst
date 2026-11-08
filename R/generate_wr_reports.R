#' generate wr reports
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param player_name Input for the receiver evaluation.
#' @param grade Input for the receiver evaluation.
#' @param yards_per_snap Input for the receiver evaluation.
#' @param receiving_snaps Input for the receiver evaluation.
#' @param data Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
generate_wr_reports <- function (player_name, grade, yards_per_snap, receiving_snaps, 
    data = model_data) 
{
    create_wr_html_reports(player_name = player_name, grade = grade, 
        yards_per_snap = yards_per_snap, receiving_snaps = receiving_snaps, 
        data = data)
    create_wr_report_charts(player_name = player_name, grade = grade, 
        yards_per_snap = yards_per_snap, receiving_snaps = receiving_snaps, 
        data = data)
    embed_wr_report_charts(player_name = player_name)
    cat("\n====================================\n")
    cat("WR EVALUATION COMPLETE\n")
    cat("====================================\n")
    cat("Player:", player_name, "\n")
    cat("Scouting card and detailed report ready.\n")
    invisible(TRUE)
}
