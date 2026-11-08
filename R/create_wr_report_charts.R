#' create wr report charts
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
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
create_wr_report_charts <- function (player_name, grade, yards_per_snap, receiving_snaps, 
    data = model_data, tolerance = 5) 
{
    prefix <- gsub("[^A-Za-z0-9]+", "_", player_name)
    peers <- data[!is.na(data$g6_grade) & !is.na(data$p4_target_yards) & 
        abs(data$g6_grade - grade) <= tolerance, ]
    if (nrow(peers) == 0) {
        stop("No historical comparisons found.")
    }
    png(paste0(prefix, "_Production.png"), width = 1200, height = 550, 
        res = 140)
    par(mar = c(5, 5, 3, 2))
    hist(peers$p4_target_yards, breaks = "Sturges", col = "#dce7f3", 
        border = "white", main = "Historical P4 Receiving Yards", 
        xlab = "P4 Receiving Yards", ylab = "Number of Transfers")
    abline(v = median(peers$p4_target_yards), col = "#b22222", 
        lwd = 3, lty = 2)
    legend("topright", legend = "Historical Median", col = "#b22222", 
        lty = 2, lwd = 3, bty = "n")
    dev.off()
    peer_profile <- data[!is.na(data$g6_grade) & abs(data$g6_grade - 
        grade) <= tolerance, ]
    efficiency_values <- peer_profile$g6_yards_per_snap
    snap_values <- peer_profile$g6_snaps
    efficiency_values <- efficiency_values[!is.na(efficiency_values)]
    snap_values <- snap_values[!is.na(snap_values)]
    efficiency_pct <- 100 * mean(efficiency_values <= yards_per_snap)
    snaps_pct <- 100 * mean(snap_values <= receiving_snaps)
    png(paste0(prefix, "_Percentiles.png"), width = 1200, height = 450, 
        res = 140)
    par(mar = c(5, 12, 3, 3))
    values <- c(efficiency_pct, snaps_pct)
    bars <- barplot(values, horiz = TRUE, names.arg = c("Yards per Receiving Snap", 
        "Receiving Snaps"), xlim = c(0, 110), col = "#3577b9", 
        border = NA, las = 1, main = "G6 Statistical Peer Percentiles", 
        xlab = "Percentile")
    text(values + 3, bars, labels = paste0(round(values, 1), 
        "%"), pos = 4)
    dev.off()
    comparisons <- find_wr_comparisons(grade = grade, data = data, 
        tolerance = tolerance, n = 10)
    comparisons <- comparisons[order(comparisons$p4_target_yards), 
        ]
    png(paste0(prefix, "_Comparisons.png"), width = 1200, height = 750, 
        res = 140)
    par(mar = c(5, 12, 3, 3))
    barplot(comparisons$p4_target_yards, horiz = TRUE, names.arg = comparisons$player_name, 
        col = "#3577b9", border = NA, las = 1, main = "Closest Historical Transfers", 
        xlab = "P4 Receiving Yards")
    dev.off()
    cat("\nThree charts created successfully:\n")
    cat(paste0(prefix, "_Production.png\n"))
    cat(paste0(prefix, "_Percentiles.png\n"))
    cat(paste0(prefix, "_Comparisons.png\n"))
    invisible(prefix)
}
