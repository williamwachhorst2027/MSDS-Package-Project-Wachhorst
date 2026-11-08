#' transfer wr score
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param data Input for the receiver evaluation.
#' @param snap_prior Input for the receiver evaluation.
#' @param target_prior Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
transfer_wr_score <- function (data, snap_prior = 150, target_prior = 30) 
{
    required <- c("REC GRD", "REC SNP", "TGT", "REC", "REC YDS", 
        "REC 1D")
    if (!all(required %in% names(data))) {
        stop("Missing required PFF columns.")
    }
    snaps <- as.numeric(data[["REC SNP"]])
    targets <- as.numeric(data[["TGT"]])
    yards <- as.numeric(data[["REC YDS"]])
    catches <- as.numeric(data[["REC"]])
    first_downs <- as.numeric(data[["REC 1D"]])
    grade <- as.numeric(data[["REC GRD"]])
    if (any(snaps < 0 | targets < 0, na.rm = TRUE)) {
        stop("Snaps and targets cannot be negative.")
    }
    avg_yards_per_snap <- sum(yards, na.rm = TRUE)/sum(snaps, 
        na.rm = TRUE)
    avg_catch_rate <- sum(catches, na.rm = TRUE)/sum(targets, 
        na.rm = TRUE)
    avg_first_down_rate <- sum(first_downs, na.rm = TRUE)/sum(targets, 
        na.rm = TRUE)
    avg_grade <- mean(grade, na.rm = TRUE)
    data$adjusted_yards_per_snap <- (yards + snap_prior * avg_yards_per_snap)/(snaps + 
        snap_prior)
    data$adjusted_catch_rate <- (catches + target_prior * avg_catch_rate)/(targets + 
        target_prior)
    data$adjusted_first_down_rate <- (first_downs + target_prior * 
        avg_first_down_rate)/(targets + target_prior)
    data$adjusted_grade <- (snaps * grade + snap_prior * avg_grade)/(snaps + 
        snap_prior)
    percentile <- function(x) {
        valid <- sum(!is.na(x))
        if (valid <= 1) {
            return(rep(50, length(x)))
        }
        100 * (rank(x, ties.method = "average", na.last = "keep") - 
            1)/(valid - 1)
    }
    data$evaluation_score <- (0.35 * percentile(data$adjusted_grade) + 
        0.3 * percentile(data$adjusted_yards_per_snap) + 0.2 * 
        percentile(data$adjusted_catch_rate) + 0.15 * percentile(data$adjusted_first_down_rate))
    data <- data[order(-data$evaluation_score), ]
    data$evaluation_rank <- seq_len(nrow(data))
    return(data)
}
