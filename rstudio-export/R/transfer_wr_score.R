# Preliminary Transfer WR Evaluation Score
# Higher scores indicate stronger receiving production
# relative to other receivers in the dataset.

#' Calculate Transfer Wide Receiver Evaluation Score
#'
#' @param data Data frame containing PFF receiving statistics.
#' @return Original data with evaluation scores and rankings.
#' @export
transfer_wr_score <- function(data) {
  
  # Check required columns
  required <- c("REC GRD", "REC SNP", "TGT",
                "REC", "REC YDS", "REC 1D")
  
  if (!all(required %in% names(data))) {
    stop("Missing required PFF columns.")
  }
  
  # Calculate efficiency statistics
  data$catch_rate <- ifelse(
    data$TGT > 0, data$REC / data$TGT, NA_real_
  )
  
  data$yards_per_snap <- ifelse(
    data$`REC SNP` > 0,
    data$`REC YDS` / data$`REC SNP`,
    NA_real_
  )
  
  data$first_down_rate <- ifelse(
    data$TGT > 0,
    data$`REC 1D` / data$TGT,
    NA_real_
  )
  
  # Percentile ranking function
  percentile <- function(x) {
    100 * (rank(x, ties.method = "average",
                na.last = "keep") - 1) /
      (sum(!is.na(x)) - 1)
  }
  
  # Weighted evaluation score
  data$evaluation_score <- (
    0.35 * percentile(data$`REC GRD`) +
      0.30 * percentile(data$yards_per_snap) +
      0.20 * percentile(data$catch_rate) +
      0.15 * percentile(data$first_down_rate)
  )
  
  # Rank players
  data <- data[order(-data$evaluation_score), ]
  
  data$evaluation_rank <- seq_len(nrow(data))
  
  return(data)
}

