#' embed wr report charts
#'
#' @description
#' Historical G6-to-P4 wide receiver
#' statistical evaluation and reporting.
#' @param player_name Input for the receiver evaluation.
#' @return Evaluation results or report files.
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom graphics hist abline legend par barplot text
#' @importFrom stats median quantile
#' @importFrom utils head
embed_wr_report_charts <- function (player_name) 
{
    prefix <- gsub("[^A-Za-z0-9]+", "_", player_name)
    html_file <- paste0(prefix, "_Detailed_Report.html")
    if (!file.exists(html_file)) {
        stop("Detailed HTML report not found.")
    }
    chart_files <- c(paste0(prefix, "_Production.png"), paste0(prefix, 
        "_Percentiles.png"), paste0(prefix, "_Comparisons.png"))
    if (!all(file.exists(chart_files))) {
        stop("One or more chart PNG files are missing.")
    }
    if (!requireNamespace("base64enc", quietly = TRUE)) {
        stop("Please install base64enc before generating reports.")
    }
    chart_titles <- c("Historical P4 Production Distribution", 
        "G6 Statistical Peer Percentiles", "Historical Transfer Outcomes")
    chart_html <- character(length(chart_files))
    for (i in seq_along(chart_files)) {
        encoded <- base64enc::base64encode(chart_files[i])
        chart_html[i] <- paste0("<section style='page-break-inside:avoid;", 
            "margin-top:25px;'>", "<h2>", chart_titles[i], "</h2>", 
            "<img src='data:image/png;base64,", encoded, "' style='width:100%;height:auto;", 
            "max-height:380px;object-fit:contain;'>", "</section>")
    }
    html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
    marker <- "<h2>Methodology and Limitations</h2>"
    if (!grepl(marker, html, fixed = TRUE)) {
        stop("Could not find methodology section.")
    }
    html <- sub(marker, paste0(paste(chart_html, collapse = "\n"), 
        marker), html, fixed = TRUE)
    writeLines(html, html_file)
    cat("\nUpdated detailed report:", html_file, "\n")
    invisible(html_file)
}
