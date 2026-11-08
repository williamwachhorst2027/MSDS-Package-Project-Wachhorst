#' create wr html reports
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
create_wr_html_reports <- function (player_name, grade, yards_per_snap, receiving_snaps, 
    data = model_data) 
{
    summary <- wr_translation_report(data, grade)
    comparisons <- find_wr_comparisons(grade, data, n = 10)
    peers <- compare_wr_profile(grade, yards_per_snap, receiving_snaps, 
        data)
    comparison_columns <- c("player_name", "transfer_year", "g6_team", 
        "p4_team", "g6_grade", "p4_target_yards")
    short_comps <- head(comparisons[, comparison_columns], 5)
    full_comps <- comparisons[, comparison_columns]
    names(short_comps) <- c("Player", "Year", "G6", "P4", "Grade", 
        "P4 Yards")
    names(full_comps) <- names(short_comps)
    names(peers) <- c("Metric", "Prospect", "Peer Average", "Percentile")
    css <- paste0("<style>", "*{box-sizing:border-box}", "body{font-family:Arial,sans-serif;", 
        "color:#17263b;background:#f1f4f8;", "margin:0;padding:24px}", 
        ".page{max-width:950px;margin:auto;", "background:white;padding:35px;", 
        "border-radius:12px}", "h1{color:#15365c;margin-bottom:5px}", 
        "h2{color:#15365c;font-size:19px;", "border-bottom:2px solid #dce5ef;", 
        "padding-bottom:8px;margin-top:28px}", ".subtitle{color:#64748b;margin-bottom:25px}", 
        ".stats{display:flex;gap:12px;flex-wrap:wrap}", ".stat{flex:1;min-width:130px;", 
        "background:#eef3f9;padding:15px;", "border-radius:8px}", 
        ".stat strong{display:block;font-size:25px;", "color:#15365c}", 
        ".stat span{font-size:12px;color:#64748b}", "table{width:100%;border-collapse:collapse;", 
        "font-size:13px;margin-top:12px}", "th{background:#15365c;color:white;", 
        "text-align:left;padding:10px}", "td{padding:9px;border-bottom:1px solid #ddd}", 
        "tr:nth-child(even){background:#f5f7fa}", ".note{font-size:12px;color:#64748b;", 
        "margin-top:20px;line-height:1.6}", ".bar{height:15px;background:#dce5ef;", 
        "border-radius:8px;overflow:hidden}", ".fill{height:100%;background:#3577b9}", 
        "@media print{", "body{background:white;padding:0}", 
        ".page{padding:15px;box-shadow:none}", "h2{break-after:avoid}", 
        "table{break-inside:avoid}", "}", "</style>")
    stat_box <- function(value, label) {
        paste0("<div class='stat'><strong>", html_escape(value), 
            "</strong><span>", html_escape(label), "</span></div>")
    }
    header <- paste0("<h1>G6 TO P4 | WR EVALUATION</h1>", "<div class='subtitle'>", 
        html_escape(player_name), " | Historical Statistical Translation</div>", 
        "<div class='stats'>", stat_box(grade, "G6 Receiving Grade"), 
        stat_box(yards_per_snap, "G6 Yards / Snap"), stat_box(receiving_snaps, 
            "Receiving Snaps"), stat_box(summary$comparisons, 
            "Historical Comparisons"), "</div>")
    production <- paste0("<h2>Historical P4 Production</h2>", 
        "<div class='stats'>", stat_box(summary$average_p4_yards, 
            "Average Receiving Yards"), stat_box(summary$median_p4_yards, 
            "Median Receiving Yards"), stat_box(paste0(summary$p25_yards, 
            "-", summary$p75_yards), "Middle 50% Range"), "</div>", 
        "<h2>Historical Production Benchmarks</h2>", "<div class='stats'>", 
        stat_box(paste0(summary$pct_500_yards, "%"), "500+ Yard Seasons"), 
        stat_box(paste0(summary$pct_750_yards, "%"), "750+ Yard Seasons"), 
        stat_box(paste0(summary$pct_1000_yards, "%"), "1,000+ Yard Seasons"), 
        "</div>")
    note <- paste0("<p class='note'>", "Based on historical G6-to-P4 transfers ", 
        "within five PFF receiving-grade points. ", "These figures describe observed historical ", 
        "outcomes, not validated individual forecasts. ", "Receiving production is influenced by ", 
        "opportunity, offensive environment, ", "quarterback play, and player role. ", 
        "Film evaluation remains essential.", "</p>")
    card <- paste0("<html><head><meta charset='UTF-8'>", css, 
        "</head><body>", "<div class='page'>", header, production, 
        "<h2>Five Closest Historical Transfers</h2>", html_table(short_comps), 
        note, "</div></body></html>")
    detailed <- paste0("<html><head><meta charset='UTF-8'>", 
        css, "</head><body>", "<div class='page'>", header, production, 
        "<h2>Historical Production Distribution</h2>", "<p>10th percentile: ", 
        summary$p10_yards, " yards</p>", "<p>25th percentile: ", 
        summary$p25_yards, " yards</p>", "<p>Median: ", summary$median_p4_yards, 
        " yards</p>", "<p>75th percentile: ", summary$p75_yards, 
        " yards</p>", "<p>90th percentile: ", summary$p90_yards, 
        " yards</p>", "<h2>G6 Statistical Peer Comparison</h2>", 
        html_table(peers), "<h2>Ten Closest Historical Transfers</h2>", 
        html_table(full_comps), "<h2>Methodology and Limitations</h2>", 
        note, "</div></body></html>")
    prefix <- gsub("[^A-Za-z0-9]+", "_", player_name)
    card_file <- paste0(prefix, "_Scouting_Card.html")
    detailed_file <- paste0(prefix, "_Detailed_Report.html")
    writeLines(card, card_file)
    writeLines(detailed, detailed_file)
    cat("\nReports created successfully:\n")
    cat(card_file, "\n")
    cat(detailed_file, "\n")
    invisible(list(card = card_file, detailed = detailed_file))
}
