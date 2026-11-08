html_table <- function (df) 
{
    headers <- paste0("<th>", html_escape(names(df)), "</th>", 
        collapse = "")
    rows <- apply(df, 1, function(row) {
        paste0("<tr>", paste0("<td>", html_escape(row), "</td>", 
            collapse = ""), "</tr>")
    })
    paste0("<table><thead><tr>", headers, "</tr></thead><tbody>", 
        paste(rows, collapse = "\n"), "</tbody></table>")
}
