# ---- Response Time Summary by Complaint Type ----
summarize_complaint_response <- function(
    DT, 
    min_records = 1L, 
    print_top = 15L
) {
  stopifnot(data.table::is.data.table(DT))
  
  stats_dt <- DT[
    !is.na(complaint_type),
    {
      vals <- duration_days[!is.na(duration_days)]
      
      if (length(vals) == 0) {
        .(
          N      = .N,
          Min    = as.numeric(NA),
          Q1     = as.numeric(NA),
          Median = as.numeric(NA),
          Mean   = as.numeric(NA),
          Q3     = as.numeric(NA),
          Max    = as.numeric(NA)
        )
      } else {
        .(
          N      = .N,
          Min    = round(min(vals), 2),
          Q1     = round(stats::quantile(vals, 0.25), 2),
          Median = round(stats::median(vals), 2),
          Mean   = round(mean(vals), 2),
          Q3     = round(stats::quantile(vals, 0.75), 2),
          Max    = round(max(vals), 2)
        )
      }
    },
    by = complaint_type
  ][N >= min_records][order(Median, complaint_type)]   # <-- ascending
  
  # ---- Console output ----
  cat("\n=== Complaint Type Response Time Summary ===\n")
  cat(sprintf("Total complaint types: %d (N >= %d)\n\n",
              nrow(stats_dt), min_records))
  cat("Sorted by Median duration (ascending):\n\n")
  
  to_print <- as.data.frame(stats_dt)[seq_len(min(print_top, nrow(stats_dt))), ]
  row.names(to_print) <- NULL   # cleaner console output
  print(to_print, row.names = FALSE)
  
  invisible(stats_dt)
}
