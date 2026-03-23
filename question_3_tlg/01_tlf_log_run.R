#--------------------------------------------------
# open log
#--------------------------------------------------
sink("01_ae_table_log.log", split = TRUE)

msg_con <- file("01_ae_table.log", open = "wt")
sink(msg_con, type = "message")

cat("tlf_01 RUN START:", format(Sys.time()), "\n\n")

#--------------------------------------------------
# run script
#--------------------------------------------------
source(
  "01_create_ae_summary_table.R",
  echo = TRUE,
  max.deparse.length = Inf
)

#--------------------------------------------------
# summary
#--------------------------------------------------
cat("\n")
cat("Rows in tlf_01:", nrow(ds), "\n")
cat("Columns in tlf_01:", ncol(ds), "\n")

cat("\ntlf_01 END:", format(Sys.time()), "\n")

#--------------------------------------------------
# close logs (IMPORTANT)
#--------------------------------------------------
sink(type = "message")
close(msg_con)
sink()

