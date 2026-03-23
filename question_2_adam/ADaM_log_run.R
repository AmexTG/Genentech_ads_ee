#--------------------------------------------------
# open log
#--------------------------------------------------
sink("adsl_domain_log.log", split = TRUE)

msg_con <- file("adsl_domain_log_messages.log", open = "wt")
sink(msg_con, type = "message")

cat("ADaM_adsl RUN START:", format(Sys.time()), "\n\n")

#--------------------------------------------------
# run script
#--------------------------------------------------
source(
  "create_adsl.R",
  echo = TRUE,
  max.deparse.length = Inf
)

#--------------------------------------------------
# summary
#--------------------------------------------------
cat("\n")
cat("Rows in ADaM_adsl:", nrow(ds), "\n")
cat("Columns in ADaM_adsl:", ncol(ds), "\n")

cat("\nADaM_adsl END:", format(Sys.time()), "\n")

#--------------------------------------------------
# close logs (IMPORTANT)
#--------------------------------------------------
sink(type = "message")
close(msg_con)
sink()

