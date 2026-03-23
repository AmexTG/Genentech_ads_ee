#--------------------------------------------------
# open log
#--------------------------------------------------
sink("ds_domain_log.log", split = TRUE)

msg_con <- file("ds_domain_log_messages.log", open = "wt")
sink(msg_con, type = "message")

cat("sdtm_ds RUN START:", format(Sys.time()), "\n\n")

#--------------------------------------------------
# run script
#--------------------------------------------------
source(
  "01_create_ds_domain.R",
  echo = TRUE,
  max.deparse.length = Inf
)

#--------------------------------------------------
# summary
#--------------------------------------------------
cat("\n")
cat("Rows in sdtm_ds:", nrow(ds), "\n")
cat("Columns in sdtm_ds:", ncol(ds), "\n")

cat("\nsdtm_ds END:", format(Sys.time()), "\n")

#--------------------------------------------------
# close logs (IMPORTANT)
#--------------------------------------------------
sink(type = "message")
close(msg_con)
sink()

