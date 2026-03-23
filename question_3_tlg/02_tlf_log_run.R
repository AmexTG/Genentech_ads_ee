#--------------------------------------------------
# open log
#--------------------------------------------------
sink("02_cr_vis_log.log", split = TRUE)

msg_con <- file("02_cr_vis.log", open = "wt")
sink(msg_con, type = "message")

cat("tlf_02 RUN START:", format(Sys.time()), "\n\n")

#--------------------------------------------------
# run script
#--------------------------------------------------
source(
  "02_create_visualizations.R",
  echo = TRUE,
  max.deparse.length = Inf
)

#--------------------------------------------------
# summary
#--------------------------------------------------
cat("\n")
cat("Rows in tlf_02:", nrow(ds), "\n")
cat("Columns in tlf_02:", ncol(ds), "\n")

cat("\ntlf_02 END:", format(Sys.time()), "\n")

#--------------------------------------------------
# close logs (IMPORTANT)
#--------------------------------------------------
sink(type = "message")
close(msg_con)
sink()

