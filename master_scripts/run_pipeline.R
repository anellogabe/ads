

# Runs in one background R session; shared .GlobalEnv within this job
source("clean_data.R", local = .GlobalEnv, echo = TRUE)
source("analysis.R",   local = .GlobalEnv, echo = TRUE)