scripts <- c(
  "GP - Overall care.R",
  "GP - Ease of contacting.R",
  "GP - Informed choice.R",
  "Out of Hours - treatment.R"
)

for (script in scripts) {
  
  message("Running: ", script)
  
  tryCatch(
    {
      source(script)
      message("Completed: ", script)
    },
    error = function(e) {
      message("FAILED: ", script)
      stop(e)
    }
  )
}

message("All scripts completed.")