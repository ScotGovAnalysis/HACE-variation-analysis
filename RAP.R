#==============================================================================
## RAP Script ##
#==============================================================================

### To be run once annual updates are completed ###
## If annual updates script is not required please specify survey_year in 
## utilities script before running the RAP 

## Use a # to temporarily block any scripts yo don't want to run 
scripts <- c(
  "1. Utility.R",
  "GP - Overall care.R",
  "GP - Ease of contacting.R",
  "GP - Informed choice.R",
  "Out of Hours - treatment.R",
  "OOH - Overall care.R",
  "Help Care Support - Overall care.R"
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
