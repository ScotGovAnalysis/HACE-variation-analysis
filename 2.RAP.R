# HACE RAP

### IMPORTANT ###
## Run script "1. Utility.R" before running the full RAP ##

# Run the rest of the scripts 
RunScripts <- c(
  "3. Data processing.R",
  "GP Urgent access within 2 days.R",
  "GP - Care rated as Excellent or Good.R",
  "GP – informed choice.R",
  "A&E GP Out of Hours – treatment.R"
)

# Run all scripts - this will run them in the order of the above list
lapply(RunScripts, source)

