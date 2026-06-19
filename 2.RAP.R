# HACE RAP

######################## IMPORTANT ########################
## Run script "1. Utility.R" before running the full RAP ##
###########################################################

# Once you have run the script "1. Utility.R" next Chose the scripts you would 
# like to run. To remove any scripts type a # (or ctrl shift c) in front of their name

RunScripts <- c(
  # "3. Data processing.R",
  "4. GP Urgent access within 2 days.R"
  # "5. GP - Care rated as Excellent or Good.R",
  # "6. GP – informed choice.R",
  # "7. A&E GP Out of Hours – treatment.R"
)

# Run all scripts - this will run them in the order of the above list
lapply(RunScripts, source)

