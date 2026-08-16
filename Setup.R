print("Installing Files")

required_packages <- c("tidyverse", "readxl", "openxlsx", "janitor", "ggrounded")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]


# Install only the missing ones
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

print("Importing Files")

print("Setup Done!")

