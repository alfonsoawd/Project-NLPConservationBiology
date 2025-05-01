#----------------------------------- INFO -------------------------------------#
# Descr: Analysis and figures
# Author: Alfonso AWADALLA
# Date: April 2025
#------------------------------------------------------------------------------#


# ============================================================
# PART 0 - SETUP & IMPORT DATA
# ============================================================

#-------------------------------------------------
# Step 0.1 Load required libraries
#-------------------------------------------------

packages = c("data.table", "tokenizers", "cld3", "textclean",
             "stringr", "stopwords", "textstem", "httr", "jsonlite",
             "stringi", "syuzhet", "sentimentr")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#-------------------------------------------------
# Step 0.2 Define working directory and paths
#-------------------------------------------------

# Setting location of the project folder
current_folder = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_folder)
project_folder = dirname(getwd())

# Path to abstract data
path_data = file.path(project_folder, "data", "data_with_info.csv")

# Path to save final output
path_save = file.path(project_folder, "data", "data_with_score.csv")

#-------------------------------------------------
# Step 0.3 Import data
#-------------------------------------------------

# Import data with informations
df = fread(path_data, na.strings = c("", "NA"))


# ============================================================
# PART 1 - Analysis
# ============================================================

# ---- Step 1.1 Sentiment analysis ----
# (Optional further processing)

# ---- Step 1.2 Topic modeling or clustering ----
# (Optional modeling, if relevant)


