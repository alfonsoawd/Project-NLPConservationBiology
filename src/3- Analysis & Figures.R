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

packages = c("data.table", "rstudioapi", "tokenizers", "cld3", "textclean",
             "stringr", "stopwords", "textstem", "taxize", "httr", "jsonlite",
             "rredlist", "sf", "stringi")

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
path_data = file.path(project_folder, "data", "data.csv")

# Path to world countries Shapefile
path_shp = file.path(project_folder, "data", "World Shapefile", 
                     "world-administrative-boundaries.shp")

# Path to mapping table
path_map = file.path(project_folder, "data", "country_mapping.csv")

# Path to save final output
path_save = file.path(project_folder, "data", "data_with_info.csv")

#-------------------------------------------------
# Step 0.3 Import data
#-------------------------------------------------

# Import abstract data / rename columns
df = fread(path_data, na.strings = c("", "NA"))
df = df[, .(year = YEAR, abstract = WoS_FIELD)]

# Import shapefile data
shp = as.data.table(st_read(path_shp))

# Import mapping table (for variants of country names)
mapping_table = fread(path_map, na.strings = c("", "NA"))


# ============================================================
# PART 1 - Analysis
# ============================================================

# ---- Step 1.1 Sentiment analysis ----
# (Optional further processing)

# ---- Step 1.2 Topic modeling or clustering ----
# (Optional modeling, if relevant)


