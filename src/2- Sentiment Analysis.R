#----------------------------------- INFO -------------------------------------#
# Descr: Performing sentiment analysis on abstracts
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
# PART 1 - Text Preparation
# ============================================================

#-------------------------------------------------
# Step 1.1 Language detection and filtering
#-------------------------------------------------

df[, abstract := vapply(abstract, function(text) {
  # split into sentences
  sentences = unlist(tokenize_sentences(text), use.names = FALSE)
  # detect each sentence’s language
  langs = detect_language(sentences)
  # keep everything except Spanish, French or German
  kept_sents = sentences[!(langs %in% c("es", "fr", "de"))]
  # re-paste into paragraphs
  paste(kept_sents, collapse = " ")
}, FUN.VALUE = character(1L))]

#-------------------------------------------------
# Step 1.2 Cleaning 
#-------------------------------------------------

# Number of abstract per year
df[, .(num_abstracts = .N), by = year][order(year)]
# We keep only year >= 1987
df = df[year>=1987]
# Create a cleaned version of each abstract
df[, clean_abstract := abstract]
df[, clean_abstract := replace_url(replace_html(clean_abstract))]
df[, clean_abstract := tolower(clean_abstract)]
df[, clean_abstract := str_replace_all(clean_abstract, "[[:punct:]]+", " ")]
df[, clean_abstract := str_replace_all(clean_abstract, "[[:digit:]]+", " ")]
df[, clean_abstract := str_squish(clean_abstract)]
# Lemmatize abstracts
df[, clean_abstract_lemm := lemmatize_strings(clean_abstract)]


#-------------------------------------------------
# Step 1.3 Create corpus
#-------------------------------------------------

# (tm::VCorpus with DataframeSource)



# ============================================================
# PART 2 - Output
# ============================================================

# ---- Step 5.1 Save cleaned abstracts ----
# (Insert export code, e.g., write.csv)

# ---- Step 5.2 Save extracted information ----
# (Insert saving code for taxa/countries)

# ---- Step 5.3 (Optional) Save corpus or DTM ----
# (Save objects if needed for later analyses)


