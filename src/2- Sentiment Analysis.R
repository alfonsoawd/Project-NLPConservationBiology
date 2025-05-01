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
             "rredlist", "sf", "stringi", "syuzhet", "sentimentr")

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
df[, clean_abstract := str_replace_all(clean_abstract, "[[:digit:]]+", " ")]
df[, clean_abstract := str_squish(clean_abstract)]

# Lemmatize abstracts
df[, clean_abstract_lemm := lemmatize_strings(clean_abstract)]

# Tokenize abstracts into sentences
df[, sentences := lapply(clean_abstract, get_sentences)]


# ============================================================
# PART 2 - Output
# ============================================================

# 1) Function to compute the average sentiment of a vector of sentences 
compute_avg_sentiment = function(sentences, method) {
  scores = if (method == "sentimentr") {
    sentiment(sentences)$sentiment
  } else {
    get_sentiment(sentences, method = method)
  }
  mean(scores, na.rm = TRUE)
}

# 2) Define sentiment methods 
sentiment_methods = c("bing", "afinn", "nrc", "syuzhet", "sentimentr")

# 3) Compute raw sentiment scores for each method 
for (m in sentiment_methods) {
  df[, (m) := sapply(sentences, compute_avg_sentiment, method = m)]
}

# 4) Z-score standardize each method’s scores
z_cols = paste0(sentiment_methods, "_z")
df[, (z_cols) := lapply(.SD, function(x) {
  (x - mean(x,   na.rm = TRUE)) / sd(x, na.rm = TRUE)
}), .SDcols = sentiment_methods]

# 5) Build an ensemble by averaging the Z-scores
df[, ensemble_z := rowMeans(.SD, na.rm = TRUE), .SDcols = z_cols]


#--------- EXPORT ---------#

# selecting columns
df_final = df[, .(
  year,
  abstract,
  clean_abstract_lemm,
  genus = Genus,
  order,
  class,
  kingdom,
  species = Species,
  iucn_cat = iucn_category,
  country = country_name,
  bing_z,
  afinn_z,
  nrc_z,
  syuzhet_z,
  sentimentr_z,
  ensemble_z
)]

# export as csv
write.csv(df_final, file = path_save)

