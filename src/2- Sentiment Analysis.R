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
path_save = file.path(project_folder, "data", "data_with_info.csv")

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
df[, clean_abstract := str_replace_all(clean_abstract, "[[:punct:]]+", " ")]
df[, clean_abstract := str_replace_all(clean_abstract, "[[:digit:]]+", " ")]
df[, clean_abstract := str_squish(clean_abstract)]

# Lemmatize abstracts
df[, clean_abstract_lemm := lemmatize_strings(clean_abstract)]


# ============================================================
# PART 2 - Output
# ============================================================

# ── 1) Compute raw scores ──
df[, sentences := lapply(clean_abstract_lemm, get_sentences)]

methods <- c("bing","afinn","nrc","syuzhet","sentimentr")

# helper for syuzhet methods
get_score <- function(s, method) mean(get_sentiment(s, method=method), na.rm=TRUE)

df[, (methods) := lapply(methods, function(m) {
  if(m!="sentimentr")
    sapply(sentences, get_score, method=m)
  else
    sapply(sentences, function(s) mean(sentiment(s)$sentiment, na.rm=TRUE))
})]

# ── 2) Z-score standardization ──
zcols <- paste0(methods, "_z")
df[, (zcols) := lapply(.SD, function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)),
   .SDcols = methods]

# ── 3) Ensemble on Z-scores ──
df[, ensemble_z := rowMeans(.SD, na.rm=TRUE), .SDcols = zcols]

