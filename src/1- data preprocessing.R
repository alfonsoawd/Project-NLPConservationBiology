#----------------------------------- INFO -------------------------------------#
# Descr: Text Mining Scientific Abstracts
# Author: Alfonso AWADALLA
# Date: April 2025
#------------------------------------------------------------------------------#


# ============================================================
# PART 1 - Setup & Import
# ============================================================

#-------------------------------------------------
# Step 1.1 Load required libraries
#-------------------------------------------------

packages = c("data.table", "rstudioapi", "tokenizers", "cld3", "textclean",
             "stringr", "stopwords", "textstem", "taxize", "httr", "jsonlite",
             "rredlist")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#-------------------------------------------------
# Step 1.2 Define working directory and paths
#-------------------------------------------------

current_folder = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_folder)
project_folder = dirname(getwd())
path_data = file.path(project_folder, "data", "data.csv")

#-------------------------------------------------
# Step 1.3 Import data
#-------------------------------------------------

df = fread(path_data)
df = df[, .(year = YEAR, abstract = WoS_FIELD)]


# ============================================================
# PART 2 - Information Extraction
# ============================================================

#-------------------------------------------------
# Step 2.1.1 Extract taxa (species, genus, family)
#-------------------------------------------------

# Define a function that takes one abstract and returns a species name
get_first_species <- function(text) {
  # Scrape species names from the text
  dt = as.data.table(
    taxize::scrapenames(
      text            = text,
      unique_names    = FALSE,
      ambiguous_names = FALSE,
      no_bayes        = TRUE,
      verification    = TRUE,
      all_matches     = FALSE,
      odds_details    = TRUE
    ))
  # filter by exact match + multi‐word (species level)
  species = dt[VerifMatchType == "Exact" & Cardinality >= 2, Name]
  if (length(species) > 0) {
    species[1]
  } else NA_character_
}

# Vectorized assignment
df[, taxa := vapply(abstract, get_first_species, FUN.VALUE = character(1))]

#-------------------------------------------------
# Step 2.1.2 Retrieve higher taxonomic categories
#-------------------------------------------------

# Replace the string below with your actual API key (for "ncbi" use)
Sys.setenv(ENTREZ_KEY = "ReplaceWithYourKeyAPI")

#------ General code ------#

df[, c("genus","order","class","kingdom") := {
  ti = taxize::tax_name(taxa,
                 get = c("genus","order","class","kingdom"),
                 db  = "ncbi")
  list(ti$genus, ti$order, ti$class, ti$kingdom)}]

#------ Dealing with missings ------#

# Select species that were not retrieved
df_miss = df[(!is.na(df[,taxa])) & is.na(df[,genus])]
# Extract first part of the multiwords taxa
df_miss[, taxa := word(taxa, 1, 1)]
# Rerun code for previously missing rows
df_miss[, c("genus","order","class","kingdom") := {
  ti = taxize::tax_name(taxa,
                 get = c("genus","order","class","kingdom"),
                 db  = "ncbi")
  list(ti$genus, ti$order, ti$class, ti$kingdom)}]

#------ Remerging everything  ------#

df[df_miss,
   c("genus","order","class","kingdom") := .(i.genus, i.order, i.class, i.kingdom),
   on = "V1"]


#-------------------------------------------------
# Step 2.2 Extract IUCN Red List Categories
#-------------------------------------------------

## 2.2.1 Set your IUCN API key (replace with your own)
Sys.setenv(IUCN_REDLIST_KEY = "ReplaceWithYourKeyAPI")

## 2.2.2 Keep only the first two words of ‘taxa’ and split into Genus + Species
df[, taxa := word(taxa, 1, 2)]  
df[, c("Genus","Species") := tstrsplit(taxa, " ", fixed = TRUE)]

## 2.2.3 Define function to fetch the latest IUCN assessment safely
get_iucn_latest = function(genus, species) {
  out = tryCatch(
    rl_species_latest(genus   = genus,
                      species = species,
                      parse   = TRUE),
    error = function(e) NULL
  )
  if (is.null(out)) {
    # no assessment found or API error → return NAs
    return(list(NA_integer_, NA_character_))
  }
  # extract SIS‐ID and Red List category code
  list(out$taxon$sis_id,
       out$red_list_category$code)
}

## 2.2.4 Apply function by Genus+Species to add iucn_id + iucn_category
df[, c("iucn_id","iucn_category") := 
       get_iucn_latest(genus, Species),
     by = .(genus, Species)]

## 2.2.5 Build lookup vector of full category descriptions
desc = c(
  EX = "Extinct – no reasonable doubt that the last individual has died.",
  EW = "Extinct in the Wild – survives only in cultivation or in captivity.",
  CR = "Critically Endangered – facing an extremely high risk of extinction in the wild.",
  EN = "Endangered – facing a very high risk of extinction in the wild.",
  VU = "Vulnerable – facing a high risk of extinction in the wild.",
  NT = "Near Threatened – close to qualifying for a threatened category in the near future.",
  LC = "Least Concern – evaluated and found to be at low risk of extinction.",
  DD = "Data Deficient – insufficient information to assess its risk of extinction.",
  NE = "Not Evaluated – has not yet been assessed against the IUCN criteria."
)

## 2.2.6 Recode the 2-letter codes to full descriptions in-place
df[, iucn_category := desc[iucn_category]]


#-------------------------------------------------
# Step 2.3.1 Extract Countries 
#-------------------------------------------------

# APIfrom the chatbox Zhipu AI
api_key <- "ReplaceWithYourKeyAPI"

# Function that asks chatbox to give a country based on abstract
ask_main_country_zhipu <- function(abstract_text) {
  if (is.na(abstract_text) || abstract_text == "") {
    return(NA_character_)
  }
  
  prompt_text <- paste0(
    "You are reading an academic abstract on species reintroduction.",
    "Identify the primary study location or the location of the focal species.",
    "Return exactly ONE country or non-sovereign territory name in English, using its official name.",
    "If multiple countries are mentioned, choose the one most central to the study.",
    "If no clear country is indicated, return exactly 'None'.",
    "Do not include any other text or comment.",
    "Abstract:\n", abstract_text
  )
  
  # Let httr serialize for us
  body_list = list(
    model       = "glm-3-turbo",
    messages    = list(list(role = "user", content = prompt_text)),
    temperature = 0
  )
  
  res = RETRY(
    "POST",
    url    = "https://open.bigmodel.cn/api/paas/v4/chat/completions",
    body   = body_list,
    encode = "json",
    add_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    config(connecttimeout = 60),  # up to 60s to resolve/connect
    timeout(120),                 # up to 120s total
    times     = 3,
    pause_base = 1
  )
  
  if (http_error(res)) {
    warning("API request failed: HTTP ", status_code(res))
    return(NA_character_)
  }
  
  raw_text = content(res, "text", encoding = "UTF-8")
  j        = fromJSON(raw_text, simplifyVector = FALSE)
  
  # --- robust choice-extraction ---
  if (!is.null(j$choices) && length(j$choices) >= 1) {
    choice1 <- j$choices[[1]]
    if (is.list(choice1) && !is.null(choice1$message$content)) {
      country_raw <- choice1$message$content
    } else if (is.character(choice1)) {
      country_raw <- choice1
    } else {
      warning("Unexpected response format; could not find message/content")
      return(NA_character_)
    }
  } else {
    warning("No choices returned by API")
    return(NA_character_)
  }
  
  # Clean up: drop non-letters, trim whitespace
  country_clean = trimws(gsub("[^A-Za-z ]+", "", country_raw))
  if (tolower(country_clean) %in% c("", "none")) {
    return(NA_character_)
  }
  country_clean
  print(country_clean)
}

# Usage
df[, country := vapply(abstract,
                       ask_main_country_zhipu,
                       FUN.VALUE = character(1))]


#-------------------------------------------------
# Step 2.3.2 Link countries to shapefile names 
#-------------------------------------------------



# ============================================================
# PART 3 - Text Preparation
# ============================================================

#-------------------------------------------------
# Step 3.1 Language detection and filtering
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
# Step 3.2 Cleaning 
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
# Step 3.3 Create corpus
#-------------------------------------------------

# (tm::VCorpus with DataframeSource)



# ============================================================
# PART 4 - Output
# ============================================================

# ---- Step 5.1 Save cleaned abstracts ----
# (Insert export code, e.g., write.csv)

# ---- Step 5.2 Save extracted information ----
# (Insert saving code for taxa/countries)

# ---- Step 5.3 (Optional) Save corpus or DTM ----
# (Save objects if needed for later analyses)


# ============================================================
# PART 5 - (Optional) Further Analyses
# ============================================================

# ---- Step 6.1 Sentiment analysis ----
# (Optional further processing)

# ---- Step 6.2 Topic modeling or clustering ----
# (Optional modeling, if relevant)

