#----------------------------------- INFO -------------------------------------#
# Descr: Extracting informations from the text (taxa, redlist, countries)
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
path_data = file.path(project_folder, "data", "1- data.csv")

# Path to world countries Shapefile
path_shp = file.path(project_folder, "data", "World Shapefile", 
                     "world-administrative-boundaries.shp")

# Path to mapping table
path_map = file.path(project_folder, "data", "country_mapping.csv")

# Path to save final output
path_save = file.path(project_folder, "data", "2- data_with_info.csv")

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
# PART 1 – TAXONOMIC INFORMATION EXTRACTION
# ============================================================

#-------------------------------------------------
# Step 1.1  Extract taxa (species, genus, family)
#-------------------------------------------------

# Define function to scrape species names from the text
get_first_species <- function(text) {
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

# Vectorized assignment of first species per abstract
df[, taxa := vapply(abstract, get_first_species, FUN.VALUE = character(1))]

#-------------------------------------------------
# Step 1.2  Retrieve higher taxonomic categories
#-------------------------------------------------

# Set NCBI API key
Sys.setenv(ENTREZ_KEY = "ReplaceWithYourKeyAPI")

# Get genus, order, class, kingdom for each taxa
df[, c("genus","order","class","kingdom") := {
  ti = taxize::tax_name(taxa,
                        get = c("genus","order","class","kingdom"),
                        db  = "ncbi")
  list(ti$genus, ti$order, ti$class, ti$kingdom)
}]

# Deal with missing taxonomy by retrying on genus-level token
df_miss = df[(!is.na(taxa)) & is.na(genus)]
df_miss[, taxa := word(taxa, 1, 1)]
df_miss[, c("genus","order","class","kingdom") := {
  ti = taxize::tax_name(taxa,
                        get = c("genus","order","class","kingdom"),
                        db  = "ncbi")
  list(ti$genus, ti$order, ti$class, ti$kingdom)
}]

# Remerge corrected rows back into main df
df[df_miss,
   c("genus","order","class","kingdom") := .(i.genus, i.order, i.class, i.kingdom),
   on = "V1"]


# ============================================================
# PART 2 – IUCN RED LIST CATEGORIES EXTRACTION
# ============================================================

#-------------------------------------------------
# Step 2.1  Setup and preprocess Genus + Species
#-------------------------------------------------

# Set IUCN API key
Sys.setenv(IUCN_REDLIST_KEY = "ReplaceWithYourKeyAPI")

# Keep only two-word taxa (Genus species)
df[, taxa := word(taxa, 1, 2)]

# Split into separate Genus and Species columns
df[, c("Genus","Species") := tstrsplit(taxa, " ", fixed = TRUE)]

#-------------------------------------------------
# Step 2.2  Fetch latest IUCN assessment
#-------------------------------------------------

# Define function to retrieve IUCN info on redlists
get_iucn_latest = function(genus, species) {
  # safely call IUCN API
  out = tryCatch(
    rl_species_latest(genus   = genus,
                      species = species,
                      parse   = TRUE),
    error = function(e) NULL
  )
  if (is.null(out)) return(list(NA_integer_, NA_character_))
  # return SIS‐ID and Red List code
  list(out$taxon$sis_id, out$red_list_category$code)
}

# Apply by genus+species
df[, c("iucn_id","iucn_category") := 
     get_iucn_latest(genus, Species),
   by = .(genus, Species)]

#-------------------------------------------------
# Step 2.3  Recode codes to full descriptions
#-------------------------------------------------

# Lookup vector of full category descriptions
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

# Replace 2-letter codes with full descriptions
df[, iucn_category := desc[iucn_category]]


# ============================================================
# PART 3 – GEOGRAPHIC INFORMATION EXTRACTION
# ============================================================

#-------------------------------------------------
# Step 3.1  Extract primary country from abstract
#-------------------------------------------------

# Set AI chat API key
api_key = "ReplaceWithYourKeyAPI"

# Define function to prompt AI on country name
ask_main_country_zhipu = function(abstract_text) {
  # handle empty input
  if (is.na(abstract_text) || abstract_text == "") return(NA_character_)
  # build prompt for primary country
  prompt_text = paste0(
    "You are reading an academic abstract on species reintroduction.",
    " Identify the primary study location or focal species location.",
    " Return exactly ONE country or territory name (official English).",
    " If multiple, choose the most central; if none, return 'None'.",
    " Do not add any extra text.\nAbstract:\n", abstract_text
  )
  body_list = list(
    model       = "glm-3-turbo",
    messages    = list(list(role = "user", content = prompt_text)),
    temperature = 0
  )
  # call chat API
  res = RETRY(
    "POST",
    url    = "https://open.bigmodel.cn/api/paas/v4/chat/completions",
    body   = body_list,
    encode = "json",
    add_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    config(connecttimeout = 60),
    timeout(120),
    times     = 3,
    pause_base = 1
  )
  if (http_error(res)) return(NA_character_)
  # parse response and clean text
  j = fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  country_raw = j$choices[[1]]$message$content
  country_clean = trimws(gsub("[^A-Za-z ]+", "", country_raw))
  if (tolower(country_clean) %in% c("", "none")) NA_character_ else country_clean
}

# Vectorized country extraction
df[, country := vapply(abstract, ask_main_country_zhipu, FUN.VALUE = character(1))]

#-------------------------------------------------
# Step 3.2  Normalize and link to shapefile names
#-------------------------------------------------

# 1) Merge on variant → official names
df = merge(df, mapping_table,
           by.x    = "country", by.y = "variant_name",
           all.x   = TRUE,      sort = FALSE)
df[, country := fifelse(!is.na(official_name), official_name, country)]
df[, official_name := NULL]

# 2) Normalizer strips accents, punctuation; lowercases
normalize_name = function(x) {
  x = stri_trans_general(x, "Latin-ASCII")
  x = tolower(x)
  x = gsub("[^a-z0-9 ]+", "", x)
  x = gsub("\\s+", " ", x)
  trimws(x)
}

# Apply normalization to both tables
df[,  country_norm := normalize_name(country)]
shp[, name_norm    := normalize_name(name)]

# 3) Left‐join shape data on normalized name
df = merge(df, shp[, .(name_norm, name)],
           by.x  = "country_norm", by.y = "name_norm",
           all.x = TRUE,         sort = FALSE)
setnames(df, "name", "country_name")

# 4) Clean up helper columns
df[, c("country_norm","country") := NULL]


#--------- EXPORT ---------#
write.csv(df, file = path_save)

