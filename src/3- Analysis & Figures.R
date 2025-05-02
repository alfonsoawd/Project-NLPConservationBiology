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
             "stringi", "ggplot2", "dplyr", "viridis", "tidytext",
             "forcats", "SnowballC", "patchwork", "ggimage", "sf", "ggh4x")

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
path_data = file.path(project_folder, "data", "data_with_score.csv")

# Path to world countries Shapefile
path_shp = file.path(project_folder, "data", "World Shapefile", 
                     "world-administrative-boundaries.shp")

# Path to figures folder
path_fig = file.path(project_folder, "report", "Figures")

#-------------------------------------------------
# Step 0.3 Import data
#-------------------------------------------------

# Import data with informations
df = fread(path_data, na.strings = c("", "NA"))

# Import shapefile data
shp = st_read(path_shp) %>%
  summarise(
    "country" = name,
    geometry
  )

# ============================================================
# PART 1 - Figures
# ============================================================

#-------------------------------------------------
# Figure 1: sample attrition
#-------------------------------------------------

# Step 1.1 - compute & reshape counts
df_counts <- df %>%
  summarise(
    "Total abstracts"                = n(),
    "Country"                        = sum(!is.na(country)),
    "Taxonomy"                       = sum(!is.na(order)),
    "Taxonomy & Country"             = sum(!is.na(order) & !is.na(country)),
    "IUCN category"                  = sum(!is.na(iucn_cat)),
    "IUCN category & country"       = sum(!is.na(iucn_cat) & !is.na(country))
  ) %>%
  pivot_longer(everything(), names_to = "group", values_to = "count") %>%
  arrange(count) %>%
  mutate(group = fct_inorder(group))

# Step 1.2 - Making the plot
ggplot(df_counts, aes(x = count, y = group)) +
  # stems
  geom_segment(aes(x = 0, xend = count, y = group, yend = group),
               color = "grey90", size = 1) +
  # gradient dots
  geom_point(aes(color = count), size = 7) +
  scale_color_viridis(option = "plasma", direction = -1, guide = FALSE) +
  # labels just past the dot
  geom_text(aes(label = count, x = count + max(count) * 0.02),
            hjust = -0.5, family = "Helvetica", size = 4, color = "#444444") +
  # titles
  labs(
    title    = "Data Availability Across Abstracts",
    subtitle = "How many abstracts contain each variable (or combinations)",
    x        = "Number of Abstracts",
    y        = NULL,
  ) +
  # remove all gridlines, keep subtle axis lines
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.major    = element_blank(),
    panel.grid.minor    = element_blank(),
    axis.line.x         = element_line(color = "grey80"),
    axis.text.y         = element_text(size = 12, face = "bold"),
    axis.text.x         = element_text(size = 10),
    plot.title          = element_text(size = 16, face = "bold", margin = margin(b = 4)),
    plot.subtitle       = element_text(size = 12, margin = margin(b = 10)),
    plot.caption        = element_text(size = 8, hjust = 1, margin = margin(t = 10))
  ) +
  # ensure labels fit
  expand_limits(x = max(df_counts$count) * 1.1)

# Export to PNG
ggsave(
  filename = file.path(path_fig, "lollipop_chart.png"),
  plot     = last_plot(),        # or plot = p
  device   = "png",
  width    = 7,                  # in inches
  height   = 6,
  dpi      = 300
)

#-------------------------------------------------
# Figure 2: Words contribution to scores
#-------------------------------------------------

# 1) Lemmatize AFINN and collapse duplicates
af <- get_sentiments("afinn") %>%
  mutate(lemma = lemmatize_strings(word)) %>%
  group_by(lemma) %>%
  summarize(value = mean(value), .groups = "drop")

# 2) Tokenize, drop digits/short tokens, then lemmatize
tk <- df %>%
  unnest_tokens(word, abstract) %>%
  filter(!str_detect(word, "\\d"), str_length(word) > 2) %>%
  mutate(lemma = lemmatize_strings(word)) %>%
  anti_join(stop_words, by = c("lemma" = "word"))

# 3) Join to lexicon, count occurrences, compute weighted contributions
wc <- tk %>%
  inner_join(af, by = "lemma") %>%
  count(lemma, value, name = "n") %>%
  mutate(
    contrib    = n * value,
    sentiment  = ifelse(contrib > 0, "positive", "negative"),
    plot_val   = ifelse(sentiment=="negative", -abs(contrib), abs(contrib))
  )

# 4) Keep top 15 positives & negatives by absolute contribution
pd <- wc %>%
  group_by(sentiment) %>%
  slice_max(order_by = abs(contrib), n = 15, with_ties = FALSE) %>%
  ungroup()

# 5) Build ordering: negatives (most negative at top), then positives (small→large so largest at top)
neg_lv <- pd %>% filter(sentiment=="negative") %>% arrange(plot_val) %>% pull(lemma)
pos_lv <- pd %>% filter(sentiment=="positive") %>% arrange(plot_val) %>% pull(lemma)
pd <- pd %>% mutate(lemma = factor(lemma, levels = c(neg_lv, pos_lv)))

# 6) Mirror‐bar plot
ggplot(pd, aes(x = plot_val, y = lemma, fill = sentiment)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, color = "grey80", size = 0.8) +
  scale_fill_manual(values = c(negative = "#C24A4A", positive = "#5F94C3")) +
  scale_x_continuous(labels = abs, expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title    = "Top Positive vs. Negative Contributors",
    subtitle = "Mirror‐bar of the 15 words with largest (count × AFINN-score)",
    x        = "Weighted Contribution (count × score)",
    y        = NULL, fill = "Sentiment"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid      = element_blank(),
    axis.line.x     = element_line(color = "grey80"),
    axis.text.y     = element_text(size = 10, face = "bold"),
    axis.text.x     = element_text(size = 9),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    legend.text     = element_text(size = 9),
    plot.title      = element_text(size = 15, face = "bold", margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 10, color = "grey40", margin = margin(b = 8))
  )

# 7) Save to PNG
ggsave(
  filename = file.path(path_fig, "weighted_contribution.png"),
  width    = 6, height = 8, dpi = 300
)

#-------------------------------------------------
# Figure 3: Mean score per year
#-------------------------------------------------

# 1) Gather the five model z‐scores into long form
annual_model <- df %>%
  pivot_longer(
    cols      = c(bing_z, afinn_z, nrc_z, syuzhet_z, sentimentr_z),
    names_to  = "model",
    values_to = "z"
  )

# 2) Compute each model’s mean z per year
annual_means <- annual_model %>%
  group_by(model, year) %>%
  summarise(annual_z = mean(z, na.rm = TRUE), .groups = "drop")

# 3) Ensemble mean ± sd
annual_ens <- annual_means %>%
  group_by(year) %>%
  summarise(
    ensemble_z = mean(annual_z),
    sd_z       = sd(annual_z),
    .groups    = "drop"
  )

# 4) Annual counts
annual_n <- df %>% count(year, name = "n")

# 5) Top panel: sentiment trend + ribbon
p1 <- ggplot(annual_ens, aes(year, ensemble_z, group = 1)) +
  geom_ribbon(aes(ymin = ensemble_z - sd_z,
                  ymax = ensemble_z + sd_z),
              fill  = "#80B1D3", alpha = 0.4) +
  geom_line(colour = "#1F78B4", size = 1.3) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "#AAAAAA") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(min(df$year), max(df$year), by = 5)
  ) +
  labs(y = "Sentiment score") +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    text              = element_text(color = "#333333"),
    panel.grid        = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.line         = element_line(colour = "#333333", size = 0.5),
    axis.title.y      = element_text(face = "bold", size = 15),
    axis.text.y       = element_text(size = 12)
  )

# 6) Bottom panel: bar chart of abstracts per year
p2 <- ggplot(annual_n, aes(year, n)) +
  geom_col(fill = "#66C2A5", width = 1) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(min(df$year), max(df$year), by = 5)
  ) +
  labs(x = "Publication year", y = "Number of abstracts") +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    text            = element_text(color = "#333333"),
    panel.grid      = element_blank(),
    axis.line       = element_line(colour = "#333333", size = 0.5),
    axis.title      = element_text(face = "bold", size = 15),
    axis.text       = element_text(size = 12)
  )

# 7) Stack with patchwork (3× height for top)
final_plot <- (p1 / p2) +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "white", colour = NA)))

# 8) Render & save
print(final_plot)
ggsave(
  file.path(path_fig, "fig_sentiment_and_n.png"),
  final_plot, width = 8, height = 6, dpi = 300
)

#-------------------------------------------------
# Figure 3: Count of IUCN categories
#-------------------------------------------------

# 1) Map the full IUCN descriptions to short names and two‐letter codes
iucn_map <- tibble(
  full = c(
    "Least Concern – evaluated and found to be at low risk of extinction.",
    "Near Threatened – close to qualifying for a threatened category in the near future.",
    "Vulnerable – facing a high risk of extinction in the wild.",
    "Endangered – facing a very high risk of extinction in the wild.",
    "Critically Endangered – facing an extremely high risk of extinction in the wild.",
    "Extinct in the Wild – survives only in cultivation or in captivity.",
    "Extinct – no reasonable doubt that the last individual has died.",
    "Data Deficient – insufficient information to assess its risk of extinction."
  ),
  short = c(
    "Least Concern", "Near Threatened", "Vulnerable",
    "Endangered", "Critically Endangered",
    "Extinct in the Wild", "Extinct", "Data Deficient"
  ),
  code = c("LC","NT","VU","EN","CR","EW","EX","DD")
)

# 2) Count & compute % (dropping NA)
df_iucn <- df %>%
  filter(!is.na(iucn_cat)) %>%
  inner_join(iucn_map, by = c("iucn_cat" = "full")) %>%
  count(short, code, name = "n") %>%
  mutate(
    pct = n / sum(n) * 100,
    short = factor(short, levels = iucn_map$short)  # preserve IUCN order
  )

# 3) Define a palette (add a color for DD)
palette <- c(
  "Least Concern"         = "#1b9e77",
  "Near Threatened"       = "#a6d854",
  "Vulnerable"            = "#ffd92f",
  "Endangered"            = "#fc8d62",
  "Critically Endangered" = "#e31a1c",
  "Extinct in the Wild"   = "#b2abd2",
  "Extinct"               = "#8073ac",
  "Data Deficient"        = "#66c2a5"
)

# 4) Plot
ggplot(df_iucn, aes(x = short, y = pct, fill = short)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", code, ")")),
            vjust = -0.4, size = 4, family = "Helvetica", color = "#333333") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL,
    y = "% articles"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    panel.grid   = element_blank(),          # no grid
    axis.text.x  = element_text(
      angle = 45, hjust = 1, face = "bold",
      color = "#333333"
    ),
    axis.text.y  = element_text(face = "bold", color = "#333333"),
    axis.title.y = element_text(face = "bold", color = "#333333"),
    plot.margin  = margin(10, 10, 10, 10)
  )

# 5) Save to PNG
ggsave(
  filename = file.path(path_fig, "iucn_cat.png"),
  width    = 6, height = 7, dpi = 300
)

#-------------------------------------------------
# Figure 3: Orders cited count & IUCN composition
#-------------------------------------------------

# 1) Map IUCN categories to short labels & colors (dropping Data Deficient)
iucn_map <- tibble(
  full = c(
    "Least Concern – evaluated and found to be at low risk of extinction.",
    "Near Threatened – close to qualifying for a threatened category in the near future.",
    "Vulnerable – facing a high risk of extinction in the wild.",
    "Endangered – facing a very high risk of extinction in the wild.",
    "Critically Endangered – facing an extremely high risk of extinction in the wild.",
    "Extinct in the Wild – survives only in cultivation or in captivity.",
    "Extinct – no reasonable doubt that the last individual has died."
  ),
  short = c(
    "Least Concern", "Near Threatened", "Vulnerable",
    "Endangered", "Critically Endangered",
    "Extinct in the Wild", "Extinct"
  )
)

iucn_cols <- c(
  "Least Concern"         = "#1b9e77",
  "Near Threatened"       = "#a6d854",
  "Vulnerable"            = "#ffd92f",
  "Endangered"            = "#fc8d62",
  "Critically Endangered" = "#e31a1c",
  "Extinct in the Wild"   = "#b2abd2",
  "Extinct"               = "#8073ac"
)

# 2) Join, drop NA & Data Deficient
df2 <- df %>%
  filter(!is.na(iucn_cat)) %>%
  inner_join(iucn_map, by = c("iucn_cat" = "full"))

# 3) Find top 20 orders by unique mentions
order_counts <- df2 %>%
  count(order, name = "n_unique")

top20 <- order_counts %>%
  slice_max(n_unique, n = 20) %>%
  pull(order)

df_top <- df2 %>% filter(order %in% top20)

# 4) Recompute counts & proportions for those 20
order_counts_top <- df_top %>%
  count(order, name = "n_unique") %>%
  arrange(desc(n_unique)) %>%
  mutate(
    order = factor(order, levels = order),
    img   = file.path(path_fig, "Orders", paste0(order, ".png"))
  )

order_iucn_top <- df_top %>%
  count(order, short, name = "n") %>%
  group_by(order) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(order = factor(order, levels = levels(order_counts_top$order)))

# 5) Compute vertical offset for images (5% above highest bar)
max_n <- max(order_counts_top$n_unique)
img_offset <- max_n * 0.12   # 12% of max bar height

# 6) Panel A: top 20 orders with gradient fill + images
pA <- ggplot(order_counts_top,
             aes(x = order, y = n_unique, fill = n_unique)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  # extend y‐axis by 20% so there's room for images + labels
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  # draw images a fixed offset above each bar‐top
  geom_image(aes(image = img),
             size    = 0.06,
             by      = "width",
             nudge_y = img_offset) +
  # labels just below the images
  geom_text(aes(label = n_unique),
            vjust = -1.2,    # pulls the text up nearer the top
            size  = 3.5,
            family = "Helvetica") +
  labs(y = "Number of articles") +
  coord_cartesian(clip = "off") +    # allow drawing outside
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    axis.title.x    = element_blank(),
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.title.y    = element_text(face = "bold"),
    axis.text.y     = element_text(face = "bold"),
    plot.margin     = margin(20, 20, 20, 20)  # extra room around
  )

# 7) Panel B: stacked proportions
pB <- ggplot(order_iucn_top,
             aes(x = order, y = prop, fill = short)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = iucn_cols, name = "Red List category") +
  labs(x = "Order", y = "Proportion of mentions") +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 45,
                                   hjust = 1,
                                   face = "bold",
                                   color = "#333333"),
    axis.title      = element_text(face = "bold"),
    legend.position = "right"
  )

# 8) Combine panels (no tags), shared x-axis on bottom
final_plot <- pA / pB +
  plot_layout(heights = c(2, 1)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

# 9) Render & save
print(final_plot)
ggsave(file.path(path_fig, "fig_orders_iucn_top20.png"),
       final_plot, width = 12, height = 8, dpi = 300)


#-------------------------------------------------
# Figure 3: World map
#-------------------------------------------------

# 1. Join df to world shapefile and compute proportion threatened
df_map <- df %>%
  left_join(shp, by = "country") %>%
  st_as_sf()

threatened_cats <- c(
  "Vulnerable – facing a high risk of extinction in the wild.",
  "Endangered – facing a very high risk of extinction in the wild.",
  "Critically Endangered – facing an extremely high risk of extinction in the wild.",
  "Extinct in the Wild – survives only in cultivation or in captivity."
)

country_summary <- df_map %>%
  filter(!is.na(iucn_cat)) %>%
  group_by(country, geometry) %>%
  summarise(
    n_total     = n_distinct(species),
    n_threat    = n_distinct(species[iucn_cat %in% threatened_cats]),
    prop_threat = n_threat / n_total,
    .groups     = "drop"
  )

# 2. Merge proportion back onto full shapefile
shp2 <- shp %>%
  left_join(st_drop_geometry(country_summary), by = "country")

# 3. Plot map with continuous gradient (YlGnBu)
ggplot(shp2) +
  geom_sf(aes(fill = prop_threat), colour = "white", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#FFFFCC", "#FED976", "#FD8D3C", "#E31A1C", "#800026"),
    limits   = c(0, 1),
    na.value = "grey90",
    name     = "Proportion of species mentioned\ncategorised as threatened, by country",
    labels   = scales::percent_format(accuracy = 1),
    guide    = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(size = 12, hjust = 0.5),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(2, "cm"),
    plot.margin      = margin(10, 10, 10, 10)
  )

# 4) Save to PNG
ggsave(
  filename = file.path(path_fig, "world_map_threatened.png"),
  width    = 10, height = 7, dpi = 300
)

#-------------------------------------------------
# Figure 3: World map per category IUCN
#-------------------------------------------------

# 1. Join df to world shapefile and compute proportion threatened
df_map <- df %>%
  left_join(shp, by = "country") %>%
  st_as_sf()

# Full IUCN strings
lc_full <- "Least Concern – evaluated and found to be at low risk of extinction."
nt_full <- "Near Threatened – close to qualifying for a threatened category in the near future."
vu_full <- "Vulnerable – facing a high risk of extinction in the wild."
en_full <- "Endangered – facing a very high risk of extinction in the wild."
cr_full <- "Critically Endangered – facing an extremely high risk of extinction in the wild."
ew_full <- "Extinct in the Wild – survives only in cultivation or in captivity."
ex_full <- "Extinct – no reasonable doubt that the last individual has died."

# Map to unified labels & colors (Extinct + EW → same category)
iucn_map <- tibble::tibble(
  full = c(lc_full, nt_full, vu_full, en_full, cr_full, ew_full, ex_full),
  label = c(
    "Least Concern", "Near Threatened", "Vulnerable",
    "Endangered", "Critically Endangered",
    "Extinct or Extinct in the Wild", "Extinct or Extinct in the Wild"
  ),
  color = c(
    "#00a700", "#a6d854", "#ffd92f",
    "#fc8d62", "#e31a1c",
    "#984ea3", "#984ea3"
  )
)

# Sample one point per “minor mention” inside each country polygon,
# recoding both Extinct strings to the same label, and drop any unmatched
df_pts <- df_map %>%
  filter(!is.na(iucn_cat)) %>%
  mutate(full2 = if_else(iucn_cat %in% c(ew_full, ex_full), ew_full, iucn_cat)) %>%
  left_join(iucn_map, by = c("full2" = "full")) %>%
  filter(!is.na(label), !st_is_empty(geometry)) %>%   # remove NA labels
  rowwise() %>%
  mutate(pt = st_sample(geometry, 1)) %>%
  ungroup() %>%
  select(label, color, pt) %>%
  st_set_geometry("pt")

# Define the display order and colors
facets <- c(
  "Least Concern", "Near Threatened", "Vulnerable",
  "Endangered", "Critically Endangered",
  "Extinct or Extinct in the Wild"
)
colors <- setNames(iucn_map$color[match(facets, iucn_map$label)], facets)

# Plot: light‐grey basemap + well‐spread dots, faceted 3×2, no NA panel
ggplot() +
  geom_sf(data = shp, fill = "grey90", color = NA) +
  geom_sf(data = df_pts,
          aes(color = label),
          size  = 1.5,
          alpha = 0.5) +
  scale_color_manual(values = colors, guide = "none") +
  ggh4x::facet_wrap2(
    ~factor(label, levels = facets),
    ncol   = 3,
    strip  = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = colors)
    )
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    strip.text     = element_text(face = "bold", size = 14, color = "white"),
    plot.margin    = margin(10, 10, 10, 10)
  )

# Save to PNG
ggsave(
  filename = file.path(path_fig, "world_map_iucn_cat.png"),
  width    = 15, height = 15, dpi = 300
)
