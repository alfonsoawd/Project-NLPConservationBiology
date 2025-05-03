#----------------------------------- INFO -------------------------------------#
# Descr: Analysis and figures
# Author: Alfonso AWADALLA
# Date: April 2025
#------------------------------------------------------------------------------#


#===============================================================================
# PART 0 ─ SETUP & IMPORTS
#===============================================================================

# 0.1 ─ Load required libraries ------------------------------------------------
pkgs <- c(
  "data.table","tokenizers","cld3","textclean","stringr","stopwords","textstem",
  "httr","jsonlite","stringi","ggplot2","dplyr","viridis","tidytext","forcats",
  "SnowballC","patchwork","ggimage","sf","ggh4x", "tidyr"
)

invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly=TRUE)) install.packages(p)
  library(p, character.only=TRUE)
}))

# 0.2 ─ Define paths ----------------------------------------------------------
current_folder  <- dirname(rstudioapi::getActiveDocumentContext()$path)
project_folder  <- dirname(current_folder)
path_data       <- file.path(project_folder, "data", "data_with_score.csv")
path_shp        <- file.path(project_folder, "data", "World Shapefile",
                             "world-administrative-boundaries.shp")
path_fig        <- file.path(project_folder, "report", "Figures")

# 0.3 ─ Read data -------------------------------------------------------------
df  <- fread(path_data, na.strings=c("","NA"))
shp <- st_read(path_shp) %>%
  summarise(country = name, geometry)


#===============================================================================
# PART 1 ─ FIGURE 1: Sample Attrition (Lollipop Chart)
#===============================================================================

# 1.1 ─ Counts per variable/combination ---------------------------------------
df_counts <- df %>%
  summarise(
    "Total abstracts"           = n(),
    "Country"                   = sum(!is.na(country)),
    "Taxonomy"                  = sum(!is.na(order)),
    "Taxonomy & Country"        = sum(!is.na(order) & !is.na(country)),
    "IUCN category"             = sum(!is.na(iucn_cat)),
    "IUCN category & Country"   = sum(!is.na(iucn_cat) & !is.na(country))
  ) %>%
  pivot_longer(everything(), names_to="group", values_to="count") %>%
  arrange(count) %>%
  mutate(group = fct_inorder(group))

# 1.2 ─ Plot ------------------------------------------------------------------
p1 <- ggplot(df_counts, aes(x=count, y=group)) +
  geom_segment(aes(x=0, xend=count, y=group, yend=group),
               colour="grey90", size=1) +
  geom_point(aes(colour=count), size=7) +
  scale_colour_viridis(option="plasma", direction=-1, guide=FALSE) +
  geom_text(aes(label=count, x=count + max(count)*0.02),
            hjust=-0.5, family="Helvetica", size=4, colour="#444444") +
  labs(
    title    = "Data Availability Across Abstracts",
    subtitle = "How many abstracts contain each variable (or combinations)",
    x        = "Number of Abstracts",
    y        = NULL
  ) +
  theme_minimal(base_family="Helvetica") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x      = element_line(colour="grey80"),
    axis.text.y      = element_text(size=12, face="bold"),
    axis.text.x      = element_text(size=10),
    plot.title       = element_text(size=16, face="bold", margin=margin(b=4)),
    plot.subtitle    = element_text(size=12, margin=margin(b=10)),
    plot.margin      = margin(10,10,10,10)
  ) +
  expand_limits(x = max(df_counts$count)*1.1)

ggsave(
  filename = file.path(path_fig, "fig1_sample_attrition.png"),
  plot     = p1,
  width    = 7, height = 6, dpi = 300
)


#===============================================================================
# PART 2 ─ FIGURE 2: Top Positive vs Negative Word Contributions
#===============================================================================

# 2.1 ─ Prepare AFINN lexicon -----------------------------------------------
af <- get_sentiments("afinn") %>%
  mutate(lemma = lemmatize_strings(word)) %>%
  group_by(lemma) %>%
  summarise(value = mean(value), .groups="drop")

# 2.2 ─ Tokenize & lemmatize abstracts --------------------------------------
tk <- df %>%
  unnest_tokens(word, abstract) %>%
  filter(!str_detect(word,"\\d"), str_length(word)>2) %>%
  mutate(lemma=lemmatize_strings(word)) %>%
  anti_join(stop_words, by=c("lemma"="word"))

# 2.3 ─ Join, count & compute contributions -------------------------------
wc <- tk %>%
  inner_join(af, by="lemma") %>%
  count(lemma, value, name="n") %>%
  mutate(
    contrib   = n * value,
    sentiment = ifelse(contrib>0,"positive","negative"),
    plot_val  = ifelse(sentiment=="negative", -abs(contrib), abs(contrib))
  )

# 2.4 ─ Top 15 of each sentiment ------------------------------------------
pd <- wc %>%
  group_by(sentiment) %>%
  slice_max(order_by=abs(contrib), n=15, with_ties=FALSE) %>%
  ungroup() %>%
  mutate(lemma=factor(lemma,
                      levels=c(
                        filter(., sentiment=="negative") %>% arrange(plot_val) %>% pull(lemma),
                        filter(., sentiment=="positive") %>% arrange(plot_val) %>% pull(lemma)
                      )))

# 2.5 ─ Mirror‐bar Plot ---------------------------------------------------
p2 <- ggplot(pd, aes(x=plot_val, y=lemma, fill=sentiment)) +
  geom_col(width=0.6) +
  geom_vline(xintercept=0, colour="grey80", size=0.8) +
  scale_fill_manual(values=c(negative="#C24A4A", positive="#5F94C3")) +
  scale_x_continuous(labels=abs, expand=expansion(mult=c(0.05,0.1))) +
  labs(
    title    = "Top Positive vs. Negative Contributors",
    subtitle = "Mirror‐bar of the 15 words with largest (count × AFINN-score)",
    x        = "Weighted Contribution\n(count × score)",
    y        = NULL,
    fill     = "Sentiment"
  ) +
  theme_minimal(base_family="Helvetica") +
  theme(
    panel.grid      = element_blank(),
    axis.line.x     = element_line(colour="grey80"),
    axis.text.y     = element_text(size=10, face="bold"),
    axis.text.x     = element_text(size=9),
    legend.position = "bottom",
    legend.title    = element_text(face="bold"),
    plot.title      = element_text(size=15, face="bold", margin=margin(b=6)),
    plot.subtitle   = element_text(size=10, color="grey40", margin=margin(b=8))
  )

ggsave(
  filename = file.path(path_fig, "fig2_weighted_words.png"),
  plot     = p2,
  width    = 6, height = 8, dpi = 300
)


#===============================================================================
# PART 3 ─ FIGURE 3: Annual Sentiment Trend + Abstract Counts
#===============================================================================

# 3.1 ─ Reshape model scores & compute means ------------------------------
annual_model <- df %>%
  pivot_longer(
    cols      = c(bing_z, afinn_z, nrc_z, syuzhet_z, sentimentr_z),
    names_to  = "model",
    values_to = "z"
  )

annual_means <- annual_model %>%
  group_by(model, year) %>%
  summarise(annual_z = mean(z, na.rm=TRUE), .groups="drop")

annual_ens <- annual_means %>%
  group_by(year) %>%
  summarise(
    ensemble_z = mean(annual_z),
    sd_z       = sd(annual_z),
    .groups    = "drop"
  )

annual_n <- df %>% count(year, name="n")

# 3.2 ─ Top panel: sentiment + ribbon --------------------------------------
p3a <- ggplot(annual_ens, aes(year, ensemble_z)) +
  geom_ribbon(aes(ymin=ensemble_z-sd_z, ymax=ensemble_z+sd_z),
              fill="#80B1D3", alpha=0.4) +
  geom_line(colour="#1F78B4", linewidth=1.3) +
  geom_hline(yintercept=0, linetype="solid", colour="#AAAAAA") +
  scale_x_continuous(expand=c(0,0), breaks=seq(min(df$year), max(df$year), by=5)) +
  labs(y="Sentiment score") +
  theme_minimal(base_family="Helvetica", base_size=14) +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.line     = element_line(colour="#333333", size=0.5),
    axis.title.y  = element_text(face="bold", size=15),
    axis.text.y   = element_text(size=12)
  )

# 3.3 ─ Bottom panel: abstract counts --------------------------------------
p3b <- ggplot(annual_n, aes(year, n)) +
  geom_col(fill="#66C2A5", width=1) +
  scale_x_continuous(expand=c(0,0), breaks=seq(min(df$year), max(df$year), by=5)) +
  labs(x="Publication year", y="Number of abstracts") +
  theme_minimal(base_family="Helvetica", base_size=14) +
  theme(
    panel.grid   = element_blank(),
    axis.line    = element_line(colour="#333333", size=0.5),
    axis.title   = element_text(face="bold", size=15),
    axis.text    = element_text(size=12)
  )

final_3 <- (p3a / p3b) +
  plot_layout(heights=c(3,1)) +
  plot_annotation(theme=theme(plot.background=element_rect(fill="white", colour=NA)))

ggsave(
  filename = file.path(path_fig, "fig3_sentiment_trend.png"),
  plot     = final_3,
  width    = 8, height = 6, dpi = 300
)


#===============================================================================
# PART 4 ─ FIGURE 4: IUCN Category Counts (% & labels)
#===============================================================================

iucn_map_full <- tibble(
  full  = c(
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
    "Least Concern","Near Threatened","Vulnerable",
    "Endangered","Critically Endangered",
    "Extinct in the Wild","Extinct","Data Deficient"
  ),
  code  = c("LC","NT","VU","EN","CR","EW","EX","DD")
)

df_iucn <- df %>%
  filter(!is.na(iucn_cat)) %>%
  inner_join(iucn_map_full, by=c("iucn_cat"="full")) %>%
  count(short, code, name="n") %>%
  mutate(
    pct   = n/sum(n)*100,
    short = factor(short, levels=iucn_map_full$short)
  )

palette_iucn <- c(
  "Least Concern"         = "#1b9e77",
  "Near Threatened"       = "#a6d854",
  "Vulnerable"            = "#ffd92f",
  "Endangered"            = "#fc8d62",
  "Critically Endangered" = "#e31a1c",
  "Extinct in the Wild"   = "#b2abd2",
  "Extinct"               = "#8073ac",
  "Data Deficient"        = "#66c2a5"
)

p4 <- ggplot(df_iucn, aes(x=short, y=pct, fill=short)) +
  geom_col(width=0.7, show.legend=FALSE) +
  geom_text(aes(label=paste0(n," (",code,")")),
            vjust=-0.4, size=4, family="Helvetica", colour="#333333") +
  scale_fill_manual(values=palette_iucn) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  labs(x=NULL, y="% articles") +
  theme_minimal(base_family="Helvetica", base_size=14) +
  theme(
    panel.grid   = element_blank(),
    axis.text.x  = element_text(angle=45, hjust=1, face="bold", colour="#333333"),
    axis.text.y  = element_text(face="bold", colour="#333333"),
    axis.title.y = element_text(face="bold", colour="#333333"),
    plot.margin  = margin(10,10,10,10)
  )

ggsave(
  filename = file.path(path_fig, "fig4_iucn_counts.png"),
  plot     = p4,
  width    = 6, height=7, dpi=300
)


#===============================================================================
# PART 5 ─ FIGURE 5: Top-20 Orders with IUCN Composition
#===============================================================================

# 5.1 ─ Map & filter iucn ----------------------------------------------------
iucn_map_short <- tibble(
  full  = c(
    "Least Concern – evaluated and found to be at low risk of extinction.",
    "Near Threatened – close to qualifying for a threatened category in the near future.",
    "Vulnerable – facing a high risk of extinction in the wild.",
    "Endangered – facing a very high risk of extinction in the wild.",
    "Critically Endangered – facing an extremely high risk of extinction in the wild.",
    "Extinct in the Wild – survives only in cultivation or in captivity.",
    "Extinct – no reasonable doubt that the last individual has died."
  ),  
  short = c(
    "Least Concern","Near Threatened","Vulnerable",
    "Endangered","Critically Endangered",
    "Extinct in the Wild","Extinct"
  )
)

iucn_cols_short <- c(
  "Least Concern"          = "#1b9e77",
  "Near Threatened"        = "#a6d854",
  "Vulnerable"             = "#ffd92f",
  "Endangered"             = "#fc8d62",
  "Critically Endangered"  = "#e31a1c",
  "Extinct in the Wild"    = "#b2abd2",
  "Extinct"                = "#8073ac"
)

df2_o <- df %>%
  filter(!is.na(iucn_cat)) %>%
  inner_join(iucn_map_short, by=c("iucn_cat"="full"))

# 5.2 ─ Top 20 orders by unique mentions -------------------------------------
order_counts <- df2_o %>%
  count(order, name="n_unique")
top20 <- order_counts %>%
  slice_max(n_unique, n=20) %>%
  pull(order)

df_top <- df2_o %>%
  filter(order %in% top20)

# 5.3 ─ Counts & images for Panel A -----------------------------------------
order_counts_top <- df_top %>%
  count(order, name="n_unique") %>%
  arrange(desc(n_unique)) %>%
  mutate(
    order = factor(order, levels=order),
    img   = file.path(path_fig, "Orders", paste0(order,".png"))
  )

# 5.4 ─ IUCN proportions for Panel B ----------------------------------------
order_iucn_top <- df_top %>%
  count(order, short, name="n") %>%
  group_by(order) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  mutate(order = factor(order, levels=levels(order_counts_top$order)))

# 5.5 ─ Plot Panel A ---------------------------------------------------------
max_n     <- max(order_counts_top$n_unique)
img_off   <- max_n * 0.12

p5A <- ggplot(order_counts_top, aes(order, n_unique, fill=n_unique)) +
  geom_col(width=0.7, show.legend=FALSE) +
  scale_fill_viridis_c(option="plasma", direction=-1) +
  scale_y_continuous(expand=expansion(mult=c(0,0.2))) +
  geom_image(aes(image=img), size=0.06, by="width", nudge_y=img_off) +
  geom_text(aes(label=n_unique), vjust=-1.2, size=3.5, family="Helvetica") +
  coord_cartesian(clip="off") +
  labs(y="Number of articles") +
  theme_minimal(base_family="Helvetica", base_size=14) +
  theme(
    panel.grid   = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(face="bold"),
    plot.margin  = margin(20,20,20,20)
  )

# 5.6 ─ Plot Panel B ---------------------------------------------------------
p5B <- ggplot(order_iucn_top, aes(order, prop, fill=short)) +
  geom_col(width=0.7) +
  scale_fill_manual(values=iucn_cols_short, name="Red List category") +
  labs(x="Order", y="Proportion of mentions") +
  theme_minimal(base_family="Helvetica", base_size=14) +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle=45, hjust=1, face="bold", colour="#333333"),
    axis.title      = element_text(face="bold"),
    legend.position = "right"
  )

final_5 <- p5A / p5B +
  plot_layout(heights=c(2,1)) &
  theme(plot.background=element_rect(fill="white",colour=NA))

ggsave(
  filename = file.path(path_fig, "fig5_orders_iucn_top20.png"),
  plot     = final_5,
  width    = 12, height=8, dpi=300
)


#===============================================================================
# PART 6 ─ FIGURE 6: World Map of Threatened-species Mentions
#===============================================================================

# 6.1 ─ Join & compute per-country proportions -----------------------------
df_map <- df %>%
  left_join(shp, by="country") %>%
  st_as_sf()

threat_cats <- c(
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
    n_threat    = n_distinct(species[iucn_cat %in% threat_cats]),
    prop_threat = n_threat/n_total,
    .groups     = "drop"
  )

shp2 <- shp %>%
  left_join(st_drop_geometry(country_summary), by="country")

# 6.2 ─ Plot map ------------------------------------------------------------
ggplot(shp2) +
  geom_sf(aes(fill=prop_threat), colour="white", size=0.1) +
  scale_fill_gradientn(
    colours   = c("#FFFFCC","#FED976","#FD8D3C","#E31A1C","#800026"),
    limits    = c(0,1),
    na.value  = "grey90",
    name      = "Proportion of species mentioned\ncategorised as threatened, by country",
    labels    = scales::percent_format(accuracy=1),
    guide     = guide_colorbar(title.position="top", title.hjust=0.5)
  ) +
  theme_void(base_family="Helvetica") +
  theme(
    plot.title      = element_text(face="bold", size=16, hjust=0.5),
    legend.position = "bottom",
    legend.title    = element_text(face="bold", size=10, hjust=0.5),
    legend.text     = element_text(size=9),
    plot.margin     = margin(10,10,10,10)
  )

ggsave(
  filename = file.path(path_fig, "fig6_world_continuous.png"),
  width    = 10, height=7, dpi=300
)


#===============================================================================
# PART 7 ─ FIGURE 7: Faceted World Dots by IUCN Category
#===============================================================================

## 7.1 ─ Prepare spatial data & full-category strings -------------------------
df_map <- df %>%
  left_join(shp, by = "country") %>%
  st_as_sf()

full_iucn <- list(
  LC = "Least Concern – evaluated and found to be at low risk of extinction.",
  NT = "Near Threatened – close to qualifying for a threatened category in the near future.",
  VU = "Vulnerable – facing a high risk of extinction in the wild.",
  EN = "Endangered – facing a very high risk of extinction in the wild.",
  CR = "Critically Endangered – facing an extremely high risk of extinction in the wild.",
  EW = "Extinct in the Wild – survives only in cultivation or in captivity.",
  EX = "Extinct – no reasonable doubt that the last individual has died."
)

## 7.2 ─ Map to unified labels + assign colors --------------------------------
iucn_map <- tibble(
  code  = names(full_iucn),
  full  = unlist(full_iucn),
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

## 7.3 ─ Sample one point per country polygon for each minor mention ----------
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

## 7.4 ─ Define facet order + colors -----------------------------------------
facets <- c(
  "Least Concern", "Near Threatened", "Vulnerable",
  "Endangered", "Critically Endangered",
  "Extinct or Extinct in the Wild"
)
colors <- setNames(iucn_map$color[match(facets, iucn_map$label)], facets)

## 7.5 ─ Plot faceted map -----------------------------------------------------
p7 <- ggplot() +
  # basemap
  geom_sf(data = shp, fill = "grey90", color = NA) +
  # points
  geom_sf(data = df_pts, aes(color = label), size = 1.5, alpha = 0.5) +
  scale_color_manual(values = colors, guide = "none") +
  # facets with colored strips
  ggh4x::facet_wrap2(
    ~ factor(label, levels = facets),
    ncol  = 2,
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = colors)
    )
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    strip.text  = element_text(face = "bold", size = 14, color = "white"),
    plot.margin = margin(10, 10, 10, 10)
  )

# 7.6 ─ Save to PNG ----------------------------------------------------------
ggsave(
  filename = file.path(path_fig, "fig7_world_facets.png"),
  plot     = p7,
  width    = 10, height = 15, dpi = 300
)


#===============================================================================
# PART 8 ─ FIGURE 8: Sentiment trends for top-3 species per IUCN category 
#===============================================================================

# 8.1 — Define the five IUCN short labels + panel fill‐colours
cats     <- c("Least Concern", "Near Threatened", "Vulnerable",
              "Endangered", "Critically Endangered")
cat_cols <- c(
  "Least Concern"         = "#66c2a4",
  "Near Threatened"       = "#b8e186",
  "Vulnerable"            = "#ffffb3",
  "Endangered"            = "#fdae61",
  "Critically Endangered" = "#f46d43"
)

# 8.2 — Simplify iucn_cat → label, combine Genus+species
df2 <- df %>%
  mutate(
    sp_full = paste(genus, species),
    label   = case_when(
      str_detect(iucn_cat, "Least Concern")         ~ "Least Concern",
      str_detect(iucn_cat, "Near Threatened")       ~ "Near Threatened",
      str_detect(iucn_cat, "Vulnerable")            ~ "Vulnerable",
      str_detect(iucn_cat, "Critically Endangered") ~ "Critically Endangered",
      str_detect(iucn_cat, "Endangered –")          ~ "Endangered",
      TRUE                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(label))

# 8.3 — Top-3 most cited species in each category
top3 <- df2 %>%
  count(label, sp_full, name="n_cite") %>%
  group_by(label) %>%
  slice_max(n_cite, n=3, with_ties=FALSE) %>%
  ungroup()

# 8.4 — Annual mean sentiment for those 15 species
df_spp <- df2 %>%
  filter(sp_full %in% top3$sp_full) %>%
  group_by(label, sp_full, year) %>%
  summarise(sentiment = mean(ensemble_z, na.rm=TRUE),
            .groups="drop")

# 8.5 — Rank each of the 15 by their final‐year sentiment (top→1)
last_year <- max(df_spp$year)
fin_sent  <- df_spp %>%
  filter(year == last_year) %>%
  select(label, sp_full, sentiment)

rank_fin <- top3 %>%
  left_join(fin_sent, by=c("label","sp_full")) %>%
  mutate(sentiment = ifelse(is.na(sentiment), -Inf, sentiment)) %>%
  group_by(label) %>%
  arrange(desc(sentiment)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# 8.6 — Common panel axes
x_min <- min(df_spp$year); x_max <- max(df_spp$year)
y_min <- min(df_spp$sentiment); y_max <- max(df_spp$sentiment)
y_pad <- 0.05 * (y_max - y_min)
y_lim <- c(y_min - y_pad, y_max + y_pad)

# 8.7 — Path to species images
img_dir <- file.path(path_figs, "Species")

# 8.8 — Build each mini-plot in strict row-major order
plots <- vector("list", length=15)
idx   <- 1

for(r in 1:3) {
  for(cat in cats) {
    sp    <- rank_fin %>% filter(label==cat, rank==r) %>% pull(sp_full)
    dat   <- df_spp %>% filter(label==cat, sp_full==sp)
    fillc <- cat_cols[cat]
    
    # decide which axes to show
    show_x <- (r==3)
    show_y <- (cat==cats[1])
    
    p <- ggplot() +
      # panel background + border
      geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf),
                fill=fillc, colour="black", size=0.8) +
      # zero line
      geom_hline(yintercept=0, linetype="dotted", colour="grey60") +
      # points + smoothing line
      geom_point(data=dat, aes(year, sentiment),
                 colour="grey30", alpha=0.6, size=2) +
      geom_smooth(data=dat, aes(year, sentiment),
                  method="lm", linewidth=0.8, colour="black", se=FALSE) +
      # fix axes
      scale_x_continuous(limits=c(x_min,x_max),
                         breaks=c(1990,2000,2010)) +
      scale_y_continuous(limits=y_lim) +
      # titles / labels
      labs(
        title = if(r==1) cat else NULL,
        x     = if(show_x) "Year" else NULL,
        y     = if(show_y) "Sentiment score" else NULL
      ) +
      theme_minimal(base_family="Helvetica", base_size=12) +
      theme(
        plot.title   = element_text(face="bold", size=11, hjust=0.5),
        axis.title.x = element_text(size=10),
        axis.text.x  = if(show_x) element_text() else element_blank(),
        axis.ticks.x = if(show_x) element_line() else element_blank(),
        axis.title.y = element_text(face="bold", size=10),
        axis.text.y  = if(show_y) element_text() else element_blank(),
        axis.ticks.y = if(show_y) element_line() else element_blank(),
        panel.grid   = element_blank(),
        plot.margin  = margin(4,4,4,4)
      )
    
    # species name (small, grey)
    x_txt <- x_min + 0.05*(x_max-x_min)
    y_txt <- y_lim[2]
    p <- p +
      annotate("text",
               x      = x_txt,
               y      = y_txt,
               label  = sp,
               hjust  = 0, vjust = 1,
               fontface = "bold",
               size     = 3,
               colour   = "grey40")
    
    # species image (bigger, just below name)
    img_file <- file.path(img_dir, paste0(sp, ".png"))
    if (file.exists(img_file)) {
      img_df <- tibble(
        x     = x_txt + 0.04*(x_max-x_min),
        y     = y_txt - 0.24*(y_max-y_min),
        image = img_file
      )
      p <- p +
        geom_image(
          data        = img_df,
          aes(x, y, image=image),
          size        = 0.25,
          by          = "width",
          inherit.aes = FALSE
        )
    }
    
    plots[[idx]] <- p
    idx <- idx + 1
  }
}

# 8.9 — Arrange & save
fig7 <- wrap_plots(plots, ncol=5)
print(fig7)

ggsave(
  filename = file.path(path_figs, "fig8_sentiment_by_species.png"),
  plot     = fig7,
  width    = 10, height = 5, dpi = 300
)


#===============================================================================
# PART 9 ─ FIGURE 9: Sentiment trends per IUCN category 
#===============================================================================


# 9.1 - Define your six IUCN categories and their colours
cats <- c(
  "Least Concern", "Near Threatened",
  "Vulnerable",   "Endangered",
  "Critically Endangered",
  "Extinct (in the Wild)"
)
cat_cols <- c(
  "Least Concern"                  = "#66c2a4",
  "Near Threatened"                = "#b8e186",
  "Vulnerable"                     = "#ffffb3",
  "Endangered"                     = "#fdae61",
  "Critically Endangered"          = "#f46d43",
  "Extinct (in the Wild)"          = "#8c6bb1"
)

# 9.2 - Simplify your iucn_cat into exactly those six labels
df_cat <- df %>%
  mutate(label = case_when(
    str_detect(iucn_cat, "Least Concern")                             ~ "Least Concern",
    str_detect(iucn_cat, "Near Threatened")                           ~ "Near Threatened",
    str_detect(iucn_cat, "Vulnerable –")                              ~ "Vulnerable",
    str_detect(iucn_cat, "Critically Endangered –")                   ~ "Critically Endangered",
    str_detect(iucn_cat, "Endangered –") & !str_detect(iucn_cat,"Crit") ~ "Endangered",
    str_detect(iucn_cat, "Extinct in the Wild") |
      str_detect(iucn_cat, "Extinct –")                                  ~ "Extinct (in the Wild)",
    TRUE                                                               ~ NA_character_
  )) %>%
  filter(!is.na(label))

# 9.3 - Precompute a common x– & y–range so every panel lines up
year_range <- range(df_cat$year, na.rm=TRUE)
# pad the sentiment axis to nice round numbers
y_min <- floor(min(df_cat$ensemble_z, na.rm=TRUE))
y_max <- ceiling(max(df_cat$ensemble_z, na.rm=TRUE))
y_range <- c(y_min, y_max)

# 9.4 - Build one ggplot for each category
plots <- lapply(cats, function(cat) {
  dat <- df_cat %>% filter(label == cat)
  ggplot(dat, aes(x=year, y=ensemble_z)) +
    # coloured background + black border
    geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf),
              fill = cat_cols[cat],
              color = "black", size = 0.6) +
    # raw points
    geom_point(color = "grey30", alpha = 0.5, size = 2) +
    # linear trend
    geom_smooth(method="lm", se=FALSE,
                color="black", linewidth=0.8) +
    # zero‐line
    geom_hline(yintercept=0, linetype="dotted", color="grey60") +
    # enforce same scales
    scale_x_continuous(
      limits = year_range,
      breaks = c(1990, 2000, 2010),
      expand = c(0,0)
    ) +
    scale_y_continuous(
      limits = y_range,
      breaks = c(y_min, 0, y_max),
      expand = c(0,0)
    ) +
    # only the first panel gets a y‐axis label,
    # and only the very bottom row (here: all) gets the x‐axis label
    labs(
      title = cat,
      x = "Year",
      y = if (cat == cats[1]) "Sentiment score" else NULL
    ) +
    theme_minimal(base_family="Helvetica") +
    theme(
      plot.title       = element_text(face="bold", size=12, hjust=0.5),
      axis.title.y     = element_text(size=10, margin=margin(r=4)),
      axis.title.x     = element_text(size=10, margin=margin(t=4)),
      axis.text.x      = element_text(size=9),
      axis.text.y      = if (cat == cats[1]) element_text(size=9) else element_blank(),
      axis.ticks.y     = if (cat == cats[1]) element_line() else element_blank(),
      panel.grid       = element_blank(),
      plot.margin      = margin(2,2,2,2)
    )
})

# 9.5 — Arrange & save
fig9 = wrap_plots(plots, nrow = 1)
print(fig9)

ggsave(
  filename = file.path(path_figs, "fig9_sentiment_by_category.png"),
  plot     = fig9,
  width    = 12, height = 4, dpi = 300
)

#===============================================================================
# PART 10─ FIGURE 10: Annual Sentiment Trend + Abstract Counts per IUCN cat
#===============================================================================

# 10.1 - Define your six labels (merge the two extinct categories)
cats <- c(
  "Least Concern",
  "Near Threatened",
  "Vulnerable",
  "Endangered",
  "Critically Endangered",
  "Extinct (in the Wild)"
)

# 10.2 - The colours for each category
cat_cols <- c(
  "Least Concern"         = "#66c2a4",  # turquoise pastel
  "Near Threatened"       = "#b8e186",  # light green
  "Vulnerable"            = "#ffffb3",  # very soft yellow
  "Endangered"            = "#fdae61",  # light orange
  "Critically Endangered" = "#f46d43",  # pastel red
  "Extinct (in the Wild)" = "#A26769"   # muted maroon
)

# 10.3 - Simplify raw iucn_cat into those six levels
df2 <- df %>%
  mutate(label = case_when(
    str_detect(iucn_cat, "Least Concern")                             ~ "Least Concern",
    str_detect(iucn_cat, "Near Threatened")                           ~ "Near Threatened",
    str_detect(iucn_cat, "Vulnerable –")                              ~ "Vulnerable",
    str_detect(iucn_cat, "Critically Endangered –")                   ~ "Critically Endangered",
    str_detect(iucn_cat, "Endangered –") & !str_detect(iucn_cat,"Crit") ~ "Endangered",
    str_detect(iucn_cat, "Extinct in the Wild") |
      str_detect(iucn_cat, "Extinct –")                                  ~ "Extinct (in the Wild)",
    TRUE                                                               ~ NA_character_
  )) %>%
  filter(!is.na(label)) %>%
  mutate(label = factor(label, levels = cats))

# 10.4 - Gather the five model z-scores
long <- df2 %>%
  pivot_longer(
    cols      = c(bing_z, afinn_z, nrc_z, syuzhet_z, sentimentr_z),
    names_to  = "model",
    values_to = "z"
  )

# 10.5 - Compute each model’s mean per category, then ensemble ± sd
ens_cat <- long %>%
  group_by(model, label) %>%
  summarise(mean_z = mean(z, na.rm=TRUE), .groups="drop") %>%
  group_by(label) %>%
  summarise(
    ensemble_z = mean(mean_z),
    sd_z       = sd(mean_z),
    .groups    = "drop"
  )

# 10.6 - Count abstracts per category
counts_cat <- df2 %>%
  count(label, name="n")

# 10.7 - Top panel: sentiment + ribbon over discrete categories
p_top <- ggplot(ens_cat, aes(label, ensemble_z, group=1)) +
  geom_ribbon(aes(ymin = ensemble_z - sd_z, ymax = ensemble_z + sd_z),
              fill = "#80B1D3", alpha = 0.4) +
  geom_line(colour = "#1F78B4", size = 0.8) +
  geom_point(aes(fill = label),
             shape = 21, size = 3, colour = "black") +
  geom_hline(yintercept = 0, colour = "grey70") +
  scale_fill_manual(values = cat_cols) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0.05,0.1))) +
  labs(y = "Sentiment score") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid     = element_blank(),
    axis.title.x   = element_blank(),
    axis.text.x    = element_blank(),
    axis.ticks.x   = element_blank(),
    axis.line.x    = element_line(colour="grey20", size=0.5),
    axis.line.y    = element_line(colour="grey20", size=0.5),
    axis.title.y   = element_text(face="bold", size=12),
    axis.text.y    = element_text(size=10),
    legend.position = "none",
    plot.margin    = margin(2,2,2,2)
  )

# 10.8 - Bottom panel: bar chart of counts
p_bottom <- ggplot(counts_cat, aes(label, n, fill = label)) +
  geom_col(width = 0.6, colour = "black", size = 0.2) +
  scale_fill_manual(values = cat_cols) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "IUCN category", y = "Number of abstracts") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid     = element_blank(),
    axis.title.x   = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.text.x    = element_text(size=10, angle=25, hjust=1),
    axis.ticks.x   = element_blank(),
    axis.line.x    = element_line(colour="grey20", size=0.5),
    axis.line.y    = element_line(colour="grey20", size=0.5),
    axis.title.y   = element_text(face="bold", size=12),
    axis.text.y    = element_text(size=10),
    legend.position = "none",
    plot.margin    = margin(2,2,2,2)
  )

# 10.9 - Stack them 3:1
final10 <- p_top / p_bottom +
  plot_layout(heights = c(3,1))

print(final10)

ggsave(
  filename = file.path(path_fig, "fig10_sentiment_trend_per_cat.png"),
  plot     = final10,
  width    = 8, height = 6, dpi = 300
)
  