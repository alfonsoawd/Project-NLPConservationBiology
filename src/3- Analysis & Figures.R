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
             "stringi", "ggplot2", "dplyr", "viridis")

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

# Path to figures folder
path_fig = file.path(project_folder, "report", "Figures")

#-------------------------------------------------
# Step 0.3 Import data
#-------------------------------------------------

# Import data with informations
df = fread(path_data, na.strings = c("", "NA"))


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
    caption  = "Source: your df_counts"
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
  dpi      = 500
)

#-------------------------------------------------
# Figure 1: sample attrition
#-------------------------------------------------





