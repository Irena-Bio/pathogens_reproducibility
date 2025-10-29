# -----------------------------
# Human pathogenic bacteria — barplots (R)
# -----------------------------

# Install once if needed:
# install.packages(c("readxl", "tidyverse", "patchwork"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(patchwork)
library(scales)
library(stringr)

# === 1) Load data ===
# Adjust the path if your file is elsewhere
excel_path <- "Human_bacterial_pathogens.xlsx"
sheet_name <- "Sheet1"   # change if your sheet is named differently

raw <- read_excel(excel_path, sheet = sheet_name)

# Expect a first column with the sample group names.
# If your first column is not named "Sample Group", set it here:
sample_col <- "Sample Group"

stopifnot(sample_col %in% names(raw))

# Keep a consistent sample order for plotting
sample_order <- c("Krka - water",
                  "Krka - sediment",
                  "Kupcina - water",
                  "Kupcina - sediment",
                  "Fish farm water")

# === 2) Tidy + compute relative abundance (%) ===
dat_long <- raw %>%
  rename(Sample = !!sym(sample_col)) %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Pathogen",
    values_to = "Abundance"
  ) %>%
  mutate(Abundance = replace_na(as.numeric(Abundance), 0)) %>%
  group_by(Sample) %>%
  mutate(RelAbund = 100 * Abundance / sum(Abundance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Sample = factor(Sample, levels = sample_order),
    # Tidy very long legend labels
    Pathogen = str_replace_all(Pathogen, "_", " "),
    Pathogen = str_wrap(Pathogen, width = 30)
  )

# Safety check in case some sample names in the file differ slightly
if (any(is.na(dat_long$Sample))) {
  dat_long$Sample <- fct_inorder(dat_long$Sample)
}

# === 3) Split pathogens into two halves for two figures ===
all_pathogens <- unique(dat_long$Pathogen)
half <- ceiling(length(all_pathogens) / 2)
part1 <- all_pathogens[1:half]
part2 <- all_pathogens[(half + 1):length(all_pathogens)]

make_plot <- function(df, title_text, tag_letter) {
  ggplot(df, aes(x = Sample, y = RelAbund, fill = Pathogen)) +
    geom_col(width = 0.75) +
    labs(
      x = NULL,
      y = "Relative abundance (%)",
      title = title_text,
      tag = tag_letter
    ) +
    scale_y_continuous(expand = expansion(c(0, 0.05))) +
    guides(fill = guide_legend(title = "Human pathogens", ncol = 1)) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.tag = element_text(face = "bold"),
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.key.height = unit(4, "mm"),
      legend.text = element_text(size = 9)
    )
}

p1 <- dat_long %>% filter(Pathogen %in% part1) %>%
  make_plot("Human pathogens (Part 1)", "A")

# If you have very few pathogens, part2 may be empty; handle gracefully
if (length(part2) > 0) {
  p2 <- dat_long %>% filter(Pathogen %in% part2) %>%
    make_plot("Human pathogens (Part 2)", "B")
} else {
  # Duplicate style with no data (rare case if list is short)
  p2 <- ggplot() + theme_void() + labs(title = "Human pathogens (Part 2)", tag = "B")
}

# === 4) Save figures ===
ggsave("human_pathogens_part1.png", p1, width = 7.5, height = 5.2, dpi = 300)
ggsave("human_pathogens_part2.png", p2, width = 7.5, height = 5.2, dpi = 300)

# Combined panel (side-by-side)
panel <- p1 + p2 + plot_layout(widths = c(1, 1))
ggsave("human_pathogens_panel.png", panel, width = 13, height = 5.5, dpi = 300)

# === 5) If you also want absolute counts instead of %:
# p_counts <- ggplot(dat_long, aes(Sample, Abundance, fill = Pathogen)) +
#   geom_col() + theme_bw() + labs(y = "Abundance (counts)")
