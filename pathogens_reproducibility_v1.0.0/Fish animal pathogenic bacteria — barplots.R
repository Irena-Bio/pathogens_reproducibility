# ------------------------------------------------------
# Fish/animal pathogenic bacteria — barplots (ABSOLUTE)
# ------------------------------------------------------

# install.packages(c("readxl","dplyr","tidyr","ggplot2","forcats","patchwork","scales","stringr"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(patchwork)
library(scales)
library(stringr)

# === 1) Load data ===
excel_path <- "Fish_bacterial_pathogens.xlsx"   # adjust if needed
sheet_name <- "Sheet1"                           # adjust if needed
sample_col <- "Sample group"                     # <-- your actual header

raw <- read_excel(excel_path, sheet = sheet_name)

# Fallback: if "Sample group" isn't found due to typos/extra spaces/case, use the first column
if (!sample_col %in% names(raw)) {
  message("Note: '", sample_col, "' not found. Using the first column as sample names.")
  names(raw)[1] <- "Sample group"
}

# === 2) Sample order + pretty labels (edit if you want) ===
sample_order <- c("Krka - water",
                  "Krka - sediment",
                  "Kupcina - water",
                  "Kupcina - sediment",
                  "Fish farm water")

label_map <- tibble(
  Sample = factor(sample_order, levels = sample_order),
  Label  = c("Krka_water", "Krka_sediment",
             "Kupcina_water", "Kupcina_sediment",
             "Vrabac_water")  # or "Fish_farm_water" if you prefer
)

# === 3) Tidy to long format (ABSOLUTE abundances) ===
dat_long <- raw %>%
  rename(Sample = !!sym(sample_col)) %>%       # use your exact header
  pivot_longer(cols = -Sample,
               names_to = "Pathogen",
               values_to = "Abundance") %>%
  mutate(
    Abundance = replace_na(as.numeric(Abundance), 0),
    Sample    = factor(Sample, levels = sample_order),
    Pathogen  = str_replace_all(Pathogen, "_", " "),
    Pathogen  = str_wrap(Pathogen, width = 30)
  ) %>%
  left_join(label_map, by = "Sample")

# === 4) Split pathogens into two halves for two panels ===
all_pathogens <- unique(dat_long$Pathogen)
half  <- ceiling(length(all_pathogens) / 2)
part1 <- all_pathogens[1:half]
part2 <- all_pathogens[(half + 1):length(all_pathogens)]

# === 5) Plot helper (absolute counts) ===
make_plot_counts <- function(df, title_text, tag_letter) {
  ggplot(df, aes(x = Label, y = Abundance, fill = Pathogen)) +
    geom_col(width = 0.75, linewidth = 0.2) +
    labs(x = NULL, y = "Abundance (counts)", title = title_text, tag = tag_letter) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma) +
    guides(fill = guide_legend(title = "Fish/animal pathogens", ncol = 1)) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.tag   = element_text(face = "bold", size = 16),
      axis.text.x = element_text(angle = 28, hjust = 1, vjust = 1,
                                 margin = margin(t = 10)),
      legend.key.height = unit(4, "mm"),
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 20, 30, 10)  # extra bottom for long labels
    ) +
    coord_cartesian(clip = "off")
}

p1 <- dat_long %>% filter(Pathogen %in% part1) %>%
  make_plot_counts("Fish/animal pathogens (Part 1)", "C")

p2 <- if (length(part2) > 0) {
  dat_long %>% filter(Pathogen %in% part2) %>%
    make_plot_counts("Fish/animal pathogens (Part 2)", "D")
} else {
  ggplot() + theme_void() + labs(title = "Fish/animal pathogens (Part 2)", tag = "D")
}

# Combined panel with a title
panel <- p1 + p2 + plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Abundance of fish/animal pathogenic bacteria across sites",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# === 6) Save ===
ggsave("fish_pathogens_abs_part1.png", p1, width = 7.6, height = 5.2, dpi = 300)
ggsave("fish_pathogens_abs_part2.png", p2, width = 7.6, height = 5.2, dpi = 300)
ggsave("fish_pathogens_abs_panel.png", panel, width = 13.5, height = 5.8, dpi = 300)

# (Optional) Two-line hyphen labels instead of underscores:
# dat_long <- dat_long %>% mutate(Label = str_replace(as.character(Sample), " - ", " -\n"))
# and re-run the plotting section.

