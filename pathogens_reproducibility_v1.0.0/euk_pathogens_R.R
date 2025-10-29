# ------------------------------------------------------------
# Eukaryotic pathogens — bar plot of ABSOLUTE abundances
# ------------------------------------------------------------

# install.packages(c("readr","dplyr","tidyr","ggplot2","forcats","scales","stringr"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

# ===== 1) Settings =====
csv_path     <- "Average_abundances_of_pathogenic_species_per_site.csv"
legend_title <- "Eukaryotic pathogens"   # legend title shown in plot

# If you want to force an order of sites, set it here; otherwise leave NULL
site_order   <- c("Krka - water","Krka - sediment",
                  "Kupcina - water","Kupcina - sediment",
                  "Fish farm water")     # set to NULL to keep CSV order

# ===== 2) Load =====
dat <- read_csv(csv_path, show_col_types = FALSE)

# Choose the site column sensibly
candidate_site_cols <- c("Sample Group","Sample group","Site","Sample")
site_col <- candidate_site_cols[candidate_site_cols %in% names(dat)][1]
if (is.na(site_col)) {
  message("Site column not found among: ",
          paste(candidate_site_cols, collapse = ", "),
          ". Using the first column as site names.")
  site_col <- names(dat)[1]
}

# ===== 3) Tidy (ABSOLUTE values retained) =====
long <- dat %>%
  rename(Site = !!sym(site_col)) %>%
  pivot_longer(
    cols = -Site,
    names_to = "Species",
    values_to = "Abundance"
  ) %>%
  mutate(
    Abundance = as.numeric(Abundance),
    Site = if (!is.null(site_order) && all(site_order %in% unique(Site))) {
      factor(Site, levels = site_order)
    } else {
      fct_inorder(Site)
    },
    Species = str_replace_all(Species, "_", " "),
    Species = str_wrap(Species, width = 30),
    Label   = str_replace_all(as.character(Site), " - ", "_")  # compact x labels
  )

# ===== 4) Plot (ABSOLUTE abundances) =====
p <- ggplot(long, aes(x = Label, y = Abundance, fill = Species)) +
  geom_col(width = 0.75, linewidth = 0.2) +
  labs(
    x = NULL,
    y = "Abundance (counts)",
    title = "Abundance of eukaryotic pathogen species across sites",
    fill = legend_title
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma) +
  guides(fill = guide_legend(ncol = 1)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.text.x  = element_text(angle = 28, hjust = 1, vjust = 1, margin = margin(t = 10)),
    legend.key.height = unit(4, "mm"),
    legend.text  = element_text(size = 9),
    plot.margin  = margin(10, 20, 30, 10)
  ) +
  coord_cartesian(clip = "off")

# Preview
p

# Save
ggsave("euk_pathogens_abs_oneplot.png", p, width = 9.5, height = 6.2, dpi = 300)

# ===== Optional tweaks =====
# Legend at bottom (nice for many species):
# p2 <- p + theme(legend.position = "bottom")
# ggsave("euk_pathogens_abs_oneplot_bottomlegend.png", p2, width = 10, height = 7, dpi = 300)

# Add a panel tag (e.g., "E"):
# p_tag <- p + labs(tag = "E") +
#   theme(plot.tag = element_text(face = "bold", size = 16),
#         plot.tag.position = c(0.01, 0.98))
# ggsave("euk_pathogens_abs_oneplot_E.png", p_tag, width = 9.5, height = 6.2, dpi = 300)

