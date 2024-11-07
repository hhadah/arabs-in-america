# This a script to downlaod
# packages and set working
# directories

# date: May 18th, 2022

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tictoc, parallel, pbapply, future, 
               future.apply, furrr, RhpcBLASctl, memoise, 
               here, foreign, mfx, tidyverse, hrbrthemes, 
               estimatr, ivreg, fixest, sandwich, lmtest, 
               margins, vtable, broom, modelsummary, 
               stargazer, fastDummies, recipes, dummy, 
               gplots, haven, huxtable, kableExtra, 
               gmodels, survey, gtsummary, data.table, 
               tidyfast, dtplyr, microbenchmark, ggpubr, 
               tibble, viridis, wesanderson, censReg, 
               rstatix, srvyr, formatR, sysfonts, 
               showtextdb, showtext, thematic, 
               sampleSelection, textme, paletteer, 
               wesanderson, patchwork, RStata, car,
               #textme, lodown,
               BiocManager, Polychrome, effects,
               maps, sf, multcomp, cdlTools,
               finalfit, ggtext, glue, scales, 
               gganimate, ggrepel, MetBrewer, fs,
               marginaleffects, gghighlight, ggview,
               camcorder, rnaturalearth, rnaturalearthdata,
               latex2exp, igraph, httpgd, scales, lemon, beep, 
               network, stringr)
options("RStata.StataPath" = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
options("RStata.StataVersion" = 17)

# devtools::install_github("thomasp85/patchwork")
# devtools::install_github("ajdamico/lodown")
## My preferred ggplot2 plotting theme (optional)
## https://github.com/hrbrmstr/hrbrthemes
# scale_fill_ipsum()
# scale_color_ipsum()
font_add_google("Fira Sans", "firasans")
font_add_google("Fira Code", "firasans")

showtext_auto()
options(modelsummary_format_numeric_latex = "mathmode")
font_add_google("Fira Sans", "firasans")
font_add_google("Fira Code", "firasans")

showtext_auto()

theme_customs <- function() {
  theme_minimal(base_family = "serif") +
    theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 40),
          plot.subtitle = element_text(size = 30),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(
            color="black", fill="white", linewidth=1.5
          ),
          legend.title = element_text(size = 40),
          axis.text.y  = element_text(size = 40),
          axis.text.x  = element_text(size = 40),
          axis.title.x = element_text(size = 46),
          axis.title.y = element_text(size = 46),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 30),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(0.1, "cm"),
          plot.caption =
            ggtext::element_textbox(
              size = 30,
              width = unit(1.0, "npc"),
              hjust = 0, vjust = 1.2,
              halign = 0, lineheight = 0.5
            )
    )
}

theme_customs_map <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.title = element_text(face = "bold", size = rel(1.5)),
          legend.text = element_text(size = rel(1.2)))
}

bold_labels <- function(breaks) {
  lapply(breaks, function(label) {
    if (label == "Full Sample") {
      return(bquote(bold(.(label))))
    } else {
      return(as.character(label))
    }
  })
}

# Make labels use IBM Plex Sans by default
update_geom_defaults("label", 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults(ggtext::GeomRichText, 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults("label_repel", 
                     list(family = "IBM Plex Sans Condensed"))

# Use the Johnson color palette
clrs <- met.brewer("Johnson")

# theme_set(theme_minimal() + theme_customs)
# theme_set(hrbrthemes::theme_ipsum() + theme_customs)
## Set master directory where all sub-directories are located
# mdir <- "/Users/hhadah/Dropbox/Research/My Research Data and Ideas/"
# mdir <- "C:\\Users\\16023\\Dropbox\\Research\\My Research Data and Ideas\\Identification_Paper"
## Set working directories
# workdir  <- paste0(mdir,"/Data/Datasets")
# rawdatadir  <- paste0(mdir,"/Data/Raw")
# tables_plots_dir <- paste0(mdir, "/Users/hhadah/Documents/GiT/Depression_Idea/outputs")

## Set working directory

# COLOR PALLETES
library(paletteer) 
# paletteer_d("awtools::a_palette")
# paletteer_d("suffrager::CarolMan")

### COLOR BLIND PALLETES
paletteer_d("colorblindr::OkabeIto")
paletteer_d("colorblindr::OkabeIto_black")
paletteer_d("colorBlindness::paletteMartin")
paletteer_d("colorBlindness::Blue2DarkRed18Steps")
paletteer_d("colorBlindness::SteppedSequential5Steps")
paletteer_d("colorBlindness::PairedColor12Steps")
paletteer_d("colorBlindness::ModifiedSpectralScheme11Steps")
colorBlindness <- paletteer_d("colorBlindness::Blue2Orange12Steps")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

random_colors <- function(n) {
  set.seed(123) # Set the seed for reproducibility
  pallette <- c("#A6611A", "#D01C8B", "#7B3294", "#E66101", "#CA0020", 
                "#CA0020", "#D7191C", "#D7191C", "#D7191C", "#7FC97F", "#1B9E77", 
                "#A6CEE3", "#FBB4AE", "#B3E2CD", "#E41A1C", "#66C2A5", "#8DD3C7", 
                "#EFF3FF", "#EDF8FB", "#EDF8FB", "#F0F9E8", "#EDF8E9", "#F7F7F7", 
                "#FEEDDE", "#FEF0D9", "#F1EEF6", "#F6EFF7", "#F1EEF6", "#F2F0F7", 
                "#FEEBE2", "#FEE5D9", "#FFFFCC", "#FFFFCC", "#FFFFD4", "#FFFFB2", 
                "#DFC27D", "#F1B6DA", "#C2A5CF", "#FDB863", "#F4A582", "#F4A582", 
                "#FDAE61", "#FDAE61", "#FDAE61", "#BEAED4", "#D95F02", "#1F78B4", 
                "#B3CDE3", "#FDCDAC", "#377EB8", "#FC8D62", "#FFFFB3", "#BDD7E7", 
                "#B2E2E2", "#B3CDE3", "#BAE4BC", "#BAE4B3", "#CCCCCC", "#FDBE85", 
                "#FDCC8A", "#BDC9E1", "#BDC9E1", "#D7B5D8", "#CBC9E2", "#FBB4B9", 
                "#FCAE91", "#C2E699", "#A1DAB4", "#FED98E", "#FECC5C", "#80CDC1", 
                "#B8E186", "#A6DBA0", "#B2ABD2", "#92C5DE", "#BABABA", "#ABD9E9", 
                "#A6D96A", "#ABDDA4", "#FDC086", "#7570B3", "#B2DF8A", "#CCEBC5", 
                "#CBD5E8", "#4DAF4A", "#8DA0CB", "#BEBADA", "#6BAED6", "#66C2A4", 
                "#8C96C6", "#7BCCC4", "#74C476", "#969696", "#FD8D3C", "#FC8D59", 
                "#74A9CF", "#67A9CF", "#DF65B0", "#9E9AC8", "#F768A1", "#FB6A4A", 
                "#78C679", "#41B6C4", "#FE9929", "#FD8D3C", "#018571", "#4DAC26", 
                "#008837", "#5E3C99", "#0571B0", "#404040", "#2C7BB6", "#1A9641", 
                "#2B83BA", "#FFFF99", "#E7298A", "#33A02C", "#DECBE4", "#F4CAE4", 
                "#984EA3", "#E78AC3", "#FB8072", "#2171B5", "#238B45", "#88419D", 
                "#2B8CBE", "#238B45", "#525252", "#D94701", "#D7301F", "#0570B0", 
                "#02818A", "#CE1256", "#6A51A3", "#AE017E", "#CB181D", "#238443", 
                "#225EA8", "#CC4C02", "#E31A1C")
  selected_colors <- sample(pallette, n, replace = FALSE)
  return(selected_colors)
}

# scale_colour_paletteer_d("colorBlindness::ModifiedSpectralScheme11Steps", dynamic = FALSE)
# To use for fills, add
scale_fill_manual(values="colorBlindness::Blue2Orange12Steps")
scale_color_paletteer_d("nord::aurora")
scale_color_paletteer_d("colorBlindness::Blue2Orange12Steps")

# To use for line and point colors, add
scale_colour_manual(values="colorBlindness::Blue2Orange12Steps")

install.packages("rcartocolor")
library(rcartocolor)
display_carto_all(colorblind_friendly = TRUE)

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

P8 = unname(createPalette(6,  c("#ff0000", "#00ff00", "#0000ff")))

scales::show_col(P8)
# Wrapper function
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
