# Generate the polyglotr hex sticker via the hexSticker R package.
#
# Prerequisites (install once or use the micromamba env in hexsticker-env.yml):
#   install.packages(c("hexSticker", "showtext", "sysfonts", "magick", "rsvg"))
#
# Run from the package root:
#   Rscript helperfunctions/make_hex_sticker.R

library(hexSticker)
library(showtext)
library(sysfonts)
library(magick)  # for SVG → raster conversion

# ---------------------------------------------------------------------------
# 1. Font setup
# ---------------------------------------------------------------------------
# Pull a clean, legible web font; works offline after first download.
font_add_google("Nunito", "nunito")
showtext_auto()

# ---------------------------------------------------------------------------
# 2. Convert the inner SVG to a raster image that hexSticker can render
#    (hexSticker < 0.5 does not support SVG subplots natively).
# ---------------------------------------------------------------------------
inner_svg  <- "man/figures/polyglotr-logo-inner.svg"
inner_png  <- tempfile(fileext = ".png")

magick::image_read_svg(inner_svg, width = 600, height = 600) |>
  magick::image_write(inner_png, format = "png")

# ---------------------------------------------------------------------------
# 3. Build the hex sticker
# ---------------------------------------------------------------------------
sticker(
  # --- inner image ---
  subplot  = inner_png,
  s_x      = 1,        # horizontal centre of subplot (1 = hex centre)
  s_y      = 1.22,     # vertical position (>1 = higher)
  s_width  = 0.72,     # fraction of hex width
  s_height = 0.72,

  # --- package name ---
  package   = "polyglotr",
  p_size    = 18,
  p_x       = 1,
  p_y       = 0.52,    # lower portion of hex
  p_color   = "#ffffff",
  p_family  = "nunito",
  p_fontface = "bold",

  # --- hex styling ---
  h_fill  = "#03045e",
  h_color = "#caf0f8",
  h_size  = 1.2,

  # --- output ---
  filename = "man/figures/hex-polyglotr-new.png",
  dpi      = 600,
  asp      = 0.857    # width / height ratio of a pointy-top hex sticker
)

message("Hex sticker written to man/figures/hex-polyglotr-new.png")
message("Preview with: magick::image_read('man/figures/hex-polyglotr-new.png')")
