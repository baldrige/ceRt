# Generate the social-card image (og.png, 1200x630) from the site palette.
#
# Run once and commit the result; build_dashboards.R re-asserts it into the site
# root the same way it does analytics.js and robots.txt. Deliberately NOT built
# in CI: it would drag a rasteriser and a webfont into every workflow to produce
# a file that changes about never.
#
#   Rscript docs/make_og_image.R
#
# Georgia, not Fraunces. The brand faces are webfonts and are not installed on a
# runner or, usually, a laptop; Georgia is the site's own declared fallback in
# every font stack it ships, so the card degrades to exactly what the site does.
#
# res = 72 is load-bearing: it makes one point equal one pixel, so every
# coordinate below is directly the pixel it lands on. The first version of this
# used res = 144 with the same numbers and drew a title twice the width of the
# canvas -- the layout was right and the coordinate system was not.

suppressPackageStartupMessages({ library(ragg); library(grid) })
source("R/palette.R")

W <- 1200; H <- 630          # the size every platform crops from
out <- "og.png"

# Anchor from the top edge, because that is how the layout was designed; grid
# measures y from the bottom.
top <- function(px) unit(H - px, "pt")

agg_png(out, width = W, height = H, units = "px", background = pal("paper"), res = 72)
grid.newpage()

# Hairline frame, inset. Gives the card an edge on a white timeline without a
# heavy border.
grid.rect(x = unit(40, "pt"), y = unit(40, "pt"),
          width = unit(W - 80, "pt"), height = unit(H - 80, "pt"),
          just = c("left", "bottom"),
          gp = gpar(col = pal("rule"), fill = NA, lwd = 1.5))

# The cochineal rule: the one place the accent is spent.
grid.rect(x = unit(92, "pt"), y = top(212), width = unit(110, "pt"),
          height = unit(5, "pt"), just = c("left", "top"),
          gp = gpar(col = NA, fill = pal("accent")))

grid.text("Supreme Court Report", x = unit(92, "pt"), y = top(256),
          just = c("left", "top"),
          gp = gpar(fontfamily = "Georgia", fontface = "bold",
                    fontsize = 68, col = pal("ink")))

grid.text("Docket analytics for the Supreme Court of the United States",
          x = unit(92, "pt"), y = top(352), just = c("left", "top"),
          gp = gpar(fontfamily = "Georgia", fontsize = 28, col = pal("ink-soft")))

grid.text("Daily petitions  ·  Conference reports  ·  Oral argument  ·  Cert forecasts",
          x = unit(92, "pt"), y = top(540), just = c("left", "top"),
          gp = gpar(fontfamily = "Georgia", fontsize = 21, col = pal("faint")))

grid.text("supremecourt.report", x = unit(W - 92, "pt"), y = top(540),
          just = c("right", "top"),
          gp = gpar(fontfamily = "Georgia", fontface = "italic",
                    fontsize = 21, col = pal("accent")))

invisible(dev.off())
cat(sprintf("wrote %s (%dx%d, %.0f KB)\n", out, W, H, file.size(out) / 1024))
