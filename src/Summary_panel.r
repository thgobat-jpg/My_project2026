###############################################################################
# FINAL SUMMARY PANEL
###############################################################################

# This final figure combines the main graphical results of the project:
# A) Spatial distribution of both species on the Swissmap
# B) PCA biplot comparing environmental niches
# C) Random Forest variable importance
# D) Predicted probability of Libellula depressa along distance to Castor fiber

library(cowplot)

figure_finale <- ggdraw() +

  draw_plot(
    p_background_map,
    x = 0.00,
    y = 0.55,
    width = 0.50,
    height = 0.45
  ) +

  draw_plot(
    p_pca_biplot,
    x = 0.50,
    y = 0.55,
    width = 0.50,
    height = 0.45
  ) +

  draw_plot(
    p_importance,
    x = 0.00,
    y = 0.00,
    width = 0.50,
    height = 0.55
  ) +

  draw_plot(
    p_prob_curve,
    x = 0.50,
    y = 0.00,
    width = 0.50,
    height = 0.55
  ) +

  draw_label("A", x = 0.02, y = 0.98, fontface = "bold", size = 16) +
  draw_label("B", x = 0.52, y = 0.98, fontface = "bold", size = 16) +
  draw_label("C", x = 0.02, y = 0.53, fontface = "bold", size = 16) +
  draw_label("D", x = 0.52, y = 0.53, fontface = "bold", size = 16)

x11()
print(figure_finale)
Sys.sleep(7)