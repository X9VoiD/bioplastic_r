library(ggplot2)
library(dplyr)

###########################
# Environment preparation #
###########################

if (!dir.exists("output")) {
  dir.create("output")
} else {
  unlink("output", recursive = TRUE)
  dir.create("output")
}

output_file <- file("output/output.txt", open = "wt")
sink(output_file)
sink(output_file, type = "message")

########
# Data #
########

# Chitosan
study_1_data <- data.frame(
  tensile_strength = c(24.47, 25.8, 26.3, 26.4, 24.39),
  tensile_strength_sem = c(0.067, 0.1, 0.25, 0.05, 0.05),
  solubility = c(42.94, 44.98, 48.97, 51.54, 52.60),
  solubility_sem = c(0.64, 0.19, 0.65, 0.54, 0.5),
  biodegradability = c(89.91, 84.8, 83.85, 83.85, 80.95),
  label = c("CG", "CG1", "CG2", "CG3", "CG4"),
  label_numbered = c(0, 1, 2, 3, 4)
)

# Oxidized
study_2_data <- data.frame(
  tensile_strength = c(1.14, 1.76, 1.88, 1.88),
  tensile_strength_sem = c(0.26, 0.04, 0.15, 0.15),
  solubility = c(13.48, 9.88, 9.55, 5.7),
  solubility_sem = c(0.63, 0.95, 0.21, 1.63),
  biodegradability = c(83.55, NULL, NULL, 93.86),
  label = c("F0", "F20", "F40", "F60"),
  label_numbered = c(0, 1, 2, 3)
)

#############
# Functions #
#############

report_regression <- function(regression) {
  print(summary(regression))
  a <- coef(regression)[2]
  b <- coef(regression)[1]
  cat(paste0("Model: y = ", a, "x + ", b), "\n\n")
}

plot_with_regression <- function(data, filename_base) {
  message(paste("\n|- ANALYSIS OF", filename_base))
  message(":-- Tensile Strength")
  ggplot(data, aes(label, tensile_strength, group = 1, weight = 1 / tensile_strength_sem^2)) +
    geom_line(color = "blue") +
    geom_point() +
    geom_linerange(aes(ymax = tensile_strength + tensile_strength_sem,
                       ymin = tensile_strength - tensile_strength_sem)) +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                fill = NA,
                alpha = 0.5,
                color = "green") +
    theme(text = element_text(size = 11, family = "Serif"),
          axis.title.y = element_text(vjust = 1),
          axis.title.x = element_text(vjust = -1),
          axis.text = element_text(size = rel(0.7))) +
    labs(x = "Group", y = "Tensile Strength (MPa)")
  ggsave(paste0("output/", filename_base, "_tensile.png"), width = 6.5, height = 4, units = "in", dpi = "print", type = "cairo-png")
  report_regression(lm(tensile_strength ~ label_numbered, data = data, weight = 1 / tensile_strength_sem^2))

  message(":-- Solubility")
  ggplot(data, aes(label, solubility, group = 1, weight = 1 / solubility_sem^2)) +
    geom_line(color = "blue") +
    geom_point() +
    geom_linerange(aes(ymax = solubility + solubility_sem,
                       ymin = solubility - solubility_sem)) +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                fill = NA,
                alpha = 0.5,
                color = "green") +
    theme(text = element_text(size = 11, family = "Serif"),
          axis.title.y = element_text(vjust = 1),
          axis.title.x = element_text(vjust = -1),
          axis.text = element_text(size = rel(0.7))) +
    labs(x = "Group", y = "Solubility (%)")
  ggsave(paste0("output/", filename_base, "_solubility.png"), width = 6.5, height = 4, units = "in", dpi = "print", type = "cairo-png")
  report_regression(lm(solubility ~ label_numbered, data = data, weight = 1 / solubility_sem^2))
  
  message(":-- Biodegradability")
  ggplot(data, aes(label, biodegradability, group = 1)) +
    geom_line(color = "blue") +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                fill = NA,
                alpha = 0.5,
                color = "green") +
    theme(text = element_text(size = 11, family = "Serif"),
          axis.title.y = element_text(vjust = 1),
          axis.title.x = element_text(vjust = -1),
          axis.text = element_text(size = rel(0.7))) +
    labs(x = "Group", y = "Biodegradability (%)")
  ggsave(paste0("output/", filename_base, "_biodegradability.png"), width = 6.5, height = 4, units = "in", dpi = "print", type = "cairo-png")
  report_regression(lm(biodegradability ~ label_numbered, data = data))
}

##############
# Processing #
##############

plot_with_regression(study_1_data, "study_1")
plot_with_regression(study_2_data, "study_2")

###########
# Cleanup #
###########

sink()
sink(type = "message")
close(output_file)