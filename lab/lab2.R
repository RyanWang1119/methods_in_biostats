load("Lab2_exercise.RData")
library("ggplot2")
library(splines)

# Plot average arm circumference against age and use the custom theme from the label
plot_arm_age <- ggplot(data = data, aes(x = age, y = arm)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  labs(x = "Age at baseline (in months)",
       y = "Arm circumference at baseline (in cm)",
       color = "Degrees of freedom") +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_y_continuous(limit = c(0, 20), breaks = seq(0, 20, 5)) +
  # Natural spine with increasing degrees of freedom
  geom_smooth(aes(color = "1"),
              method = "glm", formula = y ~ ns(x, df = 1), se = FALSE, linewidth = 1) +
  geom_smooth(aes(color = "2"),
              method = "glm", formula = y ~ ns(x, df = 2), se = FALSE, linewidth = 1) +
  geom_smooth(aes(color = "3"),
              method = "glm", formula = y ~ ns(x, df = 3), se = FALSE, linewidth = 1) +
  # Add the color legend specifications
  scale_color_manual(breaks = c("1", "2", "3"),
                     values = c("#E69F00", "#56B4E9", "#009E73")) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        axis.line = element_line(linewidth = 0.5)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key = element_blank())
plot_arm_age
