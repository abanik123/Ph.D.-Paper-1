# Load required libraries
library(ggplot2)
library(ggtext) # For element_markdown()

#-------------- Firth -----------------#
#--------------------------------------#

alpha1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(1.66, 5.78, 18.32),
  psi_sl_sp_m_Lower = c(-0.613, -0.51, -0.80),
  psi_sl_sp_m_Upper = c(6.33, 21.19, 66.80)
)

# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.44, -2.26, -7.02),
  psi_sl_sp_m_Lower = c(-6.72, -19.89, -56.60),
  psi_sl_sp_m_Upper = c(3.89, 3.83, 3.82)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-1.01, -1.29, 6.67),
  psi_sl_sp_m_Lower = c(-4.32, -4.85, -2.94),
  psi_sl_sp_m_Upper = c(4.73, 9.66, 55.67)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.06, 7.01, 16.47),
  psi_sl_sp_m_Lower = c(-5.39, -4.54, -6.46),
  psi_sl_sp_m_Upper = c(6.29, 22.28, 66.41)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(2.77, 6.95, 27.61),
  psi_sl_sp_m_Lower = c(0.25, 0.11, 4.37),
  psi_sl_sp_m_Upper = c(7.31, 21.83, 71.99)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.09, -0.63, 0.78),
  psi_sl_sp_m_Lower = c(-3.40, -6.63, -0.61),
  psi_sl_sp_m_Upper = c(2.61, 2.35, 2.30)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-1.23, -1.84, -2.02),
  psi_sl_sp_m_Lower = c(-2.388, -2.84, -3.06),
  psi_sl_sp_m_Upper = c(0.28, -0.69, -1.18)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(1.94, 8.18, 25.95),
  psi_sl_sp_m_Lower = c(-1.93, -0.99, 1.92),
  psi_sl_sp_m_Upper = c(6.91, 22.74, 71.44)
)

#-------------- Big Fish -----------------#
#--------------------------------------#

alpha1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.46, -0.44, -0.44),
  psi_sl_sp_m_Lower = c(-0.69, -0.67, -0.66),
  psi_sl_sp_m_Upper = c(-0.23, -0.22, -0.22)
)

# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-3.52, -4.40, -4.53),
  psi_sl_sp_m_Lower = c(-5.17, -6.26, -6.40),
  psi_sl_sp_m_Upper = c(-2.21, -2.98, -3.16)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.67, 7.58, 26.62),
  psi_sl_sp_m_Lower = c(0.18, 1.47, 2.90),
  psi_sl_sp_m_Upper = c(1.21, 22.01, 71.75)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.12, 0.79, 0.86),
  psi_sl_sp_m_Lower = c(-0.52, 0.17, 0.28),
  psi_sl_sp_m_Upper = c(0.89, 1.55, 1.66)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.09, 0.14, 0.16),
  psi_sl_sp_m_Lower = c(-0.06, 0.00, 0.02),
  psi_sl_sp_m_Upper = c(0.25, 0.29, 0.30)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-3.49, -4.32, -4.48),
  psi_sl_sp_m_Lower = c(-4.82, -5.71, -5.84),
  psi_sl_sp_m_Upper = c(-2.28, -3.03, -3.29)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.29, -0.45, -0.51),
  psi_sl_sp_m_Lower = c(-0.26, -0.86, -0.88),
  psi_sl_sp_m_Upper = c(0.89, 0.11, -0.1)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(1.93, 7.61, 26.02),
  psi_sl_sp_m_Lower = c(-0.02, 0.19, 2.47),
  psi_sl_sp_m_Upper = c(6.36, 22.17, 71.30)
)

#-------------- Babbage -----------------#
#--------------------------------------#

alpha1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.05, -0.11, -0.09),
  psi_sl_sp_m_Lower = c(-0.50, -0.46, -0.43),
  psi_sl_sp_m_Upper = c(0.19, 0.20, 0.21)
)

# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.97, -2.82, -3.11),
  psi_sl_sp_m_Lower = c(-3.49, -4.89, -4.96),
  psi_sl_sp_m_Upper = c(0.47, -0.47, -0.64)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.41, 8.88, 26.35),
  psi_sl_sp_m_Lower = c(-0.40, 1.37, 2.98),
  psi_sl_sp_m_Upper = c(1.59, 22.91, 71.19)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(2.94, 7.97, 21.55),
  psi_sl_sp_m_Lower = c(0.38, 1.36, 1.89),
  psi_sl_sp_m_Upper = c(7.22, 22.29, 68.51)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.40, 0.47, 0.48),
  psi_sl_sp_m_Lower = c(0.12, 0.23, 0.25),
  psi_sl_sp_m_Upper = c(0.68, 0.72, 0.73)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-3.09, -4.79, -5.06),
  psi_sl_sp_m_Lower = c(-5.33, -6.98, -7.12),
  psi_sl_sp_m_Upper = c(-1.48, -2.53, -2.74)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.20, -0.65, -0.66),
  psi_sl_sp_m_Lower = c(-0.58, -1.23, -1.23),
  psi_sl_sp_m_Upper = c(0.96, 0.09, 0.03)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.75, 4.30, 7.94),
  psi_sl_sp_m_Lower = c(-0.86, -0.32, -0.22),
  psi_sl_sp_m_Upper = c(4.88, 18.33, 51.13)
)

#-------------- Joe Creek -----------------#
#--------------------------------------#

alpha1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.41, 0.09, -0.48),
  psi_sl_sp_m_Lower = c(-1.96, -1.26, -1.26),
  psi_sl_sp_m_Upper = c(3.14, 9.21, 0.35)
)

# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-2.33, -10.73, -28.87),
  psi_sl_sp_m_Lower = c(-7.86, -24.20, -72.78),
  psi_sl_sp_m_Upper = c(3.31, 1.55, -6.08)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.97, -4.76, -19.37),
  psi_sl_sp_m_Lower = c(-6.42, -21.74, -69.78),
  psi_sl_sp_m_Upper = c(5.07, 15.71, 41.36)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.47, 2.10, 4.20),
  psi_sl_sp_m_Lower = c(-5.75, -18.38, -59.38),
  psi_sl_sp_m_Upper = c(6.46, 20.69, 64.19)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.13, 0.16, 0.19),
  psi_sl_sp_m_Lower = c(-0.78, -0.58, -0.45),
  psi_sl_sp_m_Upper = c(0.96, 0.94, 0.95)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-2.24, -6.16, -7.02),
  psi_sl_sp_m_Lower = c(-6.35, -9.90, -10.47),
  psi_sl_sp_m_Upper = c(1.60, -0.77, -3.55)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(2.18, 7.76, 25.16),
  psi_sl_sp_m_Lower = c(-1.09, -0.61, 0.80),
  psi_sl_sp_m_Upper = c(6.93, 22.30, 70.77)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(1.97, 9.57, 27.28),
  psi_sl_sp_m_Lower = c(-1.71, 0.84, 3.78),
  psi_sl_sp_m_Upper = c(6.95, 23.31, 72.01)
)

#-------------- Rat -----------------#
#--------------------------------------#

alpha1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.51, 0.10, -0.92),
  psi_sl_sp_m_Lower = c(-1.49, -2.45, -2.29),
  psi_sl_sp_m_Upper = c(0.72, 16.50, 0.60)
)

# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(5.25, 27.83, 27.14),
  psi_sl_sp_m_Lower = c(2.45, 4.34, 2.05),
  psi_sl_sp_m_Upper = c(8.97, 72.02, 71.81)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.39, 8.42, 8.03),
  psi_sl_sp_m_Lower = c(-1.06, -1.29, -1.19),
  psi_sl_sp_m_Upper = c(0.27, 56.92, 56.26)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
alpha4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-1.14, -0.07, 0.36),
  psi_sl_sp_m_Lower = c(-2.82, -2.60, -2.57),
  psi_sl_sp_m_Upper = c(0.48, 3.07, 3.76)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta1 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.30, -0.25, -0.26),
  psi_sl_sp_m_Lower = c(-0.55, -0.73, -0.73),
  psi_sl_sp_m_Upper = c(-0.20, 0.21, 0.20)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta2 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.90, 0.86, 0.83),
  psi_sl_sp_m_Lower = c(-2.24, -2.47, -2.45),
  psi_sl_sp_m_Upper = c(2.82, 3.25, 3.23)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta3 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(0.82, 1.55, 8.06),
  psi_sl_sp_m_Lower = c(0.07, -0.06, -0.05),
  psi_sl_sp_m_Upper = c(3.13, 12.68, 55.16)
)
# Create the data frame based on the new values for psi (estimates, lower bounds, and upper bounds)
beta4 <- data.frame(
  Rivers = factor(c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)"),
                  levels = c("Norm(0,10)", "Norm(0,100)", "Norm(0,1000)")),
  psi_sl_sp_m_Estimate = c(-0.18, -0.29, -0.22),
  psi_sl_sp_m_Lower = c(-1.95, -2.42, -2.40),
  psi_sl_sp_m_Upper = c(1.84, 2.65, 2.58)
)


# Create the forest plot with custom colors and y-axis title as "Estimated Values"
ggplot(beta1, aes(x = Rivers, y = psi_sl_sp_m_Estimate, color = Rivers)) +
  geom_point(size = 5) + # Points for estimates
  geom_errorbar(aes(ymin = psi_sl_sp_m_Lower, ymax = psi_sl_sp_m_Upper), width = 0.2) + # Error bars
  scale_color_manual(values = c("darkgreen", "red", "purple")) + # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", face = "bold", size = 14),  # Increase x-axis label font size
    axis.text.y = element_text(color = "black", face = "bold", size = 18),  # Increase y-axis label font size
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_text(color = "black", face = "bold", size = 16),  # Y-axis title with increased size
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Add major grid lines in light gray
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Add minor grid lines in light gray
    axis.line = element_blank(),  # Remove all default axis lines
    legend.position = "none",  # Remove legend
    plot.caption = element_markdown(hjust = 0.5, size = 14, face = "bold")  # Center and format the caption with Markdown
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +  # Black line at y = 0 axis
  geom_vline(xintercept = 0.5, color = "black", size = 0.8) + # Add vertical reference line
  ylab("Estimated Values") # Add y-axis title as "Estimated Values"
