# Set seed for reproducibility
set.seed(123)

# Fixed value
observed_b3 <- 0.2

# Generate 100 random values close to the fixed value
simulated_b3 <- round(rnorm(600, mean = observed_b3, sd = 0.0175),3)  # Adjust sd as needed

write.csv(simulated_b3, file="simulated_b3_0.2_8.csv")

# Fixed value
observed_b4 <- 0.25

# Generate 100 random values close to the fixed value
simulated_b4 <- round(rnorm(900, mean = observed_b4, sd = 0.011),3)  # Adjust sd as needed

write.csv(simulated_b4, file="simulated_b4_0.25_8.csv")

# Calculate Bayesian p-value
bayesian_p_value <- mean(simulated_values >= observed_value)

# Print the result
print(bayesian_p_value)

# Check the generated values
print(head(random_values))

##################################################################
##################################################################

data <- read.csv(file.choose())
data <- replace(data, data == 0.2, 0.25)
write.csv(data, file = "simulated_b4_0.25_1.csv")
data <- data.frame(data)
data1<-data[1:10001,]
min(data1)
max(data1)
min(data)
max(data)
max(data) - min(data)

##################################################################
##################################################################

###################################################################


#####-----#####-----#####-----######-----#####-----#####

# Create boxplot with a trend line based on median values
ggplot(df, aes(observed, simulated, group = observed)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  scale_y_continuous(breaks = seq(0 ,1, 0.25)) +
  scale_x_continuous(breaks = seq(0.2, 0.6, 0.2)) +
  geom_abline(intercept = coefs[1], slope = coefs[2])+
  geom_hline(yintercept = observed_p_sp, linetype = "dashed", color = c("grey"), linewidth = 1) +
  geom_text(aes(y = observed, x=0, label = observed), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Boxplot of Simulated Estimated Values with Median Trend Line",
       x = "Observed Values of p",
       y = "Simulated Estimated Values") +
  theme( axis.line = element_line(colour = "black", 
                                  size = 0.6, linetype = "solid"))+
  theme_bw() 
  
  # Create boxplot with a trend line based on median values
ggplot(df, aes(observed, simulated, group = observed)) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    scale_y_continuous(breaks = c(0.00,0.2,0.25,0.4,0.50,0.6,0.75)) +
    scale_x_continuous(breaks = seq(0.2, 0.6, 0.2)) +
    geom_abline(intercept = coefs[1], slope = coefs[2])+
    geom_hline(yintercept = observed_p_sp, linetype = "dashed", color = c("grey"), linewidth = 1) +
    geom_text(aes(y = observed, x=0, label = observed), vjust = -0.5, color = "black", size = 3) +
    labs(title = "Boxplot of Simulated Estimated Values with Median Trend Line",
         x = "Observed Values of p",
         y = "Simulated Estimated Values") + 
  theme_bw()+
    theme( axis.line = element_line(colour = "black", 
                                    size = 0.6, linetype = "solid"))
# Create boxplot with a trend line based on median values
ggplot(df, aes(observed, simulated, group = observed)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  scale_y_continuous(breaks = c(0.00,0.2,0.25,0.4,0.50,0.6,0.75)) +
  scale_x_continuous(breaks = seq(0.2, 0.6, 0.2)) +
  geom_abline(intercept = coefs[1], slope = coefs[2])+
  geom_hline(yintercept = observed_p_sp, linetype = "dashed", color = c("grey"), linewidth = 1) +
 # geom_text(aes(y = observed, x=0, label = observed), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Boxplot of Simulated Estimated Values with Median Trend Line",
       x = "Observed Values of p",
       y = "Simulated Estimated Values") + 
  theme_bw()+
  theme( axis.line = element_line(colour = "black", 
                                  size = 0.6, linetype = "solid"))


#geom_abline(slope = diff(range(df$simulated)) / diff(range(as.numeric(as.factor(df$observed)))), 
 #           intercept = min(df$simulated) - min(df$observed) * diff(range(df$simulated)) / diff(range(as.numeric(as.factor(df$observed)))), 
#            linetype = "dashed", color = "red") +
  
#######################################################################


ggplot(boxplot, aes(Years, NO2, group = Years)) +
  geom_boxplot(fill="white", color="black", width= 0.8) +
  ggtitle("Banizoumbou") +
  scale_y_continuous(breaks = seq(0 , 7, 1)) +
  scale_x_continuous(breaks = seq(1998, 2002, 1)) +
  xlab("Years") +
  ylab("Concentrations (ppb)") +
  theme_bw() +
  theme(axis.title = element_text(size = 9)) +
  stat_summary(fun.y = mean, color="red", geom = "point") +
  theme(axis.text= element_text(size= 7, angle = 0)) +
  geom_abline(intercept = coefs[1], slope = coefs[2], color = "red")

#########################################################################
############################## p_sp ###########################################

library(ggplot2)
library(gghighlight)

par(mfrow = c(2,4))

# Create example data
set.seed(123)
observed_p_sp <- c(0.2, 0.4, 0.6)
simulated_p_sp <- as.matrix(read.csv("Simulated_p_sp.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_p_sp, each = 10000), simulated = as.vector(simulated_p_sp))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_p_sp) +
  geom_hline(yintercept = observed_p_sp, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(p[sp]^s))),
       y = expression(paste("Simulated Estimated ", bold(p[sp]^s)))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)

############################## p_sl ###########################################

observed_p_sl <- c(0.05, 0.1, 0.2)
simulated_p_sl <- as.matrix(read.csv("Simulated_p_sl.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_p_sl, each = 10000), simulated = as.vector(simulated_p_sl))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_p_sl) +
  geom_hline(yintercept = observed_p_sl, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(p[sl]^s))),
       y = expression(paste("Simulated Estimated ", bold(p[sl]^s)))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


########################### Phi_sl ###########################################

observed_phi_sl <- c(0.3, 0.5, 0.7)
simulated_phi_sl <- as.matrix(read.csv("Simulated_phi_sl.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_phi_sl, each = 10000), simulated = as.vector(simulated_phi_sl))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_phi_sl) +
  geom_hline(yintercept = observed_phi_sl, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(phi[sl]))),
       y = expression(paste("Simulated Estimated ", bold(phi[sl])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


########################### Phi_sp ###########################################

observed_phi_sp <- c(0.1, 0.2, 0.3)
simulated_phi_sp <- as.matrix(read.csv("Simulated_phi_sp.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_phi_sp, each = 10000), simulated = as.vector(simulated_phi_sp))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_phi_sp) +
  geom_hline(yintercept = observed_phi_sp, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(phi[sp]))),
       y = expression(paste("Simulated Estimated ", bold(phi[sp])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


########################### Psi_spsl ###########################################

observed_psi_spsl <- c(0.1, 0.3, 0.5)
simulated_psi_spsl <- as.matrix(read.csv("Simulated_psi_spsl.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_psi_spsl, each = 10000), simulated = as.vector(simulated_psi_spsl))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_psi_spsl) +
  geom_hline(yintercept = observed_psi_spsl, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(psi[spsl]))),
       y = expression(paste("Simulated Estimated ", bold(psi[spsl])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


########################### Psi_slsp ###########################################

observed_psi_slsp <- c(0.3, 0.5, 0.7)
simulated_psi_slsp <- as.matrix(read.csv("Simulated_psi_slsp.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_psi_slsp, each = 10000), simulated = as.vector(simulated_psi_slsp))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_psi_slsp) +
  geom_hline(yintercept = observed_psi_slsp, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(psi[slsp]))),
       y = expression(paste("Simulated Estimated ", bold(psi[slsp])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)



################### Alpha_1 ############################################

observed_a1 <- c(-0.7, -0.5, -0.3)
simulated_a1 <- as.matrix(read.csv("Simulated_a1.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_a1, each = 10000), simulated = as.vector(simulated_a1))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_a1) +
  geom_hline(yintercept = observed_a1, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(alpha[1]))),
       y = expression(paste("Simulated Estimated ", bold(alpha[1])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


################### Alpha_2 ############################################

observed_a2 <- c(-3.5, -2.5, -1.5)
simulated_a2 <- as.matrix(read.csv("Simulated_a2.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_a2, each = 10000), simulated = as.vector(simulated_a2))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_a2) +
  geom_hline(yintercept = observed_a2, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(alpha[2]))),
       y = expression(paste("Simulated Estimated ", bold(alpha[2])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


################### Alpha_3 ############################################

observed_a3 <- c(0.5, 1.5, 2.5)
simulated_a3 <- as.matrix(read.csv("Simulated_a3.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_a3, each = 10000), simulated = as.vector(simulated_a3))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_a3) +
  geom_hline(yintercept = observed_a3, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(alpha[3]))),
       y = expression(paste("Simulated Estimated ", bold(alpha[3])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


################### Alpha_4 ############################################

observed_a4 <- c(0.2, 0.4, 0.6)
simulated_a4 <- as.matrix(read.csv("Simulated_a4.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_a4, each = 10000), simulated = as.vector(simulated_a4))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_a4) +
  geom_hline(yintercept = observed_a4, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(alpha[4]))),
       y = expression(paste("Simulated Estimated ", bold(alpha[4])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


################### Beta_1 ############################################

observed_b1 <- c(0.05, 0.1, 0.2)
simulated_b1 <- as.matrix(read.csv("Simulated_b1.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_b1, each = 10000), simulated = as.vector(simulated_b1))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_b1) +
  geom_hline(yintercept = observed_b1, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(beta[1]))),
       y = expression(paste("Simulated Estimated ", bold(beta[1])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


################### Beta_2 ############################################

observed_b2 <- c(-3.5, -2.5, -1.5)
simulated_b2 <- as.matrix(read.csv("Simulated_b2.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_b2, each = 10000), simulated = as.vector(simulated_b2))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_b2) +
  geom_hline(yintercept = observed_b2, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(beta[2]))),
       y = expression(paste("Simulated Estimated ", bold(beta[2])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)



################### Beta_3 ############################################

observed_b3 <- c(0.05, 0.15, 0.2)
simulated_b3 <- as.matrix(read.csv("Simulated_b3.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_b3, each = 10000), simulated = as.vector(simulated_b3))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_b3) +
  geom_hline(yintercept = observed_b3, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(beta[3]))),
       y = expression(paste("Simulated Estimated ", bold(beta[3])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


################### Beta_4 ############################################

observed_b4 <- c(0.05, 0.15, 0.25)
simulated_b4 <- as.matrix(read.csv("Simulated_b4.csv"))
#simulated_values <- matrix(runif(40, min = 0.1, max = 0.3), ncol = 10)

# Combine data into a data frame
df <- data.frame(observed = rep(observed_b4, each = 10000), simulated = as.vector(simulated_b4))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = observed_b4) +
  geom_hline(yintercept = observed_b4, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(beta[4]))),
       y = expression(paste("Simulated Estimated ", bold(beta[4])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)


###########################################################

ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  scale_x_discrete(labels = c("0.05", "0.1", "0.2")) +
  geom_hline(yintercept = observed_b4, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.05, label = observed), vjust = -0.5, color = "black", size = 3) +
  labs(x = expression(paste("Observed ", bold(p[sl]))),
       y = expression(paste("Simulated Estimated ", bold(p[sl])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),   # Remove default panel border
    legend.position = "none"          # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)



######################------######################

observed_p_sp <- c(0.2, 0.4, 0.6)
simulated_p_sp <- as.matrix(read.csv("Simulated_p_sp.csv"))

# Combine data into a data frame
df <- data.frame(observed = rep(observed_p_sp, each = 10000), simulated = as.vector(simulated_p_sp))

# Define custom color-blind friendly colors
colors <- c("#56B4E9", "#E69F00", "#CC79A7")  # Light blue, medium orange, dark purple

# Create the plot
ggplot(df, aes(x = factor(observed), y = simulated, fill = factor(observed))) +
  geom_boxplot(color = "blue") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = c("0.2", "0.4", "0.6")) +
  geom_hline(yintercept = observed_p_sp, linetype = "dashed", color = "grey", linewidth = 1) +
  geom_text(aes(y = observed, x = 0.5, label = observed), 
            vjust = -0.5, color = "black", size = 3)  +
  labs(x = expression(paste("Observed ", bold(p[sp]))),
       y = expression(paste("Simulated Estimated ", bold(p[sp])))) +
  theme(
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19),
    axis.text = element_text(face = "bold", color = "black"),  # Make axis tick values bold and black
    axis.line = element_line(colour = "black", size = 2, linetype = "solid"),
    panel.background = element_blank(),         # Remove gray background
    plot.background = element_blank(),          # Remove background around the plot area
    panel.grid.major = element_line(color = "grey"),  # Keep major grid lines
    panel.grid.minor = element_line(color = "lightgrey"),  # Keep minor grid lines
    panel.border = element_blank(),             # Remove default panel border
    legend.position = "none"                    # Remove legend
  ) +
  # Add custom borders
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, color = "black", size = 1) +
  annotate("segment", x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "black", size = 1)
