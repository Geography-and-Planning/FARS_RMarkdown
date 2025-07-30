setwd("/Users/zhanchaoyang/Desktop")
if (!file.exists("master_fatality_cbsa30.csv")) {
  stop("Error: The file 'master_fatality_cbsa30.csv' does not exist in the working directory. Please ensure the file is available.")
}
msa <- read.csv("master_fatality_cbsa30.csv")

if (!file.exists("youth_msa_county_wide_modified.csv")) {
  stop("Error: The file 'youth_msa_county_wide_modified.csv' does not exist in the working directory. Please ensure the file is available.")
}
county <- read.csv("youth_msa_county_wide_modified.csv")

county_main <- county %>%
  filter(Main == "Main")
# Calculate mean and standard deviation
mean_rate <- mean(msa$rate_overall, na.rm = TRUE)
sd_rate <- sd(msa$rate_overall, na.rm = TRUE)

# Plot
ggplot(msa, aes(x = rate_overall)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "lightgray", color = "black") +
  geom_density(color = "blue", linewidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean_rate, sd = sd_rate),
                color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean_rate, color = "darkgreen", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = mean_rate + sd_rate, color = "orange", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = mean_rate - sd_rate, color = "orange", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Distribution of Fatality Rates with Normal Curve and ±1 SD",
    subtitle = paste0("Mean = ", round(mean_rate, 2),
                      ", SD = ", round(sd_rate, 2),
                      " | Dashed lines represent ±1 SD"),
    x = "Fatality Rate",
    y = "Density"
  ) +
  theme_minimal()


county_mean_rate <- mean(county_main$rate_total, na.rm = TRUE)
county_sd_rate <- sd(county_main$rate_total, na.rm = TRUE)

# Plot
ggplot(county_main, aes(x = rate_total)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "lightgray", color = "black") +
  geom_density(color = "blue", linewidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = county_mean_rate, sd = county_sd_rate),
                color = "red", linetype = "dashed") +
  geom_vline(xintercept = county_mean_rate, color = "darkgreen", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = county_mean_rate + county_sd_rate, color = "orange", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = county_mean_rate - county_sd_rate, color = "orange", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Distribution of Fatality Rates with Normal Curve and ±1 SD",
    subtitle = paste0("Mean = ", round(county_mean_rate, 2),
                      ", SD = ", round(county_sd_rate, 2),
                      " | Dashed lines represent ±1 SD"),
    x = "Fatality Rate",
    y = "Density"
  ) +
  theme_minimal()
