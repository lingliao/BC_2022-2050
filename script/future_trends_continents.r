#################################################
# function to plot future_trends_prediction_continents #
#################################################



# install library
install.packages(c("ggplot2"))
install.packages("cowplot")
install.packages("patchwork")

# call the library
library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)
library(patchwork)

##########
# plot_future trends - various continents
##########

# define the function future_trends_prediction_con
future_trends_prediction_con <- function(incidence_file, mortality_file, output_prefix, title_variable) {
  incidence <- read.csv(incidence_file)
  mortality <- read.csv(mortality_file)

  # assuming both data frames have been prepared similarly
  # convert columns to appropriate data types if needed
  incidence$Year <- as.numeric(incidence$Year)
  incidence$Prediction <- as.numeric(incidence$Prediction)
  incidence$Population <- factor(incidence$Population, levels = unique(incidence$Population))

  mortality$Year <- as.numeric(mortality$Year)
  mortality$Prediction <- as.numeric(mortality$Prediction)
  mortality$Population <- factor(mortality$Population, levels = unique(mortality$Population))

  incidence <- incidence %>% mutate(sequence = row_number())
  mortality <- mortality %>% mutate(sequence = row_number())

  # join DataFrame A to DataFrame B based on key_column
  result <- left_join(incidence, mortality[, c("sequence", "Prediction")], by = "sequence")
  result <- result[complete.cases(result), ]

  max_y1 <- max(result$Prediction.x)

  custom_colors <- c("#BCBD22", "#17BECF", "#B25BAA", "#FF7F0E", "#1F77B4", "#2CA02C")

  q <- ggplot(result, aes(x = Year, color = Population, linetype = "Prediction Type")) +
    geom_point(aes(y = Prediction.x), alpha = 0.5) +
    geom_smooth(aes(y = Prediction.x, linetype = "Incidence"), method = "loess", se = FALSE) +
    geom_point(aes(y = Prediction.y)) +
    geom_smooth(aes(y = Prediction.y, linetype = "Mortality"), method = "loess", se = FALSE) +
    scale_linetype_manual(
      name = "Prediction Type",
      values = c("Incidence" = "solid", "Mortality" = "dotted"), # for conteninent
      labels = c("Incidence", "Mortality")
    ) +
    scale_color_discrete(name = "Continents") +
    scale_y_continuous(
      name = "Estimated Numbers",
      limits = c(0, max_y1),
      labels = comma
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size=10),
      legend.text = element_text(size=8),
      plot.title = element_text(hjust = 0.5, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line()
    ) +
    ggtitle(paste("Estimated Numbers of New Cases and Deaths, \nfrom 2022 to 2050, Both Sexes, Age", title_variable))

  # calculate correlation for shared color using Spearman's rank correlation
  correlation_spearman <- result %>%
    group_by(Population) %>%
    summarise(
      correlation_spearman = cor.test(Prediction.x, Prediction.y, method = "spearman")$estimate
    )

  # calculate correlation for shared color using Fisher's exact test
  Pvalue_spearman <- result %>%
    group_by(Population) %>%
    summarise(
      p_value = cor.test(Prediction.x, Prediction.y, method = "spearman")$p.value
    )


  # merge correlation data frames
  merged_correlation_data <- merge(correlation_spearman, Pvalue_spearman, by = "Population")
  # Print merged correlation data
  print(merged_correlation_data)

  # save the plot to a file
  png_file <- paste0(output_prefix, "_plot.png")
  ggsave(png_file, plot = q, width = 6, height = 6, dpi = 1000)

  # save the slope data to a CSV file
  csv_file <- paste0(output_prefix, "_correlation.csv")
  write.csv(merged_correlation_data, csv_file, row.names = FALSE)
}

# run the function future_trends_prediction_con:
# for age group 0-39
future_trends_prediction_con("/content/incidence_0_39_con.csv", "/content/mortality_0_39_con.csv", "0_39_con", "[0-39]")

# for age group 40-85
future_trends_prediction_con("/content/incidence_0_85_con.csv", "/content/mortality_0_85_con.csv", "0_85_con", "[0-85+]")

# for age group 0-85
future_trends_prediction_con("/content/incidence_40_85_con.csv", "/content/mortality_40_85_con.csv", "40_85_con", "[40-85+]")

