#############################################################
# function plot_mortality_vs_incidence - various HDI levels #
#############################################################



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
# plot_mortality_vs_incidence by various HDI
##########
# define the function plot_mortality_vs_incidence_HDI
plot_mortality_vs_incidence_HDI <- function(input, age_range, plot_output_file, table_output_file) {
  # read in the data
  input <- read.csv(input)

  # remove the non-applicable data
  input_cleaned <- subset(input, Grouped.by != "Not applicable")

  # define the desired order of levels for Grouped.by
  desired_order <- c("Low HDI", "Medium HDI", "High HDI", "Very high HDI")  # Adjust the order as needed

  # convert Grouped.by to a factor with the desired order
  input_cleaned$Grouped.by <- factor(input_cleaned$Grouped.by, levels = desired_order)

  # calculate the slope
  slope_data <- input_cleaned %>%
    group_by(Grouped.by) %>%
    summarise("{age_range}_slope" := ifelse(sum(!is.na(Mortality...ASR..World.)) > 0,
                                             coef(lm(Mortality...ASR..World. ~ Incidence...ASR..World.))[2],
                                             NA)) %>%
    rename(Continents = Grouped.by)

  # plot the mortality vs incidence
  # define custom colors
  my_colors <- c("#1F77B4","#D62728", "#FF7F0E", "#2CA02C")

  # ggplot code with custom colors and smoothed lines for Africa and Europe
  p <- ggplot(input_cleaned, aes(x=Incidence...ASR..World., y=Mortality...ASR..World., shape=Grouped.by, color=Grouped.by)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # add smoothed lines (slope) using linear regression
    labs(title=paste("Mortality vs Incidence - ASR (World) per 100,000\nBoth sexes, age", age_range, "in 2022"),
         x="Incidence", y = "Mortality",
         shape="HDI Levels", color="HDI Levels") +
    scale_color_manual(values = my_colors) +  # use custom colors
    scale_shape_manual(values = c(1, 2, 3, 4))+
     theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size=10),
      legend.text = element_text(size=8),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),  # remove major grid lines
      panel.grid.minor = element_blank(),  # remove minor grid lines
      panel.border = element_blank(),  # remove panel border
      axis.line = element_line()  # keep axis lines
    )


  # save the plot to a file
  ggsave(plot_output_file, plot = p, width = 8, height = 6, dpi = 1000)

  # save the slope data to a CSV file
  write.csv(slope_data, table_output_file, row.names = FALSE)

  return(list(plot = p, slope_data = slope_data, plot_output_file = plot_output_file, table_output_file = table_output_file))
}

# for age group 0-39
plot_mortality_vs_incidence_HDI("/content/2022-HDI-0-39.csv", "[0, 39]", "[0,39]_mrtality_vs_incidence_HDI.png", "[0,39]_mrtality_vs_incidence_HDI.csv")

# for age group 40-85+
plot_mortality_vs_incidence_HDI("/content/2022-HDI-40-85+.csv", "[40, 85+]", "[40,85+]_mrtality_vs_incidence_HDI.png", "[40,85+]_mrtality_vs_incidence_HDI.csv")

# # for age group 0-85+
plot_mortality_vs_incidence_HDI("/content/2022-HDI-0-85+.csv", "[0, 85+]", "[0,85+]_mrtality_vs_incidence_HDI.png", "[0,85+]_mrtality_vs_incidence_HDI.csv")