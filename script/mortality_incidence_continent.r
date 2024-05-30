############################################################
# function plot_mortality_vs_incidence - various continent #
############################################################

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
# plot_mortality_vs_incidence - various continent
##########

# define function plot_mortality_vs_incidence
plot_mortality_vs_incidence <- function(incidence_file, mortality_file, group_file, age_range, plot_output_file, table_output_file) {
  
  # read in the data
  incidence <- read.csv(incidence_file)
  mortality <- read.csv(mortality_file)
  group <- read.csv(group_file)

  # sort the data
  sorted_group <- group[order(group$Population), ]
  sorted_incidence <- incidence[order(incidence$Population), ]
  sorted_mortality <- mortality[order(mortality$Population), ]

  # merge the data
  merged_IM <- merge(sorted_incidence, sorted_mortality, by = "Population", all = TRUE)
  group_IM <- merge(merged_IM, sorted_group, by = "Population", all.x = TRUE)

  # calculate the slope
  slope_data <- group_IM %>%
    group_by(Grouped.by) %>%
    summarise("{age_range}_slope" := ifelse(sum(!is.na(ASR..World..per.100.000.y)) > 0,
                                             coef(lm(ASR..World..per.100.000.y ~ ASR..World..per.100.000.x))[2],
                                             NA)) %>%
    rename(Continents = Grouped.by)

  # plot the mortality vs incidence
  # define custom colors(feel free to change based on your preference)
  my_colors <- c("#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

  # plot with custom colors and smoothed lines for Africa and Europe
  p <- ggplot(group_IM, aes(x=ASR..World..per.100.000.x, y=ASR..World..per.100.000.y, shape=Grouped.by, color=Grouped.by)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +  # add smoothed lines (slope) using linear regression
    labs(title=paste("Mortality vs Incidence - ASR (World) per 100,000\nBoth sexes, age", age_range, "in 2022"),
         x="Incidence", y = "Mortality",
         shape="Continents", color="Continents") +
    scale_color_manual(values = my_colors) +  # use the defined custom colors
    scale_shape_manual(values = c(5, 6, 7, 8, 9, 10))+
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
  ggsave(plot_output_file, plot = p, width = 9, height = 6, dpi = 1000)

  # save the slope data to a CSV file
  write.csv(slope_data, table_output_file, row.names = FALSE)

  return(list(plot = p, slope_data = slope_data, plot_output_file = plot_output_file, table_output_file = table_output_file))
}

# for age group 0-39
plot_mortality_vs_incidence("/content/2022-incidence-0-39.csv", "/content/2022-mortality-0-39.csv", "/content/2022-mortality:incidence-allAge.csv", "[0, 39]", "[0,39]_mrtality_vs_incidence.png", "[0,39]_mrtality_vs_incidence.csv")

# for age group 40-85+
plot_mortality_vs_incidence("/content/2022-incidence-40-85+.csv", "/content/2022-mortality-40-85+.csv", "/content/2022-mortality:incidence-allAge.csv", "[40, 85+]", "[40,85+]_mrtality_vs_incidence.png", "[40,85+]_mrtality_vs_incidence.csv")

# for age group 0-85+
plot_mortality_vs_incidence("/content/2022-incidence-allAge.csv", "/content/2022-mortality-allAge.csv", "/content/2022-mortality:incidence-allAge.csv", "[0, 85+]", "[0,85+]_mrtality_vs_incidence.png", "[0,85+]_mrtality_vs_incidence.csv")