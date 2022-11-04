packages <- c("tidyverse", "ggpubr", "webshot", "ggrepel", "scales", "here", "readxl", "imputeTS", "padr", "survival", "stringr", "lubridate", "NADA", "gt", "webshot", "zoo", "rlang", "multcompView", "rcompanion", "forecast", "smooth", "KbMvtSkew")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

csw_original_values <- read_excel(here("Data", "CSW_2020.xlsx"))%>% mutate(
  Datetime = as_datetime(Datetime)
) %>% select(Datetime, Velocity) %>% rename("Velocity_before" = "Velocity")

csw_wregression_result <- read_excel(here("Output", "CSW_2020_withRegression_QAQC.xlsx"))
csw_wregression_result <- csw_wregression_result %>% mutate(
  Datetime = as_datetime(Datetime)
) %>% select(Datetime, Velocity, Rain, isStorm, ZScore, ZScore_bool, emwa, ewmstd) %>% rename("ewma" = "emwa", "Velocity_after" = "Velocity")

before_and_after_csw <- merge(csw_original_values, csw_wregression_result, by = "Datetime") %>% group_by(isStorm) %>% mutate(
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Velocity_before < 0) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Velocity_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Velocity_before, na.rm = T)
  ),
  ZScore_bool = case_when(
    ZScore_bool == T ~ "Outlier",
    T ~ "Not Outlier"
  ),
) %>% ungroup()

set.seed(1)

randomly_selected_storms <- as.vector(
  before_and_after_csw %>% filter(contains_outliers == T & grepl("Storm", isStorm)) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  maximum <- as.numeric(before_and_after_csw %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 5
  plot <-  before_and_after_csw %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Velocity_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.05))
      ) + 
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_REDONE", "CSW", "Outlier_detection.png"), height = 50, width = 70, units = "cm")



imputed_values <- merge(csw_original_values, csw_wregression_result, by = "Datetime") %>% mutate(
  Status = case_when(ZScore_bool == TRUE ~ "Imputed",
                     T ~ "Unchanged"
  )
) %>% group_by(isStorm) %>% mutate(
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Velocity_before < 0) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Velocity_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Velocity_before, na.rm = T)
  )
) %>% ungroup()

set.seed(1)

randomly_selected_storms <- as.vector(
  imputed_values %>% filter(contains_outliers == T & grepl("Storm", isStorm)) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  maximum <- as.numeric(imputed_values %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 5
  plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
    geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
    geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
    geom_point(aes(x = Datetime, y = Velocity_after, color = Status), size = 4, alpha = 0.9) +
    scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
    geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
    scale_y_continuous(expand = c(0, 0.01),
                       name = "Velocity (ft/s)",
                       sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.05))
    ) + 
    theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
    facet_wrap(.~isStorm, scales = "free") + 
    guides(color = guide_legend(order = 1)) +
    scale_x_datetime(date_labels = "%b %d %H:%M") +
    labs(color = "Outlier Status", fill = "", linetype = "")
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_REDONE", "CSW", "Imputation.png"), height = 50, width = 70, units = "cm")






commons_original_values <- read_excel(here("Data", "Commons_2020.xlsx"))%>% mutate(
  Datetime = as_datetime(Datetime)
) %>% select(Datetime, Depth) %>% rename("Depth_before" = "Depth")

commons_wregression_result <- read_excel(here("Output", "Commons_2020_withRegression_QAQC.xlsx"))
commons_wregression_result <- commons_wregression_result %>% mutate(
  Datetime = as_datetime(Datetime)
) %>% select(Datetime, Depth, Rain, isStorm, ZScore, ZScore_bool, emwa, ewmstd) %>% rename("ewma" = "emwa", "Depth_after" = "Depth")

before_and_after_commons <- merge(commons_original_values, commons_wregression_result, by = "Datetime") %>% group_by(isStorm) %>% mutate(
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Depth_before < 0) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Depth_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Depth_before, na.rm = T)
  ),
  ZScore_bool = case_when(
    ZScore_bool == T ~ "Outlier",
    T ~ "Not Outlier"
  ),
) %>% ungroup()

set.seed(1)

randomly_selected_storms <- as.vector(
  before_and_after_commons %>% filter(contains_outliers == T & grepl("Storm", isStorm)) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  maximum <- as.numeric(before_and_after_commons %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 5
  plot <-  before_and_after_commons %>% filter(isStorm == storm) %>% ggplot() + 
    geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
    geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
    geom_point(aes(x = Datetime, y = Depth_before, color = ZScore_bool), size = 4, alpha = 0.9) +
    scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
    geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
    scale_y_continuous(expand = c(0, 0.01),
                       name = "Depth (in.)",
                       sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.05))
    ) + 
    theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
    facet_wrap(.~isStorm, scales = "free") + 
    guides(color = guide_legend(order = 1)) +
    scale_x_datetime(date_labels = "%b %d %H:%M") +
    labs(color = "Outlier Status", fill = "", linetype = "")
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_REDONE", "Commons", "Outlier_detection.png"), height = 50, width = 70, units = "cm")



imputed_values <- merge(commons_original_values, commons_wregression_result, by = "Datetime") %>% mutate(
  Status = case_when(ZScore_bool == TRUE ~ "Imputed",
                     T ~ "Unchanged"
  )
) %>% group_by(isStorm) %>% mutate(
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Depth_before < 0) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Depth_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Depth_before, na.rm = T)
  )
) %>% ungroup()

set.seed(1)

randomly_selected_storms <- as.vector(
  imputed_values %>% filter(contains_outliers == T & grepl("Storm", isStorm)) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  maximum <- as.numeric(imputed_values %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 5
  plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
    geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
    geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
    geom_point(aes(x = Datetime, y = Depth_after, color = Status), size = 4, alpha = 0.9) +
    scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
    geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
    scale_y_continuous(expand = c(0, 0.01),
                       name = "Depth (in.)",
                       sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.05))
    ) + 
    theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
    facet_wrap(.~isStorm, scales = "free") + 
    guides(color = guide_legend(order = 1)) +
    scale_x_datetime(date_labels = "%b %d %H:%M") +
    labs(color = "Outlier Status", fill = "", linetype = "")
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_REDONE", "Commons", "Imputation.png"), height = 50, width = 70, units = "cm")

