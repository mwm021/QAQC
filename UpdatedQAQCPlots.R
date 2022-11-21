packages <- c("tidyverse", "ggforce", "ggpubr", "webshot", "ggrepel", "scales", "here", "readxl", "imputeTS", "padr", "survival", "stringr", "lubridate", "NADA", "gt", "webshot", "zoo", "rlang", "multcompView", "rcompanion", "forecast", "smooth", "KbMvtSkew")

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
) %>% select(Datetime, Velocity, Rain, isStorm, ZScore, ZScore_bool, emwa, ewmstd, CalculatedVelocity_bool) %>% rename("ewma" = "emwa", "Velocity_after" = "Velocity")

before_and_after_csw <- merge(csw_original_values, csw_wregression_result, by = "Datetime") %>% group_by(isStorm) %>% mutate(
  CalculatedVelocity_bool = replace_na(CalculatedVelocity_bool, T),
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Velocity_before < 0)  & !any(CalculatedVelocity_bool == F) ~ T,
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

# randomly_selected_storms <- as.vector(
#   before_and_after_csw %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
# )

randomly_selected_storms <- c("BaseFlow133", "StormFlow62", "StormFlow90", "StormFlow44")

plot_vector <- list()
i <- 1 

for(storm in randomly_selected_storms) {
  type <- ifelse(grepl("Base", storm), "BaseFlow", "StormFlow")
  maximum <- as.numeric(before_and_after_csw %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  print(maximum)
  scale_factor = maximum * 2
  
  if(type == "StormFlow") {
    plot <-  before_and_after_csw %>% filter(isStorm == storm) %>% ggplot() + 
        geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
        geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
        geom_point(aes(x = Datetime, y = Velocity_before, color = ZScore_bool), size = 4, alpha = 0.9) +
        geom_point(aes(x = Datetime, y = Velocity_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
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
  } else {
    plot <-  before_and_after_csw %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Velocity_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Velocity_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)") +
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  }
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_alpha_pointone", "CSW", "Outlier_detection.png"), height = 50, width = 70, units = "cm")



imputed_values <- merge(csw_original_values, csw_wregression_result, by = "Datetime") %>% mutate(
  CalculatedVelocity_bool = replace_na(CalculatedVelocity_bool, T),
  Status = case_when(ZScore_bool == TRUE ~ "Imputed",
                     T ~ "Unchanged"
  )
) %>% group_by(isStorm) %>% mutate(
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Velocity_before < 0) & !any(CalculatedVelocity_bool == F) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Velocity_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Velocity_before, na.rm = T)
  )
) %>% ungroup()

set.seed(1)

# randomly_selected_storms <- as.vector(
#   imputed_values %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
# )

randomly_selected_storms <- c("BaseFlow133", "StormFlow62", "StormFlow90", "StormFlow44")

plot_vector <- list()
i <- 1 

for(storm in randomly_selected_storms) {
  type <- ifelse(grepl("Base", storm), "BaseFlow", "StormFlow")
  maximum <- as.numeric(imputed_values %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 2
  
  if(type == "StormFlow") {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Velocity_after, color = Status), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Velocity_after * 1.2, color = Status), size = 0, alpha = 0) +
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
  } else {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Velocity_after, color = Status), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Velocity_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)") +
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  }
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_alpha_pointone", "CSW", "Imputation.png"), height = 50, width = 70, units = "cm")






commons_original_values <- read_excel(here("Data", "Commons_2020.xlsx"))%>% mutate(
  Datetime = as_datetime(Datetime)
) %>% select(Datetime, Depth) %>% rename("Depth_before" = "Depth")

commons_wregression_result <- read_excel(here("Output", "Commons_2020_withRegression_QAQC.xlsx"))
commons_wregression_result <- commons_wregression_result %>% mutate(
  Datetime = as_datetime(Datetime)
) %>% select(Datetime, Depth, Rain, isStorm, ZScore, ZScore_bool, emwa, ewmstd, CalculatedDepth_bool) %>% rename("ewma" = "emwa", "Depth_after" = "Depth")

before_and_after_commons <- merge(commons_original_values, commons_wregression_result, by = "Datetime") %>% group_by(isStorm) %>% mutate(
  CalculatedDepth_bool = replace_na(CalculatedDepth_bool, T),
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Depth_before < 0) & !any(CalculatedDepth_bool == F) ~ T,
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

set.seed(34)

# randomly_selected_storms <- as.vector(
#   before_and_after_commons %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
# )

randomly_selected_storms <- c("BaseFlow63", "BaseFlow73", "BaseFlow25", "StormFlow24")

plot_vector <- list()
i <- 1 

for(storm in randomly_selected_storms) {
  type <- ifelse(grepl("Base", storm), "BaseFlow", "StormFlow")
  maximum <- as.numeric(before_and_after_commons %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 2
  
  if(type == "StormFlow") {
    plot <-  before_and_after_commons %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Depth_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Depth_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +

      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.05))
      ) + 
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  } else {
    plot <-  before_and_after_commons %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Depth_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Depth_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)") +
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  }
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_alpha_pointone", "Commons", "Outlier_detection.png"), height = 50, width = 70, units = "cm")



imputed_values <- merge(commons_original_values, commons_wregression_result, by = "Datetime") %>% mutate(
  CalculatedDepth_bool = replace_na(CalculatedDepth_bool, T),
  Status = case_when(ZScore_bool == TRUE ~ "Imputed",
                     T ~ "Unchanged"
  )
) %>% group_by(isStorm) %>% mutate(
  contains_outliers = case_when(
    any(ZScore_bool == T) & !any(Depth_before < 0) & !any(CalculatedDepth_bool == F) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Depth_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Depth_before, na.rm = T)
  )
) %>% ungroup()

set.seed(34)

# randomly_selected_storms <- as.vector(
#   imputed_values %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
# )

randomly_selected_storms <- c("BaseFlow63", "BaseFlow73", "BaseFlow25", "StormFlow24")

plot_vector <- list()
i <- 1 

for(storm in randomly_selected_storms) {
  type <- ifelse(grepl("Base", storm), "BaseFlow", "StormFlow")
  maximum <- as.numeric(imputed_values %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 2
  
  if(type == "StormFlow") {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Depth_after, color = Status), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Depth_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.05))
      ) + 
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  } else {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Depth_after, color = Status), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Depth_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)") +
      theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  }
  plot_vector[[i]] <- plot
  i <- i + 1
}

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "top") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_alpha_pointone", "Commons", "Imputation.png"), height = 50, width = 70, units = "cm")


time_scale <- read_excel(here("Output", "CSW_2020_withRegression_QAQC.xlsx")) %>%
  mutate(type = case_when(grepl("StormFlow", isStorm) ~ "1",
                          grepl("BaseFlow", isStorm) ~ "0"
                ),
         use = case_when((Datetime >= "2020-02-07 04:00:00" & Datetime <= "2020-02-07 14:00:00") ~ TRUE)
)



time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + guides(guides(fill=guide_legend(title="Event Type"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Base Flow", "Storm Flow"), values = c(NA, "lightblue")) + 
  facet_zoom(x = isStorm == "StormFlow36", zoom.data = FALSE)
ggsave(here("Plots", "Time", "StormFlow.png"))

time_scale %>% filter(isStorm == "StormFlow36") %>% ggplot() + geom_histogram(aes( x = Velocity)) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)")
ggsave(here("Plots", "Time", "StormFlow_dist.png"))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + guides(guides(fill=guide_legend(title="Event Type"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Base Flow", "Storm Flow"), values = c(NA, "lightblue")) + 
  facet_zoom(x = isStorm == "BaseFlow37", zoom.data = FALSE)
ggsave(here("Plots", "Time", "BaseFlow.png"))

time_scale %>% filter(isStorm == "BaseFlow37") %>% ggplot() + geom_histogram(aes( x = Velocity)) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)")
ggsave(here("Plots", "Time", "BaseFlow_dist.png"))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + guides(guides(fill=guide_legend(title="Event Type"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Base Flow", "Storm Flow"), values = c(NA, "lightblue")) + 
  facet_zoom(x = use== TRUE, zoom.data = FALSE)
ggsave(here("Plots", "Time", "Abitrary.png"))

time_scale %>% filter(use == TRUE) %>% ggplot() + geom_histogram(aes( x = Velocity)) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)")
ggsave(here("Plots", "Time", "Arbitrary_dist.png"))



