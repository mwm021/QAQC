packages <- c("tidyverse", "ggforce", "Metrics", "ggpubr", "webshot", "ggrepel", "scales", "here", "readxl", "imputeTS", "padr", "survival", "stringr", "lubridate", "NADA", "gt", "webshot", "zoo", "rlang", "multcompView", "rcompanion", "forecast", "smooth", "KbMvtSkew")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# transform function
lal_trans_transform <- function(x) case_when(
  x < -1 ~ -log10(abs(x)) - 1,
  x > 1 ~ log10(x) + 1,
  TRUE ~ x
)

# inverse transform
lal_trans_inverse <- function(x) case_when(
  x < -1 ~ -10^(abs(x+1)),
  x > 1 ~ 10^(x-1),
  TRUE ~ x
)

lal_trans = trans_new(
  'lal',
  transform = lal_trans_transform,
  inverse = lal_trans_inverse,
  breaks = function(x) {
    x = x[is.finite(x)]
    
    rng = range(x)
    if (rng[1] < -1){
      min_val = -ceiling(log10(abs(rng[1])+1)) - 1
    } else if (rng[1] < 0){
      min_val = -1
    } else if (rng[1] < 1){
      min_val = 0
    } else {
      min_val = ceiling(log10(rng[1])-1) - 1
    }
    
    if (rng[2] > 1){
      max_val = floor(log10(abs(rng[2]) + 1)) + 1
    } else if (rng[2] > 0){
      max_val = 1
    } else if (rng[2] > -1){
      max_val = 0
    } else {
      max_val = -floor(log10(abs(rng[1]))-1) + 1
    }
    
    breaks = lal_trans_inverse(as.numeric(seq.int(min_val, max_val)))
    return(breaks)
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
    any(ZScore_bool == T) & !any(Velocity_before < 0) & !any(Velocity_before > 30) & !any(CalculatedVelocity_bool == F) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Velocity_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Velocity_before, na.rm = T)
  ),
  ZScore_bool = case_when(
    ZScore_bool == T | Velocity_before > 30 ~ "Outlier",
    T ~ "Not Outlier"
  ),
) %>% mutate(isStorm = sub("F", "f", isStorm)) %>% ungroup()

set.seed(90210)

randomly_selected_storms <- as.vector(
  before_and_after_csw %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

randomly_selected_storms <- list("Baseflow133", "Stormflow62", "Stormflow90", "Stormflow44")

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  type <- ifelse(grepl("Base", storm), "Baseflow", "Stormflow")
  maximum <- as.numeric(before_and_after_csw %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  print(maximum)
  scale_factor = maximum * 2
  
  if(type == "Stormflow") {
    plot <-  before_and_after_csw %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Velocity_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Velocity_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.1))
      ) + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  } else {
    plot <-  before_and_after_csw %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Velocity_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Velocity_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)") + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
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

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "right") + guides(legend = guide_legend(nrow = 1))
ggsave(plot = final_plot, here("Plots_withNewLimits", "CSW", "Outlier_detection.png"), height = 50, width = 70, units = "cm")



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
) %>% ungroup() %>% mutate(isStorm = sub("F", "f", isStorm))

set.seed(90210)

randomly_selected_storms <- as.vector(
  imputed_values %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

randomly_selected_storms <- list("Baseflow133", "Stormflow62", "Stormflow90", "Stormflow44")

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  type <- ifelse(grepl("Base", storm), "Baseflow", "Stormflow")
  maximum <- as.numeric(imputed_values %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 2
  
  if(type == "Stormflow") {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(data = . %>% filter(Status == "Unchanged"), aes(x = Datetime, y = Velocity_after, color = Status), size = 4, alpha = 0.6) +
      geom_point(data = . %>% filter(Status == "Imputed"), aes(x = Datetime, y = Velocity_after, color = Status), size = 4) +
      geom_point(aes(x = Datetime, y = Velocity_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2, alpha = 0.3) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.1))
      ) + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  } else {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(data = . %>% filter(Status == "Unchanged"), aes(x = Datetime, y = Velocity_after, color = Status), size = 4, alpha = 0.6) +
      geom_point(data = . %>% filter(Status == "Imputed"), aes(x = Datetime, y = Velocity_after, color = Status), size = 4) +
      geom_point(aes(x = Datetime, y = Velocity_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2, alpha = 0.3) +
      scale_y_continuous(expand = c(0, 0.01),
                         name = "Velocity (ft/s)") + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
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

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "right") + guides(legend = guide_legend(nrow = 1)) + theme(legend.spacing.y = unit(0, 'cm'))
ggsave(plot = final_plot, here("Plots_withNewLimits", "CSW", "Imputation.png"), height = 50, width = 70, units = "cm")






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
    any(ZScore_bool == T) & !any(Depth_before < 0) & !any(Depth_before > 18) & !any(CalculatedDepth_bool == F) ~ T,
    T ~ F
  ),
  error = ewma + (3 * ewmstd),
  max_v = case_when(
    max(error, na.rm = T) > max(Depth_before, na.rm = T) ~ 1.3 * max(error, na.rm = T),
    T ~ 1.3 * max(Depth_before, na.rm = T)
  ),
  ZScore_bool = case_when(
    ZScore_bool == T | Depth_before > 18 ~ "Outlier",
    T ~ "Not Outlier"
  ),
) %>% ungroup() %>% mutate(isStorm = sub("F", "f", isStorm))

set.seed(999)

randomly_selected_storms <- as.vector(
  before_and_after_commons %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

randomly_selected_storms <- c("Baseflow63", "Baseflow73", "Baseflow25", "Stormflow26")

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  type <- ifelse(grepl("Base", storm), "Baseflow", "Stormflow")
  maximum <- as.numeric(before_and_after_commons %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 2
  
  if(type == "Stormflow") {
    plot <-  before_and_after_commons %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Depth_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Depth_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
      
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.1))
      ) + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  } else {
    plot <-  before_and_after_commons %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(aes(x = Datetime, y = Depth_before, color = ZScore_bool), size = 4, alpha = 0.9) +
      geom_point(aes(x = Datetime, y = Depth_before * 1.2, color = ZScore_bool), size = 0, alpha = 0) +
      scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "darkgreen")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)") + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
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

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "right") + guides(legend = guide_legend(nrow = 1)) + theme(legend.spacing.y = unit(0, 'cm'))
ggsave(plot = final_plot, here("Plots_withNewLimits", "Commons", "Outlier_detection.png"), height = 50, width = 70, units = "cm")



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
) %>% ungroup() %>% mutate(isStorm = sub("F", "f", isStorm))

set.seed(999)

randomly_selected_storms <- as.vector(
  imputed_values %>% filter(contains_outliers == T) %>% select(isStorm) %>% unique() %>% slice_sample(n = 4)
)

randomly_selected_storms <- c("Baseflow63", "Baseflow73", "Baseflow25", "Stormflow26")

plot_vector <- list()
i <- 1 

for(storm in unlist(randomly_selected_storms)) {
  type <- ifelse(grepl("Base", storm), "Baseflow", "Stormflow")
  maximum <- as.numeric(imputed_values %>% filter(isStorm == storm) %>% select(max_v) %>% summarize(max_v = max(max_v)))
  scale_factor = maximum * 2
  
  if(type == "Stormflow") {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_tile(aes(x = Datetime, y = -1 * ((Rain * scale_factor)/2-maximum), height = Rain * scale_factor), color = "black") +
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(data = . %>% filter(Status == "Unchanged"), aes(x = Datetime, y = Depth_after, color = Status), size = 4, alpha = 0.6) +
      geom_point(data = . %>% filter(Status == "Imputed"), aes(x = Datetime, y = Depth_after, color = Status), size = 4) +
      geom_point(aes(x = Datetime, y = Depth_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2, alpha = 0.3) +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)",
                         sec.axis = sec_axis(trans = ~-1*(.-maximum)/scale_factor, name="Rainfall (in)", breaks = seq(0, 5, 0.1))
      ) + xlab("") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) +
      facet_wrap(.~isStorm, scales = "free") + 
      guides(color = guide_legend(order = 1)) +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      labs(color = "Outlier Status", fill = "", linetype = "")
  } else {
    plot <-  imputed_values %>% filter(isStorm == storm) %>% ggplot() + 
      geom_ribbon(aes(x = Datetime, ymin = ewma - (3 * ewmstd), ymax = ewma + (3 * ewmstd), fill = "EW Moving Standard Deviation"), alpha = 0.3) +
      geom_point(data = . %>% filter(Status == "Unchanged"), aes(x = Datetime, y = Depth_after, color = Status), size = 4, alpha = 0.6) +
      geom_point(data = . %>% filter(Status == "Imputed"), aes(x = Datetime, y = Depth_after, color = Status), size = 4) +
      geom_point(aes(x = Datetime, y = Depth_after * 1.2, color = Status), size = 0, alpha = 0) +
      scale_color_manual(values = c("Imputed" = "red", "Unchanged" = "grey")) +
      geom_line(aes(x = Datetime, y = ewma, linetype = "EW Moving Average"), size = 2, alpha = 0.2) +
      xlab("") +
      scale_y_continuous(expand = c(0, 0.1),
                         name = "Depth (in)") +
      theme_bw(base_size = 44) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "right", legend.box = "vertical") +
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

final_plot <- ggarrange(plotlist = plot_vector, common.legend = T, legend = "right") + guides(legend = guide_legend(nrow = 1)) + theme(legend.spacing.y = unit(0, 'cm'))
ggsave(plot = final_plot, here("Plots_withNewLimits", "Commons", "Imputation.png"), height = 50, width = 70, units = "cm")


## mapping change in flow
constant_v_base <- 2 #feet/second
constant_v_storm <- 4 #feet/second
constant_y_base <- 3/12 #feet
constant_y_storm <- 5/12 #feet

r_csw <- (48/12)/2
r_commons <- (18/12)/2

csw_flow <- before_and_after_csw %>% mutate(
  flow_before = case_when(
    grepl("Base", isStorm) ~ ((r_csw^2) * (2*acos((r_csw-constant_y_base)/r_csw)-sin(2*acos((r_csw-constant_y_base)/r_csw)))/2) * Velocity_before,
    grepl("Storm", isStorm) ~ ((r_csw^2) * (2*acos((r_csw-constant_y_storm)/r_csw)-sin(2*acos((r_csw-constant_y_storm)/r_csw)))/2) * Velocity_before,
  ),
  flow_after = case_when(
    grepl("Base", isStorm) ~ ((r_csw^2) * (2*acos((r_csw-constant_y_base)/r_csw)-sin(2*acos((r_csw-constant_y_base)/r_csw)))/2) * Velocity_after,
    grepl("Storm", isStorm) ~ ((r_csw^2) * (2*acos((r_csw-constant_y_storm)/r_csw)-sin(2*acos((r_csw-constant_y_storm)/r_csw)))/2) * Velocity_after
  )
)


commons_flow <- before_and_after_commons %>% mutate(
  flow_before =  case_when(
    grepl("Base", isStorm) ~ ((r_commons^2) * (2*acos((r_commons-((Depth_before)/12))/r_commons)-sin(2*acos((r_commons-((Depth_before)/12))/r_commons)))/2) * constant_v_base,
    grepl("Storm", isStorm) ~ ((r_commons^2) * (2*acos((r_commons-((Depth_before)/12))/r_commons)-sin(2*acos((r_commons-((Depth_before)/12))/r_commons)))/2) * constant_v_storm,
  ),
  flow_after = case_when(
    grepl("Base", isStorm) ~ ((r_commons^2) * (2*acos((r_commons-((Depth_after)/12))/r_commons)-sin(2*acos((r_commons-((Depth_after)/12))/r_commons)))/2) * constant_v_base,
    grepl("Storm", isStorm) ~ ((r_commons^2) * (2*acos((r_commons-((Depth_after)/12))/r_commons)-sin(2*acos((r_commons-((Depth_after)/12))/r_commons)))/2) * constant_v_storm,
  )
)



csw_volume_change <- csw_flow %>% select(isStorm, Rain, flow_before, flow_after) %>% group_by(isStorm) %>% mutate(
  flow_v_before = flow_before * 300, #cubic feet
  flow_v_after = flow_after * 300
) %>% summarize(
  total_v_before = sum(flow_v_before, na.rm = T),
  total_v_after = sum(flow_v_after, na.rm = T),
  Rain = sum(Rain, na.rm = T)
) %>% mutate(
  change = total_v_before - total_v_after,
  percent_change = ((total_v_after - total_v_before)/total_v_after) * 100
)

csw_volume_change %>% filter(grepl("Storm", isStorm)) %>% rename("Before" = "total_v_before", "After" = "total_v_after") %>%
  pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>%
  ggplot(aes(x = Rain, y = Flow, color = Type)) + geom_point(aes(x = Rain, y = Flow, color = Type), size = 5, alpha = 0.4) + 
  geom_smooth(aes(x = Rain, y = Flow, color = Type), method = "lm", formula = y ~ x, se = F) + xlab("Rainfall (in.)") + ylab("Total Flow Volume (cubic feet)") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., ..BIC.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 0.7,
    label.y.npc = 0.95,
    size = 10,
    show.legend = F
  ) + scale_color_manual(values = c("#D92122", "#6495ED")) + theme_bw(base_size = 36) + labs(color = "") + theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "volume_regression.png"), height = 24, width = 30, units = "cm")

csw_volume_change %>% select(isStorm, percent_change) %>% mutate(
  `Event Type` = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )) %>% group_by(`Event Type`) %>% 
  mutate(Average = mean(percent_change)) %>%
  ggplot() +
  geom_jitter(aes(x = `Event Type`, y = percent_change, color = `Event Type`), size = 10, alpha = 0.4) + 
  xlab("") + ylab("Percent Change (%)") + scale_y_continuous(breaks = c(10, 5, 0, -5, -10, -50, -100)) +
  theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "volume_percent_change_eventtype.png"), height = 24, width = 30, units = "cm")

csw_volume_change %>% rename("Before" = "total_v_before", "After" = "total_v_after") %>% mutate(
  event_type = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )
) %>% pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>% group_by(Type, event_type) %>% 
  mutate(Average = mean(Flow)) %>%
  ggplot() + geom_boxplot(aes(x = Type, y = Flow, fill = Type), outlier.shape = NA) +
  geom_point(aes(x = Type, y = Flow * 1.2), size = 0, alpha = 0) + 
  geom_point(aes(x = Type, y = Flow), size = 2, alpha = 0.4) + geom_point(aes(x = Type, y = Average, shape = "Average"), size = 10) +
  geom_line(aes(x = Type, y = Flow, group = isStorm), alpha = 0.15) +
  scale_shape_manual(values = c("Average" = 4)) + coord_trans(y = lal_trans) + scale_y_continuous(breaks = c(0, 1000, 10000, 100000, 300000)) +
  xlab("") + ylab("Total Flow Volume (cubic feet)") +
  scale_fill_manual(values = c("#D92122", "#6495ED")) +
  facet_wrap(.~event_type) + labs(shape = "", fill = "") + guides(fill = "none") +
  stat_compare_means(aes(x = Type, y = Flow), method = "t.test", paired = T, size = 10, label.x.npc = 0.5, label.y.npc = 0.82, label = "p.format", hjust = "center") + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) + theme(legend.spacing.y = unit(0, 'cm'))
ggsave(here("Plots_withNewLimits", "CSW", "volume_stat_comparison_eventtype.png"), height = 24, width = 30, units = "cm")
  
csw_peakflow_change <- csw_flow %>% select(isStorm, Datetime, Rain, flow_before, flow_after) %>% group_by(isStorm) %>% mutate(zero = case_when(Rain == 0 ~ 1, T ~ 0)) %>% summarize(
  peak_before = max(flow_before, na.rm = T),
  peak_after = max(flow_after, na.rm = T),
  peak_rain = max((Rain/5) * 60, na.rm = T),
  zero_rows = sum(zero),
  total_rows = n(),
  effective_duration_intensity = (sum(Rain, na.rm = T)/(as.numeric(difftime(max(Datetime), min(Datetime), units = "hours")) + 0.083)) * (total_rows/(total_rows - zero_rows)),
  duration_intensity = (sum(Rain, na.rm = T)/(as.numeric(difftime(max(Datetime), min(Datetime), units = "hours")) + 0.083)),
  Rain = sum(Rain, na.rm = T)
) %>% mutate(
  change = peak_before - peak_after,
  percent_change = ((peak_after - peak_before)/peak_after) * 100
)

csw_peakflow_change %>% filter(grepl("Storm", isStorm)) %>% rename("Before" = "peak_before", "After" = "peak_after") %>% pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>%
  ggplot(aes(x = peak_rain, y = Flow, color = Type)) + geom_point(aes(x = peak_rain, y = Flow, color = Type), size = 5, alpha = 0.4) + 
  geom_smooth(aes(x = peak_rain, y = Flow, color = Type), method = "lm", formula = y ~ x, se = F) + xlab("Peak Rainfall Intensity (in./hr.)") + ylab("Peak Flow (cubic feet/second)") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 1.5,
    size = 10,
    label.y.npc = 0.75,
    show.legend = F
  ) + scale_color_manual(values = c("#D92122", "#6495ED")) + theme_bw(base_size = 36) + labs(color = "") +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "peakflow_regression_peakint.png"), height = 24, width = 30, units = "cm")

csw_peakflow_change %>% filter(grepl("Storm", isStorm)) %>% rename("Before" = "peak_before", "After" = "peak_after") %>%
  pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>%
  ggplot(aes(x = duration_intensity, y = Flow, color = Type)) + geom_point(aes(x = duration_intensity, y = Flow, color = Type), size = 5, alpha = 0.4) + 
  geom_smooth(aes(x = duration_intensity, y = Flow, color = Type), method = "lm", formula = y ~ x, se = F) + xlab("Duration Rainfall Intensity (in./hr.)") + ylab("Peak Flow (cubic feet/second)") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 0.5,
    size = 10,
    show.legend = F
  ) + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "peakflow_regression_durint.png"), height = 24, width = 30, units = "cm")

csw_peakflow_change %>% filter(grepl("Storm", isStorm)) %>% rename("Before" = "peak_before", "After" = "peak_after") %>%
  pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>%
  ggplot(aes(x = effective_duration_intensity, y = Flow, color = Type)) + geom_point(aes(x = effective_duration_intensity, y = Flow, color = Type), size = 5, alpha = 0.4) + 
  geom_smooth(aes(x = effective_duration_intensity, y = Flow, color = Type), method = "lm", formula = y ~ x, se = F) + xlab("Effective Duration Rainfall Intensity (in./hr.)") + ylab("Peak Flow (cubic feet/second)") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 0.5,
    size = 9,
    show.legend = F
  ) + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "peakflow_regression_effectdurint.png"), height = 24, width = 30, units = "cm")

csw_peakflow_change %>% select(isStorm, percent_change) %>% mutate(
  `Event Type` = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )) %>% group_by(`Event Type`) %>% 
  mutate(Average = mean(percent_change)) %>% filter(`Event Type` == "Stormflow") %>%
  ggplot() +
  geom_jitter(aes(x = `Event Type`, y = percent_change), size = 10, alpha = 0.4) + 
  xlab("") + ylab("Percent Change (%)") + scale_y_continuous(breaks = c(0, -100, -500, -1000, -2000)) +
  theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "peakflow_percent_change.png"), height = 24, width = 30, units = "cm")

csw_peakflow_change %>% rename("Before" = "peak_before", "After" = "peak_after") %>% mutate(
  event_type = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )
) %>% filter(event_type == "Stormflow") %>% pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>% group_by(Type, event_type) %>% 
  mutate(Average = mean(Flow)) %>%
  ggplot() + geom_boxplot(aes(x = Type, y = Flow, fill = Type)) +
  geom_point(aes(x = Type, y = Flow * 1.2), size = 0, alpha = 0) + 
  geom_point(aes(x = Type, y = Average, shape = "Average"), size = 10) + geom_point(aes(x = Type, y = Flow), size = 2, alpha = 0.4) +
  geom_line(aes(x = Type, y = Flow, group = isStorm), alpha = 0.15) +
  scale_shape_manual(values = c("Average" = 4)) + scale_fill_manual(values = c("#D92122", "#6495ED")) +
  xlab("") + ylab("Peak Flow (cubic feet/second)") + coord_trans(y = lal_trans) + scale_y_continuous(breaks = c(0, 1, 10, 100)) +
  facet_wrap(.~event_type) + labs(shape = "") + guides(fill = "none") +
  stat_compare_means(aes(x = Type, y = Flow), method = "t.test", paired = T, size = 10, label.x.npc = 0.5, label.y.npc = 0.82, label = "p.format", hjust = "center") + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank()) + theme(legend.spacing.y = unit(0, 'cm'))
ggsave(here("Plots_withNewLimits", "CSW", "peakflow_stat_comparison_eventtype.png"), height = 24, width = 30, units = "cm")



commons_volume_change <- commons_flow %>% select(isStorm, Rain, flow_before, flow_after) %>% group_by(isStorm) %>% mutate(
  flow_v_before = flow_before * 300, #cubic feet
  flow_v_after = flow_after * 300
) %>% summarize(
  total_v_before = sum(flow_v_before, na.rm = T),
  total_v_after = sum(flow_v_after, na.rm = T),
  Rain = sum(Rain, na.rm = T)
) %>% mutate(
  change = total_v_before - total_v_after,
  percent_change = ((total_v_after - total_v_before)/total_v_after) * 100
)

commons_volume_change %>% filter(grepl("Storm", isStorm)) %>% rename("Before" = "total_v_before", "After" = "total_v_after") %>%
  pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>%
  ggplot(aes(x = Rain, y = Flow, color = Type)) + geom_point(aes(x = Rain, y = Flow, color = Type), size = 5, alpha = 0.4) + 
  geom_smooth(aes(x = Rain, y = Flow, color = Type), method = "lm", formula = y ~ x, se = F) + xlab("Rainfall (in.)") + ylab("Total Flow Volume (cubic feet)") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 0.75,
    label.y.npc = 1.0,
    size = 10,
    show.legend = F
  ) + scale_color_manual(values = c("#D92122", "#6495ED")) + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "volume_regression.png"), height = 24, width = 30, units = "cm")

commons_volume_change %>% select(isStorm, percent_change) %>% mutate(
  `Event Type` = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )) %>% group_by(`Event Type`) %>% 
  mutate(Average = mean(percent_change)) %>%
  ggplot() +
  geom_jitter(aes(x = `Event Type`, y = percent_change, color = `Event Type`), size = 10, alpha = 0.4) + 
  xlab("") + ylab("Percent Change (%)") + scale_y_continuous(breaks = c(-1, 0, 1, 10, 100)) + coord_trans(y = lal_trans) +
  theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "volume_percent_change_eventtype.png"), height = 24, width = 30, units = "cm")

commons_volume_change %>% rename("Before" = "total_v_before", "After" = "total_v_after") %>% mutate(
  event_type = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )
) %>% pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>% group_by(Type, event_type) %>% 
  mutate(Average = mean(Flow)) %>%
  ggplot() + geom_boxplot(aes(x = Type, y = Flow, fill = Type), outlier.shape = NA) +
  geom_point(aes(x = Type, y = Flow * 1.2), size = 0, alpha = 0) + 
  geom_point(aes(x = Type, y = Flow), size = 2, alpha = 0.4) + geom_point(aes(x = Type, y = Average, shape = "Average"), size = 10) +
  geom_line(aes(x = Type, y = Flow, group = isStorm), alpha = 0.15) +
  scale_shape_manual(values = c("Average" = 4)) + coord_trans(y = lal_trans) + scale_y_continuous(breaks = c(0, 1000, 10000, 100000, 400000), limits = c(0, 1200000)) +
  xlab("") + ylab("Total Flow Volume (cubic feet)") +
  facet_wrap(.~event_type) + labs(shape = "") + guides(fill = "none") +
  scale_fill_manual(values = c("#D92122", "#6495ED")) +
  stat_compare_means(aes(x = Type, y = Flow), method = "t.test", paired = T, size = 10, label.x.npc = 0.5, label.y.npc = 0.77, label = "p.format", hjust = "center") + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "volume_stat_comparison_eventtype.png"), height = 24, width = 30, units = "cm")

commons_peakflow_change <- commons_flow %>% select(isStorm, Rain, flow_before, flow_after) %>% group_by(isStorm) %>% summarize(
  peak_before = max(flow_before, na.rm = T),
  peak_after = max(flow_after, na.rm = T),
  peak_rain = max((Rain/5) * 60, na.rm = T)
) %>% mutate(
  change = peak_before - peak_after,
  percent_change = ((peak_after - peak_before)/peak_after) * 100
)

commons_peakflow_change %>% filter(grepl("Storm", isStorm)) %>% rename("Before" = "peak_before", "After" = "peak_after") %>%
  pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>%
  ggplot(aes(x = peak_rain, y = Flow, color = Type)) + geom_point(aes(x = peak_rain, y = Flow, color = Type), size = 5, alpha = 0.4) + 
  geom_smooth(aes(x = peak_rain, y = Flow, color = Type), method = "lm", formula = y ~ x, se = F) + xlab("Peak Rainfall Intensity (in./hr.)") + ylab("Peak Flow (cubic feet/second)") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 1.5,
    size = 10,
    show.legend = F
  ) + scale_color_manual(values = c("#D92122", "#6495ED")) + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "peakflow_regression.png"), height = 24, width = 30, units = "cm")

commons_peakflow_change %>% select(isStorm, percent_change) %>% mutate(
  `Event Type` = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )) %>% group_by(`Event Type`) %>% 
  mutate(Average = mean(percent_change)) %>% filter(`Event Type` == "Stormflow") %>%
  ggplot() +
  geom_jitter(aes(x = `Event Type`, y = percent_change), size = 10, alpha = 0.4) + 
  xlab("") + ylab("Percent Change (%)") + scale_y_continuous(breaks = c(0, -100, -500, -1000, -2000)) +
  theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "peakflow_percent_change.png"), height = 24, width = 30, units = "cm")

commons_peakflow_change %>% rename("Before" = "peak_before", "After" = "peak_after") %>% mutate(
  event_type = case_when(
    grepl("Storm", isStorm) ~ "Stormflow",
    grepl("Base", isStorm) ~ "Baseflow"
  )
) %>% filter(event_type == "Stormflow") %>% pivot_longer(cols = c(Before, After), names_to = "Type", values_to = "Flow") %>% mutate(Type = factor(Type, levels = c("Before", "After"))) %>% group_by(Type, event_type) %>% 
  mutate(Average = mean(Flow)) %>%
  ggplot() + geom_boxplot(aes(x = Type, y = Flow, fill = Type)) +
  geom_point(aes(x = Type, y = Flow * 1.2), size = 0, alpha = 0) + 
  geom_point(aes(x = Type, y = Average, shape = "Average"), size = 10) + geom_point(aes(x = Type, y = Flow), size = 2, alpha = 0.4) +
  geom_line(aes(x = Type, y = Flow, group = isStorm), alpha = 0.15) +
  scale_shape_manual(values = c("Average" = 4)) +
  scale_fill_manual(values = c("#D92122", "#6495ED")) +
  xlab("") + ylab("Peak Flow (cubic feet/second)") + coord_trans(y = lal_trans) + scale_y_continuous(breaks = c(0, 1, 10, 100)) +
  facet_wrap(.~event_type) + labs(shape = "") + guides(fill = "none") +
  stat_compare_means(aes(x = Type, y = Flow), method = "t.test", paired = T, size = 10, label.x.npc = 0.5, label.y.npc = 0.82, label = "p.format", hjust = "center") + theme_bw(base_size = 36) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "peakflow_stat_comparison_eventtype.png"), height = 24, width = 30, units = "cm")



commons_diff_full_data <- data.frame()
commons_time_intervals_files <- c("Depth_1D.csv", "Depth_12H.csv", "Depth_6H.csv", "Depth_3H.csv", "Depth_1H.csv")
for(file in commons_time_intervals_files) {
  data <- read.csv(here("Output", "Differences", file))
  data <- data %>% mutate(
    Interval = file,
    Interval = case_when(
      grepl("1D", file) ~ "1-day",
      grepl("12H", file) ~ "12-hours",
      grepl("6H", file) ~ "6-hours",
      grepl("3H", file) ~ "3-hours",
      grepl("1H", file) ~ "1-hour",
    ),
    Interval = factor(Interval, levels = c("1-day", "12-hours", "6-hours", "3-hours", "1-hour"))
  )
  commons_diff_full_data <- bind_rows(commons_diff_full_data, data)
}

commons_diff_full_data <- commons_diff_full_data  %>% rename(
  "N.S.D. from Stormflow, S.D. from Baseflow" = "ns_storm_s_base",
  "N.S.D. from Baseflow, S.D. from Stormflow" = "ns_base_s_storm",
  "S.D. from both Storm and Baseflow" = "s_both",
  "N.S.D. from both Storm and Baseflow" = "ns_both"
) %>% mutate(
  `Total Number of Intervals` = `N.S.D. from Stormflow, S.D. from Baseflow` + `N.S.D. from Baseflow, S.D. from Stormflow` + `S.D. from both Storm and Baseflow` + `N.S.D. from both Storm and Baseflow`
) %>% pivot_longer(
  cols = c(`N.S.D. from Stormflow, S.D. from Baseflow`, `N.S.D. from Baseflow, S.D. from Stormflow`, `S.D. from both Storm and Baseflow`, `N.S.D. from both Storm and Baseflow`), names_to = "Category", values_to = "Number of Intervals"
) %>% mutate(
  Percentage = round(`Number of Intervals`/`Total Number of Intervals`, 3) * 100
)


commons_diff_full_data %>% ggplot() + 
  geom_col(aes(x = Interval, y = Percentage, fill = Category), position = position_dodge(0.94), color = "black") + 
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 3, byrow = T)) + 
  geom_text(aes(x = Interval, y = -2, group = Category, label = paste0("n = ", `Number of Intervals`, sep = "")), angle = 75, position = position_dodge(0.94), hjust = "right", size = 7) + 
  geom_text(aes(x = Interval, y = Percentage + 5, group = Category, label = paste0(Percentage , "%", sep = "")), position = position_dodge(0.94), hjust = "center", size = 6.5) + 
  geom_vline(aes(xintercept = 1.5), linetype = "dashed") + 
  geom_vline(aes(xintercept = 2.5), linetype = "dashed") +
  geom_vline(aes(xintercept = 3.5), linetype = "dashed") +
  geom_vline(aes(xintercept = 4.5), linetype = "dashed") +
  ylab("Percentage (%)") +
  labs(caption = "N.S.D. = Not Significantly Different\nS.D. = Significantly Different") +
  theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-25, 100)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "Commons", "diff_2.png"), height = 30, width = 50, units = "cm")
  

csw_diff_full_data <- data.frame()
csw_time_intervals_files <- c("Velocity_1D.csv", "Velocity_12H.csv", "Velocity_6H.csv", "Velocity_3H.csv", "Velocity_1H.csv")

for(file in csw_time_intervals_files) {
  data <- read.csv(here("Output", "Differences", file))
  data <- data %>% mutate(
    Interval = file,
    Interval = case_when(
      grepl("1D", file) ~ "1-day",
      grepl("12H", file) ~ "12-hours",
      grepl("6H", file) ~ "6-hours",
      grepl("3H", file) ~ "3-hours",
      grepl("1H", file) ~ "1-hour",
    ),
    Interval = factor(Interval, levels = c("1-day", "12-hours", "6-hours", "3-hours", "1-hour"))
  )
  csw_diff_full_data <- bind_rows(csw_diff_full_data, data)
}

csw_diff_full_data <- csw_diff_full_data  %>% rename(
  "N.S.D. from Stormflow, S.D. from Baseflow" = "ns_storm_s_base",
  "N.S.D. from Baseflow, S.D. from Stormflow" = "ns_base_s_storm",
  "S.D. from both Storm and Baseflow" = "s_both",
  "N.S.D. from both Storm and Baseflow" = "ns_both"
) %>% mutate(
  `Total Number of Intervals` = `N.S.D. from Stormflow, S.D. from Baseflow` + `N.S.D. from Baseflow, S.D. from Stormflow` + `S.D. from both Storm and Baseflow` + `N.S.D. from both Storm and Baseflow`
) %>% pivot_longer(
  cols = c(`N.S.D. from Stormflow, S.D. from Baseflow`, `N.S.D. from Baseflow, S.D. from Stormflow`, `S.D. from both Storm and Baseflow`, `N.S.D. from both Storm and Baseflow`), names_to = "Category", values_to = "Number of Intervals"
) %>% mutate(
  Percentage = round(`Number of Intervals`/`Total Number of Intervals`, 3) * 100
)


csw_diff_full_data %>% ggplot() + 
  geom_col(aes(x = Interval, y = Percentage, fill = Category), position = position_dodge(0.94), color = "black") + 
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 3, byrow = T)) + 
  geom_text(aes(x = Interval, y = -2, group = Category, label = paste0("n = ", `Number of Intervals`, sep = "")), angle = 75, position = position_dodge(0.94), hjust = "right", size = 7) + 
  geom_text(aes(x = Interval, y = Percentage + 5, group = Category, label = paste0(Percentage , "%", sep = "")), position = position_dodge(0.94), hjust = "center", size = 6.5) + 
  geom_vline(aes(xintercept = 1.5), linetype = "dashed") + 
  geom_vline(aes(xintercept = 2.5), linetype = "dashed") +
  geom_vline(aes(xintercept = 3.5), linetype = "dashed") +
  geom_vline(aes(xintercept = 4.5), linetype = "dashed") +
  ylab("Percentage (%)") +
  labs(caption = "N.S.D. = Not Significantly Different\nS.D. = Significantly Different") +
  theme_bw(base_size = 32) +  theme(strip.placement = "outside", panel.spacing = unit(1, "lines")) + theme(legend.position = "top", legend.box = "vertical") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-25, 100)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 30, face = "bold")) + theme(strip.background = element_blank())
ggsave(here("Plots_withNewLimits", "CSW", "diff_2.png"), height = 30, width = 50, units = "cm")





time_scale <- read_excel(here("Output", "CSW_2020_withRegression_QAQC.xlsx")) %>%
  mutate(type = case_when(grepl("StormFlow", isStorm) ~ "1",
                          grepl("BaseFlow", isStorm) ~ "0"
  ),
  use = case_when((Datetime >= "2020-02-07 04:00:00" & Datetime <= "2020-02-07 14:00:00") ~ TRUE)
  )



time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Baseflow", "Stormflow"), values = c(NA, "lightblue")) + 
  labs(fill = "Event Type") +
  facet_zoom(x = isStorm == "StormFlow36", zoom.data = FALSE)
ggsave(here("Plots_withNewLimits", "Time", "StormFlow.png"))

time_scale %>% filter(isStorm == "StormFlow36") %>% ggplot() + geom_histogram(aes( x = Velocity), color = "black", fill = "white") +  
  theme_bw(base_size = 26) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)") +
  geom_vline(data = . %>% mutate(Average = median(Velocity)), aes(xintercept = Average, color = "Mean"), size = 2) +
  geom_vline(data = . %>% mutate(Median = mean(Velocity)), aes(xintercept = Median, color = "Median"), size = 2) +
  scale_color_manual(values = c("Mean" = "cornflowerblue", "Median" = "mediumvioletred")) + labs(color = "") + theme(legend.position = "top")
ggsave(here("Plots_withNewLimits", "Time", "StormFlow_dist.png"))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Baseflow", "Stormflow"), values = c(NA, "lightblue")) + 
labs(fill = "Event Type") +
  facet_zoom(x = isStorm == "BaseFlow37", zoom.data = FALSE)
ggsave(here("Plots_withNewLimits", "Time", "BaseFlow.png"))

time_scale %>% filter(isStorm == "BaseFlow37") %>% ggplot() + geom_histogram(aes( x = Velocity), color = "black", fill = "white") +    
  theme_bw(base_size = 26) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)") +
  geom_vline(data = . %>% mutate(Average = median(Velocity)), aes(xintercept = Average, color = "Mean"), size = 2) +
  geom_vline(data = . %>% mutate(Median = mean(Velocity)), aes(xintercept = Median, color = "Median"), size = 2) +
  scale_color_manual(values = c("Mean" = "cornflowerblue", "Median" = "mediumvioletred")) + labs(color = "") + theme(legend.position = "top")
ggsave(here("Plots_withNewLimits", "Time", "BaseFlow_dist.png"))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Baseflow", "Stormflow"), values = c(NA, "lightblue")) + 
  labs(fill = "Event Type") +
  facet_zoom(x = use== TRUE, zoom.data = FALSE)
ggsave(here("Plots_withNewLimits", "Time", "Abitrary.png"))

time_scale %>% filter(use == TRUE) %>% ggplot() + geom_histogram(aes( x = Velocity), color = "black", fill = "white") +  
  theme_bw(base_size = 26) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)") +
  geom_vline(data = . %>% mutate(Average = median(Velocity)), aes(xintercept = Average, color = "Mean"), size = 2) +
  geom_vline(data = . %>% mutate(Median = mean(Velocity)), aes(xintercept = Median, color = "Median"), size = 2) +
  scale_color_manual(values = c("Mean" = "cornflowerblue", "Median" = "mediumvioletred")) + labs(color = "") + theme(legend.position = "top")
ggsave(here("Plots_withNewLimits", "Time", "Arbitrary_dist.png"))





