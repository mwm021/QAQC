packages <- c("tidyverse", "survival", "xlsx", "stringr", "lubridate", "NADA", "here", "gt", "webshot", "zoo", "car", "ggpubr", "dlookr", "rlang", "scales", "multcompView", "rcompanion", "tseries", "forecast", "smooth", "readxl", "ggridges", "ggforce")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

webshot::install_phantomjs()

getNormality <- function(variable, data1, data2, data3, location) {
    df <- data.frame(matrix(ncol = 4, nrow = 0))
    names <- c("Measure", "Before", "After", "Regression")
    colnames(df) <- names
    
    na1 <- data1 %>%
      filter(VariableCode == variable) %>%
      filter(is.na(DataValue)) %>% nrow() %>% format(scientific = FALSE)
    na2 <- data2 %>%
      filter(VariableCode == variable) %>%
      filter(is.na(DataValue)) %>% nrow() %>% format(scientific = FALSE)
    na3 <- data3 %>%
      filter(VariableCode == variable) %>%
      filter(is.na(DataValue)) %>% nrow() %>% format(scientific = FALSE)
    
    na_row <- data.frame(Statistic = "Number of Missing Values", Before = na1, After = na2, Regression = na3)
    df <- rbind(df, na_row)
    
    drop_na(data1)
    drop_na(data2)
    drop_na(data3)

    outlier1 <- data1 %>%
      filter(VariableCode == variable) %>%
      na.omit() %>%
      mutate(value = (DataValue - mean(DataValue))/sd(DataValue)) %>% 
      filter(value > 6 | value < -6) %>% nrow() %>% format(scientific = FALSE)
    outlier2 <- data2 %>%
      filter(VariableCode == variable) %>%
      mutate(value = (DataValue - mean(DataValue))/sd(DataValue)) %>% 
      filter(value > 6 | value < -6) %>% nrow() %>% format(scientific = FALSE)
    outlier3 <- data3 %>%
      filter(VariableCode == variable) %>%
      mutate(value = (DataValue - mean(DataValue))/sd(DataValue)) %>% 
      filter(value > 6 | value < -6) %>% nrow() %>% format(scientific = FALSE)
    
    outlier_row <- data.frame(Statistic = "Number of Outliers", Before = outlier1, After = outlier2, Regression = outlier3)
    df <- rbind(df, outlier_row)
    
    sd1 <- data1 %>%
      filter(VariableCode == variable) %>%
      na.omit() %>%
      summarise(sd = sd(DataValue))
    sd2 <- data2 %>%
      filter(VariableCode == variable) %>%
      summarise(sd = sd(DataValue))
    sd3 <- data3 %>%
      filter(VariableCode == variable) %>%
      summarise(sd = sd(DataValue))
    
    sd_row <- data.frame(Statistic = "Standard Deviation", Before = round(sd1$sd, 3), After = round(sd2$sd, 3), Regression = round(sd3$sd, 3))
    df <- rbind(df, sd_row)
    
    max1 <- data1 %>%
      filter(VariableCode == variable) %>%
      na.omit() %>%
      summarise(max = max(DataValue))
    max2 <- data2 %>%
      filter(VariableCode == variable) %>%
      summarise(max = max(DataValue))
    max3 <- data3 %>%
      filter(VariableCode == variable) %>%
      summarise(max = max(DataValue))
    
    max_row <- data.frame(Statistic = "Maximum", Before = round(max1$max, 3), After = round(max2$max, 3), Regression = round(max3$max, 3))
    df <- rbind(df, max_row)
    
    average1 <- data1 %>%
      filter(VariableCode == variable) %>%
      na.omit() %>%
      summarise(average = mean(DataValue))
    average2 <- data2 %>%
      filter(VariableCode == variable) %>%
      summarise(average = mean(DataValue))
    average3 <- data3 %>%
      filter(VariableCode == variable) %>%
      summarise(average = mean(DataValue))
    
    average_row <- data.frame(Statistic = "Average", Before = round(average1$average, 3), After = round(average2$average, 3), Regression = round(average3$average, 3))
    df <- rbind(df, average_row)
    
    min1 <- data1 %>%
      filter(VariableCode == variable) %>%
      na.omit() %>%
      summarise(min = min(DataValue))
    min2 <- data2 %>%
      filter(VariableCode == variable) %>%
      summarise(min = min(DataValue))
    min3 <- data3 %>%
      filter(VariableCode == variable) %>%
      summarise(min = min(DataValue))
    
    min_row <- data.frame(Statistic = "Minimum", Before = round(min1$min, 3), After = round(min2$min, 3), Regression = round(min3$min, 3))
    df <- rbind(df, min_row)
    
    df %>% gt() %>% opt_table_font("palatino") %>% tab_header(title = "Indicators of Data Quality Before and After Processing") %>% gtsave(here("Plots", location, "quality_table.png"))
}

create_histogram_raw <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, location) {
  data %>% group_by_at(grouping) %>% 
    ggplot() + geom_histogram(aes(x = DataValue, fill = fill), alpha = 0.5) +
    scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000")) +
    facet_wrap(.~data$Type, strip.position = "bottom") +
    scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    xlab(xlab) + ylab(ylab) +
    theme_bw(base_family = "Palatino") +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(fill = fillName) + guides(fill = guide_legend(ncol = 1)) +
    ggsave(here("Plots", location, "histograms_raw.png"), width = 20, height = 10, units = "cm")
}

create_histogram_upper <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, location) {
  data %>% filter(DataValue > 10) %>% group_by_at(grouping) %>% 
    ggplot() + geom_histogram(aes(x = DataValue, fill = fill), alpha = 0.5) +
    scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000")) +
    facet_wrap(.~data$Type, scales = "free_x", strip.position = "bottom") +
    scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    xlab(xlab) + ylab(ylab) +
    theme_bw(base_family = "Palatino") +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(fill = fillName) + guides(fill = guide_legend(ncol = 1)) +
    ggsave(here("Plots", location, "histograms_upper.png"), width = 20, height = 10, units = "cm")
}

create_histogram_lower <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, location) {
  data %>% filter(DataValue < 1) %>% group_by_at(grouping) %>% 
    ggplot() + geom_histogram(aes(x = DataValue, fill = fill), alpha = 0.5) +
    scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000")) +
    facet_wrap(.~data$Type, scales = "free_x", strip.position = "bottom") +
    scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    xlab(xlab) + ylab(ylab) +
    theme_bw(base_family = "Palatino") +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(fill = fillName) + guides(fill = guide_legend(ncol = 1)) +
    ggsave(here("Plots", location, "histograms_lower.png"), width = 20, height = 10, units = "cm")
}

create_boxplot_raw <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, location) {
  data %>% group_by_at(grouping) %>% 
    ggplot() + geom_boxplot(aes(x = VariableCode, y = DataValue, fill = fill), alpha = 0.5) +
    scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000")) +
    facet_wrap(.~data$Type, scales = "free_x", strip.position = "bottom") +
    scale_y_continuous(trans = "sqrt") +
    ylab(ylab) +
    theme_bw(base_family = "Palatino") +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(fill = fillName) + guides(fill = guide_legend(ncol = 1)) +
    ggsave(here("Plots", location, "boxplots_raw.png"), width = 20, height = 10, units = "cm")
}

create_boxplot_upper <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, location) {
  data %>% filter(DataValue > 10) %>% group_by_at(grouping) %>% 
    ggplot() + geom_boxplot(aes(x = VariableCode, y = DataValue, fill = fill), alpha = 0.5) +
    scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000")) +
    facet_wrap(.~data$Type, scales = "free_x", strip.position = "bottom") +
    #scale_y_continuous(trans = "sqrt") +
    ylab(ylab) +
    theme_bw(base_family = "Palatino") +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(fill = fillName) + guides(fill = guide_legend(ncol = 1)) +
    ggsave(here("Plots", location, "boxplots_upper.png"), width = 20, height = 10, units = "cm")
}

create_boxplot_lower <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, location) {
  data %>% filter(DataValue < 1) %>% group_by_at(grouping) %>% 
    ggplot() + geom_boxplot(aes(x = VariableCode, y = DataValue, fill = fill), alpha = 0.5) +
    scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000")) +
    facet_wrap(.~data$Type, scales = "free_x", strip.position = "bottom") +
    #scale_y_continuous(trans = "sqrt") +
    ylab(ylab) +
    theme_bw(base_family = "Palatino") +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(fill = fillName) + guides(fill = guide_legend(ncol = 1)) +
    ggsave(here("Plots", location, "boxplots_lower.png"), width = 20, height = 10, units = "cm")
}

location = "Commons"

before <- read_excel(here("Data", "Commons_2020.xlsx")) %>% select(Datetime, Depth, Rain)
before <- before %>% pivot_longer(cols = Depth:Rain, names_to = "VariableCode", values_to = "DataValue", values_drop_na = FALSE)
before$Datetime <- as.Date(before$Datetime, format = "%m/%d/%Y %H:%M:%S")
before <- as_tibble(before)
after <- read_excel(here("Output", "Commons_2020_QAQC.xlsx")) %>% select(Datetime, Depth, Rain)
after <- after %>% pivot_longer(cols = Depth:Rain, names_to = "VariableCode", values_to = "DataValue", values_drop_na = FALSE)
after$Datetime <- as.Date(after$Datetime, format = "%m/%d/%Y %H:%M:%S")
after <- as_tibble(after)
regression <- read_excel(here("Output", "Commons_2020_withRegression_QAQC.xlsx")) %>% select(Datetime, Depth, Rain)
regression <- regression %>% pivot_longer(cols = Depth:Rain, names_to = "VariableCode", values_to = "DataValue", values_drop_na = FALSE)
regression$Datetime <- as.Date(regression$Datetime, format = "%m/%d/%Y %H:%M:%S")
regression <- as_tibble(regression)

getNormality("Depth", before, after, regression, location)
before <- na.omit(before)
before$"Type" <- "Before"
after$"Type" <- "After"
regression$"Type" <- "Regression"
combined_data <- rbind(before, after, regression)
combined_data$Type <- factor(combined_data$Type, levels = levels(combined_data$Type) <- c("Before", "After", "Regression")) 
combined_data <- combined_data %>% filter(VariableCode == "Depth")
upper <- combined_data %>% filter(DataValue > 10)
lower <- combined_data %>% filter(DataValue < 1)

create_histogram_raw(combined_data, vars(Type), combined_data$Type, "Type", "Histogram of Depth Before, After, and After with Regression", "Depth (in)", "Count", location)
create_histogram_upper(upper, vars(Type), upper$Type, "Type", "Histogram of Depth Before, After, and After with Regression (Highest Values)", "Depth (in)", "Count", location)
create_histogram_lower(lower, vars(Type), lower$Type, "Type", "Histogram of Depth Before, After, and After with Regression (Lowest Values)", "Depth (in)", "Count", location)

create_boxplot_raw(combined_data, vars(Type), combined_data$Type, "Type", "Boxplot of Depth Before, After, and After with Regression", "", "Depth (in)", location)
create_boxplot_upper(upper, vars(Type), upper$Type, "Type", "Boxplot of Depth Before, After, and After with Regression (Highest Values)", "", "Depth (in)", location)
create_boxplot_lower(lower, vars(Type), lower$Type, "Type", "Boxplot of Depth Before, After, and After with Regression (Lowest Values)", "", "Depth (in)", location)

location = "CSW"

before <- read_excel(here("Data", "CSW_2020.xlsx")) %>% select(Datetime, Velocity, Rain)
before <- before %>% pivot_longer(cols = Velocity:Rain, names_to = "VariableCode", values_to = "DataValue", values_drop_na = FALSE)
before$Datetime <- as.Date(before$Datetime, format = "%m/%d/%Y %H:%M:%S")
before <- as_tibble(before)
after <- read_excel(here("Output", "CSW_2020_QAQC.xlsx")) %>% select(Datetime, Velocity, Rain)
after <- after %>% pivot_longer(cols = Velocity:Rain, names_to = "VariableCode", values_to = "DataValue", values_drop_na = FALSE)
after$Datetime <- as.Date(after$Datetime, format = "%m/%d/%Y %H:%M:%S")
after <- as_tibble(after)
regression <- read_excel(here("Output", "CSW_2020_withRegression_QAQC.xlsx")) %>% select(Datetime, Velocity, Rain)
regression <- regression %>% pivot_longer(cols = Velocity:Rain, names_to = "VariableCode", values_to = "DataValue", values_drop_na = FALSE)
regression$Datetime <- as.Date(regression$Datetime, format = "%m/%d/%Y %H:%M:%S")
regression <- as_tibble(regression)

getNormality("Velocity", before, after, regression, location)
before <- na.omit(before)
before$"Type" <- "Before"
after$"Type" <- "After"
regression$"Type" <- "Regression"
combined_data <- rbind(before, after, regression)
combined_data$Type <- factor(combined_data$Type, levels = levels(combined_data$Type) <- c("Before", "After", "Regression")) 
combined_data <- combined_data %>% filter(VariableCode == "Velocity")
upper <- combined_data %>% filter(DataValue > 10)
lower <- combined_data %>% filter(DataValue < 1)

create_histogram_raw(combined_data, vars(Type), combined_data$Type, "Type", "Histogram of Velocity Before, After, and After with Regression", "Velocity (ft/s)", "Count", location)
create_histogram_upper(upper, vars(Type), upper$Type, "Type", "Histogram of Velocity Before, After, and After with Regression (Highest Values)", "Velocity (ft/s)", "Count", location)
create_histogram_lower(lower, vars(Type), lower$Type, "Type", "Histogram of Velocity Before, After, and After with Regression (Lowest Values)", "Velocity (ft/s)", "Count", location)

create_boxplot_raw(combined_data, vars(Type), combined_data$Type, "Type", "Boxplot of Velocity Before, After, and After with Regression", "", "Velocity (ft/s)", location)
create_boxplot_upper(upper, vars(Type), upper$Type, "Type", "Boxplot of Velocity Before, After, and After with Regression (Highest Values)", "", "Velocity (ft/s)", location)
create_boxplot_lower(lower, vars(Type), lower$Type, "Type", "Boxplot of Velocity Before, After, and After with Regression (Lowest Values)", "", "Velocity (ft/s)", location)


time_scale <- read_excel(here("Output", "CSW_2020_withRegression_QAQC.xlsx")) %>%
  mutate(type = case_when(grepl("StormFlow", isStorm) ~ "1",
                          grepl("BaseFlow", isStorm) ~ "0"
  ) 
) %>% mutate(use = case_when((Datetime >= "2020-02-07 04:00:00" & Datetime <= "2020-02-07 14:00:00") ~ TRUE)
)


theme_set(theme_gray(base_size = 30))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + guides(guides(fill=guide_legend(title="Event Type"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Base Flow", "Storm Flow"), values = c(NA, "lightblue")) + 
  facet_zoom(x = isStorm == "StormFlow36", zoom.data = FALSE) + ggsave(here("Plots", "Time", "StormFlow.png"))

time_scale %>% filter(isStorm == "StormFlow36") %>% ggplot() + geom_histogram(aes( x = Velocity)) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)") + ggsave(here("Plots", "Time", "StormFlow_dist.png"))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + guides(guides(fill=guide_legend(title="Event Type"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Base Flow", "Storm Flow"), values = c(NA, "lightblue")) + 
  facet_zoom(x = isStorm == "BaseFlow37", zoom.data = FALSE) + ggsave(here("Plots", "Time", "BaseFlow.png"))

time_scale %>% filter(isStorm == "BaseFlow37") %>% ggplot() + geom_histogram(aes( x = Velocity)) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)") + ggsave(here("Plots", "Time", "BaseFlow_dist.png"))

time_scale %>% filter(isStorm %in% c("BaseFlow35", "StormFlow36", "BaseFlow37", "StormFlow38", "BaseFlow39")) %>% ggplot() +
  geom_point(aes(x = Datetime, y = Velocity)) +
  geom_tile(aes(x = Datetime, y = 4.2, fill = as.factor(type)), height = Inf, alpha = 0.4) + 
  ylab("Velocity (ft/s)") + guides(guides(fill=guide_legend(title="Event Type"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(labels = c("Base Flow", "Storm Flow"), values = c(NA, "lightblue")) + 
  facet_zoom(x = use== TRUE, zoom.data = FALSE) + ggsave(here("Plots", "Time", "Abitrary.png"))

time_scale %>% filter(use == TRUE) %>% ggplot() + geom_histogram(aes( x = Velocity)) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Count") + xlab("Velocity (ft/s)") + ggsave(here("Plots", "Time", "Arbitrary_dist.png"))




