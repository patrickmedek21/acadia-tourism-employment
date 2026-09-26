# MISM6200 Final Project
# Tourism and Jobs in Maine: Evaluating Acadia National Park's Impact on Local Employment
# Patrick Medek

library(tidyverse)
library(lubridate)
library(scales)
library(glue)
library(ggpmisc)
library(gt)

# === 1. LOAD DATA ===

weather <- read_csv("C:/Users/patri/Documents/Northeastern/Intro to Business Analytics/Final Project/Data/weather-data.csv")
visitors <- read_csv("C:/Users/patri/Documents/Northeastern/Intro to Business Analytics/Final Project/Data/acadia-visitors.csv")
employment <- read_csv("C:/Users/patri/Documents/Northeastern/Intro to Business Analytics/Final Project/Data/Employment/employment_maine_4counties_allindustries.csv")


# === 2. CLEAN DATA ===

# --- WEATHER ---
if ("DATE" %in% names(weather)) names(weather)[names(weather) == "DATE"] <- "Date"

weather <- weather %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  select(Year, Month, Date, TMAX, TMIN, TAVG) %>%
  mutate(across(c(TMAX, TMIN, TAVG), as.numeric))

weather_monthly <- weather %>%
  mutate(
    Year = year(Date),
    MonthNum = month(Date),
    MonthLabel = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, MonthNum, MonthLabel) %>%
  summarise(
    AvgTemp = mean(TAVG, na.rm = TRUE),
    AvgTMax = mean(TMAX, na.rm = TRUE),
    AvgTMin = mean(TMIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, MonthNum) %>%
  mutate(
    MonthLabelFull = paste0(MonthLabel, " '", substr(Year, 3, 4))
  )

weather_monthly_avg <- weather %>%
  group_by(Month) %>%
  summarise(
    AvgHigh = mean(TMAX, na.rm = TRUE),
    AvgLow = mean(TMIN, na.rm = TRUE),
    AvgTemp = mean(TAVG, na.rm = TRUE)
  )

# --- VISITORS ---
visitors_clean <- visitors %>%
  rename(Year = YEAR) %>%
  pivot_longer(
    cols = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"),
    names_to = "Month",
    values_to = "Visitors"
  ) %>%
  mutate(
    Month = factor(
      Month,
      levels = c("JAN","FEB","MAR","APR","MAY","JUN",
                 "JUL","AUG","SEP","OCT","NOV","DEC"),
      labels = month.abb,
      ordered = TRUE
    )
  )

visitors_monthly_avg <- visitors_clean %>%
  group_by(Month) %>%
  summarise(AvgVisitors = mean(Visitors, na.rm = TRUE))

# --- EMPLOYMENT ---
employment <- employment %>%
  mutate(
    MonthNum = match(Month, month.abb),
    Date = ymd(paste(Year, MonthNum, "01", sep = "-")),
    IndustryClean = str_replace(Industry, "^[0-9]+\\s+", "")
  )

all_jobs <- employment %>%
  filter(
    Ownership == "Total Covered",
    IndustryClean == "Total, all industries"
  ) %>%
  transmute(
    Area, Tier, Approx_Distance_miles, Date,
    Industry = "All Jobs",
    Employment
  )

government <- employment %>%
  filter(
    Ownership %in% c("Federal Government", "State Government", "Local Government")
  ) %>%
  group_by(Area, Tier, Approx_Distance_miles, Date) %>%
  summarise(
    Industry = "Government",
    Employment = sum(Employment),
    .groups = "drop"
  )

subtotal_industries <- c(
  "Total, all industries",
  "Goods-producing",
  "Service-providing"
)

private_subtotals <- c(
  "Private",
  "Private, goods-producing",
  "Private, service-providing"
)

private_leaf <- employment %>%
  filter(
    Ownership == "Private",
    !IndustryClean %in% subtotal_industries,
    !IndustryClean %in% private_subtotals,
    !IndustryClean %in% "Unclassified"
  ) %>%
  transmute(
    Area, Tier, Approx_Distance_miles, Date,
    Industry = IndustryClean,
    Employment
  )

employment <- bind_rows(
  all_jobs,
  government,
  private_leaf
)

employment_monthly <- employment %>%
  mutate(
    MonthNum = month(Date),
    MonthLabel = month.abb[MonthNum]
  )

emp_monthly_avg <- employment_monthly %>%
  group_by(Area, Tier, Approx_Distance_miles, Industry, MonthNum, MonthLabel) %>%
  summarize(
    AvgEmployment = round(mean(Employment, na.rm = TRUE)),
    .groups = "drop"
  )


# === 3. WEATHER GRAPHS ===

# Monthly Temperatures in Acadia National Park (2015–2024)
weather_monthly_actual <- weather %>%
  group_by(Year, Month) %>%
  summarise(
    AvgHigh = mean(TMAX, na.rm = TRUE),
    AvgLow = mean(TMIN, na.rm = TRUE),
    AvgTemp = mean(TAVG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MonthNum = match(Month, month.abb),
    Date = as.Date(paste(Year, MonthNum, "01", sep = "-"))
  )

weather_long_actual <- weather_monthly_actual %>%
  pivot_longer(
    cols = c(AvgHigh, AvgTemp, AvgLow),
    names_to = "Type",
    values_to = "Temp"
  ) %>%
  mutate(Type = factor(Type, levels = c("AvgHigh", "AvgTemp", "AvgLow")))

ggplot(weather_long_actual, aes(x = Date, y = Temp, color = Type, group = Type)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("AvgHigh" = "firebrick", "AvgTemp" = "forestgreen", "AvgLow" = "steelblue"),
    labels = c("High", "Average", "Low")
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  labs(
    title = "Monthly Temperatures in Acadia National Park (2015–2024)",
    x = "Month",
    y = "Temperature (°F)",
    color = "Temperature Type"
  )

# Monthly Temperature Trends in Acadia National Park (2015–2024)
ggplot(weather_monthly_avg, aes(x = Month, group = 1)) +
  geom_line(aes(y = AvgHigh, color = factor("High", levels = c("High", "Average", "Low"))), size = 1.2) +
  geom_line(aes(y = AvgTemp, color = factor("Average", levels = c("High", "Average", "Low"))), size = 1.2) +
  geom_line(aes(y = AvgLow, color = factor("Low", levels = c("High", "Average", "Low"))), size = 1.2) +
  geom_point(aes(y = AvgHigh, color = factor("High", levels = c("High", "Average", "Low"))), size = 3) +
  geom_point(aes(y = AvgTemp, color = factor("Average", levels = c("High", "Average", "Low"))), size = 3) +
  geom_point(aes(y = AvgLow, color = factor("Low", levels = c("High", "Average", "Low"))), size = 3) +
  scale_color_manual(
    values = c("High" = "firebrick", "Average" = "forestgreen", "Low" = "steelblue"),
    breaks = c("High", "Average", "Low"),
    name = NULL
  ) +
  theme_minimal() +
  labs(
    title = "Monthly Temperature Trends in Acadia National Park (2015–2024)",
    y = "Average Temperature (°F)",
    x = "Month"
  )


# === 4. VISITOR GRAPHS ===

# Yearly Visitors to Acadia National Park (2015–2024)
visitors_clean %>%
  distinct(Year, TOTAL, .keep_all = FALSE) %>%
  ggplot(aes(x = Year, y = TOTAL)) +
  geom_line(color = "goldenrod3", size = 1.2) +
  geom_point(color = "black", size = 3) +
  geom_text(
    aes(label = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)(TOTAL)),
    vjust = -0.8,
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(
    limits = c(0, 5000000),
    breaks = seq(0, 5000000, 500000),
    labels = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Yearly Visitors to Acadia National Park (2015–2024)",
    x = "Year",
    y = "Visitors"
  )

# Monthly Visitors to Acadia National Park (2015–2024)
visitors_clean %>%
  mutate(
    Date = as.Date(paste(Year, match(Month, month.abb), "01", sep = "-"))
  ) %>%
  ggplot(aes(x = Date, y = Visitors)) +
  geom_line(color = "goldenrod3", size = 1.2) +
  geom_point(color = "black", size = 2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-3, suffix = "K")
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Monthly Visitors to Acadia National Park (2015–2024)",
    x = "Month",
    y = "Visitors"
  )

# Monthly Visitor Trends in Acadia National Park (2015–2024)
ggplot(visitors_monthly_avg, aes(x = Month, y = AvgVisitors, group = 1)) +
  geom_line(color = "goldenrod3", size = 1.2) +
  geom_point(color = "black", size = 3) +
  geom_text(
    aes(label = paste0(round(AvgVisitors / 1000, 1), "K")),
    vjust = -0.8, size = 3.2, color = "black"
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "K")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Monthly Visitor Trends in Acadia National Park (2015–2024)",
    y = "Average Visitors",
    x = "Month"
  )


# === 5. COMBINED WEATHER + VISITORS ===

weather_monthly_avg <- weather_monthly_avg %>%
  mutate(Month = factor(Month, levels = month.abb, ordered = FALSE))
visitors_monthly_avg <- visitors_monthly_avg %>%
  mutate(Month = factor(Month, levels = month.abb, ordered = FALSE))

combined_monthly <- left_join(visitors_monthly_avg, weather_monthly_avg, by = "Month")

# Monthly Visitor and Temperature Trends in Acadia National Park (2015–2024)
scale_factor <- max(combined_monthly$AvgVisitors, na.rm = TRUE) / max(combined_monthly$AvgHigh, na.rm = TRUE)

ggplot(combined_monthly, aes(x = Month, group = 1)) +
  geom_line(aes(y = AvgVisitors, color = "Avg Visitors"), size = 1.3) +
  geom_point(aes(y = AvgVisitors), color = "black", size = 3) +
  geom_line(aes(y = AvgHigh * scale_factor, color = "Avg High (°F)"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = AvgTemp * scale_factor, color = "Avg Temp (°F)"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = AvgLow * scale_factor, color = "Avg Low (°F)"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = AvgHigh * scale_factor, color = "Avg High (°F)"), size = 2) +
  geom_point(aes(y = AvgTemp * scale_factor, color = "Avg Temp (°F)"), size = 2) +
  geom_point(aes(y = AvgLow * scale_factor, color = "Avg Low (°F)"), size = 2) +
  
  scale_y_continuous(
    name = "Average Visitors",
    labels = label_number(scale = 1e-3, suffix = "K"),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Average Temperature (°F)",
                        breaks = seq(0, 90, 10))
  ) +
  
  scale_color_manual(
    values = c(
      "Avg Visitors" = "goldenrod3",
      "Avg High (°F)" = "firebrick",
      "Avg Temp (°F)" = "forestgreen",
      "Avg Low (°F)" = "steelblue"
    ),
    breaks = c("Avg Visitors", "Avg High (°F)", "Avg Temp (°F)", "Avg Low (°F)"),
    name = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y.left = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "black", size = 12),
    axis.text.y.left = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Monthly Visitor and Temperature Trends in Acadia National Park (2015–2024)",
    x = "Month"
  )

# Visitors vs. Temperature in Acadia National Park (2015–2024)
combined_monthly_all <- weather_monthly %>%
  left_join(
    visitors_clean,
    by = c("Year", "MonthLabel" = "Month")
  ) %>%
  select(Year, MonthNum, MonthLabel, MonthLabelFull, AvgTemp, Visitors)

model <- lm(Visitors ~ AvgTemp, data = combined_monthly_all)

a <- coef(model)[1]
b <- coef(model)[2]
r2 <- summary(model)$r.squared

eq_text <- paste0(
  "y = ", round(b, 1), "x + (", round(a, 0), ")\nR² = ", round(r2, 3)
)

ggplot(combined_monthly_all, aes(x = AvgTemp, y = Visitors)) +
  geom_point(color = "black", size = 2.5) +
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +
  annotate(
    "label",
    x = max(combined_monthly_all$AvgTemp, na.rm = TRUE) - 30,
    y = max(combined_monthly_all$Visitors, na.rm = TRUE) * 0.9,
    label = eq_text,
    color = "white",
    fill = "black",
    size = 4,
    label.r = unit(0.25, "lines")
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Visitors vs. Temperature in Acadia National Park (2015–2024)",
    x = "Average Temperature (°F)",
    y = "Monthly Visitors"
  )

# Visitors vs. Temperature in Acadia National Park (2015–2024)
model <- lm(AvgVisitors ~ AvgTemp, data = combined_monthly)

a <- coef(model)[1]
b <- coef(model)[2]
r2 <- summary(model)$r.squared

eq_text <- paste0(
  "y = ", round(b, 1), "x + (", round(a, 0), ")",
  "\nR² = ", round(r2, 3)
)

ggplot(combined_monthly, aes(x = AvgTemp, y = AvgVisitors, label = Month)) +
  geom_point(color = "black", size = 3) +
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +
  geom_text(vjust = -1, size = 3) +
  annotate(
    "label",
    x = max(combined_monthly$AvgTemp) - 20,
    y = max(combined_monthly$AvgVisitors) * 0.9,
    label = eq_text,
    color = "white",
    fill = "black",
    size = 4,
    label.r = unit(0.25, "lines")
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K")
  ) +
  theme_minimal() +
  labs(
    title = "Visitors vs. Temperature in Acadia National Park (2015–2024)",
    x = "Average Temperature (°F)",
    y = "Average Monthly Visitors"
  )

# Visitors vs. Temperature in Acadia National Park (2015–2024)
model_logistic <- nls(
  AvgVisitors ~ max_vis / (1 + exp(-k * (AvgTemp - t0))),
  data = combined_monthly,
  start = list(
    max_vis = max(combined_monthly$AvgVisitors),
    k = 0.2,
    t0 = 55
  )
)

combined_monthly$PredVisitors <- predict(model_logistic)

r2_logistic <- cor(combined_monthly$AvgVisitors, combined_monthly$PredVisitors)^2

params <- coef(model_logistic)
eq_text <- paste0(
  "y = ", round(params['max_vis'], 0),
  " / (1 + e^{−", round(params['k'], 2),
  "·(x − ", round(params['t0'], 1), ")})",
  "\nR² = ", round(r2_logistic, 3)
)

temp_seq <- seq(min(combined_monthly$AvgTemp), max(combined_monthly$AvgTemp), length.out = 200)
pred_df <- data.frame(
  AvgTemp = temp_seq,
  PredVisitors = predict(model_logistic, newdata = data.frame(AvgTemp = temp_seq))
)

ggplot() +
  geom_point(
    data = combined_monthly,
    aes(x = AvgTemp, y = AvgVisitors),
    color = "black", size = 3
  ) +
  geom_text(
    data = combined_monthly,
    aes(x = AvgTemp, y = AvgVisitors, label = Month),
    vjust = -1, size = 3
  ) +
  geom_line(
    data = pred_df,
    aes(x = AvgTemp, y = PredVisitors),
    color = "darkorange", size = 1
  ) +
  annotate(
    "label",
    x = min(combined_monthly$AvgTemp) + 18,
    y = max(combined_monthly$AvgVisitors) * 0.9,
    label = eq_text,
    color = "white",
    fill = "black",
    size = 4,
    label.r = unit(0.25, "lines"),
    family = "mono"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  theme_minimal() +
  labs(
    title = "Visitors vs. Temperature in Acadia National Park (2015–2024)",
    x = "Average Temperature (°F)",
    y = "Average Monthly Visitors"
  )


# === 6. EMPLOYMENT GRAPHS ===

# Hancock County Employment by Industry (2015–2024)
hancock <- employment %>%
  filter(Area == "Hancock County, Maine", Industry != "All Jobs") %>%
  arrange(Industry, Date) %>%   
  group_by(Industry) %>%
  mutate(point_index = row_number()) %>%  
  ungroup()

point_shapes <- c(15, 16, 17, 18, 0, 1, 2, 5, 6, 7, 8, 9)  

colors_11 <-  c(
  "#FF0000",
  "#FF7F00", 
  "#FFFF00",  
  "#7FFF00",  
  "#00FF00",  
  "#00FF7F",  
  "#00FFFF",  
  "#007FFF",  
  "#0000FF",  
  "#7F00FF",  
  "#FF00FF"   
)

ggplot(hancock, aes(x = Date, y = Employment, color = Industry)) +
  geom_line(linewidth = 0.8, alpha = 0.5) +
  geom_point(
    data = hancock %>% filter((point_index - 1) %% 12 == 0),
    aes(shape = Industry),
    size = 3
  ) +
  scale_color_manual(values = colors_11) +
  scale_shape_manual(values = point_shapes) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Hancock County Employment by Industry (2015–2024)",
    x = "Date",
    y = "Employment",
    color = "Industry",
    shape = "Industry"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16)
  )

# Hancock County Elasticity of Employment Relative to Acadia Visitors

hancock_monthly <- emp_monthly_avg%>%
  filter(Area == "Hancock County, Maine", Industry != "All Jobs")

elasticity_base <- hancock_monthly %>%
  left_join(visitors_monthly_avg %>% select(Month, AvgVisitors), by = c("MonthLabel" = "Month"))

visitor_change <- elasticity_base %>%
  arrange(MonthNum) %>%
  distinct(MonthNum, AvgVisitors) %>%
  mutate(
    pct_visitors = (AvgVisitors - lag(AvgVisitors)) / lag(AvgVisitors) * 100
  ) %>%
  mutate(
    pct_visitors = ifelse(
      is.na(pct_visitors),
      (AvgVisitors[MonthNum == 1] - AvgVisitors[MonthNum == 12]) /
        AvgVisitors[MonthNum == 12] * 100,
      pct_visitors
    )
  )

elasticity_base <- elasticity_base %>%
  left_join(visitor_change %>% select(MonthNum, pct_visitors),
            by = "MonthNum")

elasticity_base <- elasticity_base %>%
  arrange(Industry, MonthNum) %>%
  group_by(Industry) %>%
  mutate(
    pct_employment =
      (AvgEmployment - lag(AvgEmployment)) / lag(AvgEmployment) * 100,
    pct_employment = ifelse(
      is.na(pct_employment),
      (AvgEmployment[MonthNum == 1] - AvgEmployment[MonthNum == 12]) /
        AvgEmployment[MonthNum == 12] * 100,
      pct_employment
    )
  ) %>%
  ungroup()

elasticity_base <- elasticity_base %>%
  mutate(
    elasticity = pct_employment / pct_visitors,
    MonthLabel = factor(MonthLabel, levels = month.abb, ordered = TRUE)
  )

ggplot(elasticity_base,
       aes(x = MonthLabel, y = elasticity, color = Industry)) +
  
  geom_line(aes(group = Industry), linewidth = 1, alpha = 0.6) +
  geom_point(aes(shape = Industry), size = 3) +
  
  scale_color_manual(values = colors_11) +
  scale_shape_manual(values = point_shapes) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Hancock County Elasticity of Employment Relative to Acadia Visitors",
    x = "Month",
    y = "Elasticity",
    color = "Industry",
    shape = "Industry"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title  = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Hancock County Seasonality Contribution Index (SCI) by Industry (Peak Season)

industry_correlation <- elasticity_base %>%
  group_by(Industry) %>%
  summarize(
    correlation = cor(AvgEmployment, AvgVisitors, use = "complete.obs")
  )

peak_season <- elasticity_base %>%
  filter(MonthLabel %in% c("Jul", "Aug", "Sep")) %>%
  group_by(Industry) %>%
  summarize(peak_emp = sum(AvgEmployment)) %>%  
  ungroup() %>%
  mutate(
    peak_share = peak_emp / sum(peak_emp)       
  )

SCI_peak <- industry_correlation %>%
  left_join(peak_season, by = "Industry") %>%
  mutate(SCI_peak = abs(correlation) * peak_share) %>%
  arrange(desc(SCI_peak))

SCI_peak <- SCI_peak %>%
  arrange(desc(SCI_peak)) %>%
  mutate(Industry = factor(Industry, levels = rev(Industry)))  

ggplot(SCI_peak, aes(x = Industry, y = SCI_peak, fill = SCI_peak)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.3f", SCI_peak)),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, max(SCI_peak$SCI_peak) * 1.1, by = 0.05),   
    labels = function(x) sprintf("%.2f", x)                      
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Hancock County Seasonality Contribution Index (SCI) by Industry (Peak Season)",
    x = "Industry",
    y = "SCI (Correlation × Peak Season Share)",
    fill = "SCI"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold")
  ) +
  expand_limits(x = max(SCI_peak$SCI_peak) * 1.25)

# Employment by Industry and County (2015–2024)

top3 <- c(
  "Professional and business services",
  "Leisure and hospitality",
  "Trade, transportation, and utilities"
)

df <- employment %>%
  filter(Industry %in% c("All Jobs", top3)) %>%
  group_by(Area, Date) %>%
  mutate(AllJobs = Employment[Industry == "All Jobs"]) %>%
  ungroup()

everything_else <- employment %>%
  filter(!(Industry %in% c("All Jobs", top3))) %>%
  group_by(Area, Date, Tier, Approx_Distance_miles) %>%
  summarize(Employment = sum(Employment, na.rm = TRUE), .groups = "drop") %>%
  mutate(Industry = "Excluding Top 3 Industries")

plot_df <- df %>%
  select(Area, Tier, Approx_Distance_miles, Date, Industry, Employment) %>%
  bind_rows(everything_else)

plot_df <- plot_df %>%
  mutate(
    FacetLabel = glue("{Area}\n({Approx_Distance_miles} miles from Acadia)")
  )

plot_df$FacetLabel <- fct_reorder(plot_df$FacetLabel, plot_df$Tier, .fun = min)

plot_df <- plot_df %>%
  mutate(Industry = ifelse(Industry == "All Jobs", "All Industries", Industry))

color_map <- c(
  "All Industries" = "black",
  "Professional and business services" = "#007FFF",
  "Leisure and hospitality" = "#FF0000",
  "Trade, transportation, and utilities" = "#00AA00",
  "Excluding Top 3 Industries" = "#FFA500"
)

ggplot(plot_df, aes(x = Date, y = Employment, color = Industry)) +
  geom_line(size = 1) +
  facet_wrap(~ FacetLabel, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, .05)), limits = c(0, NA)) +
  scale_color_manual(values = color_map) +
  labs(
    title = "Employment by Industry and County (2015–2024)",
    x = "Date",
    y = "Employment",
    color = "Industry"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

# Employment and Monthly Acadia Visitor Trends (2015–2024)

emp_wide <- emp_monthly_avg %>%
  filter(Industry %in% c("All Jobs", top3)) %>%
  select(Area, Tier, Approx_Distance_miles, Industry, MonthNum, MonthLabel, AvgEmployment) %>%
  pivot_wider(
    names_from = Industry,
    values_from = AvgEmployment
  ) %>%
  mutate(
    `Excluding Top 3 Industries` = `All Jobs` -
      `Leisure and hospitality` -
      `Professional and business services` -
      `Trade, transportation, and utilities`
  )

plot_data <- emp_wide %>%
  pivot_longer(
    cols = c(`All Jobs`,
             `Leisure and hospitality`,
               `Professional and business services`,
               `Trade, transportation, and utilities`,
             `Excluding Top 3 Industries`),
    names_to = "EmploymentType",
    values_to = "Employment"
  )

visitors_aligned <- visitors_monthly_avg %>%
  mutate(MonthNum = match(Month, month.abb))

plot_data <- plot_data %>%
  left_join(visitors_aligned %>% select(MonthNum, AvgVisitors),
            by = "MonthNum")

plot_data <- plot_data %>%
  mutate(
    FacetLabel = glue("{Area}\n({Approx_Distance_miles} miles from Acadia)")
  )

plot_data$FacetLabel <- fct_reorder(plot_data$FacetLabel, plot_data$Tier, min)

plot_data <- plot_data %>%
  group_by(FacetLabel) %>%
  mutate(
    MaxEmp = max(Employment, na.rm = TRUE),
    VisitorShape = (AvgVisitors / max(AvgVisitors, na.rm = TRUE)) * MaxEmp
  ) %>%
  ungroup()

plot_data <- plot_data %>%
  mutate(EmploymentType = ifelse(EmploymentType == "All Jobs", "All Industries", EmploymentType))

ggplot(plot_data, aes(x = MonthNum, y = Employment, color = EmploymentType)) +
  geom_line(size = 1.1) +
  geom_point(size = 1.5) +
  geom_line(aes(y = VisitorShape),
            color = "black", linetype = "dashed", size = 1) +
  geom_point(aes(y = VisitorShape),
             color = "black", size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = color_map) +
  facet_wrap(~ FacetLabel, ncol = 2, scales = "free_y") +
  labs(
    title = "Employment and Monthly Acadia Visitor Trends (2015–2024)",
    x = "Month",
    y = "Employment",
    color = "Employment Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)
  )

# Hancock County Seasonal Employment Increase from Low to Peak Season (2015–2024)

all_jobs <- hancock_monthly %>%
  group_by(MonthNum, MonthLabel) %>%
  summarize(
    AvgEmployment = sum(AvgEmployment, na.rm = TRUE),
    Industry = "All Industries",
    .groups = "drop"
  )

combined <- bind_rows(hancock_monthly, all_jobs)

jan_aug <- combined %>%
  filter(MonthNum %in% c(1, 8)) %>%
  select(Industry, MonthNum, AvgEmployment) %>%
  pivot_wider(
    names_from = MonthNum,
    values_from = AvgEmployment,
    names_prefix = "Month_"
  ) %>%
  mutate(
    Change = Month_8 - Month_1
  )

total_change <- jan_aug$Change[jan_aug$Industry == "All Industries"]

jan_aug <- jan_aug %>%
  mutate(
    PercentOfTotal = ifelse(
      Industry != "All Industries",
      round(100 * Change / total_change, 1),
      NA
    ),
    Label = ifelse(
      Industry == "All Industries",
      paste0("Δ = ", Change),
      paste0("Δ = ", Change, " (", PercentOfTotal, "% of Total Δ)")
    )
  )

colors_12 <- c(
  "All Industries" = "black",
  "Construction" = "#FF0000",
  "Education and health services" = "#FF7F00",
  "Financial activities" = "#FFFF00",
  "Government" = "#7FFF00",
  "Information" = "#00FF00",
  "Leisure and hospitality" = "#00FF7F",
  "Manufacturing" = "#00FFFF",
  "Natural resources and mining" = "#007FFF",
  "Other services" = "#0000FF",
  "Professional and business services" = "#7F00FF",
  "Trade, transportation, and utilities" = "#FF00FF"
)

ggplot(jan_aug, aes(x = reorder(Industry, Change), y = Change, fill = Industry)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = Label), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = colors_12) +
  labs(
    title = "Hancock County Average Seasonal Employment Increase from Low to Peak Season (2015–2024)",
    subtitle = "Low Season = January & Peak Season = August",
    x = "",
    y = "Employment Change"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))


# === 7. FINAL ANALYSIS ===

# Hancock County Monthly Employment vs. Acadia National Park Monthly Visitors (2015–2024)
hancock <- emp_monthly_avg %>%
  filter(Area == "Hancock County, Maine")

everything_else <- hancock %>%
  filter(!(Industry %in% c("All Jobs", top3))) %>%
  group_by(MonthNum, MonthLabel) %>%
  summarize(
    AvgEmployment = sum(AvgEmployment, na.rm = TRUE),
    Industry = "Excluding Top 3 Industries",
    .groups = "drop"
  )

hancock_plot <- hancock %>%
  filter(Industry %in% c("All Jobs", top3)) %>%
  bind_rows(everything_else) %>%
  left_join(visitors_monthly_avg, by = c("MonthLabel" = "Month"))

eq_labels <- hancock_plot %>%
  group_by(Industry) %>%
  summarise(
    lm_fit = list(lm(AvgEmployment ~ AvgVisitors)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    slope = coef(lm_fit)[2],
    intercept = coef(lm_fit)[1],
    r2 = summary(lm_fit)$r.squared,
    label = paste0("y = ", round(intercept, 2), 
                   " + ", round(slope, 5), "x\nR² = ", round(r2,2)),
    x_pos = max(hancock_plot$AvgVisitors[hancock_plot$Industry == Industry]) + 100000,
    y_pos = max(hancock_plot$AvgEmployment[hancock_plot$Industry == Industry]) + 300
  )

ggplot(hancock_plot, aes(x = AvgVisitors, y = AvgEmployment, color = ifelse(Industry == "All Jobs", "All Industries", Industry))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  geom_text(
    data = eq_labels,
    aes(
      x = x_pos, 
      y = y_pos, 
      label = label, 
      color = ifelse(Industry == "All Jobs", "All Industries", Industry)
    ),
    hjust = 1, size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = color_map) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title = "Hancock County Monthly Employment vs. Acadia National Park Monthly Visitors (2015–2024)",
    x = "Average Monthly Visitors",
    y = "Average Monthly Employment",
    color = "Industry"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

# Interpreting the slopes
slopes_table <- eq_labels %>%
  mutate(
    Industry = ifelse(Industry == "All Jobs", "All Industries", Industry),
    InverseSlope = 1 / slope
  ) %>%
  select(Industry, slope, InverseSlope, r2)

slopes_table_plot <- slopes_table %>%
  select(Industry, slope, InverseSlope) %>%
  mutate(
    slope = round(slope, 6),
    InverseSlope = round(InverseSlope)
  ) %>%
  gt() %>%
  tab_header(
    title = "Employment Sensitivity to Park Visitation",
    subtitle = "Slope and Inverse Slope of Employment ~ Visitors Regression"
  ) %>%
  cols_label(
    Industry = "Industry",
    slope = "Slope",
    InverseSlope = "Inverse Slope"
  ) %>%
  fmt_number(
    columns = slope,
    decimals = 5
  ) %>%
  fmt_number(
    columns = InverseSlope,
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.align = "center"
  )