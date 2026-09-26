# ============================================================
# 🗂️ Combine Employment Data (All Industries, 4 Maine Counties)
# ============================================================

library(tidyverse)
library(readxl)
library(stringr)

# 1️⃣ Folder containing your Excel files
data_folder <- "C:/Users/patri/Documents/Northeastern/Intro to Business Analytics/Final Project/Data/Employment"

# 2️⃣ Get list of all quarterly files (e.g., allhlcn151.xlsx → allhlcn244.xlsx)
file_list <- list.files(path = data_folder, pattern = "^allhlcn\\d{3}\\.xlsx$", full.names = TRUE)

# 3️⃣ Function to read each file and add Year + Quarter columns
read_qcew <- function(file) {
  file_name <- basename(file)
  info <- str_match(file_name, "allhlcn(\\d{2})(\\d)")
  year <- as.numeric(paste0("20", info[2]))
  quarter <- as.numeric(info[3])
  
  read_excel(file) %>%
    mutate(Year = year, Quarter = quarter)
}

# 4️⃣ Combine all files into a single dataset
employment_raw <- map_dfr(file_list, read_qcew)

# 5️⃣ Clean columns and reshape to long format
employment_clean <- employment_raw %>%
  select(
    Year, Quarter,
    Area, `St Name`, Ownership, Industry,
    `January Employment`, `February Employment`, `March Employment`,
    `April Employment`, `May Employment`, `June Employment`,
    `July Employment`, `August Employment`, `September Employment`,
    `October Employment`, `November Employment`, `December Employment`
  ) %>%
  pivot_longer(
    cols = matches("Employment$"),
    names_to = "Month",
    values_to = "Employment"
  ) %>%
  mutate(
    Month = str_remove(Month, " Employment"),
    Month = factor(Month, levels = month.name, labels = month.abb, ordered = TRUE)
  )

# 6️⃣ Define which months belong to which quarter
month_quarter_map <- tibble(
  Month = factor(month.abb, levels = month.abb, ordered = TRUE),
  Quarter = rep(1:4, each = 3)  # Jan–Mar = Q1, Apr–Jun = Q2, etc.
)

# 7️⃣ Define the 4 Maine counties and their distances
county_info <- tibble(
  Area = c("Hancock County, Maine", "Penobscot County, Maine", "Kennebec County, Maine", "Cumberland County, Maine"),
  Tier = c(1, 2, 3, 4),
  Approx_Distance_miles = c(0, 40, 90, 160)
)

# 8️⃣ Filter to 4 Maine counties and keep only the correct month-quarter combos
employment_4counties <- employment_clean %>%
  left_join(month_quarter_map, by = "Month", suffix = c("", "_correct")) %>%
  filter(Quarter == Quarter_correct) %>%
  select(-Quarter_correct) %>%
  filter(`St Name` == "Maine", Area %in% county_info$Area) %>%
  left_join(county_info, by = "Area") %>%
  arrange(Year, Quarter, Month, Area)

# ✅ Final dataset
head(employment_4counties)

# 9️⃣ Export clean combined dataset
write_csv(employment_4counties, file.path(data_folder, "employment_maine_4counties_allindustries.csv"))