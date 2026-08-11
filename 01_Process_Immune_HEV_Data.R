# ============================================================
# IMMUNE INFILTRATION AND HEV ANALYSIS
#
# Purpose:
# Process QuPath tumour and lymphoid-aggregate measurements
# and prepare mouse-level data for statistical analysis and
# plotting in GraphPad Prism.
#
# Final metrics:
#   1. Aggregate density (aggregates/mm² tumour)
#   2. Aggregate area (% tumour area)
#   3. Mean aggregate area (µm²)
#   4. Tumour HEV-positive area (% tumour area)
#   5. Aggregate-associated HEV-positive area (% aggregate area)
#
# Raw area measurements from QuPath = µm²
# ============================================================


# ============================================================
# STEP 1. Install and load required packages
# ============================================================

required_packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "openxlsx"
)

packages_to_install <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)


# ============================================================
# STEP 2. Define project folders
# ============================================================

# Open Immune_HEV_Analysis.Rproj before running this script.

raw_dir <- "Raw_data"
output_dir <- "Processed_data"

if (!dir.exists(raw_dir)) {
  stop(
    "The folder 'Raw_data' was not found. ",
    "Open the project using Immune_HEV_Analysis.Rproj."
  )
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

output_file <- file.path(
  output_dir,
  "Immune_HEV_Prism_Data.xlsx"
)


# ============================================================
# STEP 3. Locate the raw Excel workbook
# ============================================================

excel_files <- list.files(
  path = raw_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(excel_files) != 1) {
  stop(
    "Exactly one Excel workbook is required in Raw_data. ",
    "Number found: ",
    length(excel_files)
  )
}

raw_file <- excel_files[1]

cat(
  "\nRaw workbook located successfully:\n",
  basename(raw_file),
  "\n",
  sep = ""
)


# ============================================================
# STEP 4. Check required worksheets
# ============================================================

sheet_names <- excel_sheets(raw_file)

required_sheets <- c(
  "Tumour_HEV",
  "Aggregates"
)

missing_sheets <- setdiff(
  required_sheets,
  sheet_names
)

if (length(missing_sheets) > 0) {
  stop(
    "Required worksheet(s) missing: ",
    paste(missing_sheets, collapse = ", ")
  )
}


# ============================================================
# STEP 5. Import tumour-level raw data
# ============================================================

# Row 3 contains the actual column headings.
# Blank cells and cells containing "NA" are treated as
# missing values during import.

tumour_raw <- read_excel(
  raw_file,
  sheet = "Tumour_HEV",
  skip = 2,
  na = c("", "NA")
) |>
  clean_names() |>
  select(
    mouse_id,
    treatment,
    tumour_area,
    hev_positive_area
  ) |>
  filter(
    !is.na(mouse_id)
  ) |>
  transmute(
    Mouse_ID = str_squish(
      as.character(mouse_id)
    ),
    Treatment = str_squish(
      as.character(treatment)
    ),
    Tumour_Area_um2 = as.numeric(
      tumour_area
    ),
    Tumour_HEV_Positive_Area_um2 = as.numeric(
      hev_positive_area
    )
  )

if (nrow(tumour_raw) == 0) {
  stop("No tumour-level data were imported.")
}

if (anyDuplicated(tumour_raw$Mouse_ID) > 0) {
  
  duplicate_mice <- tumour_raw |>
    count(Mouse_ID) |>
    filter(n > 1)
  
  print(duplicate_mice)
  
  stop(
    "Duplicate Mouse_ID values were detected in Tumour_HEV."
  )
}

if (any(is.na(tumour_raw$Tumour_Area_um2))) {
  stop(
    "Missing tumour-area measurements were detected."
  )
}

if (any(tumour_raw$Tumour_Area_um2 <= 0)) {
  stop(
    "Tumour-area measurements must be greater than zero."
  )
}

if (any(
  tumour_raw$Tumour_HEV_Positive_Area_um2 < 0,
  na.rm = TRUE
)) {
  stop(
    "Negative HEV-positive tumour areas were detected."
  )
}


# ============================================================
# STEP 6. Import aggregate-level raw data
# ============================================================

# Blank cells and cells containing "NA" are treated as
# genuine missing values before numerical conversion.

aggregate_raw <- read_excel(
  raw_file,
  sheet = "Aggregates",
  skip = 2,
  na = c("", "NA")
) |>
  clean_names() |>
  select(
    mouse_id,
    treatment,
    aggregate_id,
    aggregate_area,
    hev_positive_area,
    intratumoural_peripheral
  ) |>
  filter(
    !is.na(mouse_id)
  ) |>
  transmute(
    Mouse_ID = str_squish(
      as.character(mouse_id)
    ),
    Treatment = str_squish(
      as.character(treatment)
    ),
    Aggregate_ID = str_squish(
      as.character(aggregate_id)
    ),
    Aggregate_Area_um2 = as.numeric(
      aggregate_area
    ),
    Aggregate_HEV_Positive_Area_um2 = as.numeric(
      hev_positive_area
    ),
    Location = str_squish(
      as.character(intratumoural_peripheral)
    )
  )

cat(
  "\nData imported successfully:\n",
  "Tumours: ",
  nrow(tumour_raw),
  "\nAggregate-table rows: ",
  nrow(aggregate_raw),
  "\n",
  sep = ""
)


# ============================================================
# STEP 7. Validate treatment assignments
# ============================================================

valid_treatments <- c(
  "Vehicle",
  "29P"
)

unexpected_tumour_treatments <- tumour_raw |>
  filter(
    !Treatment %in% valid_treatments
  )

if (nrow(unexpected_tumour_treatments) > 0) {
  
  print(unexpected_tumour_treatments)
  
  stop(
    "Unexpected treatment labels were found in Tumour_HEV."
  )
}

unexpected_aggregate_treatments <- aggregate_raw |>
  filter(
    !is.na(Treatment),
    !Treatment %in% valid_treatments
  )

if (nrow(unexpected_aggregate_treatments) > 0) {
  
  print(unexpected_aggregate_treatments)
  
  stop(
    "Unexpected treatment labels were found in Aggregates."
  )
}


# ============================================================
# STEP 8. Check Mouse IDs across worksheets
# ============================================================

aggregate_mouse_ids <- aggregate_raw |>
  distinct(Mouse_ID)

unknown_aggregate_mice <- anti_join(
  aggregate_mouse_ids,
  tumour_raw |>
    select(Mouse_ID),
  by = "Mouse_ID"
)

if (nrow(unknown_aggregate_mice) > 0) {
  
  print(unknown_aggregate_mice)
  
  stop(
    "The Aggregates sheet contains Mouse IDs that are absent ",
    "from the Tumour_HEV sheet."
  )
}


# ============================================================
# STEP 9. Identify genuine aggregate rows
# ============================================================

# Rows without an Aggregate_ID and aggregate area represent
# mice for which no lymphoid aggregate was identified.
#
# These mice remain in the tumour-level dataset but are not
# treated as individual aggregates.

aggregates_present <- aggregate_raw |>
  filter(
    !is.na(Aggregate_ID),
    !is.na(Aggregate_Area_um2)
  )

if (any(
  aggregates_present$Aggregate_Area_um2 <= 0,
  na.rm = TRUE
)) {
  stop(
    "Aggregate-area measurements must be greater than zero."
  )
}

if (any(
  aggregates_present$Aggregate_HEV_Positive_Area_um2 < 0,
  na.rm = TRUE
)) {
  stop(
    "Negative aggregate HEV-positive areas were detected."
  )
}

cat(
  "\nValid lymphoid aggregates identified: ",
  nrow(aggregates_present),
  "\n",
  sep = ""
)


# ============================================================
# STEP 10. Check treatment consistency between sheets
# ============================================================

aggregate_treatment_check <- aggregates_present |>
  select(
    Mouse_ID,
    Aggregate_Treatment = Treatment
  ) |>
  distinct() |>
  left_join(
    tumour_raw |>
      select(
        Mouse_ID,
        Tumour_Treatment = Treatment
      ),
    by = "Mouse_ID"
  ) |>
  filter(
    Aggregate_Treatment != Tumour_Treatment
  )

if (nrow(aggregate_treatment_check) > 0) {
  
  print(aggregate_treatment_check)
  
  stop(
    "Treatment assignments differ between the Tumour_HEV ",
    "and Aggregates sheets."
  )
}


# ============================================================
# STEP 11. Summarise aggregate measurements per mouse
# ============================================================

aggregate_summary <- aggregates_present |>
  group_by(
    Mouse_ID
  ) |>
  summarise(
    
    Aggregate_Count = n(),
    
    Total_Aggregate_Area_um2 = sum(
      Aggregate_Area_um2,
      na.rm = TRUE
    ),
    
    Mean_Aggregate_Area_um2 = mean(
      Aggregate_Area_um2,
      na.rm = TRUE
    ),
    
    Total_Aggregate_HEV_Positive_Area_um2 = sum(
      Aggregate_HEV_Positive_Area_um2,
      na.rm = TRUE
    ),
    
    Intratumoural_Aggregates = sum(
      str_to_lower(Location) == "intratumoural",
      na.rm = TRUE
    ),
    
    Peripheral_Aggregates = sum(
      str_to_lower(Location) == "peripheral",
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# STEP 12. Create complete mouse-level dataset
# ============================================================

mouse_level_data <- tumour_raw |>
  left_join(
    aggregate_summary,
    by = "Mouse_ID"
  ) |>
  mutate(
    
    # --------------------------------------------------------
    # Convert tumour area from µm² to mm²
    # --------------------------------------------------------
    
    Tumour_Area_mm2 =
      Tumour_Area_um2 / 1000000,
    
    
    # --------------------------------------------------------
    # Mice without aggregates
    # --------------------------------------------------------
    
    Aggregate_Count = replace_na(
      Aggregate_Count,
      0L
    ),
    
    Total_Aggregate_Area_um2 = replace_na(
      Total_Aggregate_Area_um2,
      0
    ),
    
    Total_Aggregate_HEV_Positive_Area_um2 = replace_na(
      Total_Aggregate_HEV_Positive_Area_um2,
      0
    ),
    
    Intratumoural_Aggregates = replace_na(
      Intratumoural_Aggregates,
      0L
    ),
    
    Peripheral_Aggregates = replace_na(
      Peripheral_Aggregates,
      0L
    ),
    
    
    # --------------------------------------------------------
    # Metric 1. Aggregate density
    # --------------------------------------------------------
    
    Aggregate_Density_per_mm2 =
      Aggregate_Count /
      Tumour_Area_mm2,
    
    
    # --------------------------------------------------------
    # Metric 2. Aggregate area (% tumour area)
    # --------------------------------------------------------
    
    Aggregate_Area_Percent =
      (
        Total_Aggregate_Area_um2 /
          Tumour_Area_um2
      ) * 100,
    
    
    # --------------------------------------------------------
    # Metric 3. Mean aggregate area
    # --------------------------------------------------------
    
    # Mean_Aggregate_Area_um2 remains NA for mice in which
    # no aggregate was identified because a mean aggregate
    # size does not exist.
    
    
    # --------------------------------------------------------
    # Metric 4. Tumour HEV-positive area (% tumour area)
    # --------------------------------------------------------
    
    Tumour_HEV_Positive_Percent =
      (
        Tumour_HEV_Positive_Area_um2 /
          Tumour_Area_um2
      ) * 100,
    
    
    # --------------------------------------------------------
    # Metric 5. Aggregate-associated HEV-positive area
    # --------------------------------------------------------
    
    Aggregate_HEV_Positive_Percent =
      if_else(
        Total_Aggregate_Area_um2 > 0,
        (
          Total_Aggregate_HEV_Positive_Area_um2 /
            Total_Aggregate_Area_um2
        ) * 100,
        NA_real_
      )
  )


# ============================================================
# STEP 13. Arrange final mouse-level table
# ============================================================

mouse_level_data <- mouse_level_data |>
  select(
    Mouse_ID,
    Treatment,
    
    Tumour_Area_um2,
    Tumour_Area_mm2,
    Tumour_HEV_Positive_Area_um2,
    
    Aggregate_Count,
    Intratumoural_Aggregates,
    Peripheral_Aggregates,
    
    Total_Aggregate_Area_um2,
    Mean_Aggregate_Area_um2,
    Total_Aggregate_HEV_Positive_Area_um2,
    
    Aggregate_Density_per_mm2,
    Aggregate_Area_Percent,
    Tumour_HEV_Positive_Percent,
    Aggregate_HEV_Positive_Percent
  ) |>
  arrange(
    factor(
      Treatment,
      levels = c(
        "Vehicle",
        "29P"
      )
    ),
    Mouse_ID
  )


# ============================================================
# STEP 14. Final data-quality checks
# ============================================================

if (anyDuplicated(mouse_level_data$Mouse_ID) > 0) {
  stop(
    "Duplicate Mouse_ID values remain in the final dataset."
  )
}

if (any(
  mouse_level_data$Aggregate_Area_Percent > 100,
  na.rm = TRUE
)) {
  warning(
    "At least one aggregate-area percentage exceeds 100%. ",
    "Check the raw measurements."
  )
}

if (any(
  mouse_level_data$Tumour_HEV_Positive_Percent > 100,
  na.rm = TRUE
)) {
  warning(
    "At least one tumour HEV-positive percentage exceeds 100%. ",
    "Check the raw measurements."
  )
}

if (any(
  mouse_level_data$Aggregate_HEV_Positive_Percent > 100,
  na.rm = TRUE
)) {
  warning(
    "At least one aggregate HEV-positive percentage exceeds 100%. ",
    "Check the raw measurements."
  )
}


# ============================================================
# STEP 15. Round exported numerical values
# ============================================================

# Counts remain integers.
# Continuous numerical measurements are rounded to a maximum
# of three decimal places in the exported workbook.

mouse_level_export <- mouse_level_data |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )


# ============================================================
# STEP 16. Create Prism-ready endpoint tables
# ============================================================

create_prism_table <- function(
    data,
    metric
) {
  
  data |>
    select(
      Treatment,
      all_of(metric)
    ) |>
    filter(
      !is.na(.data[[metric]])
    ) |>
    group_by(
      Treatment
    ) |>
    mutate(
      Row = row_number()
    ) |>
    ungroup() |>
    pivot_wider(
      id_cols = Row,
      names_from = Treatment,
      values_from = all_of(metric)
    ) |>
    select(
      any_of(
        c(
          "Vehicle",
          "29P"
        )
      )
    )
}


prism_aggregate_density <- create_prism_table(
  mouse_level_export,
  "Aggregate_Density_per_mm2"
)

prism_aggregate_area_percent <- create_prism_table(
  mouse_level_export,
  "Aggregate_Area_Percent"
)

prism_mean_aggregate_area <- create_prism_table(
  mouse_level_export,
  "Mean_Aggregate_Area_um2"
)

prism_tumour_hev_percent <- create_prism_table(
  mouse_level_export,
  "Tumour_HEV_Positive_Percent"
)

prism_aggregate_hev_percent <- create_prism_table(
  mouse_level_export,
  "Aggregate_HEV_Positive_Percent"
)


# ============================================================
# STEP 17. Export one final Excel workbook
# ============================================================

workbook <- createWorkbook()

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)


add_formatted_sheet <- function(
    workbook,
    sheet_name,
    data
) {
  
  addWorksheet(
    workbook,
    sheetName = sheet_name
  )
  
  writeData(
    workbook,
    sheet = sheet_name,
    x = data,
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  freezePane(
    workbook,
    sheet = sheet_name,
    firstRow = TRUE
  )
  
  setColWidths(
    workbook,
    sheet = sheet_name,
    cols = seq_along(data),
    widths = "auto"
  )
}


add_formatted_sheet(
  workbook,
  "Mouse_Level_Data",
  mouse_level_export
)

add_formatted_sheet(
  workbook,
  "Prism_Aggregate_Density",
  prism_aggregate_density
)

add_formatted_sheet(
  workbook,
  "Prism_Aggregate_Area",
  prism_aggregate_area_percent
)

add_formatted_sheet(
  workbook,
  "Prism_Mean_Aggregate_Area",
  prism_mean_aggregate_area
)

add_formatted_sheet(
  workbook,
  "Prism_Tumour_HEV",
  prism_tumour_hev_percent
)

add_formatted_sheet(
  workbook,
  "Prism_Aggregate_HEV",
  prism_aggregate_hev_percent
)


saveWorkbook(
  workbook,
  file = output_file,
  overwrite = TRUE
)


# ============================================================
# STEP 18. Completion message
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "IMMUNE INFILTRATION AND HEV ANALYSIS COMPLETED SUCCESSFULLY\n",
  "============================================================\n",
  "Mice analysed: ",
  nrow(mouse_level_export),
  "\n",
  "Vehicle mice: ",
  sum(mouse_level_export$Treatment == "Vehicle"),
  "\n",
  "29P mice: ",
  sum(mouse_level_export$Treatment == "29P"),
  "\n",
  "Aggregates analysed: ",
  nrow(aggregates_present),
  "\n",
  "\nMetrics exported:\n",
  "  - Aggregate density (aggregates/mm² tumour)\n",
  "  - Aggregate area (% tumour area)\n",
  "  - Mean aggregate area (µm²)\n",
  "  - Tumour HEV-positive area (% tumour area)\n",
  "  - Aggregate-associated HEV-positive area (% aggregate area)\n",
  "\nFinal workbook:\n",
  output_file,
  "\n",
  "============================================================\n",
  sep = ""
)