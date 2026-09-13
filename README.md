# Interactive Dataset Cleaner & SQL Tool

An interactive R Shiny web application designed for data cleaning, visual diff previewing, and live SQL querying on structured datasets.

## Key Features
- **Multi-Format Loading:** Load datasets directly via URL (`CSV`, `TSV`, `JSON`, `XLSX`).
- **Staging & Preview System:** Inspect visual diffs and updated dimensions before committing data operations.
- **Automated Data Cleaning:** 
  - Date format parsing (`lubridate`)
  - Duplicate row removal
  - IQR outlier filtering
  - String normalization (whitespace, case, digit removal)
- **In-App SQL Console:** Run custom SQL queries directly against the dataset using `sqldf`.
- **Export:** Download processed datasets as clean CSV files.

## Tech Stack
- **Language & Framework:** R, R Shiny
- **Packages:** `tidyverse`, `sqldf`, `shinyjs`, `lubridate`, `openxlsx`, `httr`
- **UI/Styling:** Custom HTML/CSS, Google Fonts