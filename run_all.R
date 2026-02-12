# =============================================================================
# run_all.R — Single entry point for the CGT Tiered Discount Model
# =============================================================================
# Usage:
#   1. Open this project in RStudio (File > Open Project > cgt-tiered-discount-model.Rproj)
#   2. source("run_all.R")
#
# This script:
#   - Generates synthetic data (if not already present in data/)
#   - Runs the baseline banded taper calibration
#   - Prints tier summary, calibration quality, and loss decomposition
#   - Generates diagnostic plots
#   - Optionally runs the behavioural weight variant
#
# Configuration: edit the flags below to control what runs.
# =============================================================================

# --- Configuration ---
GENERATE_DATA       <- TRUE    # Set FALSE to skip data generation (uses existing CSV)
RUN_BASELINE        <- TRUE    # Run proposal-weight calibration
RUN_BEHAVIOURAL     <- FALSE   # Run behavioural weight variant (set TRUE to enable)
N_POPULATION        <- 200000  # Population size for data generation
SEED                <- 2026

# --- Setup ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(MASS)

cat("=============================================================\n")
cat("  CGT Tiered Discount Model — Full Pipeline\n")
cat(sprintf("  Timestamp: %s\n", Sys.time()))
cat(sprintf("  Seed: %d | N: %s\n", SEED, format(N_POPULATION, big.mark=",")))
cat("=============================================================\n\n")

# Ensure output directories exist
if (!dir.exists("data"))    dir.create("data", recursive = TRUE)
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

# --- Step 1: Generate synthetic data ---
data_path <- file.path("data", "ato_synthetic_family_groups.csv")

if (GENERATE_DATA || !file.exists(data_path)) {
  cat(">>> Step 1: Generating synthetic data...\n")
  source(file.path("R", "generate_data.R"), local = TRUE)
  cat(">>> Data generation complete.\n\n")
} else {
  cat(">>> Step 1: Skipped (data already exists at", data_path, ")\n\n")
}

# --- Step 2: Run calibration ---

# Source shared functions
source(file.path("R", "taper_functions.R"))
source(file.path("R", "metrics.R"))
source(file.path("R", "plots.R"))

if (RUN_BASELINE) {
  cat(">>> Step 2a: Running baseline (proposal weights) calibration...\n")
  source(file.path("R", "calibration_taper.R"), local = TRUE)
  cat("\n>>> Baseline calibration complete.\n\n")
}

if (RUN_BEHAVIOURAL) {
  cat(">>> Step 2b: Running behavioural weight calibration...\n")
  source(file.path("R", "calibration_behavioural.R"), local = TRUE)
  cat("\n>>> Behavioural calibration complete.\n\n")
}

cat("=============================================================\n")
cat("  Pipeline complete. Check outputs/ for saved files.\n")
cat("=============================================================\n")
