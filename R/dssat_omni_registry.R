# Registry-driven DSSAT omniwrapper prototype
# This prototype is designed to extend the original DSSAT_wrapper toward
# multiple DSSAT model families using runtime inference from installed files.

suppressPackageStartupMessages({
  required_packages <- c("DSSAT", "dplyr", "tidyr", "lubridate")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Missing required R package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install them before sourcing DSSAT_omniwrapper.R."
    )
  }

  library("DSSAT")
  library("dplyr")
  library("tidyr")
  library("lubridate")
})

dssat_omni_family_map <- function() {
  list(
    BSCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    CSCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT", "PlantGr2.OUT")),
    CRGRO = list(adapter = "CROPGRO", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    MLCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    MZCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    MZIXM = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    PTSUB = list(adapter = "SUBSTOR", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    RICER = list(adapter = "RICE", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    SCCAN = list(adapter = "SUGARCANE", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    SCCSP = list(adapter = "SUGARCANE", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    SCSAM = list(adapter = "SUGARCANE", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    SGCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    SWCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    PIALO = list(adapter = "ALOHA", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    TRARO = list(adapter = "AROIDS", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    TNARO = list(adapter = "AROIDS", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    TFAPS = list(adapter = "NWHEAT", outputs = c("PlantGro.OUT", "Evaluate.OUT", "PlantGr2.OUT")),
    TFCER = list(adapter = "CERES", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    WHAPS = list(adapter = "NWHEAT", outputs = c("PlantGro.OUT", "Evaluate.OUT", "PlantGr2.OUT")),
    PRFRM = list(adapter = "FORAGE", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    SUOIL = list(adapter = "OILCROP", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    CSYCA = list(adapter = "CSYCA", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    CSCAS = list(adapter = "CSCAS", outputs = c("PlantGro.OUT", "Evaluate.OUT")),
    CSCRP = list(adapter = "CROPSIM", outputs = c("PlantGro.OUT", "Evaluate.OUT"))
  )
}

dssat_normalize_windows_profile_path <- function(root_path, profile_fragment) {
  fragment <- gsub("/", "\\\\", trimws(profile_fragment))
  fragment <- gsub("^\\\\+", "", fragment)
  root_name <- basename(normalizePath(root_path, winslash = "/", mustWork = FALSE))
  if (nzchar(root_name) && startsWith(toupper(fragment), toupper(root_name))) {
    fragment <- substring(fragment, nchar(root_name) + 1)
  }
  fragment <- gsub("^\\\\+", "", fragment)
  normalizePath(file.path(root_path, if (nzchar(fragment)) fragment else "."), winslash = "/", mustWork = FALSE)
}

dssat_extract_profile_value <- function(line) {
  value <- sub("^[A-Z0-9]{3}\\s+[A-Z]:\\s*", "", trimws(line))
  trimws(value)
}

dssat_read_simulation_registry <- function(dssat_path) {
  sim_file <- file.path(dssat_path, "SIMULATION.CDE")
  profile_file <- file.path(dssat_path, "DSSATPRO.V48")
  if (!file.exists(sim_file) || !file.exists(profile_file)) {
    stop("SIMULATION.CDE or DSSATPRO.V48 not found in DSSAT path.")
  }

  sim_lines <- readLines(sim_file, warn = FALSE, encoding = "UTF-8")
  profile_lines <- readLines(profile_file, warn = FALSE, encoding = "UTF-8")
  family_map <- dssat_omni_family_map()

  model_rows <- list()
  in_models <- FALSE
  for (line in sim_lines) {
    if (grepl("^\\*Simulation/Crop Models", line)) {
      in_models <- TRUE
      next
    }
    if (!in_models) {
      next
    }
    if (grepl("^\\*", line) && !grepl("^\\*Simulation/Crop Models", line)) {
      break
    }
    if (grepl("^\\s*@", line) || grepl("^\\s*!", line) || !nzchar(trimws(line))) {
      next
    }
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(parts) < 3) {
      next
    }
    model_code <- parts[1]
    crop_code <- parts[2]
    description <- paste(parts[-c(1, 2)], collapse = " ")
    model_rows[[length(model_rows) + 1]] <- data.frame(
      model_code = model_code,
      crop_code = crop_code,
      description = description,
      stringsAsFactors = FALSE
    )
  }
  registry <- dplyr::bind_rows(model_rows)

  crop_dir_rows <- list()
  default_model_rows <- list()
  dssat_root <- normalizePath(dssat_path, winslash = "/", mustWork = FALSE)
  dssat_root_name <- basename(dssat_root)
  for (line in profile_lines) {
    clean_line <- trimws(line)
    dir_match <- regexec("^([A-Z0-9]{2})D\\s+[A-Z]:\\s+(.+)$", clean_line)
    dir_parts <- regmatches(trimws(line), dir_match)[[1]]
    if (length(dir_parts) > 0) {
      profile_path <- dssat_extract_profile_value(clean_line)
      crop_dir_path <- dssat_normalize_windows_profile_path(dssat_root, profile_path)
      crop_dir_rows[[length(crop_dir_rows) + 1]] <- data.frame(
        crop_code = toupper(dir_parts[2]),
        crop_dir = basename(crop_dir_path),
        crop_dir_path = crop_dir_path,
        stringsAsFactors = FALSE
      )
      next
    }

    mod_match <- regexec("^M([A-Z0-9]{2})\\s+[A-Z]:\\s+(.+?)\\s+([A-Za-z0-9_.-]+\\.EXE)\\s+([A-Z0-9]{8})$", clean_line)
    mod_parts <- regmatches(clean_line, mod_match)[[1]]
    if (length(mod_parts) > 0) {
      default_model_rows[[length(default_model_rows) + 1]] <- data.frame(
        crop_code = toupper(mod_parts[2]),
        default_module = toupper(mod_parts[5]),
        default_exe = mod_parts[4],
        stringsAsFactors = FALSE
      )
    }
  }

  crop_dirs <- dplyr::bind_rows(crop_dir_rows)
  default_models <- dplyr::bind_rows(default_model_rows)

  registry <- registry %>%
    dplyr::left_join(crop_dirs, by = "crop_code") %>%
    dplyr::left_join(default_models, by = "crop_code") %>%
    dplyr::mutate(
      module_code_048 = paste0(model_code, "048"),
      adapter = vapply(
        model_code,
        function(x) if (!is.null(family_map[[x]])) family_map[[x]]$adapter else "UNKNOWN",
        character(1)
      ),
      default_outputs = lapply(
        model_code,
        function(x) if (!is.null(family_map[[x]])) family_map[[x]]$outputs else c("PlantGro.OUT", "Evaluate.OUT")
      ),
      genotype_stem = paste0(crop_code, substr(model_code, 3, 5), "048"),
      is_default_profile_module = default_module == module_code_048,
      dssat_root = dssat_root_name
    )

  registry
}

dssat_parse_experiment_header <- function(filex_path) {
  lines <- readLines(filex_path, warn = FALSE, encoding = "UTF-8")
  section <- NULL
  model_code <- NULL
  treatment_numbers <- c()
  cultivar_table <- NULL

  for (line in lines) {
    clean <- trimws(gsub("\u001a", "", line))
    if (grepl("^\\*", clean)) {
      section <- strsplit(clean, ":", fixed = TRUE)[[1]][1]
      next
    }
    if (!nzchar(clean) || grepl("^@", clean)) {
      next
    }

    if (section == "*TREATMENTS") {
      parts <- strsplit(clean, "\\s+")[[1]]
      suppressWarnings({
        num <- as.integer(parts[1])
      })
      if (!is.na(num)) {
        treatment_numbers <- c(treatment_numbers, num)
      }
    }

    if (section == "*CULTIVARS") {
      cultivar_table <- c(cultivar_table, clean)
    }

    if (section == "*SIMULATION CONTROLS" && grepl("^1\\s+GE\\s+", clean)) {
      parts <- strsplit(clean, "\\s+")[[1]]
      model_candidate <- toupper(parts[length(parts)])
      if (grepl("^[A-Z0-9]{5}$", model_candidate)) {
        model_code <- model_candidate
      }
    }
  }

  stem <- tools::file_path_sans_ext(basename(filex_path))
  ext <- tools::file_ext(filex_path)
  crop_code_from_ext <- toupper(substr(ext, 1, 2))

  list(
    model_code = model_code,
    crop_code = crop_code_from_ext,
    filea = paste0(stem, ".", crop_code_from_ext, "A"),
    filet = paste0(stem, ".", crop_code_from_ext, "T"),
    treatments = unique(treatment_numbers),
    cultivar_lines = cultivar_table
  )
}

dssat_try_match_row <- function(df, target, candidates) {
  for (col in candidates) {
    if (col %in% names(df)) {
      idx <- which(trimws(as.character(df[[col]])) == trimws(as.character(target)))
      if (length(idx) > 0) {
        return(list(index = idx[1], column = col))
      }
    }
  }
  NULL
}

dssat_get_adapter_spec <- function(model_code) {
  spec <- dssat_omni_family_map()[[model_code]]
  if (is.null(spec)) {
    spec <- list(adapter = "UNKNOWN", outputs = c("PlantGro.OUT", "Evaluate.OUT"))
  }
  spec
}

dssat_run_model <- function(run_dir, model_options) {
  exe_path <- file.path(model_options$DSSAT_path, model_options$DSSAT_exe)
  if (!file.exists(exe_path)) {
    stop("DSSAT executable not found: ", exe_path)
  }

  args <- c()
  if (!is.null(model_options$module_code_048) && nzchar(model_options$module_code_048)) {
    args <- c(args, model_options$module_code_048)
  }
  args <- c(args, "B", "DSSBatch.V48")

  stdout_target <- if (isTRUE(model_options$suppress_output)) TRUE else ""
  stderr_target <- if (isTRUE(model_options$suppress_output)) TRUE else ""

  status <- system2(
    command = exe_path,
    args = args,
    stdout = stdout_target,
    stderr = stderr_target
  )

  invisible(status)
}

dssat_parse_plantgro_fallback <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  header_idx <- grep("^@YEAR\\s+DOY", lines)
  if (length(header_idx) == 0) {
    stop("No PlantGro header found in fallback parser for ", path)
  }

  blocks <- vector("list", length(header_idx))
  for (i in seq_along(header_idx)) {
    header_line_idx <- header_idx[i]
    run_start <- max(which(seq_along(lines) < header_line_idx & grepl("^\\*RUN\\s+", lines)))
    if (!is.finite(run_start)) {
      run_start <- 1
    }
    block_end <- if (i < length(header_idx)) header_idx[i + 1] - 1 else length(lines)
    run_lines <- lines[run_start:block_end]

    run_line <- run_lines[grep("^\\*RUN\\s+", run_lines)[1]]
    exp_line <- run_lines[grep("^\\s*EXPERIMENT\\s*:", run_lines)[1]]
    model_line <- run_lines[grep("^\\s*MODEL\\s*:", run_lines)[1]]
    data_lines <- run_lines[seq.int(match(lines[header_line_idx], run_lines) + 1, length(run_lines))]
    data_lines <- data_lines[grepl("^\\s*\\d", data_lines)]
    if (length(data_lines) == 0) {
      next
    }

    cols <- strsplit(sub("^@", "", trimws(lines[header_line_idx])), "\\s+")[[1]]
    block_df <- utils::read.table(
      text = paste(data_lines, collapse = "\n"),
      header = FALSE,
      fill = TRUE,
      stringsAsFactors = FALSE
    )
    names(block_df)[seq_along(cols)] <- cols
    if (ncol(block_df) > length(cols)) {
      block_df <- block_df[, seq_along(cols), drop = FALSE]
    }

    run_match <- regexec("^\\*RUN\\s+([0-9]+).+?([A-Z0-9]{8})\\s+([A-Z0-9]{8})\\s+([0-9]+)\\s*$", trimws(run_line))
    run_parts <- regmatches(trimws(run_line), run_match)[[1]]
    experiment_code <- NA_character_
    if (!is.na(exp_line) && length(exp_line) > 0) {
      experiment_code <- sub("^\\s*EXPERIMENT\\s*:\\s*([A-Z0-9]+).*$", "\\1", exp_line)
    }
    model_name <- if (!is.na(model_line) && length(model_line) > 0) {
      sub("^\\s*MODEL\\s*:\\s*([A-Z0-9]+).*$", "\\1", model_line)
    } else {
      NA_character_
    }

    block_df$RUN <- if (length(run_parts) >= 2) as.integer(run_parts[2]) else i
    block_df$TRNO <- if (length(run_parts) >= 5) as.integer(run_parts[5]) else NA_integer_
    block_df$EXPERIMENT <- experiment_code
    block_df$MODEL <- model_name

    if (all(c("YEAR", "DOY") %in% names(block_df))) {
      year_int <- suppressWarnings(as.integer(block_df$YEAR))
      doy_int <- suppressWarnings(as.integer(block_df$DOY))
      block_df$DATE <- as.Date(sprintf("%04d-%03d", year_int, doy_int), format = "%Y-%j")
    }

    blocks[[i]] <- block_df
  }

  blocks <- Filter(Negate(is.null), blocks)
  if (length(blocks) == 0) {
    stop("Fallback PlantGro parser found no data rows in ", path)
  }
  dplyr::bind_rows(blocks)
}

dssat_read_output_safe <- function(path, output_name = basename(path)) {
  tryCatch(
    as.data.frame(read_output(path)),
    error = function(e) {
      if (identical(output_name, "PlantGro.OUT")) {
        dssat_parse_plantgro_fallback(path)
      } else {
        stop(e)
      }
    }
  )
}

dssat_infer_model_options <- function(model_options) {
  if (is.null(model_options$DSSAT_path)) {
    stop("model_options$DSSAT_path is required.")
  }
  if (is.null(model_options$DSSAT_exe)) {
    model_options$DSSAT_exe <- "DSCSM048.EXE"
  }
  if (is.null(model_options$Genotype)) {
    model_options$Genotype <- "Genotype"
  }

  registry <- dssat_read_simulation_registry(model_options$DSSAT_path)
  model_options$registry <- registry

  if (!is.null(model_options$project_file)) {
    if (!file.exists(model_options$project_file)) {
      if (!is.null(model_options$Crop)) {
        maybe_path <- file.path(model_options$DSSAT_path, model_options$Crop, model_options$project_file)
        if (file.exists(maybe_path)) {
          model_options$project_file <- maybe_path
        } else {
          stop("Project file not found: ", model_options$project_file)
        }
      } else {
        stop("Project file not found: ", model_options$project_file)
      }
    }

    exp_info <- dssat_parse_experiment_header(model_options$project_file)
    model_options$crop_code <- exp_info$crop_code
    model_options$filea <- exp_info$filea
    model_options$filet <- exp_info$filet
    model_options$available_treatments <- exp_info$treatments
    if (!is.null(exp_info$model_code) && nzchar(exp_info$model_code)) {
      model_options$model_code <- exp_info$model_code
    }
    if (is.null(model_options$Crop)) {
      crop_dir_name <- basename(dirname(normalizePath(model_options$project_file, winslash = "/", mustWork = TRUE)))
      crop_row <- registry %>%
        dplyr::filter(crop_code == exp_info$crop_code, crop_dir == crop_dir_name)
      if (nrow(crop_row) == 0) {
        crop_row <- registry %>%
          dplyr::filter(crop_code == exp_info$crop_code, is_default_profile_module)
      }
      if (nrow(crop_row) == 0) {
        crop_row <- registry %>% dplyr::filter(crop_code == exp_info$crop_code)
      }
      if (nrow(crop_row) > 0 && !is.na(crop_row$crop_dir[1])) {
        model_options$Crop <- crop_row$crop_dir[1]
      }
    }
  }

  if (!is.null(model_options$module_code)) {
    model_options$model_code <- substr(toupper(model_options$module_code), 1, 5)
  }

  if (is.null(model_options$model_code)) {
    if (!is.null(model_options$crop_code)) {
      model_row <- registry %>%
        dplyr::filter(crop_code == model_options$crop_code)
      if (!is.null(model_options$Crop)) {
        model_row <- model_row %>% dplyr::filter(is.na(crop_dir) | crop_dir == model_options$Crop)
      }
      default_row <- model_row %>% dplyr::filter(is_default_profile_module)
      if (nrow(default_row) > 0) {
        model_options$model_code <- default_row$model_code[1]
      } else if (nrow(model_row) > 0) {
        model_options$model_code <- model_row$model_code[1]
      }
    }
  }

  if (is.null(model_options$model_code)) {
    if (!is.null(model_options$module_code)) {
      model_options$model_code <- substr(toupper(model_options$module_code), 1, 5)
    } else if (!is.null(model_options$ecotype_filename)) {
      crop_code <- substr(model_options$ecotype_filename, 1, 2)
      suffix <- substr(model_options$ecotype_filename, 3, 5)
      model_options$model_code <- paste0(crop_code, suffix)
      model_options$model_code <- substr(model_options$model_code, 3, 7)
    } else {
      stop("Unable to infer model code. Provide model_options$project_file or model_options$module_code.")
    }
  }

  if (is.null(model_options$Crop)) {
    crop_row <- registry %>%
      dplyr::filter(crop_code == model_options$crop_code, model_code == model_options$model_code)
    if (nrow(crop_row) == 0 && !is.null(model_options$crop_code)) {
      crop_row <- registry %>% dplyr::filter(crop_code == model_options$crop_code)
      if (nrow(crop_row) > 0 && !is.na(crop_row$crop_dir[1])) {
        model_options$Crop <- crop_row$crop_dir[1]
      }
    }
  }

  model_options$model_code <- toupper(substr(model_options$model_code, 1, 5))
  model_row <- registry %>% dplyr::filter(model_code == model_options$model_code)
  if (!is.null(model_options$crop_code)) {
    model_row <- model_row %>% dplyr::filter(crop_code == model_options$crop_code)
  }
  if (nrow(model_row) == 0) {
    stop("No registry entry found for model code: ", model_options$model_code)
  }
  if (nrow(model_row) > 1 && !is.null(model_options$crop_code)) {
    model_row <- model_row[1, , drop = FALSE]
  } else if (nrow(model_row) > 1) {
    stop("Model code maps to multiple crop codes. Supply model_options$crop_code.")
  }

  model_options$crop_code <- model_row$crop_code[1]
  model_options$module_code_048 <- model_row$module_code_048[1]
  model_options$adapter <- model_row$adapter[1]
  if (is.null(model_options$Crop) || !nzchar(model_options$Crop)) {
    model_options$Crop <- model_row$crop_dir[1]
  }
  if (is.null(model_options$out_files)) {
    model_options$out_files <- unlist(model_row$default_outputs[[1]])
  }

  genotype_stem <- model_row$genotype_stem[1]
  if (is.null(model_options$ecotype_filename)) {
    maybe_eco <- paste0(genotype_stem, ".ECO")
    if (file.exists(file.path(model_options$DSSAT_path, model_options$Genotype, maybe_eco))) {
      model_options$ecotype_filename <- maybe_eco
    }
  }
  if (is.null(model_options$cultivar_filename)) {
    maybe_cul <- paste0(genotype_stem, ".CUL")
    if (file.exists(file.path(model_options$DSSAT_path, model_options$Genotype, maybe_cul))) {
      model_options$cultivar_filename <- maybe_cul
    }
  }
  model_options$species_filename <- paste0(genotype_stem, ".SPE")
  model_options
}

dssat_companion_paths <- function(model_options) {
  if (is.null(model_options$project_file)) {
    stop("model_options$project_file is required.")
  }

  exp_full_path <- normalizePath(model_options$project_file, winslash = "/", mustWork = TRUE)
  project_source_dir <- dirname(exp_full_path)
  genotype_dir <- file.path(model_options$DSSAT_path, model_options$Genotype)

  paths <- list(
    executable = file.path(model_options$DSSAT_path, model_options$DSSAT_exe),
    simulation_cde = file.path(model_options$DSSAT_path, "SIMULATION.CDE"),
    dssat_profile = file.path(model_options$DSSAT_path, "DSSATPRO.V48"),
    project_file = exp_full_path,
    project_dir = project_source_dir,
    genotype_dir = genotype_dir,
    filea = if (!is.null(model_options$filea)) file.path(project_source_dir, model_options$filea) else NA_character_,
    filet = if (!is.null(model_options$filet)) file.path(project_source_dir, model_options$filet) else NA_character_,
    cultivar_file = if (!is.null(model_options$cultivar_filename)) file.path(genotype_dir, model_options$cultivar_filename) else NA_character_,
    ecotype_file = if (!is.null(model_options$ecotype_filename)) file.path(genotype_dir, model_options$ecotype_filename) else NA_character_,
    species_file = if (!is.null(model_options$species_filename)) file.path(genotype_dir, model_options$species_filename) else NA_character_
  )

  if (!is.null(model_options$cultivar_filename) && file.exists(file.path(project_source_dir, model_options$cultivar_filename))) {
    paths$cultivar_file <- file.path(project_source_dir, model_options$cultivar_filename)
  }
  if (!is.null(model_options$ecotype_filename) && file.exists(file.path(project_source_dir, model_options$ecotype_filename))) {
    paths$ecotype_file <- file.path(project_source_dir, model_options$ecotype_filename)
  }

  paths
}

DSSAT_omni_self_check <- function(model_options, situation = NULL, required_var = NULL) {
  model_options <- dssat_infer_model_options(model_options)
  paths <- dssat_companion_paths(model_options)

  checks <- data.frame(
    item = c(
      "DSSAT executable",
      "SIMULATION.CDE",
      "DSSATPRO.V48",
      "Project file",
      "Genotype directory",
      "Species file",
      "Cultivar file",
      "Ecotype file",
      "Companion FILEA",
      "Companion FILET"
    ),
    path = c(
      paths$executable,
      paths$simulation_cde,
      paths$dssat_profile,
      paths$project_file,
      paths$genotype_dir,
      paths$species_file,
      paths$cultivar_file,
      paths$ecotype_file,
      paths$filea,
      paths$filet
    ),
    required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, !is.null(model_options$cultivar_filename), !is.null(model_options$ecotype_filename), FALSE, FALSE),
    exists = FALSE,
    stringsAsFactors = FALSE
  )

  checks$exists <- vapply(
    checks$path,
    function(x) if (is.na(x) || !nzchar(x)) FALSE else file.exists(x),
    logical(1)
  )

  if (!is.null(situation)) {
    requested_treatments <- vapply(
      as.character(situation),
      function(x) {
        pieces <- strsplit(x, "_", fixed = TRUE)[[1]]
        candidate <- if (length(pieces) > 1) pieces[length(pieces)] else x
        suppressWarnings(as.integer(candidate))
      },
      integer(1)
    )
    checks <- rbind(
      checks,
      data.frame(
        item = "Requested situation(s)",
        path = paste(as.character(situation), collapse = ", "),
        required = TRUE,
        exists = !any(is.na(requested_treatments)) && (
          is.null(model_options$available_treatments) ||
            length(model_options$available_treatments) == 0 ||
            all(requested_treatments %in% model_options$available_treatments)
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(required_var)) {
    checks <- rbind(
      checks,
      data.frame(
        item = "Requested variable alias",
        path = paste(as.character(required_var), collapse = ", "),
        required = TRUE,
        exists = TRUE,
        stringsAsFactors = FALSE
      )
    )
  }

  failed <- checks$required & !checks$exists
  summary <- list(
    checks = checks,
    ok = !any(failed),
    model_code = model_options$model_code,
    module_code_048 = model_options$module_code_048,
    adapter = model_options$adapter,
    crop = model_options$Crop
  )

  class(summary) <- "dssat_omni_self_check"
  summary
}

