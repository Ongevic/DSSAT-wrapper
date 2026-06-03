# ===========================================================================
# dssat_omni_outputs.R  —  PARSE STAGE  (see ARCHITECTURE.md)
# Author: Victor Nyabuti Ong'era
#
# Turns DSSAT's raw output (PlantGro.OUT etc.) into the tidy column the caller
# asked for. The key idea is the ALIAS MAP: a friendly name like "biomass" or
# "yield" maps to the real DSSAT column code, which differs by family
# (e.g. biomass = CWAD for most crops, but CWAM/SHTD for SUGARCANE). Functions:
#   - dssat_add_stage_columns()      : add Zadok growth-stage day columns
#   - dssat_variable_alias_map()     : friendly-name -> family-specific columns
#   - dssat_resolve_requested_vars() : pick the actual column for the request
# To support a new variable or a family-specific column, edit the alias map.
# ===========================================================================
# Add Zadok growth-stage columns (e.g. Zadok65) giving the day-of-year each
# stage was reached. Only acts if a growth-stage column (GSTD) is present.
dssat_add_stage_columns <- function(pgro) {
  if (!("GSTD" %in% names(pgro)) || nrow(pgro) == 0) {
    return(pgro)
  }
  zadok_df <- data.frame(Zadok = unique(floor(pgro$GSTD)))
  zadok_df$firstIndex <- match(zadok_df$Zadok, floor(pgro$GSTD))
  zadok_df$dates <- pgro$Date[zadok_df$firstIndex]
  zadok_df$julDay <- julian(
    zadok_df$dates,
    origin = as.Date(paste(lubridate::year(zadok_df$dates[1]), "01", "01", sep = "-"))
  ) + 1
  for (i in seq_len(nrow(zadok_df))) {
    pgro[[paste0("Zadok", zadok_df$Zadok[i])]] <- zadok_df$julDay[i]
  }
  pgro
}

# THE ALIAS MAP: translate a friendly name (biomass, lai, yield, …) into the
# real DSSAT column code(s), which differ by family. Start from common aliases,
# then override per adapter. EDIT HERE to add a variable or fix a family's column.
dssat_variable_alias_map <- function(adapter) {
  common_aliases <- list(
    biomass = c("CWAD", "TWAD", "CWAM"),
    lai = c("LAIGD", "LAID", "LAI"),
    flowering = c("GSTD", "R1DAT", "ADAT"),
    yield = c("HWAM", "EWAD", "GWAD", "CWAM"),
    height = c("CHTD", "CHTA", "CANHT")
  )

  adapter_aliases <- switch(
    adapter,
    SUGARCANE = list(
      biomass = c("CWAM", "SHTD", "STKH", "BADMH"),
      lai = c("LAIGD", "LAIGH", "LAIX", "LAITD")
    ),
    AROIDS = list(
      yield = c("EWAD", "CWAD", "TWAD")
    ),
    CSCAS = list(
      biomass = c("CWAD", "TWAD", "HWAM"),
      yield = c("HWAM", "CWAD")
    ),
    common_aliases
  )

  utils::modifyList(common_aliases, adapter_aliases)
}

# Given the parsed output and the variable(s) the caller asked for, return the
# actual columns present — accepting either a real DSSAT code (e.g. CWAD) or a
# friendly alias (e.g. biomass) resolved via the alias map above.
dssat_resolve_requested_vars <- function(pgro, requested_vars, adapter) {
  if (is.null(requested_vars)) {
    return(pgro)
  }

  alias_map <- dssat_variable_alias_map(adapter)
  resolved <- c("Date")
  for (var_name in requested_vars) {
    if (var_name %in% names(pgro)) {
      resolved <- c(resolved, var_name)
      next
    }

    alias_candidates <- alias_map[[tolower(var_name)]]
    if (is.null(alias_candidates)) {
      alias_candidates <- alias_map[[var_name]]
    }
    if (is.null(alias_candidates)) {
      alias_candidates <- c(var_name)
    }

    matched <- alias_candidates[alias_candidates %in% names(pgro)]
    if (length(matched) > 0) {
      resolved <- c(resolved, matched[1])
    } else {
      warning("Requested variable '", var_name, "' not found for adapter ", adapter, ".")
    }
  }

  resolved <- unique(resolved)
  dplyr::select(pgro, dplyr::all_of(resolved))
}

# Top-level PARSE entry: read the family's OUT files from the run directory,
# add stage columns, resolve the requested variable, and return a tidy
# per-situation list of Date + variable data frames.
dssat_read_outputs_generic <- function(project_path, model_options, situation_names = NULL, trt_numbers = NULL, var = NULL) {
  pgro_path <- file.path(project_path, "PlantGro.OUT")
  if (!file.exists(pgro_path)) {
    return(list(sim_list = list(), error = TRUE))
  }

  pgro_tot <- dssat_read_output_safe(pgro_path, "PlantGro.OUT") %>%
    dplyr::mutate(Date = DATE) %>%
    dplyr::select(-DATE) %>%
    dplyr::relocate(Date)

  if ("PlantGr2.OUT" %in% model_options$out_files && file.exists(file.path(project_path, "PlantGr2.OUT"))) {
    pgr2 <- dssat_read_output_safe(file.path(project_path, "PlantGr2.OUT"), "PlantGr2.OUT") %>%
      dplyr::mutate(Date = DATE) %>%
      dplyr::select(-DATE) %>%
      dplyr::relocate(Date)
    pgro_tot <- dplyr::left_join(
      pgro_tot,
      pgr2[, c("Date", "EXPERIMENT", "TRNO", setdiff(names(pgr2), names(pgro_tot)))],
      by = c("Date", "EXPERIMENT", "TRNO")
    )
  }

  if (file.exists(file.path(project_path, "Evaluate.OUT"))) {
    eval_df <- tryCatch(
      {
        dssat_read_output_safe(file.path(project_path, "Evaluate.OUT"), "Evaluate.OUT") %>%
          dplyr::mutate(EXPERIMENT = EXCODE) %>%
          dplyr::select(-EXCODE)
      },
      error = function(e) NULL
    )
    if (!is.null(eval_df)) {
      if (!("TRNO" %in% names(eval_df)) && "TN" %in% names(eval_df)) {
        eval_df <- dplyr::rename(eval_df, TRNO = TN)
      }
      eval_df$EXPERIMENT <- sapply(
        eval_df$EXPERIMENT,
        function(x) {
          if (nchar(x) > 2 && substr(x, nchar(x) - 1, nchar(x)) == model_options$crop_code) {
            substr(x, 1, nchar(x) - 2)
          } else {
            x
          }
        }
      )
      id_sim_var <- grep(pattern = "S$", names(eval_df))
      sim_var_names <- names(eval_df)[id_sim_var]
      if (length(sim_var_names) > 0 && "TRNO" %in% names(eval_df)) {
        join_cols <- c("EXPERIMENT", "TRNO")
        if ("RUN" %in% names(eval_df) && "RUN" %in% names(pgro_tot)) {
          join_cols <- c(join_cols, "RUN")
        }
        eval_df <- dplyr::select(eval_df, dplyr::all_of(c(join_cols, sim_var_names)))
        eval_df <- eval_df[!duplicated(eval_df[, join_cols, drop = FALSE]), , drop = FALSE]
        names(eval_df)[(length(join_cols) + 1):ncol(eval_df)] <- sapply(sim_var_names, function(x) substr(x, 1, nchar(x) - 1))
        pgro_tot <- dplyr::left_join(
          pgro_tot,
          eval_df[, c(join_cols, setdiff(names(eval_df), names(pgro_tot)))],
          by = join_cols
        )
      }
    }
  }

  extra_files <- setdiff(model_options$out_files, c("PlantGro.OUT", "Evaluate.OUT", "PlantGr2.OUT"))
  for (out_file in extra_files) {
    out_path <- file.path(project_path, out_file)
    if (!file.exists(out_path)) {
      next
    }
    tmp <- dssat_read_output_safe(out_path, out_file) %>%
      dplyr::mutate(Date = DATE) %>%
      dplyr::select(-DATE) %>%
      dplyr::relocate(Date)
    pgro_tot <- dplyr::left_join(
      pgro_tot,
      tmp[, c("Date", "EXPERIMENT", "TRNO", setdiff(names(tmp), names(pgro_tot)))],
      by = c("Date", "EXPERIMENT", "TRNO")
    )
  }

  if (is.null(trt_numbers)) {
    trt_numbers <- sort(unique(pgro_tot$TRNO))
  }
  if (is.null(situation_names)) {
    if ("EXPERIMENT" %in% names(pgro_tot)) {
      exp_names <- unique(pgro_tot$EXPERIMENT)
      if (length(exp_names) == 1) {
        situation_names <- paste0(exp_names, "_", trt_numbers)
      } else {
        situation_names <- as.character(seq_along(trt_numbers))
      }
    } else {
      situation_names <- as.character(trt_numbers)
    }
  }

  sim_list <- setNames(vector("list", length(trt_numbers)), situation_names)
  for (i in seq_along(trt_numbers)) {
    trno <- trt_numbers[i]
    pgro <- dplyr::filter(pgro_tot, TRNO == trno)
    if ("EXPERIMENT" %in% names(pgro_tot) && grepl("_", situation_names[i], fixed = TRUE)) {
      exp_name <- strsplit(situation_names[i], "_", fixed = TRUE)[[1]][1]
      pgro_filtered <- dplyr::filter(pgro, EXPERIMENT == exp_name)
      if (nrow(pgro_filtered) > 0) {
        pgro <- pgro_filtered
      }
    }
    pgro <- pgro[!duplicated(pgro$Date), ]
    if ("TWAD" %in% names(pgro) && !("HWAM" %in% names(pgro))) {
      pgro$HWAM <- pgro$TWAD
    }
    if ("ADAT" %in% names(pgro) && inherits(pgro$ADAT, "Date")) {
      pgro$ADAT <- julian(pgro$ADAT, origin = as.Date(paste(lubridate::year(pgro$ADAT[1]), "01", "01", sep = "-"))) + 1
    }
    pgro <- dssat_add_stage_columns(pgro)
    pgro <- dssat_resolve_requested_vars(pgro, var, model_options$adapter)
    sim_list[[i]] <- pgro
  }
  attr(sim_list, "class") <- "cropr_simulation"
  list(sim_list = sim_list, error = FALSE)
}

