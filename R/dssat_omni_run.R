DSSAT_omniwrapper <- function(param_values = NULL, situation = NULL, model_options, var = NULL, ...) {
  model_options <- dssat_infer_model_options(model_options)
  preflight <- DSSAT_omni_self_check(model_options, situation = situation, required_var = var)
  if (!isTRUE(preflight$ok)) {
    failed <- preflight$checks[preflight$checks$required & !preflight$checks$exists, , drop = FALSE]
    stop(
      "DSSAT_omni_self_check failed for: ",
      paste(failed$item, collapse = ", "),
      "."
    )
  }

  project_path <- file.path(model_options$DSSAT_path, model_options$Crop)
  genotype_path <- file.path(model_options$DSSAT_path, model_options$Genotype)
  if (is.null(model_options$project_file)) {
    stop("model_options$project_file is required for DSSAT_omniwrapper prototype.")
  }
  exp_full_path <- normalizePath(model_options$project_file, winslash = "/", mustWork = TRUE)
  project_source_dir <- dirname(exp_full_path)
  project_file <- basename(exp_full_path)

  flag_eco_param <- FALSE
  flag_cul_param <- FALSE
  results <- list(sim_list = list(), error = FALSE)
  ini_wd <- getwd()

  options(DSSAT.CSM = file.path(model_options$DSSAT_path, model_options$DSSAT_exe))

  ecotype_filename <- model_options$ecotype_filename
  cultivar_filename <- model_options$cultivar_filename
  ecotype_path <- genotype_path
  cultivar_path <- genotype_path
  if (!is.null(ecotype_filename) && file.exists(file.path(project_source_dir, ecotype_filename))) {
    ecotype_path <- project_source_dir
  }
  if (!is.null(cultivar_filename) && file.exists(file.path(project_source_dir, cultivar_filename))) {
    cultivar_path <- project_source_dir
  }

  cultivar_match_columns <- if (!is.null(model_options$cultivar_match_columns)) {
    model_options$cultivar_match_columns
  } else {
    switch(
      model_options$adapter,
      SUBSTOR = c("VAR#", "VAR-NAME", "VRNAME", "CNAME", "INGENO"),
      CROPGRO = c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO"),
      CERES = c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO"),
      RICE = c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO"),
      SUGARCANE = c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO"),
      AROIDS = c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO"),
      ALOHA = c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO"),
      c("VAR-NAME", "VAR#", "VRNAME", "CNAME", "INGENO")
    )
  }

  exp_info <- dssat_parse_experiment_header(exp_full_path)
  if (is.null(situation)) {
    trt_numbers <- exp_info$treatments
    if (length(trt_numbers) == 0) {
      trt_numbers <- 1
    }
    situation_names <- paste0(tools::file_path_sans_ext(project_file), "_", trt_numbers)
  } else {
    requested <- as.character(situation)
    trt_numbers <- vapply(
      requested,
      function(x) {
        pieces <- strsplit(x, "_", fixed = TRUE)[[1]]
        candidate <- if (length(pieces) > 1) pieces[length(pieces)] else x
        suppressWarnings(as.integer(candidate))
      },
      integer(1)
    )
    if (any(is.na(trt_numbers))) {
      stop("Could not infer treatment numbers from situation argument.")
    }
    situation_names <- requested
  }

  run_dir <- file.path(project_path, paste0("omni_run_", Sys.getpid(), "_", sample(1:10000, 1)))
  dir.create(run_dir, showWarnings = FALSE)
  if (!dir.exists(run_dir)) {
    stop("Could not create temporary run directory.")
  }
  file.copy(exp_full_path, file.path(run_dir, project_file), overwrite = TRUE)

  on.exit({
    setwd(ini_wd)
    if (flag_eco_param && file.exists(file.path(ecotype_path, paste0(ecotype_filename, "_tmp")))) {
      file.rename(file.path(ecotype_path, paste0(ecotype_filename, "_tmp")), file.path(ecotype_path, ecotype_filename))
    }
    if (flag_cul_param && file.exists(file.path(cultivar_path, paste0(cultivar_filename, "_tmp")))) {
      file.rename(file.path(cultivar_path, paste0(cultivar_filename, "_tmp")), file.path(cultivar_path, cultivar_filename))
    }
    unlink(run_dir, recursive = TRUE)
  }, add = TRUE)

  if (!is.null(param_values) && !is.null(ecotype_filename) && file.exists(file.path(ecotype_path, ecotype_filename))) {
    eco <- read_eco(file.path(ecotype_path, ecotype_filename))
    eco_params <- names(eco)
    eco_param_names <- intersect(names(param_values), eco_params)
    if (length(eco_param_names) > 0) {
      file.copy(file.path(ecotype_path, ecotype_filename), file.path(ecotype_path, paste0(ecotype_filename, "_tmp")), overwrite = TRUE)
      flag_eco_param <- TRUE
      eco_target <- dssat_try_match_row(eco, model_options$ecotype, c("ECO#", "ECO", "ECO_NAME"))
      if (is.null(eco_target)) {
        warning("Ecotype not found in ecotype file: ", model_options$ecotype)
      } else {
        for (param in eco_param_names) {
          eco[[param]][eco_target$index] <- param_values[[param]]
        }
        attr(eco, "comments") <- NULL
        write_eco(eco, file.path(ecotype_path, ecotype_filename))
      }
    }
  }

  if (!is.null(param_values) && !is.null(cultivar_filename) && file.exists(file.path(cultivar_path, cultivar_filename))) {
    cul <- read_cul(file.path(cultivar_path, cultivar_filename))
    cul_params <- names(cul)
    cul_param_names <- intersect(names(param_values), cul_params)
    if (length(cul_param_names) > 0) {
      file.copy(file.path(cultivar_path, cultivar_filename), file.path(cultivar_path, paste0(cultivar_filename, "_tmp")), overwrite = TRUE)
      flag_cul_param <- TRUE
      cultivar_target <- dssat_try_match_row(cul, model_options$cultivar, cultivar_match_columns)
      if (is.null(cultivar_target)) {
        warning("Cultivar not found in cultivar file: ", model_options$cultivar)
      } else {
        for (param in cul_param_names) {
          if (is.character(cul[[param]])) {
            cul[[param]][cultivar_target$index] <- as.character(round(param_values[[param]], digits = 2))
          } else {
            cul[[param]][cultivar_target$index] <- param_values[[param]]
          }
        }
        write_cul(cul, file.path(cultivar_path, cultivar_filename))
      }
    }
  }

  if (!is.null(ecotype_filename) && file.exists(file.path(ecotype_path, ecotype_filename))) {
    file.copy(file.path(ecotype_path, ecotype_filename), file.path(run_dir, ecotype_filename), overwrite = TRUE)
  }
  if (!is.null(cultivar_filename) && file.exists(file.path(cultivar_path, cultivar_filename))) {
    file.copy(file.path(cultivar_path, cultivar_filename), file.path(run_dir, cultivar_filename), overwrite = TRUE)
  }
  if (!is.null(model_options$species_filename)) {
    species_path <- file.path(model_options$DSSAT_path, model_options$Genotype, model_options$species_filename)
    if (file.exists(species_path)) {
      file.copy(species_path, file.path(run_dir, model_options$species_filename), overwrite = TRUE)
    }
  }
  if (!is.null(model_options$filea)) {
    filea_path <- file.path(project_source_dir, model_options$filea)
    if (file.exists(filea_path)) {
      file.copy(filea_path, file.path(run_dir, model_options$filea), overwrite = TRUE)
    }
  }
  if (!is.null(model_options$filet)) {
    filet_path <- file.path(project_source_dir, model_options$filet)
    if (file.exists(filet_path)) {
      file.copy(filet_path, file.path(run_dir, model_options$filet), overwrite = TRUE)
    }
  }

  setwd(run_dir)
  write_dssbatch(x = project_file, trtno = trt_numbers)
  run_status <- dssat_run_model(
    run_dir = run_dir,
    model_options = modifyList(
      model_options,
      list(suppress_output = ifelse(is.null(model_options$suppress_output), TRUE, model_options$suppress_output))
    )
  )
  setwd(ini_wd)

  if (file.exists(file.path(run_dir, "ERROR.OUT"))) {
    results$error <- TRUE
  }
  if (!is.null(run_status) && !identical(run_status, 0L) && !file.exists(file.path(run_dir, "PlantGro.OUT"))) {
    stop(
      "DSSAT run exited with status ", run_status,
      " and did not produce PlantGro.OUT. Check ERROR.OUT or WARNING.OUT in the run directory."
    )
  }

  out <- dssat_read_outputs_generic(
    project_path = run_dir,
    model_options = model_options,
    situation_names = situation_names,
    trt_numbers = trt_numbers,
    var = var
  )
  out
}

DSSAT_omni_read_obs <- function(model_options, situation, read_end_season = FALSE) {
  model_options <- dssat_infer_model_options(model_options)
  obs_file_path <- function(experiment, suffix) {
    file_name <- paste0(experiment, ".", model_options$crop_code, suffix)
    candidate_dirs <- c()

    if (!is.null(model_options$project_file) && nzchar(model_options$project_file)) {
      candidate_dirs <- c(candidate_dirs, dirname(normalizePath(model_options$project_file, winslash = "/", mustWork = TRUE)))
    }

    candidate_dirs <- c(
      candidate_dirs,
      file.path(model_options$DSSAT_path, model_options$Crop)
    )
    candidate_dirs <- unique(candidate_dirs)

    for (candidate_dir in candidate_dirs) {
      candidate_path <- file.path(candidate_dir, file_name)
      if (file.exists(candidate_path)) {
        return(candidate_path)
      }
    }

    file.path(candidate_dirs[1], file_name)
  }

  situation_df <- setNames(
    as.data.frame(t(dplyr::bind_rows(sapply(situation, FUN = strsplit, "_")))),
    c("EXPERIMENT", "TRNO")
  )
  obs_df <- NULL

  for (experiment in unique(situation_df$EXPERIMENT)) {
    filtered_situation_df <- dplyr::filter(situation_df, EXPERIMENT == experiment)
    trno <- as.integer(filtered_situation_df$TRNO)

    file_name_a <- obs_file_path(experiment, "A")
    file_name_t <- obs_file_path(experiment, "T")

    in_season_obs_df <- NULL
    if (file.exists(file_name_t)) {
      in_season_obs_df <- read_filet(file_name_t, na_strings = NA) %>%
        dplyr::mutate(Date = DATE) %>%
        dplyr::select(-DATE) %>%
        dplyr::relocate(Date) %>%
        dplyr::filter(TRNO %in% trno)
    }

    end_season_obs_df <- NULL
    if (file.exists(file_name_a) && read_end_season) {
      end_season_obs_df <- read_filea(file_name_a, na_strings = NA) %>% dplyr::filter(TRNO %in% trno)
      if ("MDAT" %in% names(end_season_obs_df)) {
        end_season_obs_df <- end_season_obs_df %>% dplyr::mutate(Date = MDAT) %>% dplyr::select(-MDAT) %>% dplyr::relocate(Date)
      } else if ("HDAT" %in% names(end_season_obs_df)) {
        end_season_obs_df <- end_season_obs_df %>% dplyr::mutate(Date = HDAT) %>% dplyr::select(-HDAT) %>% dplyr::relocate(Date)
      } else if ("ADAT" %in% names(end_season_obs_df)) {
        end_season_obs_df <- end_season_obs_df %>% dplyr::mutate(Date = ADAT) %>% dplyr::relocate(Date)
      }
    }

    if (is.null(in_season_obs_df) && is.null(end_season_obs_df)) {
      next
    }

    obs_df_tmp <- if (is.null(end_season_obs_df)) {
      in_season_obs_df
    } else if (is.null(in_season_obs_df)) {
      end_season_obs_df
    } else {
      dplyr::full_join(in_season_obs_df, end_season_obs_df, by = c("TRNO", "Date"))
    }

    obs_df_tmp <- dplyr::mutate(obs_df_tmp, situation = paste0(experiment, "_", TRNO)) %>%
      dplyr::select(-TRNO)
    obs_df <- dplyr::bind_rows(obs_df, obs_df_tmp)
  }

  obs_list <- split(obs_df, f = obs_df$situation)
  obs_list <- lapply(obs_list, function(x) dplyr::select(x, -situation))
  obs_list[situation]
}
