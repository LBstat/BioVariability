
run_model_safely <- function(spec, data, optimizer = "nlimnb") {

  # Assertions
  assertList(spec, names = "named")
  assertDataFrame(data, any.missing = FALSE)
  assertString(optimizer)
  assertChoice(optimizer, choices = c("nlimnb", "optim"))

  # 1. Get the data
  current_data <- as.data.frame(data[[spec$data]])

  # 2. Cean formulas
  formula_mu_str <- paste(deparse(spec$formula), collapse = "")
  formula_mu     <- as.formula(formula_mu_str)

  if(!is.null(spec$sigma)) {
    formula_sigma_str <- paste(deparse(spec$sigma), collapse = "")
    formula_sigma   <- as.formula(formula_sigma_str)
  } else {
    formula_sigma <- ~1
  }

  # Set environment to local scope
  environment(formula_mu) <- environment()
  environment(formula_sigma) <- environment()

  # 3. Data check: ensure the response variable is numeric
  response_var <- all.vars(formula_mu)[1]
  current_data[[response_var]] <- as.numeric(current_data[[response_var]])

  # 4. Run model
  if (optimizer == "nlimnb") {
    result <- tryCatch({
      gamlss(
        formula = formula_mu,
        sigma.formula = formula_sigma,
        family = spec$family, 
        data = current_data,
        method = RS(),
        control = gamlss.control(trace = FALSE, c.crit = 0.01, n.cyc = 30)
      )
    }, error = function(e) {
      return(list(status = "error", message = e$message, spec = spec))
    })
  } else {
    result <- tryCatch({
      gamlss(
        formula = formula_mu,
        sigma.formula = formula_sigma,
        family = spec$family, 
        data = current_data,
        method = RS(),
        control = gamlss.control(trace = FALSE, c.crit = 0.01, n.cyc = 30),
        c.control = lmeControl(opt = "optim", maxIter = 50, msMaxIter = 50)
      )
    }, error = function(e) {
      return(list(status = "error", message = e$message, spec = spec))
    })
  }

  return(result)
}

model_selection_criteria <- function(obj) {
  warning("Models should be compared only if they share the same response variable")
  message("Computation of selection criteria... Handling successes and failures")

  # 1. Pre-processing
  obj_processed <- lapply(obj, function(x) {
    if (is.list(x) && !inherits(x, c("gamlss", "glmerMod", "lmerMod", "glm", "lm"))) {
      empty_obj <- "Structurally Unstable Model"
      class(empty_obj) <- "fail"
      return(empty_obj)
    } else {
      return(x)
    }
  })

  res_list <- lapply(obj_processed, function(x) {

    # Failed model
    if (inherits(x, "fail")) {
      return(data.table(
        Model = NA_character_,
        AIC = NA_real_,
        BIC = NA_real_,
        `Filliben correlation` = NA_real_,
        `Residual mean` = NA_real_,
        `Residual variance` = NA_real_,
        `Residual skewness` = NA_real_,
        `Residual kurtosis` = NA_real_,
        Notes = as.character(x)
      ))
    }

    clean_res <- tryCatch({
      if (inherits(x, "gamlss")) {
        # GAMLSS -- > extracts already randomized quantile residuals
        as.numeric(residuals(x))

      } else if (inherits(x, "glmerMod")) {
        # glmerMod --> use of DHARMa package to derive uniform residuals
        sim <- simulateResiduals(x, n = 250, plot = FALSE)

        # qnorm transforms residuals in standard normal residuals
        qnorm(pmax(1e-7, pmin(sim$scaledResiduals, 1 - 1e-7)))

      } else if (inherits(x, "lmerMod") || inherits(x, "lm")) {
        # Pearson/Standardized residuals already randomized quantile residuals
        as.numeric(residuals(x, type = "pearson"))
        
      } else {
        # Fallback --> other possible GLMs
        sim <- simulateResiduals(x, n = 250)
        qnorm(pmax(1e-7, pmin(sim$scaledResiduals, 1 - 1e-7)))
      }
    }, error = function(e) { 
      return(NULL)
    })
    
    # Protezione se i residui sono incalcolabili o assenti
    if (is.null(clean_res) || all(is.na(clean_res))) {
      return(data.table(
        Model = NA_character_,
        AIC = round(AIC(x), 2),
        BIC = round(BIC(x), 2),
        `Filliben correlation` = NA_real_,
        `Residual mean` = NA_real_,
        `Residual variance` = NA_real_,
        `Residual skewness` = NA_real_, 
        `Residual kurtosis` = NA_real_,
        Notes = "Converged (Resid. Unstable)"))
    }

    w <- tryCatch({
      weights(x)
    }, error = function(e) {
      rep(1, length(clean_res))
    })

    clean_res <- clean_res[is.finite(clean_res)]

    n <- length(clean_res)
    sorted_res <- sort(clean_res)

    theo_q <- qnorm(ppoints(n))
    filliben <- cor(sorted_res, theo_q)
    
    m1 <- mean(clean_res)
    m2 <- var(clean_res)

    m3 <- sum((clean_res - m1)^3) / n
    m4 <- sum((clean_res - m1)^4) / n

    skew <- m3 / (m2^1.5)
    kurt <- m4 / (m2^2)

    return(data.table(
      Model = NA_character_,
      AIC = round(AIC(x), 2),
      BIC = round(BIC(x), 2),
      `Filliben correlation` = round(filliben, 4),
      `Residual mean` = round(m1, 4),
      `Residual variance` = round(m2, 4),
      `Residual skewness` = round(skew, 4),
      `Residual kurtosis` = round(kurt, 4),
      Notes = "Optimized (Converged)"
    ))
  })

  dt_final <- rbindlist(res_list, fill = TRUE)
  dt_final$Model <- names(obj_processed)

  return(dt_final)
}

extract_gamlss_fixed <- function(model, n_mu, n_sigma, n_nu, n_tau) {

  # Assertions
  assertClass(model, classes = "gamlss")

  # Summary extraction
  invisible(capture.output(summ_mat <- summary(model)))
  df_raw <- as.data.frame(as.matrix(summ_mat))

  df_clean <- data.frame(
    Estimate = round(as.numeric(as.character(df_raw[[1]])), 4),
    Std_Error = round(as.numeric(as.character(df_raw[[2]])), 4),
    t_value = round(as.numeric(as.character(df_raw[[3]])), 4),
    p_value = as.numeric(as.character(df_raw[[4]]))
  )

  # String manipulation
  raw_names <- rownames(df_raw)
  clean_names <- gsub("X\\.[[:alpha:]]*\\.", "Baseline Log", raw_names)
  clean_names <- gsub("\\.", " ", clean_names)

  clean_names <- vapply(clean_names, function(x) {
    strsplit(x, split = " ")[[1]][[1]]
  }, character(1))

  df_clean$Variable <- clean_names

  # Checks
  total_rows_expected <- n_mu + n_sigma + n_nu + n_tau
  if (nrow(df_clean) != total_rows_expected) {
    stop(paste("Model has", nrow(df_clean), "rows, but input has", total_rows_expected))
  }

  df_clean$Parameter <- c(rep("mu", n_mu), rep("sigma", n_sigma), rep("nu", n_nu), rep("tau", n_tau))

  df_clean$Signif <- cut(df_clean$p_value, breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels = c("***", "**", "*", ".", " "))
  df_clean$p_value <- round(df_clean$p_value, 4)

  df_clean <- df_clean[, c("Parameter", "Variable", "Estimate", "Std_Error", "p_value", "Signif")]
  colnames(df_clean) <- c("Parameter", "Variable", "Estimate", "Std Error", "Pr(>|t|)", "Signif")

  rownames(df_clean) <- NULL
  return(df_clean)
}

extract_random_variance <- function(model) {

  # Assertions
  assertClass(model, classes = "gamlss")

  # Random effects extraction
  smo <- getSmo(model)
  var_corr <- VarCorr(smo)

  df_random <- data.frame(
    Group = rownames(var_corr),
    Variance = round(as.numeric(var_corr[, "Variance"]), 4),
    StdDev = round(as.numeric(var_corr[, "StdDev"]), 4),
    Correlation = if("Corr" %in% colnames(var_corr)) round(as.numeric(var_corr[, "Corr"]), 4) else NA
  )

  # String manipulation
  df_random$Group <- gsub("(Intercept)", "Farm (Random Intercept)", df_random$Group)
  df_random$Group <- gsub("YearNum", "Year (Random Slope)", df_random$Group)
  df_random$Group <- gsub("Residual", "Residual variance", df_random$Group)

  df_random$Correlation <- as.character(df_random$Correlation)
  df_random$Correlation[is.na(df_random$Correlation)] <- "-"

  rownames(df_random) <- NULL

  return(df_random)
}

# Esempio:
# tab_random <- extract_random_variance(gamlss_results$ST2Year.FarmCBT)

evaluate_gamlss_performance <- function(model, test_data, train_data, target_var, threshold = 0.5) {

  # Assertions
  assert(
    checkClass(model, classes = "gamlss"),
    checkClass(model, classes = "glmerMod"),
    checkClass(model, classes = "lmerMod"),
    checkClass(model, classes = "glm"),
    checkClass(model, classes = "lm"),
    combine = "or"
  )
  assertDataFrame(test_data, any.missing = FALSE)
  assertDataFrame(train_data, any.missing = FALSE)
  assertString(target_var, na.ok = FALSE, null.ok = FALSE)
  assertNumber(threshold)

  # 1. Predictions
  preds <- predict(model, newdata = test_data, type = "response", data = train_data)

  # 2. Error computations
  actual <- test_data[[target_var]]
  absolute_residuals <- abs(actual - preds)

  MAE  <- mean(absolute_residuals, na.rm = TRUE)
  MSE <- mean((actual - preds)^2, na.rm = TRUE)
  RMSE <- sqrt(mean((actual - preds)^2, na.rm = TRUE))

  BIAS <- mean(preds - actual, na.rm = TRUE) 

  # 3. Critical cases
  test_dt <- copy(test_data)
  test_dt[, pred := preds]
  test_dt[, diff := abs(get(target_var) - pred)]

  errors_df <- test_dt[diff > threshold]

  # 4. Output
  result <- list(
    metrics = data.table(MAE = MAE, MSE = MSE, RMSE = RMSE, BIAS = BIAS, N_Errors = nrow(errors_df)),
    errors_data = errors_df,
    predictions = preds
  )

  return(result)
}
