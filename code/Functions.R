
run_model_safely <- function(spec, data, optimizer = "nlimnb") {
  require(gamlss)

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
      gamlss::gamlss(
        formula = formula_mu,
        sigma.formula = formula_sigma,
        family = spec$family, 
        data = current_data,
        method = RS(),
        control = gamlss.control(trace = FALSE)
      )
    }, error = function(e) {
      return(list(status = "error", message = e$message, spec = spec))
    })
  } else {
    result <- tryCatch({
      gamlss::gamlss(
        formula = formula_mu,
        sigma.formula = formula_sigma,
        family = spec$family, 
        data = current_data,
        method = RS(),
        control = gamlss.control(trace = FALSE),
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
        sim <- DHARMa::simulateResiduals(x, n = 250, plot = FALSE)

        # qnorm transforms residuals in standard normal residuals
        qnorm(pmax(1e-7, pmin(sim$scaledResiduals, 1 - 1e-7)))

      } else if (inherits(x, "lmerMod") || inherits(x, "lm")) {
        # Pearson/Standardized residuals already randomized quantile residuals
        as.numeric(residuals(x, type = "pearson"))
        
      } else {
        # Fallback --> other possible GLMs
        sim <- DHARMa::simulateResiduals(x, n = 250)
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
