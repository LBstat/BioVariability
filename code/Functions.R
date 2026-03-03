
run_model_safely <- function(spec, data) {
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

  message("Computation of selection criteria... Randomized Quantile Residuals")

  # Assertions
  assertList(obj, names = "named")
  lapply(obj, function(x) {
    assert(
      checkClass(x, classes = "glm"),
      checkClass(x, classes = "lmerMod"),
      checkClass(x, classes = "glmerMod"),
      checkClass(x, classes = "gamlss"),
      combine = "or"
    )
  })

  res_list <- lapply(obj, function(x) {
    res <- if(inherits(x, "gamlss")) residuals(x) else residuals(x, type = "pearson")

    clean_res <- if (inherits(x, "gamlss")) {
      # GAMLSS -- > extracts already randomized quantile residuals
      as.numeric(residuals(x))

    } else if (inherits(x, "glmerMod")) {
      # glmerMod --> use of DHARMa package to derive uniform residuals
      sim <- DHARMa::simulateResiduals(x, n = 250)

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

    w <- tryCatch({
      weights(x)
    }, error = function(e) {
      rep(1, length(clean_res))
    })

    is_ok <- is.finite(res)

    clean_res <- as.numeric(res)[is_ok]
    clean_w <- as.numeric(w)[is_ok]

    if (length(clean_res) == 0) stop("No valid residuals found for one of the models")

    sorted_res <- sort(clean_res)
    n <- length(sorted_res)
    theo_q <- qnorm(ppoints(n))

    filliben <- cor(sorted_res, theo_q)

    m1 <- round(mean(clean_res), 4)
    m2 <- round(var(clean_res), 4)

    m3 <- sum((clean_res - m1)^3) / n
    m4 <- sum((clean_res - m1)^4) / n

    skew <- round(sign(m3) * sqrt(abs(m3^2 / m2^3)), 4)
    kurt <- round(m4 / m2^2, 4)

    data.table(Model = NA_character_, AIC = AIC(x), BIC = BIC(x), `Filliben correlation` = filliben,
      `Residual mean` = m1, `Residual variance` = m2, `Residual skewness` = skew, `Residual kurtosis` = kurt)
  })

  dt_final <- rbindlist(res_list)
  dt_final$Model <- names(obj)

  return(dt_final)
}
