
# Function to plot densities for different categorical variables
ggplot_density <- function(data, x, y, palette = NULL, out_dir = "plot/", save = TRUE) {

  # Assertions
  assertDataFrame(data)
  assertString(x)
  assertString(y)
  assertCharacter(palette, any.missing = FALSE)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[y]])) +
    geom_density_ridges(alpha = 0.7, bandwidth = 0.05)

  if (!is.null(palette)) {
    plot <- plot + scale_fill_manual(values = palette)
  }

  plot <- plot +
    labs(title = paste("Distribution of", x, "by", y),
         x = x,
         y = y,
         fill = y
         ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  if (save) {
    filename <- paste0(x, " density by ", y, ".png")

    ggsave(
      file.path(out_dir, filename),
      plot = plot,
      width = 12,
      height = 6,
      dpi = 300
    )
  }

  plot
}

# Function that performs graphical diagnostics for glm-objects
ggplot_glm_diagnostics <- function(obj, data, y, model = "glm", out_dir = "plot/", save = TRUE) {

  # Assertions
  assertClass(obj, classes = "glm")
  assertDataFrame(data)
  assertString(model)
  assertString(out_dir)
  assertFlag(save)

  if (missing(y)) stop("Response variable is missing")
  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  }

  # Fitted values and residuals
  eta.hat <- predict(obj, type = "link")
  mu.hat <- predict(obj, type = "response")
  dev.res <- residuals(obj, type = "deviance")
  std.res <- rstandard(obj)
  sqrt_std_res <- sqrt(abs(std.res))

  # Component + residual plot for link function
  derivate <- obj$family$mu.eta(eta.hat)
  zeta.hat <- eta.hat + (y - mu.hat) * derivate

  # Standardized residuals vs. variance (Variance plot)
  res_var <- dev.res / sqrt(obj$family$variance(mu.hat))

  # Palette Viridis
  pal <- viridis(6)

  # Linearity
  p1 <- ggplot(data, aes(x = eta.hat, y = dev.res)) +
    geom_point(alpha = 0.5, color = pal[1]) +
    geom_hline(yintercept = 0, col = "#9B1B30", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "#9B1B30") +
    labs(title = "Linearity",
         x = expression(eta),
         y = "Deviance Residuals") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Link
  p2 <- ggplot(data, aes(x = eta.hat, y = zeta.hat)) +
    geom_point(alpha = 0.5, color = pal[2]) +
    geom_smooth(method = "lm", se = FALSE, color = "#9B1B30") +
    labs(title = "Link (Component + Residual)",
         x = expression(eta),
         y = expression(zeta)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Residuals vs Fitted
  p3 <- ggplot(data, aes(x = mu.hat, y = std.res)) +
    geom_point(alpha = 0.5, color = pal[3]) +
    geom_hline(yintercept = 0, col = "#9B1B30",linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "#9B1B30") +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Standardized Residuals") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Scale-Location
  p4 <- ggplot(data, aes(x = mu.hat, y = sqrt_std_res)) +
    geom_point(alpha = 0.5, color = pal[4]) +
    geom_smooth(method = "loess", se = FALSE, color = "#9B1B30") +
    labs(title = "Scale-Location Plot",
         x = "Fitted values",
         y = expression(sqrt("|Standardized Residuals|"))) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Variance plot
  p5 <- ggplot(data, aes(x = mu.hat, y = res_var)) +
    geom_point(alpha = 0.5, color = pal[5]) +
    geom_hline(yintercept = 0, col = "#9B1B30", linetype = "dashed",linewidth = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "#9B1B30") +
    labs(title = "Variance Plot",
         x = "Fitted values",
         y = "Residuals / sqrt(Var)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Normal Q-Q
  qq <- qqnorm(std.res, plot.it = FALSE)

  p6 <- ggplot(data.frame(x = qq$x, y = qq$y), aes(x = x,y = y)) +
    stat_qq(color = pal[6]) +
    stat_qq_line(color = "#9B1B30", linewidth = 1) +
    labs(title = "Normal Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Standardized Residuals") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  wp <- wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 2)

  if (save) {
    filename <- paste0(model, " diagnostics.png")
    filepath <- file.path(out_dir, filename)

    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))

      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(NULL))
      }
    }

    ggsave(
      filename = filepath,
      plot = wp,
      width = 12,
      height = 6,
      dpi = 300
    )

    message("Plot saved to: ", filepath)
  }

  wp
}

# Function that performs graphical diagnostics for GAMLSS-objects
ggplot_gamlss_diagnostics <- function(obj, xvar = NULL, summaries = TRUE, model = "gamlss", out_dir = "plot/", save = TRUE) {

  # Assertions
  assertClass(obj, classes = "gamlss")
  assertFlag(summaries)
  assertString(model)
  assertString(out_dir)

  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  }

  residx <- residuals(obj)
  w <- obj$weights

  # X-axis variable
  if (is.null(xvar)) {
    xvar <- seq_along(residx)
    xlab <- "Index"
  } else {
    xlab <- deparse(substitute(xvar))
  }

  # Fitted values
  fittedvalues <- if (length(residx) == obj$N) {
    if (is.null(fitted(obj))) fitted(obj, "sigma") else fitted(obj)
  } else {
    rep(if (is.null(fitted(obj))) fitted(obj, "sigma") else fitted(obj), w)
  }

  df <- data.frame(
    residuals = residx,
    fitted = fittedvalues,
    xvar = xvar
  )

  pal <- viridis(6)

  # Residuals vs Fitted
  p1 <- ggplot(df, aes(fitted, residuals)) +
    geom_point(color = pal[5], alpha = 0.7) +
    geom_hline(yintercept = 0, col = "#9B1B30", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Against Fitted Values",
      x = "Fitted Values",
      y = "Quantile Residuals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Residuals vs x
  p2 <- ggplot(df, aes(xvar, residuals)) +
    geom_point(color = pal[5], alpha = 0.7) +
    geom_hline(yintercept = 0, col = "#9B1B30", linetype = "dashed", linewidth = 1) +
    labs(
      title = paste("Against", xlab),
      x = xlab,
      y = "Quantile Residuals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # Density plot
  p3 <- ggplot(df, aes(residuals)) +
    geom_density(color = "#9B1B30", linewidth = 0.6) +
    geom_rug(color = "#001", alpha = 0.4) +
    labs(
      title = "Density Estimate",
      x = "Quantile Residuals",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  # QQ plot
  p4 <- ggplot(df, aes(sample = residuals)) +
    stat_qq(color = pal[6]) +
    stat_qq_line(color = "#9B1B30", linewidth = 1) +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12), axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_text(face = "bold")
    )

  wp <- wrap_plots(p1, p2, p3, p4, ncol = 2)

  if (save) {
    filename <- paste0(model, " diagnostics.png")
    filepath <- file.path(out_dir, filename)

    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))

      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(NULL))
      }
    }

   ggsave(
      filename = filepath,
      plot = wp,
      width = 12,
      height = 6,
      dpi = 300
    )

    message("Plot saved to: ", filepath)
  }

  # Summary statistics
  if (isTRUE(summaries)) {

    is_ok <- is.finite(residx)
    clean_res <- as.numeric(residx)[is_ok]

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

    cat("******************************************************************\n")
    if (obj$type == "Continuous") {
      cat("\tSummary of the Quantile Residuals\n")
    } else {
      cat("\tSummary of the Randomised Quantile Residuals\n")
    }

    cat("mean      =", m1, "\n")
    cat("variance  =", m2, "\n")
    cat("skewness  =", skew, "\n")
    cat("kurtosis  =", kurt, "\n")
    cat("Filliben correlation =", filliben, "\n")
    cat("******************************************************************\n")
  }

  wp
}

ggplot_coefficients <- function(models, colours = NULL, model = "model", out_dir = "plot/", save = TRUE) {

  # Assertions
  assertList(models, names = "named", min.len = 1, max.len = 5)
  assertCharacter(colours, any.missing = FALSE, null.ok = TRUE)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (is.null(colours)) {
    colours <- c("#9DC183","#9B1B30", "#DAA520", "#003366", "#9575CD")
    colours <- setNames(colours[1:length(models)], names(models))
  }

  plot_data <- list()

  # 1. Extract coefficients (mu only)
  for (i in seq_along(models)) {
    plot_data[[i]] <- tidy(models[[i]]) |> filter(parameter == "mu") |> mutate(Model = names(models)[[i]])
  }

  dt <- bind_rows(plot_data)

  # 2. Data preparation
  plot_data_final <- dt %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = gsub("`", "", term),
      term = gsub("Risk - ", "", term),
      term = gsub("Season", "Season: ", term),
      Group = dplyr::case_when(
        grepl("Season", term) ~ "Seasonal patterns",
        grepl("Year", term) ~ "Annual trends",
        TRUE ~ "Sanitary risks (Screening)"
      )
    )

  # 3. Plot
  plot <- ggplot(plot_data_final, aes(x = estimate, y = term, color = Model, shape = Model)) +
    geom_point(size = 2.5, position = position_dodge(width = 1)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                       xmax = estimate + 1.96 * std.error),
                   width = 0.5, position = position_dodge(width = 1)) +
    scale_color_manual(values = colours) +
    facet_grid(Group ~ ., scales = "free_y", space = "free_y") +
    labs(
      title = "Comparative analysis of milk quality drivers",
      subtitle = "Impact of sanitary and environmental factors on the mean (mu parameter)",
      x = "Estimated effect (Beta coefficient in log scale)",
      y = NULL,
      color = "Indicator",
      shape = "Indicator"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold")
    )

  # 4. Save plot (single save)
  if (save) {
    filename <- paste0(model, " coefficients.png")
    filepath <- file.path(out_dir, filename)

    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))
      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(plot))
      }
    }

    ggsave(filename = filepath, plot = plot, width = 12, height = 6, dpi = 300)
    message("Plot saved to: ", filepath)
  }

  plot
}
