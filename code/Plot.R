
ggplot_trend <- function(data, x, y, col, palette = NULL, title, is_month = FALSE, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertDataFrame(data)
  assertString(x)
  assertString(y)
  assertString(col)
  assertCharacter(palette, any.missing = FALSE, null.ok = TRUE)
  assertString(title)
  assertFlag(is_month)
  assertString(out_dir)
  assertFlag(save)
  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Palette ───────────────────────────────────────────────────────────
  n_levels <- nlevels(factor(data[[col]]))

  if (is.null(palette)) {
    palette <- setNames(
      colorRampPalette(c("#2C4A6E", "#AEC6CF"))(n_levels),
      levels(factor(data[[col]]))
    )
  } else if (length(palette) == 1 && exists("palettes") && palette %in% names(palettes)) {
    palette <- palettes[[palette]]
  }

  # ── 3. Theme ─────────────────────────────────────────────────────────────
  my_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9)
    )

  accent <- "#9B1B30"

  # ── 4. Plot ──────────────────────────────────────────────────────────────
  plot <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), colour = !!sym(col), fill = !!sym(col), group  = !!sym(col))) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, alpha = 0.12, linewidth = 1) +
    scale_color_manual(values = palette) +
    scale_fill_manual(values  = palette) +
    labs(
      title = title,
      x = x,
      y = y,
      color = col,
      fill = col
    ) +
    my_theme

  # ── 5. Month scale ───────────────────────────────────────────────────────
  if (is_month) {
    month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    plot <- plot +
      scale_x_continuous(
        breaks = 1:12,
        labels = month_labels
      )
  }

  # ── 6. Save ──────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(title, ".png")
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

  invisible(plot)
}

ggplot_interaction <- function(data, x, y, color, group = color, facet_formula, facet_type = c("wrap", "grid"),
  palette, title, subtitle, xlab = x, ylab = y, colorlab = color, show_error = TRUE, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertDataFrame(data)
  assertString(x)
  assertString(y)
  assertString(color)
  assertCharacter(palette, any.missing = FALSE)
  assertString(title)
  assertString(out_dir)
  assertFlag(save)
  assertFlag(show_error)
  facet_type <- match.arg(facet_type)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ── 2. Theme ─────────────────────────────────────────────────────────────
  my_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 10),
      strip.text = element_text(face = "bold", size = 10)
    )

  accent <- "#9B1B30"

  # ── 3. Plot ───────────────────────────────────────────────────────────────
  plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], color = .data[[color]], group = .data[[group]])) +
    stat_summary(fun = mean, geom = "line",  linewidth = 1.0) +
    stat_summary(fun = mean, geom = "point", size = 2.5) +
    scale_color_manual(values = palette) +
    labs(title = title,
         subtitle = subtitle,
         x = xlab,
         y = ylab,
         color = colorlab) +
    my_theme

  if (show_error) {
    plot <- plot + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, linewidth = 0.5, alpha = 0.7)
  }

  if (facet_type == "wrap") {
    plot <- plot + facet_wrap(facet_formula, labeller = label_both)
  } else {
    plot <- plot + facet_grid(facet_formula, labeller = label_both)
  }

  # ── 4. Save ───────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(title, ".png")
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

  invisible(plot)
}

ggplot_density <- function(data, x, y, palette = NULL, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertDataFrame(data)
  assertString(x)
  assertString(y)
  assertCharacter(palette, any.missing = FALSE, null.ok = TRUE)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Palette ───────────────────────────────────────────────────────────
  n_levels <- nlevels(factor(data[[y]]))

  if (is.null(palette)) {
    palette <- setNames(
      colorRampPalette(c("#2C4A6E", "#AEC6CF"))(n_levels),
      levels(factor(data[[y]]))
    )
  }

  # ── 3. Summary statistics ────────────────────────────────────────────────
  stats_dt <- data[, .(
    N = .N,
    Mean = round(mean(.SD[[x]], na.rm = TRUE), 4),
    Median = round(median(.SD[[x]], na.rm = TRUE), 4),
    SD = round(sd(.SD[[x]], na.rm = TRUE), 4),
    Min = round(min(.SD[[x]], na.rm = TRUE), 4),
    Max = round(max(.SD[[x]], na.rm = TRUE), 4)
  ), by = y, .SDcols = x]

  # ── 4. Theme ─────────────────────────────────────────────────────────────
  my_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )

  accent <- "#001"

  # ── 5. Plot ──────────────────────────────────────────────────────────────
  plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[y]], color = .data[[y]])) +
    ggridges::geom_density_ridges(alpha = 0.6, bandwidth = 0.05,
      quantile_lines = TRUE, quantiles = 2, vline_color = accent,
      vline_linetype = "dashed", vline_size = 0.7
    ) + 
    stat_summary(fun = mean, geom = "point", color = accent, size = 2.5, shape = 18) +
    scale_fill_manual(values  = palette) +
    scale_color_manual(values = palette) +
    labs(
      title = paste("Distribution of", x, "by", y),
      subtitle = "Dashed line = Median | Diamond = Mean",
      x = x,
      y = NULL
    ) +
    my_theme +
    theme(
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.margin = margin(t = 20, r = 15, b = 10, l = 15)
    )

  # ── 6. Save ──────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(x, " density by ", y, ".png")
    filepath <- file.path(out_dir, filename)
    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))
      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(plot))
      }
    }
    h <- max(5, n_levels * 1.2)
    ggsave(filename = filepath, plot = plot, width = 12, height = h, dpi = 300)
    message("Plot saved to: ", filepath)
  }
  
  invisible(plot)
}

ggplot_normal_distribution <- function(data, var, title = NULL, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertDataFrame(data)
  assertString(var)
  assertString(out_dir)
  assertFlag(save)
  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Data ───────────────────────────────────────────────────────────────
  x_vals <- data[[var]][is.finite(data[[var]])]
  mu <- mean(x_vals)
  sigma <- sd(x_vals)
  skew <- mean(((x_vals - mu) / sigma)^3)
  kurt <- mean(((x_vals - mu) / sigma)^4)

  stats_label <- sprintf(
    "Mean = %.3f\nSD = %.3f\nSkewness = %.3f\nKurtosis = %.3f",
    mu, sigma, skew, kurt
  )

  # ── 3. Theme ─────────────────────────────────────────────────────────────
  my_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )

  # ── 4. Plot ───────────────────────────────────────────────────────────────
  plot <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = min(50, ceiling(sqrt(length(x_vals)))),
      fill = "#3D5A80", color = "white", alpha = 0.6, linewidth = 0.2) +
    geom_density(color = "#3D5A80", linewidth = 0.8, adjust = 1.2) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "#9B1B30",
      linewidth = 0.9, linetype = "dashed") +
    geom_vline(xintercept = mu, color = "#9B1B30", linetype = "dotted", linewidth = 0.7) +
    annotate("label", x = Inf, y = Inf, label = stats_label, hjust = 1.1, vjust = 1.2,
      size = 3, color = "gray30", fill = "white", label.size = 0.3, fontface = "italic") +
    labs(
      title  = title %||% paste("Distribution of", var),
      subtitle = paste0("n = ", format(length(x_vals), big.mark = ",")),
      x = var,
      y = "Density"
    ) +
    my_theme

  # ── 5. Save ───────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(var, " distribution.png")
    filepath <- file.path(out_dir, filename)
    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))
      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(plot))
      }
    }
    ggsave(filename = filepath, plot = plot, width = 10, height = 6, dpi = 300)
    message("Plot saved to: ", filepath)
  }

  invisible(plot)
}

ggplot_glm_diagnostics <- function(obj, model = "glm", out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertClass(obj, classes = "glm")
  assertString(model)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Diagnostic quantities ─────────────────────────────────────────────
  y <- obj$y
  eta.hat <- predict(obj, type = "link")
  mu.hat <- predict(obj, type = "response")
  dev.res <- residuals(obj, type = "deviance")
  std.res <- residuals(obj, type = "pearson") / sqrt(1 - hatvalues(obj))
  sqrt_stdres <- sqrt(abs(std.res))
  derivate <- obj$family$mu.eta(eta.hat)
  zeta.hat <- eta.hat + (y - mu.hat) * derivate
  res_var <- dev.res / sqrt(obj$family$variance(mu.hat))
  cooks <- cooks.distance(obj)

  diag_dt <- data.frame(
    eta_hat = eta.hat,
    mu_hat = mu.hat,
    dev_res = dev.res,
    std_res = std.res,
    sqrt_stdres = sqrt_stdres,
    zeta_hat = zeta.hat,
    res_var = res_var,
    cooks = cooks,
    obs = seq_along(y)
  )

  # ── 3. Adaptive parameters based on n ────────────────────────────────────
  n <- nrow(diag_dt)
  use_bin2d <- n > 10000
  pt_size <- ifelse(n > 50000, 0.3,  ifelse(n > 10000, 0.6,  1.2))
  pt_alpha <- ifelse(n > 50000, 0.08, ifelse(n > 10000, 0.20, 0.50))
  loess_span <- ifelse(n > 10000, 0.3,  0.75)
  soglia_cook <- 4 / n
  outlier_idx <- abs(diag_dt$std_res) > 3

  # ── 4. "Muted Professional" palette ──────────────────────────────────────
  pal <- c(
    p1 = "#3D5A80",
    p2 = "#5C7FA3",
    p3 = "#2E8B57",
    p4 = "#6B8F71",
    p5 = "#8B6914",
    p6 = "#B8965A"
  )
  accent <- "#9B1B30"

  # ── 5. Base theme ────────────────────────────────────────────────────────
  my_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold", margin = margin(b = 8)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position  = "none" 
    )

  # ── 6. Reusable adaptive layers ──────────────────────────────────────────
  scatter_layer <- function(x_var, y_var, col) {
    if (use_bin2d) {
      list(
        geom_bin2d(aes(x = .data[[x_var]], y = .data[[y_var]]), bins = 80),
        scale_fill_gradient(low = "gray90", high = col)
      )
    } else {
      list(
        geom_point(aes(x = .data[[x_var]], y = .data[[y_var]]), color = col, size = pt_size, alpha = pt_alpha)
      )
    }
  }

  outlier_layer <- function(x_var, y_var) {
    if (!any(outlier_idx)) return(NULL)
    list(
      geom_point(data  = diag_dt[outlier_idx, ], aes(x = .data[[x_var]], y = .data[[y_var]]), color = accent, size = 1.5, shape = 1, stroke = 0.8),
      ggrepel::geom_text_repel(
        data     = diag_dt[outlier_idx, ],
        aes(x    = .data[[x_var]], y = .data[[y_var]], label = obs),
        size     = 2.5, color = accent, max.overlaps = 15,
        segment.color = "gray70"
      )
    )
  }

  smooth_layer <- function(x_var, y_var) {
    geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]), method = "loess", formula = y ~ x, span = loess_span,
      se = TRUE, color = accent, fill = accent, alpha = 0.12, linewidth = 0.8)
  }

  ref_bands <- list(
    geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "gray50", linewidth = 0.4),
    geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "gray65", linewidth = 0.3)
  )

  # ── 7. Plot ──────────────────────────────────────────────────────────────
  # P1 — Linearity
  p1 <- ggplot(diag_dt) +
    scatter_layer("eta_hat", "dev_res", pal["p1"]) +
    geom_hline(yintercept = 0, color = accent, linetype = "dashed", linewidth = 0.7) +
    smooth_layer("eta_hat", "dev_res") + 
    outlier_layer("eta_hat", "dev_res") +
    ref_bands +
    labs(title = "Linearity",
         subtitle = "Deviance residuals vs linear predictor",
         x = expression(hat(eta)), y = "Deviance residuals") +
    my_theme

  # P2 — Link function
  p2 <- ggplot(diag_dt) +
    scatter_layer("eta_hat", "zeta_hat", pal["p2"]) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    geom_smooth(aes(x = .data[["eta_hat"]], y = .data[["zeta_hat"]]), method = "lm", formula = y ~ x, se = TRUE,
      color = accent, fill = accent, alpha = 0.12, linewidth = 0.8)
    labs(title = "Link function",
         subtitle = "Component + residual plot",
         x = expression(hat(eta)), y = expression(hat(zeta))) +
    my_theme

  # P3 — Residuals vs Fitted
  p3 <- ggplot(diag_dt) +
    scatter_layer("mu_hat", "std_res", pal["p3"]) +
    geom_hline(yintercept = 0, color = accent, linetype = "dashed", linewidth = 0.7) +
    smooth_layer("mu_hat", "std_res") + 
    outlier_layer("mu_hat", "std_res") +
    ref_bands +
    labs(title = "Residuals vs Fitted",
         subtitle = "Standardized residuals vs fitted values",
         x = "Fitted values", y = "Standardized residuals") +
    my_theme

  # P4 — Scale-Location
  p4 <- ggplot(diag_dt) +
    scatter_layer("mu_hat", "sqrt_stdres", pal["p4"]) +
    geom_hline(yintercept = mean(diag_dt$sqrt_stdres), color = "gray50", linetype = "dashed", linewidth = 0.6) +
    smooth_layer("mu_hat", "sqrt_stdres") +
    labs(title = "Scale-Location",
         subtitle = "Homoscedasticity check",
         x = "Fitted values", y = expression(sqrt("|Std. residuals|"))) +
    my_theme

  # P5 — Variance plot
  p5 <- ggplot(diag_dt) +
    scatter_layer("mu_hat", "res_var", pal["p5"]) +
    geom_hline(yintercept = 0, color = accent, linetype = "dashed", linewidth = 0.7) +
    smooth_layer("mu_hat", "res_var") +
    outlier_layer("mu_hat", "res_var") +
    ref_bands +
    labs(title = "Variance plot",
         subtitle = "Residuals vs variance function",
         x = "Fitted values", y = "Residuals / sqrt(Var)") +
    my_theme

  # P6 — Q-Q con banda di confidenza (qqplotr)
  p6 <- if (requireNamespace("qqplotr", quietly = TRUE)) {
    ggplot(diag_dt, aes(sample = std_res)) +
      qqplotr::stat_qq_band(distribution = "norm", fill = pal["p6"], alpha = 0.25) +
      qqplotr::stat_qq_line(distribution = "norm", color = accent, linewidth = 0.8) +
      qqplotr::stat_qq_point(distribution = "norm", color = pal["p6"], size = pt_size, alpha = min(pt_alpha * 2, 0.8)) +
      labs(title = "Normal Q-Q",
           subtitle = "With 95% confidence band",
           x = "Theoretical quantiles", y = "Standardized residuals") +
      my_theme
  } else {
    # fallback
    ggplot(diag_dt, aes(sample = std_res)) +
      stat_qq(color = pal["p6"], size = pt_size, alpha = min(pt_alpha * 2, 0.8)) +
      stat_qq_line(color = accent, linewidth = 0.8) +
      labs(title = "Normal Q-Q",
           subtitle = "Install {qqplotr} for confidence bands",
           x = "Theoretical quantiles", y = "Standardized residuals") +
      my_theme
  }

  # P7 — Cook's Distance (solo punti sopra soglia evidenziati)
  cook_above <- diag_dt[diag_dt$cooks > soglia_cook, ]
  
  p7 <- ggplot(diag_dt, aes(x = obs, y = cooks)) +
    geom_hline(yintercept = soglia_cook, color = accent, linetype = "dashed", linewidth = 0.7) +
    geom_point(color = "gray75", size = 0.4, alpha = 0.4) +
    {if (nrow(cook_above) > 0) list(
      geom_point(data  = cook_above, color = accent, size = 1.8),
      ggrepel::geom_text_repel(
        data = cook_above,
        aes(label = obs),
        size = 2.5, color = accent,
        segment.color = "gray70", max.overlaps = 20
      )
    )} +
    annotate("text", x = max(diag_dt$obs) * 0.92, y = soglia_cook, label = "4/n", vjust = -0.6, size = 3, color = accent, fontface = "italic") +
    labs(title = "Cook's Distance",
         subtitle = paste0(nrow(cook_above), " influential observations (> 4/n)"),
         x = "Observation index", y = "Cook's D") +
    my_theme

  # ── 8. Final composition ─────────────────────────────────────────────────
  wp <- wrap_plots(p1, p2, p3, p4, p5, p6, p7, ncol = 2, widths = 1) +
    plot_annotation(
      title = paste0("GLM Diagnostics — ", model),
      subtitle = paste0("n = ", format(n, big.mark = ","), " | Family: ", obj$family$family, " | Link: ", obj$family$link),
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50")
      )
    )

  # ── 9. Save ──────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(model, " diagnostics.png")
    filepath <- file.path(out_dir, filename)
    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))
      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(wp))
      }
    }
    h <- ifelse(n > 10000, 20, 16)
    ggsave(filename = filepath, plot = wp, width = 14, height = h, dpi = 300)
    message("Plot saved to: ", filepath)
  }
  wp
}

ggplot_gamlss_diagnostics <- function(obj, xvar = NULL, model = "gamlss", out_dir = "plot/", save = TRUE, summaries = TRUE) {
  
  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertClass(obj, classes = "gamlss")
  assertFlag(summaries)
  assertString(model)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Quantile residuals & fitted ───────────────────────────────────────
  residx <- as.numeric(residuals(obj))
  fittedvalues <- as.numeric(fitted(obj))

  # X-axis variable
  if (is.null(xvar)) {
    xvar_vals <- seq_along(residx)
    xlab <- "Index"
  } else {
    xvar_vals <- xvar
    xlab <- deparse(substitute(xvar))
  }

  diag_dt <- data.table(
    residuals = residx,
    fitted = fittedvalues,
    xvar = xvar_vals,
    obs = seq_along(residx)
  )
  diag_dt <- diag_dt[is.finite(residuals)]

  # ── 3. Adaptive parameters ───────────────────────────────────────────────
  n <- nrow(diag_dt)
  use_bin2d <- n > 10000
  pt_size <- ifelse(n > 50000, 0.3,  ifelse(n > 10000, 0.6,  1.2))
  pt_alpha <- ifelse(n > 50000, 0.08, ifelse(n > 10000, 0.20, 0.50))
  loess_span <- ifelse(n > 10000, 0.3,  0.75)
  outlier_idx <- abs(diag_dt$residuals) > 3

  # ── 4. Palette & theme ───────────────────────────────────────────────────
  pal <- c(
    p1 = "#3D5A80",
    p2 = "#5C7FA3",
    p3 = "#2E8B57",
    p4 = "#6B8F71",
    p5 = "#8B6914",
    p6 = "#B8965A"
  )
  accent <- "#9B1B30"

  my_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold", margin = margin(b = 6)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  # ── 5. Reusable adaptive layers ──────────────────────────────────────────
  scatter_layer <- function(x_var, y_var, col) {
    if (use_bin2d) {
      list(
        geom_bin2d(aes(x = .data[[x_var]], y = .data[[y_var]]), bins = 80), scale_fill_gradient(low = "gray95", high = col, name = "Count")
      )
    } else {
      list(geom_point(aes(x = .data[[x_var]], y = .data[[y_var]]), color = col, size = pt_size, alpha = pt_alpha))
    }
  }

  smooth_layer <- function(x_var, y_var) {
    geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]), method = "loess", formula = y ~ x,
      span = loess_span, se = TRUE, color = accent, fill = accent, alpha = 0.12, linewidth = 0.8)
  }
  
  outlier_layer <- function(x_var, y_var) {
    if (!any(outlier_idx)) return(NULL)
    list(
      geom_point(data  = diag_dt[outlier_idx, ], aes(x = .data[[x_var]], y = .data[[y_var]]), color = accent, size = 1.8, shape = 1, stroke = 0.8),
      ggrepel::geom_text_repel(
        data = diag_dt[outlier_idx, ],
        aes(x = .data[[x_var]], y = .data[[y_var]], label = obs),
        size = 2.5, color = accent,
        segment.color = "gray70", max.overlaps = 15
      )
    )
  }

  ref_bands <- list(
    geom_hline(yintercept = c(-3,  3), linetype = "dashed", color = "gray50", linewidth = 0.4),
    geom_hline(yintercept = c(-2,  2), linetype = "dotted", color = "gray65", linewidth = 0.3),
    geom_hline(yintercept = 0, linetype = "dashed", color = accent, linewidth = 0.7)
  )

  # ── 6. Plots ─────────────────────────────────────────────────────────────
  # P1 — Residuals vs Fitted
  p1 <- ggplot(diag_dt) +
    scatter_layer("fitted", "residuals", pal["p1"]) +
    ref_bands +
    smooth_layer("fitted", "residuals") +
    outlier_layer("fitted", "residuals") +
    labs(title    = "Residuals vs Fitted",
         subtitle = "Should show no pattern",
         x = "Fitted values (mu)", y = "Quantile residuals") +
    my_theme

  # P2 — Residuals vs x
  p2 <- ggplot(diag_dt) +
    scatter_layer("xvar", "residuals", pal["p2"]) +
    ref_bands +
    smooth_layer("xvar", "residuals") +
    outlier_layer("xvar", "residuals") +
    labs(title    = paste("Residuals vs", xlab),
         subtitle = "Check for remaining structure",
         x = xlab, y = "Quantile residuals") +
    my_theme

  # P3 — Density + curva N(0,1) teorica sovrapposta
  p3 <- ggplot(diag_dt, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), bins = min(50, ceiling(sqrt(n))), fill = pal["p3"], alpha = 0.4, color = "white") +
    geom_density(color = pal["p3"], linewidth = 0.8) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = accent, linewidth = 0.9, linetype = "dashed") +
    geom_rug(color = "gray40", alpha = 0.3, linewidth = 0.3) +
    annotate("text", x = Inf, y = Inf, label = "— N(0,1) theoretical", hjust = 1.1, vjust = 1.8, size = 3, color = accent, fontface = "italic") +
    labs(title = "Density of Quantile Residuals",
         subtitle = "Should approximate N(0,1)",
         x = "Quantile residuals", y = "Density") +
    my_theme

  # P4 — Q-Q con banda di confidenza
  p4 <- if (requireNamespace("qqplotr", quietly = TRUE)) {
    ggplot(diag_dt, aes(sample = residuals)) +
      qqplotr::stat_qq_band(distribution = "norm", fill = pal["p4"], alpha = 0.25) +
      qqplotr::stat_qq_line(distribution = "norm", color = accent, linewidth = 0.8) +
      qqplotr::stat_qq_point(distribution = "norm", color = pal["p4"], size = pt_size, alpha = min(pt_alpha * 2, 0.8)) +
      labs(title = "Normal Q-Q Plot",
           subtitle = "With 95% confidence band",
           x = "Theoretical quantiles", y = "Sample quantiles") +
      my_theme
  } else {
    ggplot(diag_dt, aes(sample = residuals)) +
      stat_qq(color = pal["p4"], size = pt_size, alpha = min(pt_alpha * 2, 0.8)) +
      stat_qq_line(color = accent, linewidth = 0.8) +
      labs(title = "Normal Q-Q Plot",
           subtitle = "Install {qqplotr} for confidence bands",
           x = "Theoretical quantiles", y = "Sample quantiles") +
      my_theme
  }

  # P5 — ACF dei residui
  acf_vals <- acf(diag_dt$residuals, plot = FALSE, lag.max = 40)
  acf_dt   <- data.table(
    lag = as.numeric(acf_vals$lag[-1]),
    acf = as.numeric(acf_vals$acf[-1])
  )
  ci_acf <- qnorm(0.975) / sqrt(n)

  p5 <- ggplot(acf_dt, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_hline(yintercept = c(-ci_acf, ci_acf), linetype = "dashed", color = accent, linewidth = 0.5) +
    geom_segment(aes(xend = lag, yend = 0), color = pal["p5"], linewidth = 0.7) +
    geom_point(color = pal["p5"], size = 1.5) +
    annotate("text", x = max(acf_dt$lag) * 0.85, y = ci_acf, label = "95% CI", vjust = -0.6, size = 3, color = accent, fontface = "italic") +
    labs(title = "ACF of Quantile Residuals",
         subtitle = "Should lie within confidence bands",
         x = "Lag", y = "Autocorrelation") +
    my_theme

  # P6 — Worm plot (deviazioni locali dalla normalità)
  sorted_res <- sort(diag_dt$residuals)
  theo_q <- qnorm(ppoints(length(sorted_res)))
  worm_dt <- data.table(
    x = theo_q,
    y = sorted_res - theo_q
  )

  se_worm <- (1 / dnorm(theo_q)) * sqrt(ppoints(length(sorted_res)) * (1 - ppoints(length(sorted_res))) / length(sorted_res))
  worm_dt$upper <- 1.96 * se_worm
  worm_dt$lower <- -1.96 * se_worm
  
  p6 <- ggplot(worm_dt, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = pal["p6"], alpha = 0.2) +
    geom_hline(yintercept = 0, color = accent, linetype = "dashed", linewidth = 0.7) +
    geom_line(color = pal["p6"], linewidth = 0.7) +
    labs(title = "Worm Plot",
         subtitle = "Local deviations from N(0,1) — should stay within band",
         x = "Theoretical quantiles", y = "Deviation from Q-Q line") +
    my_theme

  # ── 7. Final composition ─────────────────────────────────────────────────
  wp <- wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 2) +
    plot_annotation(
      title = paste0("GAMLSS Diagnostics — ", model),
      subtitle = paste0("n = ", format(n, big.mark = ","), " | Family: ", obj$family[1], " | Quantile residuals"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50")
      )
    )

  # ── 8. Summary statistics ────────────────────────────────────────────────
  if (isTRUE(summaries)) {
    clean_res  <- diag_dt$residuals
    m1 <- mean(clean_res)
    m2 <- var(clean_res)
    m3 <- mean((clean_res - m1)^3)
    m4 <- mean((clean_res - m1)^4)
    skew <- m3 / m2^(3/2)
    kurt <- m4 / m2^2
    filliben <- cor(sort(clean_res), qnorm(ppoints(length(clean_res))))

    summary_dt <- data.table(
      Statistic = c("Mean", "Variance", "Skewness", "Kurtosis", "Filliben correlation"),
      Value = round(c(m1, m2, skew, kurt, filliben), 4),
      Reference = c("≈ 0", "≈ 1", "≈ 0", "≈ 3", "≈ 1")
    )
    cat("\n──────────────────────────────────────────────────────────────────\n")
    cat(ifelse(obj$type == "Continuous", "\tQuantile Residuals Summary\n", "\tRandomised Quantile Residuals Summary\n"))
    print(summary_dt, row.names = FALSE)
    cat("──────────────────────────────────────────────────────────────────\n\n")
  }
  
  # ── 9. Save ──────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(model, " diagnostics.png")
    filepath <- file.path(out_dir, filename)
    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))
      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(wp))
      }
    }
    h <- ifelse(n > 10000, 22, 18)
    ggsave(filename = filepath, plot = wp, width = 14, height = h, dpi = 300)
    message("Plot saved to: ", filepath)
  }

  invisible(wp)
}

ggplot_coefficients <- function(models, colours = NULL, model = "model", out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ────────────────────────────────────────────────────────
  assertList(models, names = "named", min.len = 1, max.len = 5)
  assertCharacter(colours, any.missing = FALSE, null.ok = TRUE)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (is.null(colours)) {
    colours <- c("#9DC183","#9B1B30", "#DAA520", "#003366", "#9575CD")
    colours <- setNames(colours[1:length(models)], names(models))
  }

  # ── 2. Coefficients ───────────────────────────────────────────────────────
  dt <- rbindlist(lapply(seq_along(models), function(i) {
    as.data.table(tidy(models[[i]]))[parameter == "mu"][, Model := names(models)[i]]
  }))

  # ── 3. Data preparation ───────────────────────────────────────────────────
  plot_data_final <- dt[term != "(Intercept)"]
  plot_data_final[, term := gsub("`", "", term)]
  plot_data_final[, term := gsub("Risk - ", "", term)]
  plot_data_final[, term := gsub("Season", "Season: ", term)]

  plot_data_final[, term := gsub("pb\\(YearNum, by = Season: \\)", "Trend (pb YearNum x Season)", term)]
  plot_data_final[, term := gsub("pb\\(YearNum, by = (\\w+)\\)", "Trend (pb YearNum x \\1)", term)]

  plot_data_final[, Group := fcase(
    grepl("Season", term), "Seasonal patterns",
    grepl("Year|pb\\(|YearNum", term),  "Annual trends",
    default = "Sanitary risks (Screening)"
  )]

  thresh_low  <- 0.15
  thresh_high <- 0.30

  col_negligible <- "#EEF2F7"
  col_moderate <- "#D6E4F0" 
  col_relevant <- "#AEC6CF"

  reference <- list(
    annotate("rect", xmin = -Inf, xmax = -thresh_high, ymin = -Inf, ymax = Inf, fill = col_relevant, alpha = 0.6),
    annotate("rect", xmin = -thresh_high, xmax = -thresh_low, ymin = -Inf, ymax = Inf, fill = col_moderate, alpha = 0.6),
    annotate("rect", xmin = -thresh_low,  xmax =  thresh_low, ymin = -Inf, ymax = Inf, fill = col_negligible, alpha = 0.6),
    annotate("rect", xmin = thresh_low,  xmax =  thresh_high, ymin = -Inf, ymax = Inf, fill = col_moderate, alpha = 0.6),
    annotate("rect", xmin = thresh_high, xmax =  Inf, ymin = -Inf, ymax = Inf, fill = col_relevant, alpha = 0.6),

    # Labels
    annotate("text", x = 0, y = Inf, vjust = -0.4, label = "negligible", size = 2.8, color = "gray50", fontface = "italic"),
    annotate("text", x = (thresh_low + thresh_high)/2,y = Inf, vjust = -0.4, label = "moderate", size = 2.8, color = "gray50", fontface = "italic"),
    annotate("text", x = -(thresh_low + thresh_high)/2, y = Inf, vjust = -0.4, label = "moderate", size = 2.8, color = "gray50", fontface = "italic"),
    annotate("text", x =  0.55, y = Inf, vjust = -0.4, label = "relevant", size = 2.8, color = "gray50", fontface = "italic"),
    annotate("text", x = -0.55, y = Inf, vjust = -0.4, label = "relevant", size = 2.8, color = "gray50", fontface = "italic")
  )

  # ── 4. Plot ───────────────────────────────────────────────────────────────
  plot <- ggplot(plot_data_final, aes(x = estimate, y = term, color = Model, shape = Model)) +
    reference + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error),
      height = 0.4, position = position_dodge(width = 0.8), linewidth = 0.5) +
    geom_point(size = 2.5, position = position_dodge(width = 0.8)) +
    scale_x_continuous(
      breaks = seq(-0.8, 1.6, by = 0.2),
      minor_breaks = seq(-0.8, 1.6, by = 0.1)
    ) +
    coord_cartesian(xlim = c(-0.8, 1.6), clip = "off") +
    scale_color_manual(values = colours) +
    scale_shape_manual(values = c(16, 17, 15, 18, 8)[1:length(models)]) +
    facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(
      title = "Comparative analysis of milk quality drivers",
      subtitle = "Impact of sanitary and environmental factors on the mean (\u03bc parameter)",
      x = "Estimated effect (95% CI)",
      y = NULL,
      color = "Model",
      shape = "Model"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
      axis.text.y = element_text(size = 8, hjust = 1),
      axis.text.x = element_text(size = 9),
      axis.title.x = element_text(size = 10, margin = margin(t = 8)),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "gray85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      strip.text.y.left = element_text(angle = 0, face = "bold", size = 10, hjust = 1, color = "gray20"),
      strip.placement = "outside",
      panel.spacing = unit(0.8, "lines")
    )

  # ── 5. Save ──────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(model, " coefficients.png")
    filepath <- file.path(out_dir, filename)
    if (file.exists(filepath)) {
      answer <- readline(paste0("File '", filename, "' already exists. Overwrite? (Y/N): "))
      if (toupper(answer) != "Y") {
        message("No saving.")
        return(invisible(wp))
      }
    }
    ggsave(filename = filepath, plot = wp, width = 14, height = 10, dpi = 300)
    message("Plot saved to: ", filepath)
  }
  
  invisible(plot)
}

