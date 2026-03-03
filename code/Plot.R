
# Function that performs graphical diagnostics for glm-objects
ggplot_glm_diagnostics <- function(obj, data, y, model = "glm", out_dir = "plot/", save = TRUE) {

  # Assertions
  assertClass(obj, classes = "glm")
  assertDataFrame(data)

  if (missing(y)) stop("Response variable is missing")
  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
        warning("Duplicates might arise")
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
  pal <- viridis::viridis(6)

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

  wp <- patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 2)

  if (save) {
    existing_files <- list.files(out_dir, pattern = paste0(model, " diagnostics[0-9]+\\.png"))

    if (length(existing_files) == 0) {
      filename <- paste0(model," diagnostics1.png")

      ggsave(
        filename = file.path(out_dir, filename),
        plot = wp,
        width = 12,
        height = 6,
        dpi = 300
      )
    } else {
      val <- as.numeric(unlist(regmatches(existing_files, gregexec(pattern = "[0-9]", existing_files, perl = TRUE))))

      filename <- paste0(model, " diagnostics", max(val) + 1, ".png")

      ggsave(
        filename = file.path(out_dir, filename),
        plot = wp,
        width = 12,
        height = 6,
        dpi = 300
      )
    }
  }

  print(wp)

  invisible(wp)
}

# Function that performs grpahical diagnostics for GAMLSS-objects
autoplot_gamlss <- function(obj, xvar = NULL, summaries = TRUE, model = "gamlss", out_dir = "plot/", save = TRUE) {

  if (save) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    warning("Duplicates might arise")
  }

  # Assertions
  assertClass(obj, classes = "gamlss")

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

  pal <- viridis::viridis(6)

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

  wp <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)

  if (save) {
    existing_files <- list.files(out_dir, pattern = paste0(model, " diagnostics[0-9]+\\.png"))

    if (length(existing_files) == 0) {
      filename <- paste0(model," diagnostics1.png")

      ggsave(
        filename = file.path(out_dir, filename),
        plot = wp,
        width = 12,
        height = 6,
        dpi = 300
      )
    } else {
      val <- as.numeric(unlist(regmatches(existing_files, gregexec(pattern = "[0-9]", existing_files, perl = TRUE))))

      filename <- paste0(model, " diagnostics", max(val) + 1, ".png")

      ggsave(
        filename = file.path(out_dir, filename),
        plot = wp,
        width = 12,
        height = 6,
        dpi = 300
      )
    }
  }

  print(wp)

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

  invisible(wp)
}
