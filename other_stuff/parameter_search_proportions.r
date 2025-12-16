

guess_binom_counts <- function(p_hat,
                               ci_low,
                               ci_high,
                               alpha    = 0.05,
                               n_max    = 5000,
                               methods  = NULL,
                               window_x = 2,
                               top_k    = 20) {
  # ---------------------------------------------------------------------------
  # guess_binom_counts
  #
  # Aim:
  #   Given a reported proportion p_hat and a 95% confidence interval
  #   [ci_low, ci_high], guess which (x, n) (successes, total sample size)
  #   could have produced those numbers under common binomial CI methods.
  #
  #   This function performs a small "grid search" over:
  #     - sample size n = 1, 2, ..., n_max
  #     - success counts x near p_hat * n
  #     - CI methods in 'methods'
  #
  #   For each (method, n, x) combination, it computes:
  #     - the estimated proportion p_est = x / n
  #     - the confidence interval [ci_low_calc, ci_high_calc]
  #
  #   Then it measures how close (p_est, ci_low_calc, ci_high_calc) are to
  #   the reported (p_hat, ci_low, ci_high) via a simple distance metric.
  #   The best (smallest distance) candidates are returned.
  #
  # This is a neat example of parameter search / grid search:
  #   - The parameter grid is discrete (n, x, method)
  #   - We define a score function (distance between reported and implied values)
  #   - We keep the top_k parameter combinations with the smallest score.
  # ---------------------------------------------------------------------------

  # -----------------------------
  # 1. Normalize inputs
  # -----------------------------

  # If any of the inputs is > 1, we assume percentages and divide by 100.
  if (max(p_hat, ci_low, ci_high, na.rm = TRUE) > 1.0001) {
    p_hat  <- p_hat  / 100
    ci_low <- ci_low / 100
    ci_high <- ci_high / 100
  }

  # Sanity checks for the logical range of a proportion.
  if (p_hat <= 0 || p_hat >= 1) {
    warning("p_hat lies at or beyond the boundary (0 or 1). ",
            "Results might be less reliable.")
  }

  if (ci_low < 0 || ci_high > 1) {
    warning("Confidence interval bounds are outside [0, 1]. ",
            "Check if inputs are correctly specified.")
  }

  # Vector of supported CI methods.
  available_methods <- c("exact",        # Clopper–Pearson via binom.test
                         "wilson",       # Wilson score interval
                         "wald",         # Plain normal (Wald) interval
                         "agresti-coull" # Agresti–Coull interval
                         )

  # If user did not specify 'methods', we search over all available ones.
  if (is.null(methods)) {
    methods <- available_methods
  }

  # Match and validate requested methods against the available ones.
  methods <- match.arg(methods, choices = available_methods, several.ok = TRUE)

  # Z-value for (1 - alpha) CI, e.g. alpha = 0.05 -> z ~ 1.96.
  z <- qnorm(1 - alpha / 2)

  # -----------------------------
  # 2. Helper function for CIs
  # -----------------------------
  # This internal function encapsulates the CI formulas for each method.
  # It returns a list with elements:
  #   - p_est: point estimate (x / n)
  #   - ci_low: lower CI bound
  #   - ci_high: upper CI bound

  compute_ci <- function(x, n, method) {
    # Ensure integer counts (in case of numeric inputs).
    x <- as.integer(x)
    n <- as.integer(n)

    if (method == "exact") {
      # Clopper–Pearson exact CI via binom.test().
      bt  <- binom.test(x, n, conf.level = 1 - alpha)
      est <- as.numeric(bt$estimate)
      ci  <- as.numeric(bt$conf.int)

    } else if (method == "wilson") {
      # Wilson score interval.
      ph <- x / n
      denom  <- 1 + z^2 / n
      center <- ph + z^2 / (2 * n)
      half   <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2))
      ci_low_calc  <- (center - half) / denom
      ci_high_calc <- (center + half) / denom
      est <- ph
      ci  <- c(ci_low_calc, ci_high_calc)

    } else if (method == "wald") {
      # "Wald" (plain normal) interval: phat ± z * SE.
      # This is the classical textbook interval, but known to perform poorly
      # at small n or extreme proportions.
      ph <- x / n
      se <- sqrt(ph * (1 - ph) / n)
      ci_low_calc  <- ph - z * se
      ci_high_calc <- ph + z * se
      est <- ph
      ci  <- c(ci_low_calc, ci_high_calc)

    } else if (method == "agresti-coull") {
      # Agresti–Coull interval.
      # Uses "adjusted" counts x_tilde and n_tilde.
      n_tilde <- n + z^2
      x_tilde <- x + z^2 / 2
      ph_tilde <- x_tilde / n_tilde
      se_tilde <- sqrt(ph_tilde * (1 - ph_tilde) / n_tilde)
      ci_low_calc  <- ph_tilde - z * se_tilde
      ci_high_calc <- ph_tilde + z * se_tilde
      est <- ph_tilde
      ci  <- c(ci_low_calc, ci_high_calc)

    } else {
      # This should never happen due to match.arg, but just in case:
      stop("Unknown method: ", method)
    }

    # Truncate CI to [0, 1] to avoid values slightly outside due to rounding.
    ci[1] <- max(0, ci[1])
    ci[2] <- min(1, ci[2])

    list(
      p_est  = est,
      ci_low = ci[1],
      ci_high = ci[2]
    )
  }

  # -----------------------------
  # 3. Grid search over (n, x, method)
  # -----------------------------
  #
  # Core idea:
  #   For each n = 1, ..., n_max, we consider only x values around p_hat * n
  #   (a local window) to keep the search feasible.
  #
  #   For each combination and each method, we:
  #     - compute the implied p_est and CI
  #     - compute a distance (score) to the reported (p_hat, ci_low, ci_high)
  #   Then we sort all candidates by that score.
  #
  # This is a discrete parameter search:
  #   parameters = (n, x, method)
  #   objective  = minimize distance between implied and reported statistics.

  results <- list()
  idx <- 1L

  for (n in 1:n_max) {
    # Center of the local search window for x.
    x_center <- round(p_hat * n)

    # Candidate x values within 'window_x' around x_center.
    xs <- unique(x_center + (-window_x):window_x)

    # Restrict to feasible counts [0, n].
    xs <- xs[xs >= 0 & xs <= n]

    # Skip if there are no valid candidate x values (can happen for very small n).
    if (length(xs) == 0L) next

    # For each candidate x and each CI method, compute score.
    for (x in xs) {
      for (method in methods) {

        # Compute CI and point estimate for this combination.
        ci_info <- compute_ci(x = x, n = n, method = method)

        # Extract computed values.
        est_calc    <- ci_info$p_est
        ci_low_calc <- ci_info$ci_low
        ci_high_calc <- ci_info$ci_high

        # Distance / score:
        # We measure how far (in Euclidean distance) the triple
        # (p_est, ci_low, ci_high) is from the reported (p_hat, ci_low, ci_high).
        # This is a simple scalar objective to minimize.
        score <- sqrt(
          (est_calc    - p_hat )^2 +
          (ci_low_calc - ci_low)^2 +
          (ci_high_calc - ci_high)^2
        )

        # Store candidate in results list.
        results[[idx]] <- c(
          method  = method,
          n       = n,
          x       = x,
          p_est   = est_calc,
          ci_low  = ci_low_calc,
          ci_high = ci_high_calc,
          score   = score
        )
        idx <- idx + 1L
      }
    }
  }

  # -----------------------------
  # 4. Assemble and post-process
  # -----------------------------

  if (length(results) == 0L) {
    stop("No candidates found – please check your inputs and n_max/window_x.")
  }

  # Convert list of rows to data.frame.
  res <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)

  # Ensure numeric columns are numeric (they arrive as characters).
  res$n      <- as.integer(res$n)
  res$x      <- as.integer(res$x)
  res$p_est  <- as.numeric(res$p_est)
  res$ci_low <- as.numeric(res$ci_low)
  res$ci_high <- as.numeric(res$ci_high)
  res$score  <- as.numeric(res$score)

  # Sort by score (ascending) so that the best matches come first.
  res <- res[order(res$score), ]

  # Optionally: keep only the top_k best candidates overall.
  if (!is.null(top_k) && nrow(res) > top_k) {
    res <- res[1:top_k, ]
  }

  # Reset row names for clean output.
  rownames(res) <- NULL

  return(res)
}
