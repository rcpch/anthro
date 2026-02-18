#' Calculate measurements from z-scores (inverse LMS)
#'
#' @description
#'
#' Given a requested z-score and the child's sex/age, return the corresponding
#' anthropometric measurement (inverse of the LMS z-score calculation). This
#' implements the LMS inversion for the WHO growth standards and mirrors the
#' adjusted handling used for extreme z-scores in `compute_zscore_adjusted()`.
#'
#' @param sex A numeric or text variable containing gender information.
#' @param age A numeric variable containing age information; if `is_age_in_month` is
#'   TRUE then age is interpreted as months and converted to days.
#' @param is_age_in_month Logical; whether `age` is in months (default FALSE).
#' @param requested_z Numeric vector of requested z-scores to invert.
#' @param measurement_method Character: one of `"length"`, `"weight"`, `"bmi"`, `"headc"`.
#' @param measurement_precision Integer digits to round the returned measurement to (default 2L).
#' @param correct_extreme Logical; if TRUE apply the same linear extreme-value correction
#'   used in `compute_zscore_adjusted()` for |z| > 3. Defaults to FALSE.
#'
#' @return A data.frame with a single column named according to the requested
#' measurement (e.g. `lenhei`, `weight`, `bmi`, `headc`) containing the computed
#' measurement values (or NA where not computable).
#'
#' @examples
#' anthro_measurements(sex = 1, age = 365, requested_z = 0, measurement_method = "length")
#' @include anthro-package.R
#' @include assertions.R
#' @export
anthro_measurements <- function(
  sex,
  age = NA_real_,
  is_age_in_month = FALSE,
  requested_z = NA_real_,
  measurement_method = c("length", "weight", "bmi", "headc"),
  measurement_precision = 2L,
  correct_extreme = FALSE
) {
  assert_logical(is_age_in_month)
  assert_length(is_age_in_month, 1L)
  assert_character_or_numeric(sex)
  assert_numeric(age)
  assert_numeric(requested_z)

  measurement_method <- match.arg(measurement_method)

  assert_logical(correct_extreme)
  assert_length(correct_extreme, 1L)

  # ensure measurement_precision is an integer >= 0
  assert_numeric(measurement_precision)
  measurement_precision <- as.integer(measurement_precision)
  stopifnot(!is.na(measurement_precision) && measurement_precision >= 0)

  # recycle inputs to common length
  n <- max(length(sex), length(age), length(requested_z))
  sex <- rep_len(sex, n)
  age <- rep_len(age, n)
  requested_z <- rep_len(requested_z, n)

  # clean sex and age
  csex <- standardize_sex_var(sex)
  age_in_days <- age_to_days(age, is_age_in_month = is_age_in_month)
  age_in_months <- age_to_months(age, is_age_in_month = is_age_in_month)

  # select growth standard table and output column name
  if (measurement_method %in% c("length", "len", "height", "lenhei")) {
    gs <- growthstandards_lenanthro
    out_name <- "lenhei"
    allowed_age_range <- c(0, 1856)
  } else if (measurement_method %in% c("weight", "wei")) {
    gs <- growthstandards_weianthro
    out_name <- "weight"
    allowed_age_range <- c(0, 1856)
  } else if (measurement_method %in% c("bmi")) {
    gs <- growthstandards_bmianthro
    out_name <- "bmi"
    allowed_age_range <- c(0, 1856)
  } else if (measurement_method %in% c("headc", "headcirc")) {
    gs <- growthstandards_hcanthro
    out_name <- "headc"
    allowed_age_range <- c(0, 1856)
  } else {
    stop("Unsupported measurement_method. Use one of: length, weight, bmi, headc")
  }

  # prepare input and merge with growth standards (by age and sex)
  input_df <- data.frame(measure = requested_z, age_in_days = as.integer(round_up(age_in_days)), sex = csex, ordering = seq_len(n))
  merged_df <- merge(
    input_df,
    gs,
    by.x = c("age_in_days", "sex"),
    by.y = c("age", "sex"),
    all.x = TRUE,
    sort = FALSE
  )
  merged_df <- merged_df[order(merged_df$ordering), , drop = FALSE]

  m <- merged_df[["m"]]
  l <- merged_df[["l"]]
  s <- merged_df[["s"]]
  z <- merged_df[["measure"]]

  # helper to compute the SD-based values handling l == 0
  calc_sd <- function(mv, lv, sv, sd) {
    ifelse(lv == 0, mv * exp(sv * sd), mv * ((1 + lv * sv * sd)^(1 / lv)))
  }

  y <- rep(NA_real_, n)

  # valid LMS rows
  valid_idx <- !is.na(m) & !is.na(l) & !is.na(s) & !is.na(merged_df[["age_in_days"]])

  # non-extreme inversion (|z| <= 3)
  non_extreme <- valid_idx & (z <= 3 & z >= -3)
  if (any(non_extreme)) {
    lv <- l[non_extreme]
    mv <- m[non_extreme]
    sv <- s[non_extreme]
    zv <- z[non_extreme]
    y[non_extreme] <- ifelse(lv == 0, mv * exp(sv * zv), mv * ((1 + lv * sv * zv)^(1 / lv)))
  }

  # handle extremes only when requested
  if (isTRUE(correct_extreme)) {
    # positive extreme (z > 3): linear extrapolation in SD units used by compute_zscore_adjusted
    pos_ext <- valid_idx & (z > 3)
    if (any(pos_ext)) {
      mv <- m[pos_ext]
      lv <- l[pos_ext]
      sv <- s[pos_ext]
      SD3pos <- calc_sd(mv, lv, sv, 3)
      SD2pos <- calc_sd(mv, lv, sv, 2)
      SD23pos <- SD3pos - SD2pos
      y[pos_ext] <- SD3pos + (z[pos_ext] - 3) * SD23pos
    }

    # negative extreme (z < -3)
    neg_ext <- valid_idx & (z < -3)
    if (any(neg_ext)) {
      mv <- m[neg_ext]
      lv <- l[neg_ext]
      sv <- s[neg_ext]
      SD3neg <- calc_sd(mv, lv, sv, -3)
      SD2neg <- calc_sd(mv, lv, sv, -2)
      SD23neg <- SD2neg - SD3neg
      y[neg_ext] <- SD3neg + (z[neg_ext] + 3) * SD23neg
    }
  }

  # enforce allowed age and under-5 constraint
  valid_age <- age_in_months < 60
  valid_final <- valid_idx & !is.na(merged_df[["age_in_days"]]) & merged_df[["age_in_days"]] >= allowed_age_range[1] & merged_df[["age_in_days"]] <= allowed_age_range[2] & valid_age
  y[!valid_final] <- NA_real_

  # round
  y <- round(y, digits = measurement_precision)

  out <- as.data.frame(matrix(nrow = n, ncol = 0))
  out[[out_name]] <- y
  out

}