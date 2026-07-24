# MAS291 - Statistical Analysis of Online Shopping Behavior
# Full analysis for Tasks 1-5 using base R.

options(stringsAsFactors = FALSE, scipen = 999)

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

data_candidates <- c(
  "online_shoppers_intention.csv",
  "../../online+shoppers+purchasing+intention+dataset/online_shoppers_intention.csv",
  "D:/prjcomputer/online+shoppers+purchasing+intention+dataset/online_shoppers_intention.csv"
)
data_path <- data_candidates[file.exists(data_candidates)][1]
if (is.na(data_path)) stop("Cannot find online_shoppers_intention.csv. See README_VI.md.")

shop <- read.csv(data_path, check.names = FALSE)
shop$Revenue_int <- as.integer(shop$Revenue)
shop$Weekend_int <- as.integer(shop$Weekend)
shop$Returning_int <- as.integer(shop$VisitorType == "Returning_Visitor")
shop$Session_ID <- seq_len(nrow(shop))

alpha <- 0.05
numeric_variables <- c(
  "Administrative", "Administrative_Duration", "Informational",
  "Informational_Duration", "ProductRelated", "ProductRelated_Duration",
  "BounceRates", "ExitRates", "PageValues", "SpecialDay"
)
variables <- numeric_variables

excel_skew <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  s <- sd(x)
  if (n < 3 || s == 0) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / s)^3)
}

iqr_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75), type = 7, na.rm = TRUE)
  spread <- q[2] - q[1]
  sum(x < q[1] - 1.5 * spread | x > q[2] + 1.5 * spread, na.rm = TRUE)
}

fmt <- function(x, digits = 3) format(round(x, digits), nsmall = digits, big.mark = ",")

# -----------------------------------------------------------------------------
# TASK 1 - EXPLORATORY DATA ANALYSIS
# -----------------------------------------------------------------------------

original_columns <- setdiff(names(shop), c("Revenue_int", "Weekend_int", "Returning_int", "Session_ID"))
duplicate_rows <- sum(duplicated(shop[original_columns]))

structure_quality <- data.frame(
  Measure = c("Rows", "Original variables", "Missing cells", "Duplicate rows",
              "Purchase sessions", "Non-purchase sessions", "Minimum group size"),
  Result = c(
    nrow(shop), length(original_columns), sum(is.na(shop[original_columns])), duplicate_rows,
    sum(shop$Revenue_int == 1), sum(shop$Revenue_int == 0),
    min(table(shop$Revenue_int))
  ),
  Interpretation = c(
    "One row per online session",
    "Original UCI variables, including Revenue",
    "No missing values expected",
    "Exact duplicate feature vectors; retained for analysis",
    "Revenue = TRUE", "Revenue = FALSE", "Exceeds required 500 observations"
  )
)

summary_rows <- list()
for (group_name in c("Purchase", "No purchase")) {
  criterion <- if (group_name == "Purchase") 1 else 0
  group_data <- shop[shop$Revenue_int == criterion, , drop = FALSE]
  for (variable in variables) {
    x <- group_data[[variable]]
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      Group = group_name,
      Variable = variable,
      n = length(x),
      Mean = mean(x),
      Median = median(x),
      SD = sd(x),
      Min = min(x),
      Q1 = unname(quantile(x, 0.25, type = 7)),
      Q3 = unname(quantile(x, 0.75, type = 7)),
      Max = max(x),
      Skewness = excel_skew(x),
      IQR_outliers = iqr_outliers(x)
    )
  }
}
descriptive_statistics <- do.call(rbind, summary_rows)

write.csv(structure_quality, "results/task1_structure_quality.csv", row.names = FALSE)
write.csv(descriptive_statistics, "results/task1_descriptive_statistics.csv", row.names = FALSE)

# Full dataset evidence table used by the HTML report.
evidence_columns <- c(
  "Session_ID", "Administrative", "Administrative_Duration", "Informational",
  "Informational_Duration", "ProductRelated", "ProductRelated_Duration",
  "BounceRates", "ExitRates", "PageValues", "SpecialDay", "Month",
  "OperatingSystems", "Browser", "Region", "TrafficType", "VisitorType",
  "Weekend", "Revenue", "Revenue_int", "Weekend_int", "Returning_int"
)
dataset_full <- shop[, evidence_columns]
write.csv(dataset_full, "results/dataset_full_12330.csv", row.names = FALSE)

group_label <- ifelse(shop$Revenue_int == 1, "Purchase", "No purchase")
group_colors <- c("No purchase" = "#4F81BD", "Purchase" = "#C0504D")

excel_plot_theme <- function() {
  par(bg = "white", fg = "#404040", col.axis = "#404040", col.lab = "#404040",
      family = "sans", mar = c(5.5, 5, 3.5, 1.5), las = 1)
}

excel_kurt <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  s <- sd(x)
  if (n < 4 || s == 0) return(NA_real_)
  z4 <- sum(((x - mean(x)) / s)^4)
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * z4 -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

sample_mode <- function(x) {
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

png("figures/task1_duration_histogram.png", width = 1200, height = 650, res = 130)
excel_plot_theme()
par(mar = c(7.0, 6.5, 4.5, 1.5), las = 1)

duration <- shop$ProductRelated_Duration
regular_breaks <- seq(0, 6000, by = 300)
regular_counts <- hist(
  duration[duration <= 6000],
  breaks = regular_breaks,
  plot = FALSE,
  right = FALSE,
  include.lowest = TRUE
)$counts
overflow_count <- sum(duration > 6000)
histogram_counts <- c(regular_counts, overflow_count)
histogram_labels <- c(
  paste0(
    format(regular_breaks[-length(regular_breaks)], big.mark = ","),
    "\u2013",
    format(regular_breaks[-1], big.mark = ",")
  ),
  ">6,000"
)

hist_bp <- barplot(
  histogram_counts,
  space = 0,
  col = "#4F81BD",
  border = "white",
  names.arg = rep("", length(histogram_labels)),
  las = 2,
  ylab = "Frequency (number of sessions)",
  xlab = "",
  main = "Histogram of product-page duration (300-second bins; last bar >6,000)"
)
histogram_tick_index <- unique(c(seq(1, length(hist_bp), by = 3), length(hist_bp)))
axis(1, at = hist_bp[histogram_tick_index],
     labels = histogram_labels[histogram_tick_index],
     las = 2, tick = FALSE, line = -0.5)
mtext("Product-page duration interval (seconds)", side = 1, line = 5.8)
dev.off()

png("figures/task1_duration_boxplot.png", width = 1000, height = 700, res = 130)
excel_plot_theme()
par(mar = c(5.5, 7, 3.5, 1.5))
boxplot(ProductRelated_Duration ~ group_label, data = shop,
        col = unname(group_colors), outline = FALSE, axes = FALSE,
        ylim = c(0, 6000),
        boxlwd = 2, medlwd = 4, whisklwd = 2, staplelwd = 2, staplewex = 0.65,
        ylab = "", xlab = "Conversion outcome",
        main = "Product-page duration boxplot by conversion outcome")
mtext("Product-page duration (seconds)", side = 2, line = 4.2, las = 0)
axis(1, at = 1:2, labels = c("No purchase", "Purchase"), tick = FALSE)
axis(2, at = seq(0, 6000, 1000), labels = format(seq(0, 6000, 1000), big.mark = ","))
abline(h = seq(0, 6000, 1000), col = "#D9E1F2", lwd = 1)
representative_outliers <- function(x, maximum = 6000, count = 6) {
  values <- sort(boxplot.stats(x)$out)
  values <- values[values <= maximum]
  if (length(values) <= count) return(values)
  unique(as.numeric(quantile(values, probs = seq(0.15, 0.95, length.out = count), names = FALSE)))
}
np_out <- representative_outliers(shop$ProductRelated_Duration[shop$Revenue_int == 0])
p_out <- representative_outliers(shop$ProductRelated_Duration[shop$Revenue_int == 1])
points(rep(1, length(np_out)), np_out, pch = 21, bg = "white", col = "#404040", cex = 0.8)
points(rep(2, length(p_out)), p_out, pch = 21, bg = "white", col = "#404040", cex = 0.8)
dev.off()

calendar_months <- c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_conversion <- aggregate(Revenue_int ~ Month, data = shop, FUN = mean)
month_conversion$Month <- factor(month_conversion$Month, levels = calendar_months)
month_conversion <- month_conversion[order(month_conversion$Month), ]
write.csv(month_conversion, "results/task1_conversion_by_month.csv", row.names = FALSE)

png("figures/task1_conversion_by_month.png", width = 1000, height = 650, res = 130)
excel_plot_theme()
month_bp <- barplot(month_conversion$Revenue_int, names.arg = month_conversion$Month,
        col = "#70AD47", border = NA, ylim = c(0, 0.29), axes = FALSE,
        ylab = "Purchase sessions / all sessions in month",
        main = "Purchase conversion rate within each month")
axis(1, at = month_bp, labels = month_conversion$Month, tick = FALSE)
axis(2, at = seq(0, 0.25, 0.05), labels = paste0(seq(0, 25, 5), "%"))
abline(h = seq(0, 0.25, 0.05), col = "#E2F0D9", lwd = 1)
text(month_bp, month_conversion$Revenue_int + 0.012,
     labels = sprintf("%.1f%%", 100 * month_conversion$Revenue_int), cex = 0.8)
dev.off()

visitor_conversion <- aggregate(Revenue_int ~ VisitorType, data = shop, FUN = mean)
visitor_levels <- c("New_Visitor", "Other", "Returning_Visitor")
visitor_conversion$VisitorType <- factor(visitor_conversion$VisitorType, levels = visitor_levels)
visitor_conversion <- visitor_conversion[order(visitor_conversion$VisitorType), ]
visitor_outcomes <- data.frame(
  VisitorType = as.character(visitor_conversion$VisitorType),
  No_purchase = 1 - visitor_conversion$Revenue_int,
  Purchase = visitor_conversion$Revenue_int
)
write.csv(visitor_outcomes, "results/task1_conversion_by_visitor.csv", row.names = FALSE)

png("figures/task1_conversion_by_visitor.png", width = 1000, height = 650, res = 130)
excel_plot_theme()
par(mar = c(6.5, 5, 3.5, 1.5))
visitor_rates <- rbind(visitor_outcomes$No_purchase, visitor_outcomes$Purchase)
visitor_bp <- barplot(visitor_rates, beside = TRUE,
        names.arg = visitor_outcomes$VisitorType,
        col = c("#4F81BD", "#C0504D"), border = NA,
        ylim = c(0, 1), axes = FALSE, ylab = "Proportion within visitor type",
        main = "Purchase outcome within visitor type")
axis(1, at = colMeans(visitor_bp), labels = visitor_outcomes$VisitorType, tick = FALSE)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"))
abline(h = seq(0, 1, 0.2), col = "#D9D9D9", lwd = 1, lty = 2)
legend("bottom", inset = c(0, -0.25), xpd = TRUE, horiz = TRUE,
       legend = c("No purchase", "Purchase"),
       fill = c("#4F81BD", "#C0504D"), bty = "n")
dev.off()

# -----------------------------------------------------------------------------
# TASK 2 - INFERENCE FOR ONE POPULATION
# -----------------------------------------------------------------------------

duration <- shop$ProductRelated_Duration
mean_test <- t.test(duration, mu = 1200, alternative = "two.sided", conf.level = 0.95)
task2_mean <- data.frame(
  Measure = c("n", "Sample mean", "Sample SD", "Hypothesized mean", "t statistic",
              "Degrees of freedom", "Two-sided p-value", "95% CI lower", "95% CI upper", "Decision"),
  Value = c(length(duration), mean(duration), sd(duration), 1200,
            unname(mean_test$statistic), unname(mean_test$parameter), mean_test$p.value,
            mean_test$conf.int[1], mean_test$conf.int[2],
            ifelse(mean_test$p.value < alpha, "Reject H0", "Fail to reject H0"))
)

n <- nrow(shop)
x <- sum(shop$Revenue_int)
p_hat <- x / n
p0 <- 0.15
z_one <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
p_one <- 2 * pnorm(-abs(z_one))
z_crit <- qnorm(1 - alpha / 2)
ci_prop <- p_hat + c(-1, 1) * z_crit * sqrt(p_hat * (1 - p_hat) / n)
task2_proportion <- data.frame(
  Measure = c("Sessions n", "Purchases x", "Sample proportion", "Hypothesized proportion",
              "z statistic", "Two-sided p-value", "95% CI lower", "95% CI upper", "Decision"),
  Value = c(n, x, p_hat, p0, z_one, p_one, ci_prop[1], ci_prop[2],
            ifelse(p_one < alpha, "Reject H0", "Fail to reject H0"))
)
write.csv(task2_mean, "results/task2_one_mean.csv", row.names = FALSE)
write.csv(task2_proportion, "results/task2_one_proportion.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# TASK 3 - COMPARISON OF TWO POPULATIONS
# -----------------------------------------------------------------------------

purchase_duration <- shop$ProductRelated_Duration[shop$Revenue_int == 1]
nonpurchase_duration <- shop$ProductRelated_Duration[shop$Revenue_int == 0]
two_mean_test <- t.test(purchase_duration, nonpurchase_duration,
                        alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

task3_means <- data.frame(
  Measure = c("Purchase n", "Purchase mean", "Purchase SD", "No-purchase n",
              "No-purchase mean", "No-purchase SD", "Difference", "Welch t", "Welch df",
              "Two-sided p-value", "95% CI lower", "95% CI upper", "Decision"),
  Value = c(length(purchase_duration), mean(purchase_duration), sd(purchase_duration),
            length(nonpurchase_duration), mean(nonpurchase_duration), sd(nonpurchase_duration),
            unname(two_mean_test$estimate[1] - two_mean_test$estimate[2]), unname(two_mean_test$statistic),
            unname(two_mean_test$parameter), two_mean_test$p.value,
            two_mean_test$conf.int[1], two_mean_test$conf.int[2],
            ifelse(two_mean_test$p.value < alpha, "Reject H0", "Fail to reject H0"))
)

n1 <- sum(shop$Revenue_int == 1)
n2 <- sum(shop$Revenue_int == 0)
x1 <- sum(shop$Returning_int[shop$Revenue_int == 1])
x2 <- sum(shop$Returning_int[shop$Revenue_int == 0])
p1 <- x1 / n1
p2 <- x2 / n2
diff_p <- p1 - p2
p_pool <- (x1 + x2) / (n1 + n2)
z_two <- diff_p / sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
p_two <- 2 * pnorm(-abs(z_two))
se_unpooled <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
ci_two_prop <- diff_p + c(-1, 1) * z_crit * se_unpooled

task3_proportions <- data.frame(
  Measure = c("Purchase returning count", "Purchase n", "Purchase proportion",
              "No-purchase returning count", "No-purchase n", "No-purchase proportion",
              "Difference", "Pooled proportion", "z statistic", "Two-sided p-value",
              "95% CI lower", "95% CI upper", "Decision"),
  Value = c(x1, n1, p1, x2, n2, p2, diff_p, p_pool, z_two, p_two,
            ci_two_prop[1], ci_two_prop[2], ifelse(p_two < alpha, "Reject H0", "Fail to reject H0"))
)
write.csv(task3_means, "results/task3_two_means.csv", row.names = FALSE)
write.csv(task3_proportions, "results/task3_two_proportions.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# TASK 4 - SIMPLE LINEAR REGRESSION
# -----------------------------------------------------------------------------

model <- lm(ProductRelated_Duration ~ ProductRelated, data = shop)
model_summary <- summary(model)
coef_table <- model_summary$coefficients
r_value <- cor(shop$ProductRelated, shop$ProductRelated_Duration)
r_squared <- model_summary$r.squared
slope_ci <- confint(model, "ProductRelated", level = 0.95)
slope_p <- coef_table["ProductRelated", "Pr(>|t|)"]
slope_p_display <- if (slope_p < 0.001) "< 0.001" else sprintf("%.4f", slope_p)
residual_df <- df.residual(model)
residuals_model <- residuals(model)
sse <- sum(residuals_model^2)
mse <- sse / residual_df
sxx <- sum((shop$ProductRelated - mean(shop$ProductRelated))^2)
t_critical <- qt(1 - alpha / 2, df = residual_df)

task4_model <- data.frame(
  Measure = c("Observations n", "Correlation r", "R squared", "Intercept b0", "Slope b1",
              "Sxx", "SSE", "Residual degrees of freedom", "MSE",
              "Residual standard error", "Slope standard error", "Slope t statistic", "Two-sided t critical",
              "Slope p-value", "Slope 95% lower", "Slope 95% upper", "Decision"),
  Value = c(nrow(shop), r_value, r_squared, coef(model)[1], coef(model)[2],
            sxx, sse, residual_df, mse,
            model_summary$sigma, coef_table["ProductRelated", "Std. Error"],
            coef_table["ProductRelated", "t value"], t_critical, slope_p_display,
            slope_ci[1], slope_ci[2],
            ifelse(slope_p < alpha, "Reject H0", "Fail to reject H0"))
)

prediction_x <- data.frame(ProductRelated = c(10, 25, 50, 100, 200))
prediction_ci <- as.data.frame(predict(model, newdata = prediction_x, interval = "confidence"))
prediction_table <- data.frame(
  Pages_viewed = prediction_x$ProductRelated,
  Predicted_mean_seconds = prediction_ci$fit,
  Mean_CI_95_lower = prediction_ci$lwr,
  Mean_CI_95_upper = prediction_ci$upr,
  Predicted_mean_minutes = prediction_ci$fit / 60
)
write.csv(task4_model, "results/task4_regression_model.csv", row.names = FALSE)
write.csv(prediction_table, "results/task4_predictions.csv", row.names = FALSE)

png("figures/task4_scatter_regression.png", width = 1100, height = 750, res = 130)
plot(shop$ProductRelated, shop$ProductRelated_Duration,
     pch = 16, cex = 0.35, col = rgb(0.20, 0.45, 0.75, 0.25),
     xlim = c(0, 200), ylim = c(0, 12000),
     xlab = "Number of product-related pages viewed",
     ylab = "Product-related duration (seconds)",
     main = "Product pages viewed and total product-page duration",
     sub = "Display zoom: 0-200 pages and 0-12,000 seconds; model uses all observations")
abline(model, col = "#C0504D", lwd = 3)
legend("topleft", legend = sprintf("Y-hat = %.3f + %.3fX; r = %.3f; R-squared = %.3f",
                                    coef(model)[1], coef(model)[2], r_value, r_squared),
       bty = "n", text.col = "#C0504D")
dev.off()

# -----------------------------------------------------------------------------
# TASK 5 - FINAL INTERPRETATION
# -----------------------------------------------------------------------------

duration_difference <- mean(purchase_duration) - mean(nonpurchase_duration)
key_findings <- data.frame(
  Analysis = c("EDA", "One mean", "One proportion", "Two means", "Two proportions", "Regression"),
  Finding = c(
    sprintf("%s purchase and %s non-purchase sessions; no missing values; right-skewed behavioral measures with high-end outliers.", n1, n2),
    sprintf("Mean duration %.1f seconds; 95%% CI %.1f to %.1f; p = %.4f against 1,200 seconds.", mean(duration), mean_test$conf.int[1], mean_test$conf.int[2], mean_test$p.value),
    sprintf("Conversion %.2f%%; 95%% CI %.2f%% to %.2f%%; p = %.4f against 15%%.", 100*p_hat, 100*ci_prop[1], 100*ci_prop[2], p_one),
    sprintf("Buyers spend %.1f seconds longer on product pages; 95%% CI %.1f to %.1f; p < .001.", duration_difference, two_mean_test$conf.int[1], two_mean_test$conf.int[2]),
    sprintf("Returning-visitor share difference %.1f percentage points; 95%% CI %.1f to %.1f; p < .001.", 100*diff_p, 100*ci_two_prop[1], 100*ci_two_prop[2]),
    sprintf("r = %.3f, R-squared = %.1f%%, Y-hat = %.2f + %.2fX; slope p < .001.", r_value, 100*r_squared, coef(model)[1], coef(model)[2])
  )
)

implications <- data.frame(
  Number = 1:5,
  Conclusion = c(
    "Deeper product exploration is strongly associated with successful conversion.",
    "High bounce and exit behavior is concentrated more strongly among non-purchase sessions.",
    "Product-page count predicts engagement time but does not establish purchase causation.",
    "Visitor-type conversion rates should be examined before targeting returning versus new visitors.",
    "A/B tests are needed to establish causality; this observational dataset supports association and inference."
  )
)
write.csv(key_findings, "results/task5_key_findings.csv", row.names = FALSE)
write.csv(implications, "results/task5_implications.csv", row.names = FALSE)

cat("\nMAS291 R analysis completed successfully.\n")
cat("Results folder:", normalizePath("results"), "\n")
cat("Figures folder:", normalizePath("figures"), "\n")
cat(sprintf("Key check: n = %d; purchases = %d; mean duration = %.6f; r = %.6f\n",
            nrow(shop), sum(shop$Revenue_int), mean(duration), r_value))
