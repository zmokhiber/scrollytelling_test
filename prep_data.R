library(readr)
library(dplyr)
library(stringr)

raw <- "chart_data"
out <- "data"
dir.create(out, showWarnings = FALSE)

parse_year <- function(x) as.integer(str_extract(x, "^\\d{4}"))
parse_num <- function(x) {
  # For values like "2013: / 243.1%" extract the portion after "/ "
  x <- ifelse(str_detect(x, "\\/"), str_extract(x, "(?<=\\/)\\s*[\\d$.,%-]+"), x)
  as.numeric(str_remove_all(x, "[$,%\\s]"))
}

# --- Figure 1: Middle-class income, actual vs projected (1979-2011) ---
# Some rows have inline labels in one column — parse independently and interpolate
f1 <- read_tsv(file.path(raw, "figure1.tsv"), show_col_types = FALSE)
names(f1) <- c("date", "actual", "projected")
f1_clean <- f1 |>
  mutate(year = parse_year(date),
         actual = parse_num(actual),
         projected = parse_num(projected)) |>
  filter(!is.na(year)) |>
  mutate(
    actual = approx(year[!is.na(actual)], actual[!is.na(actual)], xout = year)$y,
    projected = approx(year[!is.na(projected)], projected[!is.na(projected)], xout = year)$y
  ) |>
  filter(!is.na(actual), !is.na(projected)) |>
  select(year, actual, projected)
write_csv(f1_clean, file.path(out, "fig1.csv"))

# --- Figure 2: Productivity vs compensation (1948-2013) ---
f2 <- read_tsv(file.path(raw, "figure2.tsv"), skip = 2, show_col_types = FALSE)
names(f2) <- c("year_raw", "compensation", "productivity")
f2_clean <- f2 |>
  mutate(year = parse_year(year_raw),
         compensation = parse_num(compensation),
         productivity = parse_num(productivity)) |>
  filter(!is.na(year), year >= 1948, !is.na(compensation), !is.na(productivity)) |>
  select(year, compensation, productivity)
write_csv(f2_clean, file.path(out, "fig2.csv"))

# --- Figure 3: Top 1% vs bottom 90% wages (1979-2013) ---
f3 <- read_tsv(file.path(raw, "figure3.tsv"), skip = 2, show_col_types = FALSE)
names(f3) <- c("year_raw", "top1", "bottom90")
f3_clean <- f3 |>
  mutate(year = parse_year(year_raw),
         top1 = parse_num(top1),
         bottom90 = parse_num(bottom90)) |>
  filter(!is.na(year), year >= 1979, !is.na(top1), !is.na(bottom90)) |>
  select(year, top1, bottom90)
write_csv(f3_clean, file.path(out, "fig3.csv"))

# --- Figure 4: Wages by percentile (1979-2013) ---
f4 <- read_tsv(file.path(raw, "figure4.tsv"), skip = 2, show_col_types = FALSE)
names(f4) <- c("year_raw", "high", "middle", "low")
f4_clean <- f4 |>
  mutate(year = parse_year(year_raw),
         high = parse_num(high),
         middle = parse_num(middle),
         low = parse_num(low)) |>
  filter(!is.na(year), !is.na(high), !is.na(middle), !is.na(low)) |>
  select(year, high, middle, low)
write_csv(f4_clean, file.path(out, "fig4.csv"))

# --- Figure 5: Young college grad wages (1989-2014) ---
f5 <- read_tsv(file.path(raw, "figure5.tsv"), skip = 2, show_col_types = FALSE)
names(f5) <- c("year_raw", "all", "men", "women")
f5_clean <- f5 |>
  mutate(year = parse_year(year_raw),
         all = parse_num(all),
         men = parse_num(men),
         women = parse_num(women)) |>
  filter(!is.na(year), !is.na(all), !is.na(men), !is.na(women)) |>
  select(year, all, men, women)
write_csv(f5_clean, file.path(out, "fig5.csv"))

# --- Figure 6: Employer health insurance for young workers (1989-2012) ---
f6 <- read_tsv(file.path(raw, "figure6.tsv"), skip = 2, show_col_types = FALSE)
names(f6) <- c("year_raw", "high_school", "college")
f6_clean <- f6 |>
  mutate(year = parse_year(year_raw),
         high_school = parse_num(high_school),
         college = parse_num(college)) |>
  filter(!is.na(year), !is.na(high_school), !is.na(college)) |>
  select(year, high_school, college)
write_csv(f6_clean, file.path(out, "fig6.csv"))

# --- Figure 7: CEO-to-worker compensation ratio (1965-2013) ---
f7 <- read_tsv(file.path(raw, "figure7.tsv"), skip = 2, show_col_types = FALSE)
names(f7) <- c("year_raw", "ratio")
f7_clean <- f7 |>
  mutate(year = parse_year(year_raw),
         ratio = parse_num(ratio)) |>
  filter(!is.na(year), !is.na(ratio)) |>
  select(year, ratio)
write_csv(f7_clean, file.path(out, "fig7.csv"))

# --- Figure 8: Minimum wage vs productivity vs hourly wage (1968-2014) ---
f8 <- read_tsv(file.path(raw, "figure8.tsv"), skip = 2, show_col_types = FALSE)
names(f8) <- c("year_raw", "min_wage", "hourly_wage", "productivity")
f8_clean <- f8 |>
  mutate(year = parse_year(year_raw),
         min_wage = parse_num(min_wage),
         hourly_wage = parse_num(hourly_wage),
         productivity = parse_num(productivity)) |>
  filter(!is.na(year), year >= 1968, !is.na(min_wage), !is.na(hourly_wage), !is.na(productivity)) |>
  select(year, min_wage, hourly_wage, productivity)
write_csv(f8_clean, file.path(out, "fig8.csv"))

# --- Figure 9: Union membership vs top 10% income share (1917-2012) ---
f9 <- read_tsv(file.path(raw, "figure9.tsv"), skip = 2, show_col_types = FALSE)
names(f9) <- c("year_raw", "union", "top_income")
f9_clean <- f9 |>
  mutate(year = parse_year(year_raw),
         union = parse_num(union),
         top_income = parse_num(top_income)) |>
  filter(!is.na(year), !is.na(union), !is.na(top_income)) |>
  select(year, union, top_income)
write_csv(f9_clean, file.path(out, "fig9.csv"))

cat("Cleaned data written to", out, "/\n")
for (f in sort(list.files(out, full.names = TRUE))) {
  cat(sprintf("  %s: %d rows\n", basename(f), nrow(read_csv(f, show_col_types = FALSE))))
}
