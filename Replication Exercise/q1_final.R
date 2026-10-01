library(dplyr)
library(knitr)


setwd("C:/Users/palak/Desktop/ECO735/Replication Exercise/Replication Exercise")
list.files()

school_info  <- read.csv("schoolvar.csv")
pupil_survey <- read.csv("pupq.csv")
names_old    <- read.csv("namelist_old.csv")


combined <- pupil_survey |>
  left_join(school_info, by = c("schid_98_2" = "schid"))


combined <- combined |>
  mutate(
    male       = sex_98_9,
    eligible   = if_else(sex_98_9 == 0 & age_98_18 > 12, 0L, 1L),
    grade_prog = std_98_3 - (age_98_18 - 6),
    yob        = yearborn_98_19 + 1900,
    attend     = 1- (absdays_98_6/20),
    latrine    = havelatr_98_33,
    livestock  = if_else(
      coalesce(cows_98_23,  0) > 0 |
        coalesce(goats_98_24, 0) > 0 |
        coalesce(pigs_98_26,  0) > 0 |
        coalesce(sheep_98_25, 0) > 0,
      1L, 0L),
    bloodst    = bloodst_98_58,
    sick       = if_else(fallsick_98_37 == "Often", 1L, 0L),
    malaria    = malaria_98_48,
    clean      = if_else(clean_98_15 == "Clean", 1L, 0L)
  )


sig_stars <- function(p) {
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}


summarise_by_group <- function(varname, df = combined) {
  
  vals   <- df[[varname]]
  groups <- df$wgrp
  
  g1_mean <- mean(vals[groups == 1], na.rm = TRUE)
  g2_mean <- mean(vals[groups == 2], na.rm = TRUE)
  g3_mean <- mean(vals[groups == 3], na.rm = TRUE)
  
  run_test <- function(ga, gb) {
    va  <- vals[groups == ga]
    vb  <- vals[groups == gb]
    
    # Explicit t-test for equality of means (Welch t-test)
    res <- t.test(va, vb, var.equal = FALSE)
    
    list(
      diff = mean(va, na.rm = TRUE) - mean(vb, na.rm = TRUE),
      se   = res$stderr,
      p    = res$p.value
    )
  }
  
  cmp13 <- run_test(1, 3)
  cmp23 <- run_test(2, 3)
  cmp12 <- run_test(1, 2)
  
  fmt_diff <- function(cmp) {
    sprintf("%.3f%s", cmp$diff, sig_stars(cmp$p))
  }
  fmt_se <- function(cmp) sprintf("%.3f", cmp$se)
  fmt_p  <- function(cmp) sprintf("%.3f", cmp$p)
  
  data.frame(
    Variable      = varname,
    Mean_G1       = round(g1_mean, 3),
    Mean_G2       = round(g2_mean, 3),
    Mean_G3       = round(g3_mean, 3),
    Diff_G1_G3    = fmt_diff(cmp13),
    SE_G1_G3      = fmt_se(cmp13),
    p_G1_G3       = fmt_p(cmp13),
    Diff_G2_G3    = fmt_diff(cmp23),
    SE_G2_G3      = fmt_se(cmp23),
    p_G2_G3       = fmt_p(cmp23),
    Diff_G1_G2    = fmt_diff(cmp12),
    SE_G1_G2      = fmt_se(cmp12),
    p_G1_G2       = fmt_p(cmp12),
    stringsAsFactors = FALSE
  )
}

summarise_school <- function(varname) summarise_by_group(varname, df = school_info)


print_panel <- function(panel_df, title) {
  cat("\n", title, "\n", sep = "")
  print(knitr::kable(panel_df, format = "simple", align = "lrrrrrrrrrrrrr"))
  cat("\n*** p<0.01  ** p<0.05  * p<0.10\n")
}

# ── Panel A: All grades (pre-school to grade 8) ───────────────────────────────
panel_a <- bind_rows(lapply(
  c("male", "eligible", "grade_prog", "yob"),
  summarise_by_group
))
print_panel(panel_a, "===== PANEL A: Pre-school to Grade 8 =====")


grades_3to8 <- combined |>
  filter(!is.na(std_98_3), between(std_98_3, 3, 8))

# ── Panel B: Grades 3 to 8 ───────────────────────────────────────────────────
panel_b <- bind_rows(lapply(
  c("attend","latrine","livestock","waz_98","bloodst","sick","malaria","clean"),
  summarise_by_group,
  df = grades_3to8
))
print_panel(panel_b, "===== PANEL B: Grades 3 to 8 =====")


school_info <- school_info |>
  mutate(pop1_36km = pop1_6km_original - pop1_3km_original)

# ── Panel C: School characteristics ──────────────────────────────────────────
panel_c <- bind_rows(lapply(
  c("mk96_s","distlake","pup_pop","latr_pup","z_inf98","pop1_3km_original","pop1_36km"),
  summarise_school
))
print_panel(panel_c, "===== PANEL C: School Characteristics =====")


panel_a$Panel <- "A: Pre-school to Grade 8"
panel_b$Panel <- "B: Grades 3 to 8"
panel_c$Panel <- "C: School Characteristics"

master_df <- bind_rows(panel_a, panel_b, panel_c) |>
  select(Panel, everything())