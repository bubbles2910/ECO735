library(dplyr)
library(stringr)
library(lmtest)
library(sandwich)

raw <- read.csv("namelist_old.csv")

dat <- raw %>%
  transmute(
    year = as.numeric(str_extract(visit, "\\d{4}")),
    obs, prs, pupid, sch98v1, wgrp,
    std98v1, elg,
    female = sex == 0 & !is.na(sex),
    male   = sex == 1 & !is.na(sex),
    dropout = is.na(std98v1) | std98v1 > 8
  )

make_line <- function(z, cond, yr, panel, tag) {
  
  zf <- z[z$year == yr & z$obs == 1, ]
  zf <- zf[eval(substitute(cond), zf, parent.frame()), ]
  
  agg <- zf %>%
    group_by(sch98v1, wgrp) %>%
    summarise(r = sum(prs, na.rm=TRUE)/sum(obs, na.rm=TRUE),
              w = length(unique(pupid)),
              .groups="drop") %>%
    filter(!is.na(r))
  
  if (nrow(agg) < 2) {
    return(data.frame(Subgroup=tag, A=NA, B=NA, C=NA, D=NA, E=NA))
  }
  
  means <- agg %>%
    group_by(wgrp) %>%
    summarise(m = weighted.mean(r, w), .groups="drop")
  
  vals <- setNames(rep(NA,3), c("1","2","3"))
  vals[as.character(means$wgrp)] <- sprintf("%.3f", means$m)
  

  d1 <- if (panel == "A") agg else agg[agg$wgrp %in% c(1,3), ]
  d1$g1 <- as.integer(d1$wgrp == 1)
  fit1 <- lm(r ~ g1, data=d1, weights=w)
  t1 <- coeftest(fit1, vcov = vcovHC(fit1, type="HC1"))[2,]

  d2 <- agg[agg$wgrp %in% c(2,3), ]
  d2$g2 <- as.integer(d2$wgrp == 2)
  fit2 <- lm(r ~ g2, data=d2, weights=w)
  t2 <- coeftest(fit2, vcov = vcovHC(fit2, type="HC1"))[2,]
  
  starify <- function(est, p, se) {
    s <- if (is.na(p)) "" else if (p < .01) "***" else if (p < .05) "**" else if (p < .1) "*" else ""
    paste0(sprintf("%.3f", est), s, " (", sprintf("%.3f", se), ")")
  }
  
  data.frame(
    Subgroup = tag,
    A = vals["1"],
    B = vals["2"],
    C = vals["3"],
    D = starify(t1["Estimate"], t1["Pr(>|t|)"], t1["Std. Error"]),
    E = starify(t2["Estimate"], t2["Pr(>|t|)"], t2["Std. Error"])
  )
}


specs_A <- list(
  list(cond = quote(elg == 1), label = "Girls <13 years, and all boys"),
  list(cond = quote(female & elg == 0), label = "Girls >=13 years"),
  list(cond = quote(elg == 1 & std98v1 %in% 0:2), label = "Preschool, Gr 1, Gr 2 in early 1998"),
  list(cond = quote(elg == 1 & std98v1 %in% 3:5), label = "Gr 3, Gr 4, Gr 5 in early 1998"),
  list(cond = quote(elg == 1 & std98v1 %in% 6:8), label = "Gr 6, Gr 7, Gr 8 in early 1998"),
  list(cond = quote(elg == 1 & dropout), label = "Recorded as 'dropped out' in early 1998"),
  list(cond = quote(female), label = "Females"),
  list(cond = quote(male), label = "Males")
)

panel_A <- do.call(rbind,
                   lapply(specs_A, function(s)
                     make_line(dat, eval(s$cond), 1998, "A", s$label))
)

colnames(panel_A) <- c("Subgroup","Group 1","Group 2","Group 3",
                       "Grp 1 - (Grp 2&3)","Grp 2 - Grp 3")

print(panel_A)

specs_B <- specs_A
specs_B[[2]]$label <- "Girls >=14 years"

panel_B <- do.call(rbind,
                   lapply(specs_B, function(s)
                     make_line(dat, eval(s$cond), 1999, "B", s$label))
)

colnames(panel_B) <- c("Subgroup","Group 1","Group 2","Group 3",
                       "Grp 1 - Grp 3","Grp 2 - Grp 3")

print(panel_B)