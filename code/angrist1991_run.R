library(AER)
library(dplyr)
library(readr)
library(stargazer)

df <- read_csv("angrist1991.csv", show_col_types = FALSE)

yr_regs <- paste(paste0("YR", 1930:1938), collapse = " + ")
insts   <- c(unlist(lapply(1:3, function(q) paste0("QTR", q, "_", 1930:1939))),
             paste0("YR", 1930:1938))
inst_formula <- paste(insts, collapse = " + ")

f1 <- as.formula(paste("LWKLYWGE ~ EDUC +", yr_regs))
f2 <- as.formula(paste("LWKLYWGE ~ EDUC +", yr_regs, "|", yr_regs, "+", inst_formula))

ols <- lm(f1, data = df)
iv  <- ivreg(f2, data = df)

stargazer(ols, iv,
          type = "text",
          keep = "EDUC",
          dep.var.labels = "log(weekly wage)",
          digits = 4,
          omit.stat = c("f","ser","adj.rsq"),
          title = "Angrist & Krueger (1991) — Table V — Columns (1) and (2)"
)

cat("\nFirst-stage F-test (weak instruments test):\n")
print(summary(iv, diagnostics = TRUE)$diagnostics["Weak instruments", , drop = FALSE])

cat("\nSargan's J test (overidentification test):\n")
diag_iv <- summary(iv, diagnostics = TRUE)$diagnostics
print(diag_iv[grep("Sargan", rownames(diag_iv)), , drop = FALSE])

cat("\nDurbin–Wu–Hausman test (endogeneity of EDUC):\n")
print(diag_iv[grep("Wu-Hausman", rownames(diag_iv)), , drop = FALSE])
