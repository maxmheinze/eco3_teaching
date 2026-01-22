library(haven)
library(dplyr)

if (!file.exists("NEW7080.dta")) {
  if (!file.exists("NEW7080_1.rar"))
    download.file("https://economics.mit.edu/sites/default/files/inline-files/NEW7080_1.rar",
                  "NEW7080_1.rar", mode = "wb")
  system("unrar x -y NEW7080_1.rar", ignore.stdout = TRUE)
}

df <- read_dta("NEW7080.dta")

nm <- c("v4"="EDUC","v9"="LWKLYWGE","v16"="CENSUS","v18"="QOB","v27"="YOB")
for (k in names(nm)) if (k %in% names(df)) names(df)[names(df)==k] <- nm[[k]]

df <- df %>%
  mutate(AGEQ = ifelse(CENSUS == 80, NA, NA),                # placeholder, dropped later
         COHORT = ifelse(YOB >= 30 & YOB <= 39, 30, NA)) %>%
  filter(COHORT == 30)

# Year-of-birth dummies (YR1930–YR1939)
for (y in 1930:1939) {
  df[[paste0("YR", y)]] <- as.integer(df$YOB == (y - 1900))
}

# Quarter-of-birth dummies (QTR1–QTR3; QTR4 base)
for (q in 1:4) df[[paste0("QTR", q)]] <- as.integer(df$QOB == q)

# Interactions QTR1–QTR3 × YR1930–YR1939
for (q in 1:3) for (y in 1930:1939)
  df[[paste0("QTR", q, "_", y)]] <- df[[paste0("QTR", q)]] * df[[paste0("YR", y)]]

keep <- c("LWKLYWGE","EDUC",
          paste0("YR",1930:1939),
          paste0("QTR",1:3),
          unlist(lapply(1:3, function(q) paste0("QTR",q,"_",1930:1939))))
df <- df[keep]

write.csv(df, "angrist1991.csv", row.names = FALSE)
