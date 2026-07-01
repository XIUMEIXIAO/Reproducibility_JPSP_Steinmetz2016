# ==================================================
# Study 1 数据导入与预处理
# ==================================================

# — 0. Packages ———————————————
library(haven)
library(dplyr)
library(bruceR)

# — 1. Import ———————————————
df <- read_sav("3.Rawdata/Study1.sav")

# — 2. Quick overview ———————————
summary(df)


# — 3. Select, rename & type conversion ———————————
df1 <- df %>%
  select(
    ID                            = V1,
    condition                     = condition,
    food_portion_size_1_9         = Q27.0,
    food_calorie_1_9              = Q29,
    today_intake_contribution_1_9 = Q31,
    food_enjoyment_level_1_9      = Q33.0,
    food_consumed_amount_1_4      = Q39,
    perceived_observation_1_9     = Q41,
    remaining_food_weight_g       = Q30,
    gender                        = Q6,
    age                           = Q8,
    English_native                = Q12
  ) %>%
  mutate(
    # factor: categorical
    condition = factor(condition,
      levels = c(1, 2), labels = c("camera", "no_camera")),
    gender = factor(gender,
      levels = c(1, 2, 3), labels = c("Male", "Female", "Other")),
    English_native = factor(English_native,
      levels = c(1, 2), labels = c("Yes", "No")),
    # numeric: Likert scales (food_consumed kept numeric for t-test)
    food_portion_size_1_9         = as.numeric(food_portion_size_1_9),
    food_calorie_1_9              = as.numeric(food_calorie_1_9),
    today_intake_contribution_1_9 = as.numeric(today_intake_contribution_1_9),
    food_enjoyment_level_1_9      = as.numeric(food_enjoyment_level_1_9),
    perceived_observation_1_9     = as.numeric(perceived_observation_1_9),
    food_consumed_amount_1_4      = as.numeric(food_consumed_amount_1_4)
  )


# — 4. Verify ———————————
cat("\n✔ Data imported:", nrow(df1), "rows,", ncol(df1), "columns\n")
glimpse(df1)


# ==================================================
# Study 1 描述统计
# ==================================================

cat("\n\n======= Descriptive Statistics =======\n\n")

# — 5. Group stats ———————————————
desc <- df1 %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    obsv_M   = round(mean(perceived_observation_1_9, na.rm = TRUE), 2),
    obsv_SD  = round(sd(perceived_observation_1_9, na.rm = TRUE), 2),
    portion_M = round(mean(food_portion_size_1_9, na.rm = TRUE), 2),
    portion_SD = round(sd(food_portion_size_1_9, na.rm = TRUE), 2),
    remain_M = round(mean(remaining_food_weight_g, na.rm = TRUE), 2),
    remain_SD = round(sd(remaining_food_weight_g, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(desc)

# — 6. Demographics ———————————————
cat("\n--- Demographics ---\n")
demo <- df1 %>%
  summarise(
    n_total  = n(),
    n_female = sum(gender == "Female"),
    n_male   = sum(gender == "Male"),
    age_M    = round(mean(age, na.rm = TRUE), 2),
    age_SD   = round(sd(age, na.rm = TRUE), 2)
  )
print(demo)


# ==================================================
# Study 1 推断统计
# ==================================================

cat("\n\n======= Inferential Statistics =======\n\n")

# — 10. Independent t-tests ———————————
calc_eta2 <- function(t_val, df) round(t_val^2 / (t_val^2 + df), 3)

vars_ttest <- c("perceived_observation_1_9", "food_portion_size_1_9",
  "remaining_food_weight_g", "food_calorie_1_9",
  "today_intake_contribution_1_9", "food_enjoyment_level_1_9",
  "food_consumed_amount_1_4")

tt_results <- list()
for (v in vars_ttest) {
  cat(sprintf("\n========== %s ==========\n", v))
  TTEST(df1, y = v, x = "condition", var.equal = TRUE)
  # Manually compute η²
  res_tt <- t.test(df1[[v]] ~ df1[["condition"]], var.equal = TRUE)
  eta2 <- calc_eta2(res_tt$statistic, res_tt$parameter)
  cat(sprintf("  η² = %.3f\n\n", eta2))
  # Store for Table 5
  tt_results[[v]] <- list(
    t = round(as.numeric(res_tt$statistic), 2),
    df = as.numeric(res_tt$parameter),
    p = round(res_tt$p.value, 3),
    CI = paste0("[", round(res_tt$conf.int[1], 3), ", ", round(res_tt$conf.int[2], 3), "]"),
    eta2 = eta2
  )
}


# ==================================================
# Study 1 结果输出
# ==================================================

# — Extract values for Table 2 ———————————————
camera_n   <- desc$n[desc$condition == "camera"]
control_n  <- desc$n[desc$condition == "no_camera"]

rep_camera  <- c(desc$obsv_M[desc$condition == "camera"],
                 desc$portion_M[desc$condition == "camera"],
                 desc$remain_M[desc$condition == "camera"])
rep_camera_SD <- c(desc$obsv_SD[desc$condition == "camera"],
                   desc$portion_SD[desc$condition == "camera"],
                   desc$remain_SD[desc$condition == "camera"])
rep_control <- c(desc$obsv_M[desc$condition == "no_camera"],
                 desc$portion_M[desc$condition == "no_camera"],
                 desc$remain_M[desc$condition == "no_camera"])
rep_control_SD <- c(desc$obsv_SD[desc$condition == "no_camera"],
                    desc$portion_SD[desc$condition == "no_camera"],
                    desc$remain_SD[desc$condition == "no_camera"])

orig_camera  <- c(5.51, 4.85, 2.64)
orig_camera_SD <- c(2.27, 1.94, 5.74)
orig_control <- c(2.84, 4.00, 1.51)
orig_control_SD <- c(2.48, 1.53, 5.61)

# — δ & rating for Table 2 ———————————
calc_pe_desc <- function(o, r) {
  if (is.na(o) || o == 0) return(NA)
  round(abs(o - r) / abs(o) * 100, 2)
}
rating_desc <- function(pe) {
  if (is.na(pe)) return("N/A")
  if (pe == 0) return("完全一致")
  if (pe < 10) return("次要偏差")
  return("主要偏差")
}

pe_camera  <- mapply(calc_pe_desc, orig_camera, rep_camera)
pe_control <- mapply(calc_pe_desc, orig_control, rep_control)
rt_camera  <- sapply(pe_camera, rating_desc)
rt_control <- sapply(pe_control, rating_desc)

# — Build Table 2 ———————————————
tbl2_lines <- c(
"",
"### 表 2 描述性统计结果的比较",
"",
"| 变量 | 条件 | 来源 | N | Mean | SD | δ(%) | 评级 |",
"|------|------|------|---|------|-----|------|------|",
sprintf("| perceived_observation | camera | 原研究 | 82 | %.2f | %.2f | - | - |",
        orig_camera[1], orig_camera_SD[1]),
sprintf("| | | 本研究 | %d | %.2f | %.2f | %.2f | %s |",
        camera_n, rep_camera[1], rep_camera_SD[1], pe_camera[1], rt_camera[1]),
sprintf("| | control | 原研究 | 82 | %.2f | %.2f | - | - |",
        orig_control[1], orig_control_SD[1]),
sprintf("| | | 本研究 | %d | %.2f | %.2f | %.2f | %s |",
        control_n, rep_control[1], rep_control_SD[1], pe_control[1], rt_control[1]),
sprintf("| food_portion_size | camera | 原研究 | 82 | %.2f | %.2f | - | - |",
        orig_camera[2], orig_camera_SD[2]),
sprintf("| | | 本研究 | %d | %.2f | %.2f | %.2f | %s |",
        camera_n, rep_camera[2], rep_camera_SD[2], pe_camera[2], rt_camera[2]),
sprintf("| | control | 原研究 | 82 | %.2f | %.2f | - | - |",
        orig_control[2], orig_control_SD[2]),
sprintf("| | | 本研究 | %d | %.2f | %.2f | %.2f | %s |",
        control_n, rep_control[2], rep_control_SD[2], pe_control[2], rt_control[2]),
sprintf("| remaining_food_weight | camera | 原研究 | 82 | %.2f | %.2f | - | - |",
        orig_camera[3], orig_camera_SD[3]),
sprintf("| | | 本研究 | %d | %.2f | %.2f | %.2f | %s |",
        camera_n, rep_camera[3], rep_camera_SD[3], pe_camera[3], rt_camera[3]),
sprintf("| | control | 原研究 | 82 | %.2f | %.2f | - | - |",
        orig_control[3], orig_control_SD[3]),
sprintf("| | | 本研究 | %d | %.2f | %.2f | %.2f | %s |",
        control_n, rep_control[3], rep_control_SD[3], pe_control[3], rt_control[3]),
"",
"注：δ = |X_O − X_R| / |X_O| × 100%；评级：完全一致(δ=0%) / 次要偏差(0%<δ<10%) / 主要偏差(≥10%)",
"",
"### Demographics",
"",
sprintf("总 N = %d, 女性 = %d, 男性 = %d, M_age = %.2f, SD_age = %.2f",
        demo$n_total, demo$n_female, demo$n_male, demo$age_M, demo$age_SD),
"",
""
)

# Original paper values (per the paper text)
orig_table5 <- list(
  perceived_observation_1_9   = list(t = 5.08, df = 80, p = "<.001", CI = "[1.628, 3.723]", eta2 = .244),
  food_portion_size_1_9       = list(t = 2.20, df = 80, p = ".030",  CI = "[0.082, 1.610]", eta2 = .057),
  remaining_food_weight_g     = list(t = 0.90, df = 80, p = ".370",  CI = "[-1.366, 3.624]", eta2 = NA),
  food_calorie_1_9            = list(t = NA,   df = NA, p = ">.125", CI = "",               eta2 = NA),
  today_intake_contribution_1_9 = list(t = NA, df = NA, p = ">.125", CI = "",               eta2 = NA),
  food_enjoyment_level_1_9    = list(t = NA,   df = NA, p = ">.125", CI = "",               eta2 = NA),
  food_consumed_amount_1_4    = list(t = NA,   df = NA, p = ">.125", CI = "",               eta2 = NA)
)

calc_pe_val <- function(o, r) {
  if (is.na(o) || is.na(r) || o == 0) return(NA)
  round(abs(o - r) / abs(o) * 100, 2)
}

rating_pe <- function(pe) {
  if (is.na(pe)) return("N/A")
  if (pe == 0) return("完全一致")
  if (pe < 10) return("次要偏差")
  return("主要偏差")
}

# 3 core variables with full paper stats
vlist <- c("perceived_observation_1_9", "food_portion_size_1_9", "remaining_food_weight_g")

tbl5_lines <- c(
  "",
  "### 表 5 推断性统计结果的比较（原文献方法）",
  "",
  "| 变量 | 来源 | t | df | p | 95% CI | η² | δ_t(%) | 评级_t | δ_η²(%) | 评级_η² |",
  "|------|------|---|----|---|--------|-----|--------|--------|---------|--------|"
)

for (v in vlist) {
  o <- orig_table5[[v]]
  r <- tt_results[[v]]
  short_name <- switch(v,
    "perceived_observation_1_9" = "Observed",
    "food_portion_size_1_9" = "Portion",
    "remaining_food_weight_g" = "Remain")
  pe_t <- calc_pe_val(o$t, r$t)
  pe_eta <- calc_pe_val(o$eta2, r$eta2)
  rat_t <- rating_pe(pe_t)
  rat_eta <- rating_pe(pe_eta)

  tbl5_lines <- c(tbl5_lines,
    sprintf("| %s | 原研究 | %.2f | %.0f | %s | %s | %s | - | - | - | - |",
            short_name, o$t, o$df, o$p, o$CI,
            if (is.na(o$eta2)) "" else sprintf("%.3f", o$eta2)),
    sprintf("| | 本研究 | %.2f | %.0f | %.3f | %s | %.3f | %.2f | %s | %.2f | %s |",
            r$t, r$df, r$p, r$CI, r$eta2,
            pe_t, rat_t, pe_eta, rat_eta)
  )
}

# Other variables (paper only reported p > .125)
for (v in setdiff(vars_ttest, vlist)) {
  r <- tt_results[[v]]
  tbl5_lines <- c(tbl5_lines,
    sprintf("| %s | 原研究 | — | — | >.125 | — | — | - | - | - | - |", v),
    sprintf("| | 本研究 | %.2f | %.0f | %.3f | %s | %.3f | — | — | — | — |",
            r$t, r$df, r$p, r$CI, r$eta2)
  )
}

tbl5_lines <- c(tbl5_lines,
  "",
  "注：δ = |X_O − X_R| / |X_O| × 100%；评级：完全一致(δ=0%) / 次要偏差(0%<δ<10%) / 主要偏差(≥10%)",
  "p > .125 的变量为论文化填项，仅报告本研究结果。",
  ""
)

# ==================================================
# 替代分析: 线性回归（原方法: 独立 t 检验）
# ==================================================

cat("\n\n======= Alternative: Linear Regression =======\n\n")

# 条件 dummy 编码: camera=1, no_camera=0
df1 <- df1 %>% mutate(condition_num = ifelse(condition == "camera", 1, 0))

vars_reg <- c("perceived_observation_1_9", "food_portion_size_1_9",
              "remaining_food_weight_g", "food_calorie_1_9",
              "today_intake_contribution_1_9", "food_enjoyment_level_1_9",
              "food_consumed_amount_1_4")
reg_results <- list()

for (v in vars_reg) {
  cat(sprintf("\n========== %s ~ condition ==========\n", v))
  m <- lm(as.formula(paste(v, "~ condition_num")), data = df1)
  s <- summary(m)
  print(s)
  reg_results[[v]] <- s
}

# 提取关键结果对比表
alt_lines <- c("",
  "### 替代分析: 线性回归结果",
  "",
  "| 因变量 | B(condition) | SE | t | p | F(1,80) | R² |",
  "|--------|------------|---|----|----|--------|-----|")

for (v in names(reg_results)) {
  s <- reg_results[[v]]
  coef_row <- s$coefficients[2, ]
  f_stat <- s$fstatistic
  f_val <- round(f_stat[1], 2)
  r2 <- round(s$r.squared, 3)
  alt_lines <- c(alt_lines,
    sprintf("| %s | %.4f | %.4f | %.2f | %.3f | %.2f | %.3f |",
            v, coef_row[1], coef_row[2], coef_row[3], coef_row[4], f_val, r2))
}

alt_lines <- c(alt_lines, "",
  "注: t 检验与含单一二分类自变量的线性回归等价（t² = F）。B 为 camera 与 control 的均值差。",
  "")

# Append to existing md
md_path <- "5.Reports/Study 1/Study1_结果对比.md"
writeLines(c(tbl2_lines, tbl5_lines, alt_lines), md_path)
cat("\n✔ Tables 2 & 5 saved to:", md_path, "\n")
