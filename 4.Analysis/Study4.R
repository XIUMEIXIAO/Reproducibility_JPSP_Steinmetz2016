# ==================================================
# Study 4 数据导入与预处理
# ==================================================

# — 0. Packages ———————————————
library(haven)
library(dplyr)
library(bruceR)

# — 1. Import ———————————————
setwd("K:/master/R_learning/JPSP_2016")
df <- read_sav("3.Rawdata/Study4.sav")

# — 2. Quick overview ———————————
summary(df)


# — 3. Select, rename & type conversion ———————————
df4 <- df %>%
  select(
    Team, Member,
    play_order_skill             = Order,
    earned_points                = Point,
    spectators_n                 = Adc,
    recalled_points              = Recalled,
    perceived_contribution_1_10  = Overclm,
    team_outcome                 = Outcome,
    tournament                   = Tnm,
    match_type                   = Game,
    gender                       = Gender,
    accuracy_recalled_minus_earned = Accuracy,
    team_size                    = Teammate
  ) %>%
  mutate(
    team_outcome = factor(team_outcome,
      levels = c(-1, 1), labels = c("loss", "win")),
    gender = factor(gender,
      levels = c(0, 1), labels = c("Female", "Male")),
    tournament = factor(tournament),
    match_type = factor(match_type,
      levels = c(1, 2), labels = c("Single", "Double"))
  )


# — 4. Verify ———————————
cat("\n✔ Data imported:", nrow(df4), "rows,", ncol(df4), "columns\n")
glimpse(df4)


# ==================================================
# Study 4 描述统计
# ==================================================

cat("\n\n======= Descriptive Statistics =======\n\n")

# — 5. Group stats ———————————
vars_desc <- c("perceived_contribution_1_10", "spectators_n",
               "earned_points", "play_order_skill")
desc <- df4 %>%
  summarise(across(all_of(vars_desc),
    list(M = ~round(mean(.x, na.rm = TRUE), 2),
         SD = ~round(sd(.x, na.rm = TRUE), 2)),
    .names = "{fn}_{col}"), n = n())
print(desc)

# — 6. Demographics ———————————
cat("\n--- Gender & Outcome ---\n")
print(table(df4$gender))
print(table(df4$team_outcome))


# ==================================================
# Study 4 推断统计
# ==================================================

cat("\n\n======= Inferential Statistics =======\n\n")

# — 7. Correlations ———————————
cat("\n========== Correlation: spectators_n vs earned_points ==========\n")
cor_pt <- cor.test(df4$spectators_n, df4$earned_points)
print(cor_pt)

cat("\n========== Correlation: spectators_n vs play_order_skill ==========\n")
cor_od <- cor.test(df4$spectators_n, df4$play_order_skill)
print(cor_od)


# — 8. Main effect model ———————————
cat("\n========== Reg: perceived_contribution ~ spectators_n + team_outcome ==========\n")
m0 <- regress(perceived_contribution_1_10 ~ spectators_n + team_outcome, data = df4)

# — 9. Interaction model ———————————
cat("\n========== Reg: perceived_contribution ~ spectators_n * team_outcome ==========\n")
m1 <- regress(perceived_contribution_1_10 ~ spectators_n * team_outcome, data = df4)

# — 10. Covariate regressions ———————————
cat("\n========== Reg: + earned_points ==========\n")
m2 <- regress(perceived_contribution_1_10 ~ spectators_n + earned_points, data = df4)

cat("\n========== Reg: + play_order_skill ==========\n")
m3 <- regress(perceived_contribution_1_10 ~ spectators_n + play_order_skill, data = df4)


# ==================================================
# Study 4 结果输出
# ==================================================

# — 11. δ & rating helpers ———————————
calc_pe <- function(o, r) {
  if (is.na(o) || o == 0) return(NA)
  round(abs(o - r) / abs(o) * 100, 2)
}
rating_pe <- function(pe) {
  if (is.na(pe)) return("N/A")
  if (pe == 0) return("完全一致")
  if (pe < 10) return("次要偏差")
  return("主要偏差")
}

# — 12. Build Table 2 ———————————
tbl2_lines <- c("",
  "### 表 2 描述性统计结果",
  "",
  "| 变量 | N | Mean | SD |",
  "|------|---|------|-----|",
  sprintf("| perceived_contribution_1_10 | %.0f | %.2f | %.2f |", desc$n, desc$M_perceived_contribution_1_10, desc$SD_perceived_contribution_1_10),
  sprintf("| spectators_n | %.0f | %.2f | %.2f |", desc$n, desc$M_spectators_n, desc$SD_spectators_n),
  sprintf("| earned_points | %.0f | %.2f | %.2f |", desc$n, desc$M_earned_points, desc$SD_earned_points),
  sprintf("| play_order_skill | %.0f | %.2f | %.2f |", desc$n, desc$M_play_order_skill, desc$SD_play_order_skill),
  "")

# — 13. Build Table 5 ———————————
# Use rounded reproduced values for δ comparison
rep_r_pt <- round(cor_pt$estimate, 3)
rep_r_od <- round(cor_od$estimate, 3)

# Paper reported values
orig <- list(
  cor_pt  = list(r = .051),
  cor_od  = list(r = .011),
  adc_main = list(B = 0.11, t = 3.98),
  outcome  = list(B = -0.44, t = -0.86),
  adc_co1  = list(B = 0.112, t = 4.03),
  point_co = list(B = 0.043, t = 3.02),
  adc_co2  = list(B = 0.115, t = 4.12),
  order_co = list(B = 0.20, t = 2.58)
)

# Extract coefficients from models (use numeric outcome coding & interaction model to match paper's t(117))
df4 <- df4 %>% mutate(outcome_num = ifelse(team_outcome == "win", 1, -1))
m0_lm <- lm(perceived_contribution_1_10 ~ spectators_n * outcome_num, data = df4)
m2_lm <- lm(perceived_contribution_1_10 ~ spectators_n + earned_points, data = df4)
m3_lm <- lm(perceived_contribution_1_10 ~ spectators_n + play_order_skill, data = df4)

coef_m0 <- summary(m0_lm)$coefficients
coef_m2 <- summary(m2_lm)$coefficients
coef_m3 <- summary(m3_lm)$coefficients

b_spec_m0  <- round(coef_m0["spectators_n", "Estimate"], 2)
t_spec_m0  <- round(coef_m0["spectators_n", "t value"], 2)
p_spec_m0  <- if (coef_m0["spectators_n", "Pr(>|t|)"] < 0.001) "<.001" else format(round(coef_m0["spectators_n", "Pr(>|t|)"], 3), nsmall = 3)
b_outcome  <- round(coef_m0["outcome_num", "Estimate"], 2)
t_outcome  <- round(coef_m0["outcome_num", "t value"], 2)
p_outcome  <- format(round(coef_m0["outcome_num", "Pr(>|t|)"], 3), nsmall = 3)

b_spec_co1 <- round(coef_m2["spectators_n", "Estimate"], 3)
t_spec_co1 <- round(coef_m2["spectators_n", "t value"], 2)
p_spec_co1 <- format(round(coef_m2["spectators_n", "Pr(>|t|)"], 3), nsmall = 3)
b_point    <- round(coef_m2["earned_points", "Estimate"], 3)
t_point    <- round(coef_m2["earned_points", "t value"], 2)
p_point    <- format(round(coef_m2["earned_points", "Pr(>|t|)"], 3), nsmall = 3)

b_spec_co2 <- round(coef_m3["spectators_n", "Estimate"], 3)
t_spec_co2 <- round(coef_m3["spectators_n", "t value"], 2)
p_spec_co2 <- format(round(coef_m3["spectators_n", "Pr(>|t|)"], 3), nsmall = 3)
b_order    <- round(coef_m3["play_order_skill", "Estimate"], 3)
t_order    <- round(coef_m3["play_order_skill", "t value"], 2)
p_order    <- format(round(coef_m3["play_order_skill", "Pr(>|t|)"], 3), nsmall = 3)

tbl5_lines <- c("",
  "### 表 5 推断性统计结果的比较",
  "",
  "| 模型 | 预测变量 | 来源 | B | t | p | δ_B(%) | 评级_B |",
  "|------|---------|------|---|---|----|--------|--------|",
  "| 交互模型 | spectators_n | 原研究 | 0.11 | 3.98 | <.001 | - | - |",
  sprintf("| 交互模型 | spectators_n | 本研究 | %.2f | %.2f | %s | %.2f | %s |",
          b_spec_m0, t_spec_m0, p_spec_m0,
          calc_pe(0.11, b_spec_m0), rating_pe(calc_pe(0.11, b_spec_m0))),
  "| 交互模型 | outcome_num | 原研究 | -0.44 | -0.86 | .390 | - | - |",
  sprintf("| 交互模型 | outcome_num | 本研究 | %.2f | %.2f | %s | %.2f | %s |",
          b_outcome, t_outcome, p_outcome,
          calc_pe(0.44, abs(b_outcome)), rating_pe(calc_pe(0.44, abs(b_outcome)))),
  "| 协变量1 | spectators_n | 原研究 | 0.112 | 4.03 | <.001 | - | - |",
  sprintf("| 协变量1 | spectators_n | 本研究 | %.3f | %.2f | %s | %.2f | %s |",
          b_spec_co1, t_spec_co1, p_spec_co1,
          calc_pe(0.112, b_spec_co1), rating_pe(calc_pe(0.112, b_spec_co1))),
  "| 协变量1 | earned_points | 原研究 | 0.043 | 3.02 | .003 | - | - |",
  sprintf("| 协变量1 | earned_points | 本研究 | %.3f | %.2f | %s | %.2f | %s |",
          b_point, t_point, p_point,
          calc_pe(0.043, b_point), rating_pe(calc_pe(0.043, b_point))),
  "| 协变量2 | spectators_n | 原研究 | 0.115 | 4.12 | <.001 | - | - |",
  sprintf("| 协变量2 | spectators_n | 本研究 | %.3f | %.2f | %s | %.2f | %s |",
          b_spec_co2, t_spec_co2, p_spec_co2,
          calc_pe(0.115, b_spec_co2), rating_pe(calc_pe(0.115, b_spec_co2))),
  "| 协变量2 | play_order_skill | 原研究 | 0.20 | 2.58 | .011 | - | - |",
  sprintf("| 协变量2 | play_order_skill | 本研究 | %.3f | %.2f | %s | %.2f | %s |",
          b_order, t_order, p_order,
          calc_pe(0.20, b_order), rating_pe(calc_pe(0.20, b_order))),
  "",
  "注：δ_B = |B_O − B_R| / |B_O| × 100%。相关结果见描述统计。",
  "")

# ==================================================
# 替代分析: 线性混合效应模型（原方法: OLS 回归，考虑队伍聚类）
# ==================================================

cat("\n\n======= Alternative: LMM with Team Random Intercept =======\n\n")

library(lme4)

# OLS 作为 baseline
m_ols <- lmer(perceived_contribution_1_10 ~ spectators_n + team_outcome + earned_points + play_order_skill + (1 | Team),
              data = df4)
cat("\n--- LMM: perceived_contribution ~ spectators + outcome + points + skill + (1|Team) ---\n")
print(summary(m_ols))

# 随机效应显著性检验（与不含 Team 的模型比较）
m_no_team <- lm(perceived_contribution_1_10 ~ spectators_n + team_outcome + earned_points + play_order_skill,
                data = df4)
library(lmtest)
lrt <- lrtest(m_no_team, m_ols)

cat("\n--- Likelihood Ratio Test: Team random effect ---\n")
print(lrt)

# Extract variance components
vc <- as.data.frame(VarCorr(m_ols))
icc_team <- vc$vcov[1] / sum(vc$vcov)

fe <- summary(m_ols)$coefficients
alt_lines <- c("",
  "### 替代分析: LMM 结果（控制队伍内聚类）",
  "",
  "**模型**: perceived_contribution ~ spectators + outcome + points + skill + (1 | Team)",
  "",
  "**固定效应:**",
  "| 预测变量 | Estimate | SE | t |",
  "|---------|---------|----|---|")

for (i in 1:nrow(fe)) {
  alt_lines <- c(alt_lines,
    sprintf("| %s | %.4f | %.4f | %.2f |", rownames(fe)[i], fe[i,1], fe[i,2], fe[i,3]))
}

alt_lines <- c(alt_lines, "",
  "**随机效应:**",
  sprintf("- 队伍间方差 (Team): %.4f", vc$vcov[1]),
  sprintf("- 残差方差: %.4f", vc$vcov[2]),
  sprintf("- ICC (队伍内相关): %.4f", icc_team),
  sprintf("- 似然比检验: χ²(1) = %.2f, p = %.4f", lrt$Chisq[2], lrt$`Pr(>Chisq)`[2]),
  "",
  "注: ICC > 0 表明队员在队伍内存在非独立性，LMM 比 OLS 给出更准确的标准误。",
  "")

# — 14. Save to md ———————————
md_path <- "5.Reports/Study 4/Study4_结果对比.md"
writeLines(c(tbl2_lines, tbl5_lines, alt_lines), md_path)
cat("\n✔ Tables saved to:", md_path, "\n")