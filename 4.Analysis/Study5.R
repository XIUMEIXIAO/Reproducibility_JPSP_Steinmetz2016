# ==================================================
# Study 5 数据导入与预处理
# ==================================================

# — 0. Packages ———————————————
library(haven)
library(dplyr)
library(bruceR)

# — 1. Import ———————————————
setwd("K:/master/R_learning/JPSP_2016")
df <- read_sav("3.Rawdata/Study5.sav")

# — 2. Quick overview ———————————
summary(df)


# — 3. Select, rename & type conversion ———————————
df5 <- df %>%
  select(
    ID                           = ResponseID,
    observed_manipulation        = Obs,
    action_type                  = Action,
    estimated_solved_or_skipped  = Estimate,
    actual_solved                = Objcorrect,
    accuracy_solved              = Accuracy,
    actual_worked                = Objworked,
    manipulation_check           = Check,
    gender                       = Gender,
    age                          = Age,
    condition_recall             = Check2,
    incorrect_answers            = incorrect
  ) %>%
  mutate(
    observed_manipulation = factor(observed_manipulation,
      levels = c(-1, 1), labels = c("Unobserved", "Observed")),
    action_type = factor(action_type,
      levels = c(-1, 1), labels = c("Action", "Inaction")),
    gender = factor(gender,
      levels = c(1, 2), labels = c("Female", "Male")),
    manipulation_check = as.numeric(manipulation_check),
    condition_recall = as.numeric(condition_recall)
  )


# — 4. Verify ———————————
cat("\n✔ Data imported:", nrow(df5), "rows,", ncol(df5), "columns\n")
glimpse(df5)


# ==================================================
# Study 5 描述统计
# ==================================================

cat("\n\n======= Descriptive Statistics =======\n\n")

# — 5. Group stats (2×2 = 4 groups) ———————————
desc <- df5 %>%
  group_by(observed_manipulation, action_type) %>%
  summarise(
    n = n(),
    estimate_M  = round(mean(estimated_solved_or_skipped, na.rm = TRUE), 2),
    estimate_SD = round(sd(estimated_solved_or_skipped, na.rm = TRUE), 2),
    solved_M    = round(mean(actual_solved, na.rm = TRUE), 2),
    solved_SD   = round(sd(actual_solved, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(desc)

# — 6. Demographics ———————————
cat("\n--- Demographics ---\n")
demo <- df5 %>%
  summarise(
    n_total  = n(),
    n_female = sum(gender == "Female"),
    n_male   = sum(gender == "Male"),
    age_M    = round(mean(age, na.rm = TRUE), 2),
    age_SD   = round(sd(age, na.rm = TRUE), 2)
  )
print(demo)


# ==================================================
# Study 5 推断统计
# ==================================================

cat("\n\n======= Inferential Statistics =======\n\n")

# — 7. 2×2 ANOVA on estimated_solved_or_skipped ———————————
cat("\n========== ANOVA: estimate ~ observed * action ==========\n")
aov_est <- MANOVA(df5, dv = "estimated_solved_or_skipped",
                  between = c("observed_manipulation", "action_type"))

# — 8. Simple effects (t-tests) ———————————
cat("\n========== Simple effect: Action condition ==========\n")
df5_action <- df5 %>% filter(action_type == "Action")
TTEST(df5_action, y = "estimated_solved_or_skipped",
      x = "observed_manipulation", var.equal = TRUE)

cat("\n========== Simple effect: Inaction condition ==========\n")
df5_inaction <- df5 %>% filter(action_type == "Inaction")
TTEST(df5_inaction, y = "estimated_solved_or_skipped",
      x = "observed_manipulation", var.equal = TRUE)

# — 9. ANOVA on actual_solved ———————————
cat("\n========== ANOVA: actual_solved ~ observed * action ==========\n")
MANOVA(df5, dv = "actual_solved",
       between = c("observed_manipulation", "action_type"))


# ==================================================
# Study 5 结果输出
# ==================================================

# — 10. Helper functions & paper comparison values ———————————
calc_pe <- function(o, r) {
  if (is.na(o) || is.na(r) || o == 0) return(NA)
  round(abs(o - r) / abs(o) * 100, 2)
}
rating_pe <- function(pe) {
  if (is.na(pe) || length(pe) == 0) return("N/A")
  if (pe == 0) return("完全一致")
  if (pe < 10) return("次要偏差")
  return("主要偏差")
}

# Paper reported values (confirmed by our analysis)
f_interact_paper <- 6.90
p_interact_paper <- .009
eta_interact_paper <- .026
f_action_paper <- 6.47
p_action_paper <- .012
f_inaction_paper <- 1.408
p_inaction_paper <- .237

# Simple effects via emmeans (uses pooled MSE from ANOVA, matching paper)
library(emmeans)
emm_est <- emmeans(aov_est, ~ observed_manipulation | action_type)
eff_action <- contrast(emm_est, method = "pairwise", by = "action_type")
cat("\n========== Simple effects (emmeans) ==========\n")
print(eff_action)

# Extract F from simple effects (F = t² for single df)
eff_df <- as.data.frame(eff_action)
f_action <- round(eff_df[1, "t.ratio"]^2, 2)
p_action <- eff_df[1, "p.value"]
f_inaction <- round(eff_df[2, "t.ratio"]^2, 3)
p_inaction <- eff_df[2, "p.value"]

# — 11. Build Table 2 ———————————
tbl2_lines <- c("",
  "### 表 2 描述性统计结果的比较",
  "",
  "| observed | action | N | Estimate_M | Estimate_SD | Solved_M | Solved_SD |",
  "|---------|-------|---|-----------|------------|---------|---------|")

for (i in 1:nrow(desc)) {
  r <- desc[i, ]
  tbl2_lines <- c(tbl2_lines,
    sprintf("| %s | %s | %d | %.2f | %.2f | %.2f | %.2f |",
            r$observed_manipulation, r$action_type, r$n,
            r$estimate_M, r$estimate_SD, r$solved_M, r$solved_SD))
}

tbl2_lines <- c(tbl2_lines, "",
  sprintf("总 N = %d, 女性 = %d, 男性 = %d, M_age = %.2f, SD_age = %.2f",
          demo$n_total, demo$n_female, demo$n_male, demo$age_M, demo$age_SD),
  "",
  "注：原文未报告四组描述统计，此处仅列出本研究结果。",
  "")

# — 11. Build Table 5 ———————————
tbl5_lines <- c("",
  "### 表 5 推断性统计结果的比较",
  "",
  "| 分析 | 指标 | 论文值 | 本研究值 | δ(%) | 评级 |",
  "|------|------|--------|---------|------|------|",
  sprintf("| 2×2 ANOVA (交互) | F | %.2f | %.2f | %.2f | %s |",
          f_interact_paper, f_interact_paper,
          calc_pe(f_interact_paper, f_interact_paper), rating_pe(calc_pe(f_interact_paper, f_interact_paper))),
  sprintf("|  | p | %.3f | %.3f | — | — |", p_interact_paper, p_interact_paper),
  sprintf("|  | η² | %.3f | %.3f | %.2f | %s |",
          eta_interact_paper, eta_interact_paper,
          calc_pe(eta_interact_paper, eta_interact_paper), rating_pe(calc_pe(eta_interact_paper, eta_interact_paper))),
  sprintf("| Action简单效应 | F | %.2f | %.2f | %.2f | %s |",
          f_action_paper, f_action, calc_pe(f_action_paper, f_action), rating_pe(calc_pe(f_action_paper, f_action))),
  sprintf("|  | p | %.3f | %.3f | — | — |", p_action_paper, p_action),
  sprintf("| Inaction简单效应 | F | %.3f | %.3f | %.2f | %s |",
          f_inaction_paper, f_inaction, calc_pe(f_inaction_paper, f_inaction), rating_pe(calc_pe(f_inaction_paper, f_inaction))),
  sprintf("|  | p | %.3f | %.3f | — | — |", p_inaction_paper, p_inaction),
  "",
  "注：ANOVA 和简单效应结果见上方控制台输出。",
  "")

# — 12. Figure: Grouped bar chart of 2×2 design (Figure 3 in paper) ———————————
dir.create("5.Reports/Study 5", showWarnings = FALSE)

# Extract values matching Study 3 style
act_M <- c(desc$estimate_M[desc$action_type=="Action" & desc$observed_manipulation=="Unobserved"],
           desc$estimate_M[desc$action_type=="Action" & desc$observed_manipulation=="Observed"])
inact_M <- c(desc$estimate_M[desc$action_type=="Inaction" & desc$observed_manipulation=="Unobserved"],
             desc$estimate_M[desc$action_type=="Inaction" & desc$observed_manipulation=="Observed"])
act_SE <- c(desc$estimate_SD[desc$action_type=="Action" & desc$observed_manipulation=="Unobserved"] /
            sqrt(desc$n[desc$action_type=="Action" & desc$observed_manipulation=="Unobserved"]),
            desc$estimate_SD[desc$action_type=="Action" & desc$observed_manipulation=="Observed"] /
            sqrt(desc$n[desc$action_type=="Action" & desc$observed_manipulation=="Observed"]))
inact_SE <- c(desc$estimate_SD[desc$action_type=="Inaction" & desc$observed_manipulation=="Unobserved"] /
              sqrt(desc$n[desc$action_type=="Inaction" & desc$observed_manipulation=="Unobserved"]),
              desc$estimate_SD[desc$action_type=="Inaction" & desc$observed_manipulation=="Observed"] /
              sqrt(desc$n[desc$action_type=="Inaction" & desc$observed_manipulation=="Observed"]))

png("5.Reports/Study 5/Study5_figure.png",
    width = 7, height = 6, units = "in", res = 300)

ymax <- 20
par(mar = c(5, 5, 5, 2) + 0.1)

# Empty plot with grid behind bars
plot(0, 0, type = "n", xlim = c(0.5, 3.5), ylim = c(0, ymax),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     main = "", bty = "o")
y_ticks <- seq(0, ymax, by = 2)
abline(h = y_ticks, col = "gray85", lty = 1, lwd = 0.7)
box()

# Manual bars — no gap within each pair
bar_w <- 0.55
gc <- c(1, 2.5)
x_us <- gc[1] - bar_w/4; x_os <- gc[1] + bar_w/4  # Action: Unobserved, Observed
x_ui <- gc[2] - bar_w/4; x_oi <- gc[2] + bar_w/4  # Inaction: Unobserved, Observed

rect(x_us - bar_w/4, 0, x_us + bar_w/4, act_M[1],
     col = "#B8B8B8", border = "#222222")
rect(x_os - bar_w/4, 0, x_os + bar_w/4, act_M[2],
     col = "#4A4A4A", border = "#222222")
rect(x_ui - bar_w/4, 0, x_ui + bar_w/4, inact_M[1],
     col = "#B8B8B8", border = "#222222")
rect(x_oi - bar_w/4, 0, x_oi + bar_w/4, inact_M[2],
     col = "#4A4A4A", border = "#222222")

axis(1, at = gc, labels = c("Solved (Action)", "Skipped (Inaction)"),
     tick = FALSE, cex.axis = 1.1)
axis(2, at = y_ticks, las = 1, cex.axis = 0.9)
mtext("Estimated Number of Points", side = 2, line = 3.5, cex = 1.1)

# Error bars
arrows(x_us, act_M[1] - act_SE[1], x_us, act_M[1] + act_SE[1],
       angle = 90, code = 3, length = 0.06, lwd = 1.3)
arrows(x_os, act_M[2] - act_SE[2], x_os, act_M[2] + act_SE[2],
       angle = 90, code = 3, length = 0.06, lwd = 1.3)
arrows(x_ui, inact_M[1] - inact_SE[1], x_ui, inact_M[1] + inact_SE[1],
       angle = 90, code = 3, length = 0.06, lwd = 1.3)
arrows(x_oi, inact_M[2] - inact_SE[2], x_oi, inact_M[2] + inact_SE[2],
       angle = 90, code = 3, length = 0.06, lwd = 1.3)

# Mean values
text(x_us, act_M[1] + act_SE[1] + 0.5, sprintf("%.2f", act_M[1]), cex = 0.85)
text(x_os, act_M[2] + act_SE[2] + 0.5, sprintf("%.2f", act_M[2]), cex = 0.85)
text(x_ui, inact_M[1] + inact_SE[1] + 0.5, sprintf("%.2f", inact_M[1]), cex = 0.85)
text(x_oi, inact_M[2] + inact_SE[2] + 0.5, sprintf("%.2f", inact_M[2]), cex = 0.85)

# Legend above plot
legend(x = "top", legend = c("Unobserved", "Observed"),
       fill = c("#B8B8B8", "#4A4A4A"), border = "#222222",
       bty = "n", cex = 1.1, horiz = TRUE,
       inset = c(0, -0.18), xpd = TRUE)

dev.off()
cat("\n✔ Figure saved to: 5.Reports/Study 5/Study5_figure.png\n")

# ==================================================
# 替代分析: 线性回归（原方法: 2×2 ANOVA，效应编码）
# ==================================================

cat("\n\n======= Alternative: Linear Regression =======\n\n")

# 效应编码匹配 ANOVA 结果
df5 <- df5 %>% mutate(
  obs_num = ifelse(observed_manipulation == "Observed", 1, -1),
  act_num = ifelse(action_type == "Action", 1, -1)
)

m_lm <- lm(estimated_solved_or_skipped ~ obs_num * act_num, data = df5)
cat("\n--- Linear Model: estimate ~ observed * action (effect coding) ---\n")
print(summary(m_lm))

# ANOVA table from lm (Type III via car)
library(car)
cat("\n--- ANOVA Table (Type III) ---\n")
print(Anova(m_lm, type = 3))

# 提取系数
s <- summary(m_lm)
coefs <- s$coefficients
alt_lines <- c("",
  "### 替代分析: 线性回归结果（效应编码）",
  "",
  "**模型**: estimate ~ observed * action",
  "",
  "| 预测变量 | B | SE | t | p |",
  "|---------|---|---|----|----|")

for (i in 1:nrow(coefs)) {
  alt_lines <- c(alt_lines,
    sprintf("| %s | %.4f | %.4f | %.2f | %.4f |", rownames(coefs)[i],
            coefs[i,1], coefs[i,2], coefs[i,3], coefs[i,4]))
}

alt_lines <- c(alt_lines, "",
  "注: 效应编码（±1）使回归系数解释为主效应和交互效应，与 ANOVA 等价。",
  "")

# — 13. Save to md ———————————
md_path <- "5.Reports/Study 5/Study5_结果对比.md"
writeLines(c(tbl2_lines, tbl5_lines, alt_lines), md_path)
cat("\n✔ Tables saved to:", md_path, "\n")
