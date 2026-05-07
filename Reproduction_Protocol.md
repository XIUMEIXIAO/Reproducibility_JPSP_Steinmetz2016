# Reproduction Protocol: Steinmetz et al. (2016)

## 0. 复现目标
基于 OSF 原始数据，复现 JPSP (2016) 论文的核心发现：**被观察会放大行为的心理量值（Magnitude）**。
- 最终目标：复现 Study 1–5 的核心统计效应（t-test, ANOVA, Regression）。
- 关键指标：样本量 $N$、检验统计量 ($t/F$)、显著性 $p$、效应量 ($\eta_p^2 / d$)。

---

## 1. 建立复现工作目录
```text
reproduction/
  01_raw/        # 从 OSF 下载的 .sav 原始数据
  02_scripts/    # R 脚本 (01_study1.R, 02_study2.R...)
  03_outputs/    # 导出的统计表 (CSV) 与 绘图 (PNG)
  04_logs/       # 样本量核对日志与报错记录

## 2. 数据导入与清洗标准

### 执行动作
- **工具**：使用 `haven::read_sav()` 读取 SPSS 格式数据。
- **Exclusion Criteria（剔除标准）**：
    - 必须根据各 Study 章节的 "Participants" 段落核对剔除理由。
    - 重点核对：是否剔除了“未完成实验者”或“未通过注意力检测者”。

### 完成标准
- **目标**：清洗后的 $N$ 必须与原文 Table 1 或正文描述完全一致。

---

## 3. Study 逐项复现清单

### Study 1 & 2: 进食量感知 (t-test)
- **自变量 (IV)**：Condition (Observed vs. Unobserved)。
- **因变量 (DV)**：Recalled portion size。
- **统计任务**：
    - [ ] 运行 Independent Samples t-test。
    - [ ] 计算 Cohen's $d$。
    - [ ] 验证：被观察组的均值是否显著更高。

### Study 3: 任务表现 (2x2 ANOVA)
- **设计**：2 (Observed vs. Anonymous) × 2 (Correct vs. Incorrect answers)。
- **统计任务**：
    - [ ] 运行 Two-way ANOVA。
    - [ ] 验证：是否存在 Observation 的主效应。
    - [ ] 验证：是否存在 Interaction（理论预判：应无交互作用，两类行为均被放大）。

### Study 4: 现场研究 (Regression)
- **核心**：Audience Size 作为连续变量。
- **统计任务**：
    - [ ] 运行相关分析与回归模型。
    - [ ] 验证：观众人数是否正向预测贡献量的感知。

### Study 5: 行为 vs. 不作为 (Interaction)
- **关键**：这是论文的边界条件（Boundary Condition）。
- **统计任务**：
    - [ ] 运行 2 (Observed vs. Unobserved) × 2 (Action vs. Inaction) 混合方差分析。
    - [ ] **核心指标**：验证交互作用 $F$ 值。
    - [ ] **简单效应分析**：验证在 Inaction 条件下，观察效应消失。

---

## 4. 指标验证看板 (Scorecard)

### 每一项分析产出必须包含：
- [ ] 自由度 ($df$) 是否匹配？
- [ ] $F$ 或 $t$ 值的数值差异是否在 0.01 以内？
- [ ] 效应量是否缺失？（原文强调了效应量，复现必须补全）。

---

## 5. 结果对不上时的排查顺序

1. **样本层**：核对 $N$。检查是否错误地包含了试测（Pilot）数据。
2. **算法层**：ANOVA 的平方和类型。R 默认 Type II，若不一致请改用 `car::Anova(type = 3)`。
3. **变量层**：Study 5 的反向计分或题目加总是否准确。
4. **软件层**：检查 SPSS 的缺失值处理（Exclude cases analysis-by-analysis）在 R 中是否一致。

---

## 6. 交付文件清单

- [ ] `Reproduction_Protocol.md` (本文件)
- [ ] `reproduction/02_scripts/` 下的所有 `.R` 脚本
- [ ] `reproduction/03_outputs/` 下的复现结果对比表
