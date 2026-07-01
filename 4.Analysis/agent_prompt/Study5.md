# Agent Prompt — Study 5 数据导入与预处理

让我们开始研究五的工作，新建一个Study 5的R脚本，先导入数据，利用haven包read_sav读取sav格式的Study 5，原始数据赋值到df，再用summary查看数据。然后用管道符%>%进行select选择列并重命名赋给df5：

- ID（ResponseID）
- observed_manipulation（Obs：-1=Unobserved，1=Observed）
- action_type（Action：-1=Action解题，1=Inaction跳过）
- estimated_solved_or_skipped（Estimate）
- actual_solved（Objcorrect）
- accuracy_solved（Accuracy）
- actual_worked（Objworked）
- manipulation_check（Check：1=Unobserved，2=Observed）
- gender（Gender：1=Female，2=Male）
- age（Age）
- condition_recall（Check2）
- incorrect_answers（incorrect）

转为factor（分类变量）：
- observed_manipulation：-1=Unobserved, 1=Observed
- action_type：-1=Action, 1=Inaction
- gender：1=Female, 2=Male

其余保留numeric。

# Agent Prompt — Study 5 描述统计 & 推断统计 & 结果对比

先做描述统计（estimated_solved_or_skipped、actual_solved 按 observed_manipulation × action_type 四组的均值和标准差，gender 和 age 人口学）。

然后做推断统计。使用 bruceR::MANOVA() 或 afex::aov_4 进行 2(observed) × 2(action_type) 被试间方差分析：

1. **主假设**：estimated_solved_or_skipped ~ observed_manipulation * action_type，报告 F, p, η²
2. **简单效应**：分别在 action_type=Action 和 action_type=Inaction 条件下，对 estimated_solved_or_skipped 做独立样本 t 检验（Observed vs Unobserved）
3. **客观表现**：actual_solved ~ observed_manipulation * action_type 方差分析

所有内容写在一个R脚本里。根据可重复性检验指南做结果对比（描述性统计表2 + 推断性统计表5），结果四舍五入，表格存为md文档到 5.Reports/Study 5/。
