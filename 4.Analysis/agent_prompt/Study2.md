# Agent Prompt — Study 2 数据导入与预处理

让我们开始研究二的工作，新建一个Study 2的R脚本，先导入数据，利用haven包read_sav读取sav格式的Study 2，原始数据赋值到df，再用summary查看数据。然后用管道符%>%进行select选择列并重命名赋给df2：

- V1（ID）
- condition（1=observed，2=control）
- perceived_observation_1_9（Q26.0）
- perceived_looked_1_9（Q31.0）
- perceived_observed_composite（Q26.0+Q31.0 均值）
- perceived_grapes_n（Q30）
- perceived_almonds_n（Q33）
- perceived_mms_n（Q34）
- perceived_grapes_g（Q30×3）
- perceived_almonds_g（Q33×1.2）
- perceived_mms_g（Q34×0.8）
- Zgrapes
- Zalmonds
- Zmm
- zoverall_food
- actual_grapes_remaining_g（Q30.0）
- actual_almonds_remaining_g（Q35.0）
- actual_mms_remaining_g（Q36）
- actual_grapes_consumed_g（grapes_total）
- actual_almonds_consumed_g（almonds_total）
- actual_mms_consumed_g（mm_total）
- actual_overall_consumed_g（overall_grams_eaten）
- accuracy_grapes_g（grapes_accuracy）
- accuracy_almonds_g（almond_accuracy）
- accuracy_mms_g（mm_accuracy）
- accuracy_overall_g（overall_grams_inaccurate）
- food_portion_size_1_9（Q27）
- food_calorie_1_9（Q19）
- today_intake_contribution_1_9（Q20）
- food_enjoyment_level_1_9（Q18）
- amount_satisfaction_1_9（Q34.0）
- types_satisfaction_1_9（Q35）
- gender（Q6）
- age（Q8）
- English_native（Q12）

转为factor（分类变量）：
- condition：1=observed, 2=control
- gender：1=Male, 2=Female
- English_native：1=Yes, 2=No

其余保留numeric。

# Agent Prompt — Study 2 推断统计 & 结果输出

先做描述统计（perceived_observed_composite、zoverall_food、perceived_grapes_n、perceived_almonds_n、perceived_mms_n、actual_grapes_consumed_g、actual_almonds_consumed_g、actual_mms_consumed_g、accuracy_overall_g 的均值和标准差，gender 和 age 人口学）。

然后做推断统计。对以下变量分别进行独立样本 t 检验（observed vs control），使用 bruceR::TTEST() 进行检验，对缺失值 pairwise deletion，报告 t, p, 95% CI 和 η²（手动计算 η² = t² / (t²+df)）：

主假设与操纵检验：
- perceived_observed_composite（操纵检验）
- zoverall_food（主DV，z标准化均值）

各食物自报吃量：
- perceived_grapes_n
- perceived_almonds_n
- perceived_mms_n

实际吃量：
- actual_grapes_consumed_g
- actual_almonds_consumed_g
- actual_mms_consumed_g

准确性与填充项：
- accuracy_overall_g
- food_portion_size_1_9、food_calorie_1_9、today_intake_contribution_1_9、food_enjoyment_level_1_9、amount_satisfaction_1_9、types_satisfaction_1_9

所有内容写在一个R脚本里。根据可重复性检验指南做结果对比（描述性统计表2 + 推断性统计表5），结果四舍五入，表格存为md文档到 5.Reports/Study 2/。要求同 Study 1。
