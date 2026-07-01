# Agent Prompt — Study 1 数据导入与预处理

很好，让我们开始研究一的工作，新建一个Study1的R脚本，先导入数据，利用haven包read_sav读取sav格式的Study 1，原始数据赋值到df，再用summary查看数据。然后用管道符%>%进行select选择列并重命名赋给df1：

- V1（ID）
- condition
- Q27.0（food_portion_size_1_9）
- Q29（food_calorie_1_9）
- Q31（today_intake_contribution_1_9）
- Q33.0（food_enjoyment_level_1_9）
- Q39（food_consumed_amount_1_4）
- Q41（perceived_observation_1_9）
- Q30（remaining_food_weight_g）
- Q6（gender）
- Q8（age）
- Q12（English_native）

Select & rename和转类型放在同一步骤：

转为factor（分类变量）：
- condition：1=camera，2=no_camera
- gender：1=Male, 2=Female
- English_native：1=Yes, 2=No

保留numeric（连续/量表变量）：
- food_portion_size_1_9：1-9
- food_calorie_1_9：1-9
- today_intake_contribution_1_9：1-9
- food_enjoyment_level_1_9：1-9
- perceived_observation_1_9：1-9
- food_consumed_amount_1_4：1-4（t检验需要）
- remaining_food_weight_g：连续
- age：连续

# Agent Prompt — Study 1 描述统计

现在开始做描述统计的部分，计算两种条件下 perceived_observation_1_9、food_portion_size_1_9、remaining_food_weight_g 的均值和标准差，性别的均值、标准差和人数。

# Agent Prompt — Study 1 推断统计

然后做推断统计，对 perceived_observation_1_9、food_portion_size_1_9、remaining_food_weight_g、food_calorie_1_9 / today_intake_contribution_1_9 / food_enjoyment_level_1_9、food_consumed_amount_1_4 进行独立样本 t 检验（camera vs no_camera）。使用 bruceR::TTEST() 进行检验，对缺失值 pairwise deletion，报告 t, p, 95% CI 和 η²（手动计算 η² = t² / (t²+df)）。

# Agent Prompt — Study 1 结果输出

所有内容（导入+描述+推断+表格）写在一个R脚本里。根据可重复性检验指南做结果对比（描述性统计表2 + 推断性统计表5），结果四舍五入，表格存为md文档到 5.Reports/Study 1/。
