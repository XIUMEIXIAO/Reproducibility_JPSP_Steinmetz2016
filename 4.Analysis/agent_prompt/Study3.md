# Agent Prompt — Study 3 数据导入与预处理

让我们开始研究三的工作，新建一个Study 3的R脚本，先导入数据，利用haven包read_sav读取sav格式的Study 3，原始数据赋值到df，再用summary查看数据。然后用管道符%>%进行select选择列并重命名，排除高于均值147个SD的极端值被试，赋给df3：

- V1（ID）
- condition（1=observed，2=alone）
- perceived_observation_1_9（A6.1）
- selfreport_gains（A4.2）
- selfreport_losses（A4.3）
- selfreport_total（A4.4）
- accuracy_gain（accuracy_gain）
- accuracy_lost（accuracy_lost）
- others_gains（A5.2）
- others_losses（A5.3）
- gender（A6.3）
- age（A6.4）
- English_native（A6.5）

转为factor（分类变量）：
- condition：1=observed, 2=alone
- gender：1=Male, 2=Female
- English_native：1=Yes, 2=No

其余保留numeric。

# Agent Prompt — Study 3 描述统计 & 结果对比

先做描述统计（perceived_observation_1_9、selfreport_gains、selfreport_losses、selfreport_total 的均值和标准差，性别和年龄）。

然后做推断统计：
1. 操纵检验：perceived_observation_1_9 独立样本 t 检验（observed vs alone），报告 t, p, 95% CI, η²
2. 主假设：先进行 2(observed vs alone) × 2(gains vs losses) 重复测量方差分析（afex::aov_4），报告 F, p, η²_g
3. 简单效应：selfreport_gains、selfreport_losses 分别做独立样本 t 检验，报告 t, p, 95% CI, η²
4. 总分：selfreport_total 独立样本 t 检验，报告 t, p, 95% CI, η²

所有 t 检验使用 bruceR::TTEST()，对缺失值 pairwise deletion，η² = t² / (t²+df)。

所有内容写在一个R脚本里。根据可重复性检验指南做结果对比（描述性统计表2 + 推断性统计表5），结果四舍五入，表格存为md文档到 5.Reports/Study 3/。

要求同 Study 1、2。
