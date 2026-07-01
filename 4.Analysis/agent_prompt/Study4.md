# Agent Prompt — Study 4 数据导入与预处理

让我们开始研究四的工作，新建一个Study 4的R脚本，先导入数据，利用haven包read_sav读取sav格式的Study 4，原始数据赋值到df，再用summary查看数据。然后用管道符%>%进行select选择列并重命名赋给df4：

- Team
- Member
- play_order_skill（Order：出场顺序，越大越强）
- earned_points（Point：得分）
- spectators_n（Adc：观众数）
- recalled_points（Recalled：回忆得分）
- perceived_contribution_1_10（Overclm：感知贡献 1-10）
- team_outcome（Outcome：-1=loss，1=win）
- tournament（Tnm：1=第一场，2=第二场）
- match_type（Game：1=Single，2=Double）
- gender（Gender：0=Female，1=Male）
- accuracy_recalled_minus_earned（Accuracy：Recalled - Point）
- team_size（Teammate：同队人数）

转为factor（分类变量）：
- team_outcome：-1=loss, 1=win
- gender：0=Female, 1=Male
- tournament、match_type

其余保留numeric。

# Agent Prompt — Study 4 推断统计 & 结果输出

先做描述统计（perceived_contribution_1_10、spectators_n、earned_points、play_order_skill 的均值和标准差，性别分布）。

然后做推断统计。使用 bruceR::regress() 进行线性回归：

1. **主效应模型**：`perceived_contribution_1_10 ~ spectators_n + team_outcome`，报告 B, t, p
2. **交互模型**：`perceived_contribution_1_10 ~ spectators_n * team_outcome`，报告 B, t, p
3. **相关性检验**：spectators_n 与 earned_points、spectators_n 与 play_order_skill 的 Pearson 相关
4. **协变量回归**：`perceived_contribution_1_10 ~ spectators_n + earned_points` 和 `perceived_contribution_1_10 ~ spectators_n + play_order_skill`

所有内容写在一个R脚本里。根据可重复性检验指南做结果对比（描述性统计表2 + 推断性统计表5），结果四舍五入，表格存为md文档到 5.Reports/Study 4/。对回归系数 B 和 t 计算 δ(%) 和评级。
