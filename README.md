最近有关对风险的兴趣，我别用机器学习的数据分类方法，想应用最优贝叶斯分类器的。但是，我希望分类高维数的数据的。还有，因有对风险的兴趣，我想练习一下如何做有参数的分布拟合。但是，我们知道在不少状况下（比如，人的脸的表面）我们没有一个合适数据的参数分布，所以想用非参数统计分布。其实，我最近看的有关在风险分析被应用的分布的书的作者说我们应该用参数分布只在一下的4个状况下：1）根据理论这个数据应该服从一个分布，2）没有理论承认它，但是普遍承认一个随机变量服从一个分布，3）分布是个专家意见的好模型，并没有很高的准确性的需要，4）希望用长尾（超过极值/观测值）的分布。这些状况写以外的状况下作者推荐用非参数分布。那么，我做了这样的计划：

1. 先学习如何用R做高维随机向量。

2. 学习如何用R做主成分分析。

3. 学习如何用R做分布拟合。

4. 学习如何用R做非参数分布求出。

5. 应用简单的贝叶斯判定规则。

看看结果在 main.R 文件里。
