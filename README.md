# effecttree

This compendium contains helper functions to use the Mantel-Haenszel odds ratio effect size measure in Rasch trees and the partial gamma coefficient in partial credit trees to evaluate the magnitude of DIF and/or DSF.

It can be installed like an R package using devtools: 
``` r
devtools::install_github("mirka-henninger/effecttree")
library(effecttree)
```

It uses functions from the [psychotree](https://github.com/cran/psychotree/) and [partykit](https://github.com/cran/partykit) packages to fit the rasch tree and partial credit tree in the recursive partitioning framework. This repository adds to these packages by integrating effect size measures (Mantel-Haenszel odds ratio, partial gamma coefficient) for differential item functioning and differential step functioning. These effect size measures can be added to the trees, allow researchers to display effect sizes for each split, to reverse splits when effect sizes are negligible, and to color items in each end node based on the effect size measure. Additional helper functions are provided to extract the effect size measures from the tree object. This repository is currently under development and undergoes ongoing changes and improvement. Please report any bugs that you encounter. 

``` r
## example dichotomous items: 
data("SPISA", package = "psychotree")
RT <- raschtree(spisa ~ gender + age + semester, data = SPISA)
RT_eff <- add_effectsize(RT, purification = "iterative", p.adj = "fdr", reverse_splits = FALSE, direction = "topdown")
RT_eff$info$effectsize
plot(RT_eff, color_by_node = 1)
plot(RT_eff, color_by_node = 3)
```

## References
Barbiero, A., & Hitaj, A. (2020). Goodman and Kruskal’s gamma coefficient for ordinalized bivariate normal distributions. Psychometrika, 85(4), 905–925. https://doi.org/10.1007/s11336-020-09730-5

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to multiple testing. Journal of the Royal Statistical Society. Series B: Statistical Methodology, 57, 289–300. http://www.jstor.com/stable/2346101

Bjorner, J. B., Kreiner, S., Ware, J. E., Damsgaard, M. T., & Bech, P. (1998). Differential item functioning in the Danish translation of the SF-36. Journal of Clinical Epidemiology, 51, 1189–1202. https://doi.org/10. 1016/S0895-4356(98)00111-5

Davis, J. A. (1967). A partial coefficient for Goodman and Kruskal’s gamma. Journal of the American Statistical Association, 62, 189–193. https: //www.jstor.org/stable/2282922

Glas, C. A. W., & Verhelst, N. D. (1995). Testing the Rasch model. In G. H. Fischer & I. W. Molenaar (Eds.), Rasch models: Foundations, recent developments, and applications (pp. 69–95). Springer. https://doi.org/10.1007/978-1-4612-4230-7_5

Henninger, M., Debelak, R., & Strobl, C. (2023). A new stopping criterion for Rasch trees based on the Mantel-Haenszel effect size measure for differential item functioning. Educational Psychological Measurement, 83, 181-212. doi:10.1177/ 00131644221077135.

Holland, P. W., & Thayer, D. T. (1985). An alternate definition of the ETS delta scale of item difficulty. 85, 85–64. https://doi.org/10.1002/j.2330-8516.1985.tb00128

Holland, P. W., & Thayer, D. T. (1986). Differential item functioning and the Mantel-Haenszel procedure. Program Statistics Research Technical Report No. 86-69, 1–24. https://doi.org/10.1002/j.2330-8516.1986.tb00186.x

Hothorn, T., & Zeileis, A. (2015). partykit: A modular toolkit for recursive partioning in {R}. Journal of Machine Learning Research, 16, 3905–3909. http://jmlr.org/papers/v16/hothorn15a.html

Komboz, B., Strobl, C., & Zeileis, A. (2018). Tree-based global model tests for polytomous Rasch models. Educational and Psychological Measure- ment, 78, 128–166. https://doi.org/10.1177/0013164416664394

Magis, D., Béland, S., Tuerlinckx, F., & De Boeck, P. (2010). A general framework and an R package for the detection of dichotomous differential item functioning. Behavior Research Methods, 42, 847–862. https://doi.org/10.3758/BRM.42.3.847

Mantel, N. (1963). Chi-Square tests with one degree of freedom: Extensions of the Mantel-Haenszel procedure. Journal of the American Statistical Association, 58(303), 690–700. https://doi.org/10.1080/01621459.1963.10500879

Paek, I., & Holland, P. W. (2015). A note on statistiscal hypothesis testing based on log transformation of the Mantel-Haenszel common odds ratio for differential item functioning classification. Psychometrika, 80, 406–411. https://doi.org/10.1007/s11336-013-9394-5

Phillips, A., & Holland, P. W. (1987). Estimators of the variance of the Mantel-Haenszel log-odds-ratio estimate. Biometrics, 43, 425–431. https://doi.org/10.2307/2531824

Steinberg, L., & Thissen, D. (2006). Using effect sizes for research reporting: Examples using item response theory to analyze differential item functioning. Psychological Methods, 11(4), 402–415. https://doi.org/10.1037/1082-989X.11.4.402

Strobl, C., Kopf, J., & Zeileis, A. (2015). Rasch trees: A new method for detecting differential item functioning in the Rasch model. Psychometrika, 80, 289–316. https://doi.org/10.1007/s11336-013-9388-3

Zwick, R. (2012). A review of ETS differential item functioning assessment procedures: Flagging rules, minimum sample size requirements, and criterion refinement. In ETS Research Report Series. https://doi.org/10.1002/j.2333-8504.2012.tb02290.x
