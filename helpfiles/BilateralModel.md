## **The bilateral flows ratio model**

### **General model assumptions**

The bilateral migration flow ratios are constructed by taking a flow from country $X$ to a group of countries with high-quality data reported by country $X$ and dividing it by the same flow reported by the reference countries. As a default set of the reference countries $Y$, we have selected the Nordic countries (Denmark, Finland, Sweden, Norway, and Iceland), Belgium, the Netherlands, and Switzerland. These countries have been widely recognized by experts for having high-quality data. For example, these countries have been identified by both the IMEM and QuantMig projects as having low or very low levels of undercounting. Additionally, as indicated by our model, they exhibit the lowest levels of undercounting when compared to various reference country sets. Because the minimum duration of stay used in the definition of international migration may differ from country to country, the flows reported by each country need to be adjusted accordingly.

Formally, the undercounting ratio $U^E_{X,Y,t}$ for emigration data between country $X$ and set of countries $Y$ in year $t$, can be defined as follows: 

$$ U^E_{X,Y,t} = \frac{\sum_c M\left(X_t\rightarrow Y_{c,t}, X_t\right) R_{X_t}}{\sum_{c} M\left(X_t\rightarrow Y_{c,t}, Y_{c,t}\right) R_{Y_{c,t}}},$$ 

where $M\left(X_t\rightarrow Y_{c,t}, X_t\right)$ is the emigration flow from country $X$ to country $Y_c$ reported by country $X$ in year $t$, $M\left(X_t\rightarrow Y_{c,t}, Y_{c,t}\right)$ is the immigration flow from country $X$ to country $Y_c$ reported by country $Y_c$ in year $t$, $R_{X_t}$ is the correction for duration of stay of country $X$ in year $t$, and $R_{Y_{c,t}}$ is the correction for duration of stay of country $Y_c$ reported in year $t$. The immigration bilateral flows ratios are calculated analogically. The correction reduces flows for stays of less than 12 months and increases them for permanent stays.

### **Duration of stay correction coefficients**

The correction coefficients, denoted by $R$, are a set of parameters that increase monotonically with the duration of stay. These parameters are obtained through a process of constrained optimization aimed at minimizing undercounting across all European countries from 2002 to 2019. Specifically, the $R$ coefficients for each duration class (zero, three, and six months and permanent) are represented on a cumulative scale and estimated using the L-BFGS-M algorithm, which is a modification of the BFGS quasi-Newton method (Byrd et al. 1995). To address local minima problems, we use a robust approach that involves starting from multiple random points. For countries with durations other than those mentioned above, linear interpolation is employed. For countries with a duration of stay of 12 months, the correction coefficient is set to one.

By default, the optimization process is performed using the squared difference between the reported flows of a given country $X$ and the flows reported by countries with high-quality data $Y_c$. For emigration data, the objective function used in the optimization is defined as follows:

$$ H^E (R) = \sum_{X,Y,t} (\sum_{c} M\left(X_t\rightarrow Y_{c,t}, X_t\right) R_{X_t} - \sum_{c} M\left(X_t\rightarrow Y_{c,t}, Y_{c,t}\right) R_{Y_{c,t}} )^2 $$

The Shiny app also offers the option to use |log $U$| (where $U$ is defined above) as an objective function, but the default option (shown above) is considered more suitable for our purposes as it is more sensitive to the magnitude of differences between flows. The correction coefficients can be estimated jointly (which is the default in the app) or separately for each type of migration. Alternatively, the correction coefficients can be obtained directly from previous models, such as the IMEM model (Raymer et al. 2013) or other models summarized by Willekens (2019).

### **Bilateral flows ratios and their confidence intervals for year-specific model**

The probability $P^E_{X,Y,y}$ of emigration from country $X$ to a group of countries $Y$ in year $y$ can be defined as:

$$ P^E_{X,Y,y} = \frac{M(X_y \rightarrow Y_y)}{M(X_y \rightarrow \Omega_y)}, $$

where $M(X_y \rightarrow \Omega_y)$ is the total emigration count from country $X$. Similarly, the probability $P^I_{X,Y,y}$ of immigration flow to country $X$ from country $Y$ is defined analogously.

We can simulate the migration process by drawing the number of migration events $m(X_y \rightarrow Y_y)$ in year $y$ from a binomial distribution $B(n, p)$, where $n$ is the number of trials, given by $M(X_y \rightarrow \Omega)$, and $p$ is the probability of success on each trial, given by $P_{X,Y,y}$. Therefore, the expected value of $m$ is $\mathbb{E}(m) = M(X_y \rightarrow Y_y)$.

As defined in the previous section, $U^E_{X,Y,y}$ and $U^I_{X,Y,y}$ are ratios of two independent migration processes. Consequently, each of these migration processes can be simulated independently as described above. By performing 100,000 simulations, we obtain a bootstrap sample that is used to calculate bootstrapped 95% confidence intervals (using the percentile method, e.g., Efron and Tibshirani, 1993) and standard deviations of the ratio.

### **Obtaining normalized undercounting scores**

Since bilateral flow ratios $U$ (quotients, see equation above) are placed on the multiplicative scale, it is convenient to refer to their logarithms. The logarithm of bilateral flow ratios adjusted for the duration of stay ranges from - $\infty$ to + $\infty$. Values greater than zero are considered overcounted, zero denotes no undercounting or overcounting, and negative values represent undercounting. These values are then projected to a discretized 0---1 scale (e.g., for a classification with five categories, the values we have chosen are zero, 0.25, 0.5, 0.75, and one), as these thresholds are particularly useful in the next steps of our model. For flexibility, we use categorizations based on evenly spaced thresholds or quantiles (default in our Shiny app).

The projection is not trivial as overcounting problems often arise. Generally, overcounting occurs when the reported migration flows exceed the actual number of migrants who leave or enter a country, which may happen for various reasons, such as double counting, reporting errors, irregular migration, or coverage differences. In our case, overcounting arises if a country reports more migration than the reference group (log $U > 0$). However, it is unlikely that a pure overcounting class exists, as none of the countries have perfect data quality. As overcounting and undercounting issues may occur at the same time, disentangling them can be challenging.

To address this issue, we propose two options. The first option is to combine the overcounting class with the lowest undercounting class, which is the default setting in our Shiny app. The second option is to treat overcounting as a separate class. The results can be directly used to classify countries in migration models, or, after numerical representation (as described above), combined with expert opinion and metadata scores.

### **Combining multiple source of information on undercounting**

The proposed procedure for combining undercounting from different sources requires scoring; i.e., a numerical representation of the classifications in the range $(0,1)$. Scores obtained from various sources are then combined by using a weighted average. By default, we have chosen a set of weights that reflect our subjective assessment of the relative importance of each factor: 20% for the expert opinion scores, 10% for the metadata scores and 70% for the model scores. We have chosen relatively small weights for metadata scores because of their limited availability. Future work in this area may collect more precise metadata directly from the NSIs, which can lead to the updating of these weights. Currently, our Shiny app offers users considerable flexibility in setting the weights and testing the sensitivity of the resulting scores to these assumptions.

As was previously mentioned (in the expert opinions section), after 2007, there was a significant change in the processes of migration data collection and organization in the European Union following the implementation of Regulation (EC) 862/2007\cite{EC_regulation_2007} which harmonized the definition of the duration of stay for all EU countries. Furthermore, migration statistics could have been retrospectively updated by using the 2011 round of censuses. In addition, the IMEM and the QuantMig expert opinions refer to separate time periods (2002---2008 and 2009---2019, respectively). For this reason, we have split the contributions of the IMEM and QuantMig expert opinions, metadata, and models into two periods. This cut-off is defined by a flexible threshold parameter (set to 2009).

### **References**

<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F., and Bijak, J. (2013), 'Integrated Modeling of European Migration', Journal of the American Statistical Association 108(503), 801--819.</a>

<a href="https://sciendo.com/article/10.2478/jos-2019-0011"> Willekens, F. (2019), 'Evidence-Based Monitoring of International Migration Flows in Europe'. Journal of Official Statistics, Vol.35 (Issue 1), pp. 231-277.</a>

<a href="https://books.google.de/books/about/An_Introduction_to_the_Bootstrap.html?id=gLlpIUxRntoC&redir_esc=y">Efron, B. and Tibshirani, R.J. (1993), 'An introduction to the bootstrap'. Taylor & Francis. ISBN: 0412-04231-2.</a>

<a href="https://epubs.siam.org/doi/10.1137/0916069"> Byrd, R. H., Lu, P., Nocedal, J. and Zhu, C. (1995). A limited memory algorithm for bound constrained optimization. SIAM Journal on Scientific Computing, 16, 1190--1208. <doi:10.1137/0916069>.</a>
