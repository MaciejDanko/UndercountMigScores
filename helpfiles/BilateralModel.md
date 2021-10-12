**The bilateral flows ratio model**

The undercount scores can be calculated using the bilateral migration data, by comparing the same type of flow reported by different countries. Precisely, it is realized by taking flows from a country X to a group of good data quality countries (the reference countries) reported by country X and divide it by the same flow reported by the the reference countries. Because the duration of stay may differ among countries the flows reported by each country should be somehow corrected. Here, as the default correction we use IMEM (Raymer et al. 2013) model coefficients for the duration of stay. This and other correction coefficients are listed in the table below.

<br>

<style>

th {
  border: 1px solid black;
  border-collapse: collapse;
  background-color: #CCBBFF;
  text-align: center;
}

td {
  border: 1px solid black;
  border-collapse: collapse;
}

tr:hover {background-color: #E0DDFF;}

table {
  border-collapse: collapse;
  width:95%;
}

</style>

<center>

|Duration of stay <br>in months | IMEM <br>model | Expert <br>judgement (\*) | Poisson <br>model (\*) | Mixture <br>model (\*) 
|:---:|:---:|:---:|:---:|:---:|
| <b>0</b> |  0.53   | 0.51  | 0.79  | 0.51  |
| <b>3</b> |  0.63   | 0.61  | 0.84  | 0.64  |
| <b>6</b> |  0.73   | 0.81  | 0.89  | 0.71  |
| <b>12</b> |  1.00   | 1.00  | 1.00  | 1.00 |
| <b>Permanent <br>(or 5 years)</b>  |  2.26   | 1.61 | 2.61  | 1.80  |
(\*) \- Willekens 2019

</center>
<br>

Formally, estimation of the undercounting ratio $U^E{X,Y,y}$ of emigration data between country $X$ and set of countries $Y$ in year $y$, can be calculated as follows:

$$
U^E_{X,Y,y} = \frac{\sum_c M\left(X_y\rightarrow Y_{c,y}, X_y\right) R_{X_y}}{\sum_{c} M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right) R_{Y_{c,y}}},
$$


where $M\left(X_y\rightarrow Y_{c,y}, X_y\right)$ is the emigration flow from country $X$ to country $Y_c$ reported by country $X$ in year $y$, $M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right)$ is the immigration flow from country $X$ to country $Y_c$ reported by country $Y_c$ in year $y$, $R_{X_y}$ is the IMEM correction for duration of stay of country $X$ in year $y$, and $R_{Y_{c,y}}$ is the IMEM correction for duration of stay of country $Y_c$ reported in year $y$). 


The undercounting ratio $U^I_{X,Y,y}$ of immigration data is calculated analogically:

$$
U^I_{X,Y,y} = \frac{\sum_c M\left(X_y\leftarrow Y_{c,y}, X_y\right) R_{X_y}}{\sum_{c} M\left(X_y\leftarrow Y_{c,y}, Y_{c,y}\right) R_{Y_{c,y}}},
$$

where $M\left(X_y\rightarrow Y_{c,y}, X_y\right)$ is the immigration flow to country $X$ from country $Y_c$ reported by country $X$ in year $y$, $M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right)$ is the emigration flow to country $X$ from country $Y_c$ reported by country $Y_c$ in year $y$,

There are two main disadvantages of the constructed undercounting measure:

- It cannot exclude the effect of under-coverage, i.e., the index measures a combined effect of undercounting and coverage problems.

- It may not completely exclude the effect of the duration of stay that is different than 12 months, i.e., IMEM estimates are taken from the model that does not include the most recent data and its coefficients are the same for immigration and emigration data.

**Median bilateral flows ratios and their confidence intervals**

<<< under construction >>>

The probability $P\left(X\rightarrow Y\right)$ of emigration form country $X$ to a group of countries $Y$ can be defined as
$$ P\left(X\rightarrow Y\right) = \frac{M\left(X\rightarrow Y\right)}{M\left(X\rightarrow \Omega\right)},$$
where $M\left(X\rightarrow \Omega\right)$ is the total emigration of country $X$. The probability $P\left(X\rightarrow Y\right)$ of immigration flow to country $X$ from country $Y$ is defined analogically. 

We can simulate the migration process by drawing the number of migration events $m\left(X\rightarrow Y\right)$ from the binomial distribution $B(n, p)$, where $n = M\left(X\rightarrow \Omega\right)$ is a number of trials and $p = P\left(X\rightarrow Y\right)$ is a probability of success on each trial, so that $\mathbb{E}(m) = M\left(X\rightarrow Y\right)$

As defined in previous section $U^E_{X,Y,y}$ and $U^I_{X,Y,y}$ are simply ratios of two migration processes. Each of these migration processes can be simulated independently as described above. 100,000 of simulations defines our bootstrap sample that is used to calculate the bootstrapped median, standard deviation, and the confidence intervals (percentile method, e.g., Efron and Tibshirani 1993).

The ratios and their confidence intervals are calculated for each year, however in the model we need aggregates the migration into groups

... to be continued ...

**References**

<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F., and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>

<a href="https://sciendo.com/article/10.2478/jos-2019-0011"> Willekens, F. (2019), 'Evidence-Based Monitoring of International Migration Flows in Europe'. Journal of Official Statistics, Vol.35 (Issue 1), pp. 231-277.</a>

<a href="https://books.google.de/books/about/An_Introduction_to_the_Bootstrap.html?id=gLlpIUxRntoC&redir_esc=y">Efron, B. and Tibshirani, R.J. (1993), 'An introduction to the bootstrap'. Taylor & Francis. ISBN: 0412-04231-2.</a>
