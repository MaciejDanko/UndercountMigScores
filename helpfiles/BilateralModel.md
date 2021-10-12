**The bilateral flows ratio model**

The undercount scores can be calculated using the bilateral migration data, by comparing the same type of flow reported by different countries. Precisely, it is realized by taking flows from a country X to a group of good data quality countries (the reference countries) reported by country X and divide it by the same flow reported by the group of good data quality countries. Because the duration of stay may differ among countries the flows reported by each country should be somehow corrected. Here, as the correction we use IMEM (2013) model coefficients for the duration of stay.

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
  width:80%;
}

</style>

<center>

|        Duration of stay in months       |        IMEM coefficient     |
|:---------------------------------------:|:-------------------------------:|
|                     0                   |              0.53               |
|                     3                   |              0.63               |
|                     6                   |              0.73               |
|                    12                   |              1.00               |
|                Permanent                |              2.26               |

</center>
<br>

Formally, estimation of the undercounting ratio $U^E{X,Y,y}$ of emigration data between country $X$ and set of countries $Y$ in year $y$, can be calculated as follows:

$$
U^E_{X,Y,y} = \frac{\sum_c M\left(X_y\rightarrow Y_{c,y}, X_y\right) R_{X_y}}{\sum_{c} M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right) R_{Y_{c,y}}},
$$


where $M\left(X_y\rightarrow Y_{c,y}, X_y\right)$ is the emigration flow from country $X$ to country $Y_c$ reported by country $X$ in year $y$, $M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right)$ is the emigration flow from country $X$ to country $Y_c$ reported by country $Y_c$ in year $y$, $R_{X_y}$ is the IMEM correction for duration of stay of country $X$ in year $y$, and $R_{Y_{c,y}}$ is IMEM correction for duration of stay of country $Y_c$ reported in year $y$). The group of good data quality countries ($Y$) includes all Nordic countries, Switzerland, Nederland, Belgium, Austria, Germany, and Lichtenstein.


The undercounting ratio $U^I_{X,Y,y}$ of immigration data is calculated analogically:

$$
U^I_{X,Y,y} = \frac{\sum_c M\left(X_y\leftarrow Y_{c,y}, X_y\right) R_{X_y}}{\sum_{c} M\left(X_y\leftarrow Y_{c,y}, Y_{c,y}\right) R_{Y_{c,y}}},
$$

where $M\left(X_y\rightarrow Y_{c,y}, X_y\right)$ is the immigration flow to country $X$ from country $Y_c$ reported by country $X$ in year $y$, $M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right)$ is the migration flow to country $X$ from country $Y_c$ reported by country $Y_c$ in year $y$,

There are two main disadvantages of the constructed undercounting measure:

- It cannot exclude the effect of under-coverage, i.e., the index measures a combined effect of undercounting and coverage problems.

- It may not completely exclude the effect of the duration of stay that is different than 12 months, i.e., IMEM estimates are taken from the model that does not include the most recent data and its coefficients are the same for immigration and emigration data.

<check equations above for the IMEMs correction>

**Median bilateral flows ratios and their confidence intervals**

[ To be written]

The probability $P\left(X\rightarrow Y\right)$ of emigration form country $X$ to a group of countries $Y$ can be expressed as
$$ P\left(X\rightarrow Y\right) = \frac{M\left(X\rightarrow Y\right)}{M\left(X\rightarrow \Omega\right)}$$

The number of emigration events $m\left(X\rightarrow Y\right)$ can be drawn from binomial distribution $B(N, P)$, where $N$ is a number of trials and $P$ is a probability of success on each trial. 

**References**

<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F. and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>

