**The bilateral flows ratio model**

The undercount scores can be calculated using the bilateral migration data, by comparing the same flows reported by different countries. Precisely, it is realized by taking flows from a country X to a group of good data quality countries reported by country X and divide it by the same flow reported by the group of good data quality countries (the reference countries). Because the duration of stay may differs among countries the flows reported by each country should be somehow corrected. Here, we use Raymer's (2013) estimates for duration of stay as the duration of stay correction coefficients.

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

|        Duration of stay in months       |        Raymer's coefficient     |
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


where $M\left(X_y\rightarrow Y_{c,y}, X_y\right)$ is the emigration flow from country $X$ to country $Y_c$ reported by country $X$ in year $y$, $M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right)$ is the emigration flow from country $X$ to country $Y_c$ reported by country $Y_c$ in year $y$, $R_{X_y}$ is the Raymer's correction for duration of stay of country $X$ in year $y$, and $R_{Y_{c,y}}$ is Raymer's correction for duration of stay of country $Y_c$ reported in year $y$). The group of good data quality countries ($Y$) includes all Nordic countries, Switzerland, Nederland, Belgium, Austria, Germany, and Lichtenstein.


The undercounting ratio $U^I_{X,Y,y}$ of immigration data is calculated analogically:

$$
U^I_{X,Y,y} = \frac{\sum_c M\left(X_y\leftarrow Y_{c,y}, X_y\right) R_{X_y}}{\sum_{c} M\left(X_y\leftarrow Y_{c,y}, Y_{c,y}\right) R_{Y_{c,y}}},
$$

where $M\left(X_y\rightarrow Y_{c,y}, X_y\right)$ is the immigration flow to country $X$ from country $Y_c$ reported by country $X$ in year $y$, $M\left(X_y\rightarrow Y_{c,y}, Y_{c,y}\right)$ is the migration flow to country $X$ from country $Y_c$ reported by country $Y_c$ in year $y$,

Because, bilateral migration for some years are missing a simple interpolation (spline) and extrapolation (weighted mean of last observations) for tails was used. 

There are two main disadvantages of the constructed undercounting measure:

- It cannot exclude the effect of under-coverage, i.e., the index measures a combined effect of undercounting and coverage problems.

- It may not completely exclude the effect of the duration of stay that is different than 12 months, i.e., Raymer's estimates are taken from the model that does not include the most recent data and its coefficients are the same for immigration and emigration data.

**Confidence intervals of the bilateral flows ratio**

[ To be written]

**References**

<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F. and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>

