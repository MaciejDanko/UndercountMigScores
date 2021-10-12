### **Duration of stay correction**

When the option is different than "No correction" a correction is used to adjust migration flows for the duration of stay. The correction coefficients are taken from the Raymer et al. (2013) and Willekens (2019) papers:

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

<caption>Table 1. Duration of stay correction coefficients. (\*) \- Willekens 2019.</caption>
|Duration of stay <br>in months | IMEM <br>model | Expert <br>judgement (\*) | Poisson <br>model (\*) | Mixture <br>model (\*) 
|:---:|:---:|:---:|:---:|:---:|
| <b>0</b> |  0.53   | 0.51  | 0.79  | 0.51  |
| <b>3</b> |  0.63   | 0.61  | 0.84  | 0.64  |
| <b>6</b> |  0.73   | 0.81  | 0.89  | 0.71  |
| <b>12</b> |  1.00   | 1.00  | 1.00  | 1.00 |
| <b>Permanent <br>(or 5 years)</b>  |  2.26   | 1.61 | 2.61  | 1.80  | 

</center>

The values of the coefficients for durations of stay missing in the table are interpolated. 

See help (?) in "Overview" section for more information on how IMEM correction coefficients are used in the bilateral flows ratio model.

### **References**

<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F. and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>

<a href="https://sciendo.com/article/10.2478/jos-2019-0011"> Willekens, F. (2019), 'Evidence-Based Monitoring of International Migration Flows in Europe'. Journal of Official Statistics, Vol.35 (Issue 1), pp. 231-277.</a>
