# xmerit

The goal of `xmerit` is to facility teaching and researching on
Econometrics.

## Installation

You can install the development version of xmerit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("huhuaping/xmerit")
```

## Example

This is a basic example which shows you how to show math equation
(`LaTex` equation) easily in Rmarkdown Writing.

### Dataset and model

``` r
library(xmerit)
data(mtcars)
## basic example code

df <- mtcars
mod <- mpg ~ cyl + disp + wt +gear

xvars <- all.vars(mod)[-1]
yvars <- all.vars(mod)[1]

lm.fit <- lm(formula = mod, data = df)
summary(lm.fit)
#> 
#> Call:
#> lm(formula = mod, data = df)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.9159 -1.2484 -0.3566  1.4719  5.9253 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 43.539847   4.860059   8.959 1.42e-09 ***
#> cyl         -1.784296   0.613889  -2.907  0.00722 ** 
#> disp         0.006944   0.012007   0.578  0.56782    
#> wt          -3.792867   1.081819  -3.506  0.00161 ** 
#> gear        -0.490445   0.790285  -0.621  0.54007    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.624 on 27 degrees of freedom
#> Multiple R-squared:  0.835,  Adjusted R-squared:  0.8105 
#> F-statistic: 34.15 on 4 and 27 DF,  p-value: 3.36e-10
```

### xmerit::lx.psm

Here we will show the use of function
[`xmerit::lx.psm()`](https://huhuaping.github.io/xmerit/reference/lx.psm.md).

#### R chunk

``` r
lx.out <- xmerit::lx.psm(
  x = xvars, y = yvars,
  begin = 0,
  #greek.n = length(xvars)+1,
  n.row = 3,
  lm.label = "lx-psm",
  lm.tag = "lx.psm",
  no_dollar = FALSE)
```

#### source style

``` R
$$\begin{align}
\begin{split}
mpg_i=&+\beta_{0}+\beta_{1}cyl_i+\beta_{2}disp_i\\&+\beta_{3}wt_i+\beta_{4}gear_i+u_i
\end{split}
\quad \text{(lx.psm)}\quad
(\#eq:lx-psm)
\end{align}$$

The cross-reffer syntax of `see \@ref(eq:lx-psm)`  will be rendered as see \@ref(eq:lx-psm).
```

#### render effect

$$\begin{array}{r}
{\begin{aligned}
{mpg_{i} =} & {+ \beta_{0} + \beta_{1}cyl_{i} + \beta_{2}disp_{i}} \\
 & {+ \beta_{3}wt_{i} + \beta_{4}gear_{i} + u_{i}}
\end{aligned}\quad\text{(lx.psm)}\quad(\# eq:lx - psm)}
\end{array}$$

The cross-reference syntax of `see \@ref(eq:lx-psm)` will be rendered as
see @ref(eq:lx-psm).

### xmerit::lx.est

Now let me show the use of function
[`xmerit::lx.est()`](https://huhuaping.github.io/xmerit/reference/lx.est.md).

#### R chunk

``` r
lx.out <- xmerit::lx.est(
  lm.mod = mod, lm.dt = df,
  lm.n = 3, lm.label = "lx-est", 
  lm.tag = "lx.est",
  no_dollar = FALSE)
```

#### source style

``` R
$$\begin{equation}
\begin{alignedat}{999}
&\widehat{mpg}=&&+43.54&&-1.78cyl_i&&+0.01disp_i\\ 
&(s)&&(4.8601)&&(0.6139)&&(0.0120)\\ 
&(t)&&(+8.96)&&(-2.91)&&(+0.58)\\ 
&(cont.)&&-3.79wt_i&&-0.49gear_i &&\\ 
&(s)&&(1.0818)&&(0.7903) &&\\ 
&(t)&&(-3.51)&&(-0.62) &&
\end{alignedat}
\quad \text{(lx.est)}\quad
(\#eq:lx-est)
\end{equation}$$

The cross-reference syntax of `see \@ref(eq:lx-est)`  will be rendered as see \@ref(eq:lx-est).
```

#### render effect

$$\begin{aligned}
 & {\widehat{mpg} =} & & {+ 43.54} & & {- 1.78cyl_{i}} & & {+ 0.01disp_{i}} \\
 & (s) & & (4.8601) & & (0.6139) & & (0.0120) \\
 & (t) & & ( + 8.96) & & ( - 2.91) & & ( + 0.58) \\
 & (cont.) & & {- 3.79wt_{i}} & & {- 0.49gear_{i}} & & \\
 & (s) & & (1.0818) & & (0.7903) & & \\
 & (t) & & ( - 3.51) & & ( - 0.62) & & 
\end{aligned}\quad\text{(lx.est)}\quad(\# eq:lx - est)$$

The cross-reference syntax of `see \@ref(eq:lx-est)` will be rendered as
see @ref(eq:lx-est).
