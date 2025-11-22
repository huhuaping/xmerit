# vignette

``` r
library(xmerit)
#> Warning: replacing previous import 'stats::filter' by 'dplyr::filter' when
#> loading 'xmerit'
#> Warning: replacing previous import 'stats::lag' by 'dplyr::lag' when loading
#> 'xmerit'
```

## The LaTex Equation hangdling and rendering

要求在多种输出格式下，统一代码风格地进行`LaTex`公式显示，这是一项具有挑战性的工作。尤其是对于复杂数学公式，其中的细节、区别和难度都会快速地放大到令人惊异的程度。

这里我们进行综合性的比较，并得到当前可行的解决路径和方案。

### Basic Knowlege about LaTex Equation

#### Equation environment

（1）对于传统的`LaTex`写作用户，主要输出格式为.pdf。而LaTex对公式环境
（equation
environments）做了多样化的定义和分类，如`\begin{align} ... \end{align}`；`\begin{aligned} ... \end{aligned}`等。

    \begin{align}
    a  = b + c
    \end{align} 

（2）对于当代的`Markdown`写作用户，天然要求多格式输出（.ppdf、.html、.docx）。`.Rmd`
文档和`.qmd`文档都默认以双美元符号（`$$...$$`）对定义数学`公式代码块`（equation
block）。

    $$
    a  = b + c + 5
    $$

#### Cross-reference and numbering

**公式引用**（cross-reference）的前提是正确地设定公式代码中的标签（label）指令，例如`\label{eq1}`。

**公式标号**（numbering）分为手动标号和自动标号。手动标号需要设定公式代码中手动标号（tag）指令，例如`\tag{1}`。要实现公式自动标号，则需要考虑具体适用场景，例如：写作文档`.qmd`还是`.rmd`？输出格式`html`、`pdf`还是`docx`？同样输出`docx`，采用不同R包`bookdown::word2_document()`、还是`Rmarkdown::word_document()`？

#### Complicate LaTex Equation

`LaTex`语言能很好定义复杂数学公式及版式风格

##### Nested Equation

（1）`\begin{split} ... \end{split}`嵌套于`\begin{aligned} ... \end{aligned}`

    \begin{aligned}
      \begin{split}
    a & = 0.01 b + 4c \\
    c & = 1.4321 d + 2.22 e
      \end{split} 
      \text{（方程1）}
      \\
      \begin{split}
      m & =  b + c +d \\
      p & =  d + e
      \end{split}
      \text{（方程2）}
    \end{aligned}

$$\begin{array}{r}
{\begin{aligned}
a & {= 0.01b + 4c} \\
c & {= 1.4321d + 2.22e}
\end{aligned}\text{（方程1）}} \\
{\begin{aligned}
m & {= b + c + d} \\
p & {= d + e}
\end{aligned}\text{（方程2）}}
\end{array}$$

##### Align/arrange style

（1）版式风格`\begin{align} ... \end{align}`：

    \begin{align}
    a & = 0.01 b + 4c  \\
    c & = 1.4321 d + 2.22 e
    \end{align}

$$\begin{aligned}
a & {= 0.01b + 4c} \\
c & {= 1.4321d + 2.22e}
\end{aligned}$$

（2）版式风格`\begin{alignedat}{number} ... \end{alignedat}`：

    \begin{alignedat}{999}
    &e_t^2=&& + \alpha_{1} && + \alpha_{2} lquan&& 
       + \alpha_{3} mon&& + \alpha_{4} tue\\
       & && + \alpha_{5} wed&& + \alpha_{6} thu&& 
       + \alpha_{7} stormy&& + \alpha_{8} cold\\
       & && + \alpha_{9} change&& + \alpha_{10} (lquan)^2&&+v_t\\
    \end{alignedat}

$$\begin{aligned}
 & {e_{t}^{2} =} & & {+ \alpha_{1}} & & {+ \alpha_{2}lquan} & & {+ \alpha_{3}mon} & & {+ \alpha_{4}tue} \\
 & & & {+ \alpha_{5}wed} & & {+ \alpha_{6}thu} & & {+ \alpha_{7}stormy} & & {+ \alpha_{8}cold} \\
 & & & {+ \alpha_{9}change} & & {+ \alpha_{10}(lquan)^{2}} & & {+ v_{t}} & & \\
 & & & & & & & & & 
\end{aligned}$$

    \begin{alignedat}{999}
    &\widehat{lprice}=&&+0.65&&-0.10lquan_i&&-0.08mon_i&&-0.08tue_i\\ 
    &(s)&&(0.4408)&&(0.0501)&&(0.1042)&&(0.1048)\\ 
    &(t)&&(+1.48)&&(-1.95)&&(-0.80)&&(-0.80)\\ 
    &(cont.)&&-0.07wed_i&&+0.05thu_i&&+0.29stormy_i&&+0.09cold_i\\ 
    &(s)&&(0.1077)&&(0.1008)&&(0.0815)&&(0.0713)\\ 
    &(t)&&(-0.68)&&(+0.53)&&(+3.60)&&(+1.21)\\ 
    &(cont.)&&-0.15change_i && && &&\\ 
    &(s)&&(0.0738) && && &&\\ 
    &(t)&&(-2.00) && && &&
    \end{alignedat}

$$\begin{aligned}
 & {\widehat{lprice} =} & & {+ 0.65} & & {- 0.10lquan_{i}} & & {- 0.08mon_{i}} & & {- 0.08tue_{i}} \\
 & (s) & & (0.4408) & & (0.0501) & & (0.1042) & & (0.1048) \\
 & (t) & & ( + 1.48) & & ( - 1.95) & & ( - 0.80) & & ( - 0.80) \\
 & (cont.) & & {- 0.07wed_{i}} & & {+ 0.05thu_{i}} & & {+ 0.29stormy_{i}} & & {+ 0.09cold_{i}} \\
 & (s) & & (0.1077) & & (0.1008) & & (0.0815) & & (0.0713) \\
 & (t) & & ( - 0.68) & & ( + 0.53) & & ( + 3.60) & & ( + 1.21) \\
 & (cont.) & & {- 0.15change_{i}} & & & & & & \\
 & (s) & & (0.0738) & & & & & & \\
 & (t) & & ( - 2.00) & & & & & & 
\end{aligned}$$

### Writing tools and habits

#### `MathJax` version and Writing tools

``` r
url_rmd <- "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
url_qmd <- "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"

v_rmd <- read_html(url_rmd) %>%
  html_text() %>%
  str_extract(., "(?<=MathJax.version=).*(?=;MathJax.fileversion)") %>%
  str_extract(., "\\d\\.\\d\\.\\d{1,2}")

v_qmd <- read_html(url_qmd) %>%
  html_text() %>%
  str_extract(., "(?<=mathjaxVersion=).*(?=,r.url)")%>%
  str_extract(., "\\d\\.\\d\\.\\d{1,2}")
```

``` r
v_qmd <- "3.2.1"
v_rmd <- "2.7.2"
```

对于数学公式的支持，RStudio的 `.Rmd`
文档和Quarto的`.qmd`文档都默认调用`MathJax`进行公示渲染和呈现。

二者对于`MathJax`版本及调用是不同的：

- RStudio的 `.Rmd` 文档默认调用的是`MathJax`
  2.7.2版本。而`MathJax v2.7`发布于 2017，
  年这是一个相当陈旧的版本。如何查看RStudio调用`MathJax`哪个版本？只需要浏览器打开链接<https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML>即可查看其版本（搜索关键字”MathJax.version”）。

- Quarto的`.qmd`文档默认调用的是`MathJax`
  3.2.1版本。浏览器打开js链接<https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js>即可查看其版本（搜索关键字”mathjaxVersion”）。

#### Traditional `LaTex` writing

传统`LaTex`写作者会严格遵从Latex语法规则：

- 添加标签命令：在公式代码块内，添加公式标签的语法，例如`a = b + c \label{eq1}`

- 正文引用公式：引用公式的标准语法包括：`\eqref{}`或`\ref{}`。

这里显示的是Quarto写作 `.qmd`渲染为html格式的效果：

    \begin{align}
    a  = b + c \label{eq1}
    \end{align} 

$$\begin{array}{r}
{a = b + c}
\end{array}$$

引用公式语法`\eqref{eq1}`显示为；或引用公式语法`\ref{eq1}`，显示为

> **注意**：因为这里是`.qmd`渲染为html格式，因此不会正确显示公式标号和公式引用。具体原因后面会进一步解释。

#### Modern `Markdown` writing

当代`Markdown`写作者会遵从Quarto官方的说明文档语法书写公式：

- 添加标签命令：在公式代码块的双美元符号对外使用`{#eq-}`语法命令，添加公式标签，例如`$$a = b + c $$ {#eq-add}`

- 正文引用公式：引用公式的标准语法为`@eq-`，例如`@eq-add`。

    $$
    a  = b + c + 5
    $$ {#eq-add}

$$a = b + c + 5$$ {#eq-add}

引用公式语法`@eq-add`显示为 @eq-add。

#### Hybrid syntax writing

**混合语法**写作者可能不严格遵守上述任何一个语法体系，而是更加关注便利性和最终显示效果。

    $$
    \begin{align}
    a & = b + c \label{eq2} \\
    c & = d + e \label{eq3}
    \end{align}
    $$

$$\begin{aligned}
a & {= b + c} \\
c & {= d + e}
\end{aligned}$$

- 混用情形1：双美元符号对`$$...$$`内部蕴含了典型的Latex语法`\label{}`。

- 公式引用效果1：引用方法`\eqref{eq2}`，显示。

- 副作用1：a.以上语法在`.qmd`文档内不能正常预览该公式。b.在html输出下，引用语法`\eqref{eq2}`无法显示，且两个子方程不会自动标号（但在pdf输出下能正常显示，具体可参看节[pdf测试](https://huhuaping.github.io/xmerit/articles/equation-test/formats-pdf.qmd#sec-lx-label-eqref)
  ）。

以下的混用情形2是禁止使用的：

    $$
    \begin{align}
    a & = b + c +4 \label{eq4} \\
    c & = d + e +5 \label{eq5}
    \end{align}
    $$  {#eq-hybrid}

$$\begin{aligned}
a & {= b + c} \\
c & {= d + e}
\end{aligned}$$ {#eq-hybrid}

- 混用情形2：双美元符号对`$$...$$`结合`#eq-hybrid`标签，但是内部蕴含了典型的Latex语法`\label{}`。

- 公式引用效果2：引用方法`\eqref{eq2}`，显示。引用方法`@eq-hybrid`，显示
  @eq-hybrid。

- 副作用2：a.以上语法在`.qmd`文档内不能正常预览该公式。b.在html输出下，引用语法`\eqref{eq2}`无法显示。c.无论是html还是pdf输出，都不会显示公式！html输出会仅仅显示源代码，而pdf输出则会直接报错无法渲染！！

### For Rmarkdown/bookdown/.Rmd Writing

#### Basic Syntax

`bookdown`解决的`Rmarkdown`生态下公式书写中自动标号和交叉引用的问题。

对于`.docx`格式输出，`bookdown`下公式语法规则如下：

- 添加公式标签（label）：`(\#eq:your-label)`

- 公式引用（cross-reference）：正文中`\@ref(eq:your-label)`

- 公式自动标号（numbering）：为确保公式自动标号，请**不要**使用双美元符号对`$$...$$`环境

> **经验规则**：`bookdown::word_document2`输出`.docx`使用情境下，任何时候都不建议使用双美元符号对`$$...$$`环境。

#### Rules of Nested environment

经测试，如下嵌套环境规则是受到支持的（也正是自定义R包`xmerit`（版本`0.0.11`）目前所采用的语法规则）：

- 支持的有效单一LaTex公式环境（environment）：

a）`equation`环境

    \begin{equation}
      ...
    \end{equation}

b）`align`环境

    \begin{align}
      ...
    \end{align}

- 支持的有效嵌套LaTex公式环境（environment）：

> a）局部对齐排列的嵌套环境

    \begin{align} 
      \begin{split}
       ... 
      \end{split} 
    \end{align}

> b）完全对齐排列的嵌套环境

    \begin{equation} 
      \begin{alignedat}{999}
       ... 
      \end{alignedat} 
    \end{equation}

#### `xmerit` function and syntax

##### Dataset and model

``` r
library(xmerit)

data(mtcars)
df <- mtcars
mod <- mpg ~ cyl + disp + wt +gear
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

xvars <- all.vars(mod)[-1]
yvars <- all.vars(mod)[1]
```

##### `xmerit::lx.psm` : align+split

[`xmerit::lx.psm()`](https://huhuaping.github.io/xmerit/reference/lx.psm.md)函数默认形式为嵌套结构`\begin{align}`内含`\begin{split}`。

``` r
lx.out <- xmerit::lx.psm(
  x = xvars, y = yvars,
  begin = 0,
  #greek.n = length(xvars)+1,
  n.row = 3,
  lm.label = "lx-psm",
  lm.tag = "(lx.psm)",
  no_dollar = TRUE)
```

    \begin{align}
    \begin{split}
    mpg_i=&+\beta_{0}+\beta_{1}cyl_i+\beta_{2}disp_i\\&+\beta_{3}wt_i+\beta_{4}gear_i+u_i
    \end{split}
    \quad \text{(lx.psm)}\quad
    (\#eq:lx-psm)
    \end{align}

    引用语法 `\@ref(eq:lx-psm)`显示为 \@ref(eq:lx-psm)。

$$\begin{array}{r}
{\begin{aligned}
{mpg_{i} =} & {+ \beta_{0} + \beta_{1}cyl_{i} + \beta_{2}disp_{i}} \\
 & {+ \beta_{3}wt_{i} + \beta_{4}gear_{i} + u_{i}}
\end{aligned}\quad\text{(lx.psm)}\quad(\# eq:lx - psm)}
\end{array}$$

引用语法 `\@ref(eq:lx-psm)`显示为 @ref(eq:lx-psm)。

##### `xmerit::lx.est`: equation + alignedat

[`xmerit::lx.est()`](https://huhuaping.github.io/xmerit/reference/lx.est.md)函数默认形式为嵌套结构`\begin{equation}`内含`\begin{alignedat}{999}`。

``` r
lx.out <- lx.est(lm.mod = mod,lm.dt = df,lm.n = 3,lm.label = "lx-est", lm.tag = "lx.est",
                 no_dollar = TRUE)
```

$$\begin{aligned}
 & {\widehat{mpg} =} & & {+ 43.54} & & {- 1.78cyl_{i}} & & {+ 0.01disp_{i}} \\
 & (s) & & (4.8601) & & (0.6139) & & (0.0120) \\
 & (t) & & ( + 8.96) & & ( - 2.91) & & ( + 0.58) \\
 & (cont.) & & {- 3.79wt_{i}} & & {- 0.49gear_{i}} & & \\
 & (s) & & (1.0818) & & (0.7903) & & \\
 & (t) & & ( - 3.51) & & ( - 0.62) & & 
\end{aligned}\quad\text{(lx.est)}\quad(\# eq:lx - est)$$

引用语法 `\@ref(eq:lx-est)`显示为 @ref(eq:lx-est)。

### For Quarto/.qmd Writing

#### News of the `xmerit` release

`xmerit`包在版本`>0.0.12`以后，增加了对Quarto文档公式风格的支持。主要使用的函数包括：

- `xmerit::px.psm()`函数，书写特定回归下，总体或样本回归模型的理论表达式。默认形式为嵌套结构`\begin{aligned}`内含`\begin{split}`。

- `xmerit::px.est()`函数，书写特定回归下，估计结果的方程数值表达式。默认形式为嵌套结构`\begin{alignedat}{999}`内含`\begin{split}`。

#### `xmerit`包说明

`xmerit`包在版本`>0.0.12`以后，增加了对Quarto文档公式风格的支持。主要使用的函数包括：

- `xmerit::px.psm()`函数，书写特定回归下，总体或样本回归模型的理论表达式。默认形式为嵌套结构`\begin{aligned}`内含`\begin{split}`。

- `xmerit::px.est()`函数，书写特定回归下，估计结果的方程数值表达式。默认形式为嵌套结构`\begin{alignedat}{999}`内含`\begin{split}`。

``` r
library(xmerit)

data(mtcars)
df <- mtcars
mod <- mpg ~ cyl + disp + wt +gear
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

xvars <- all.vars(mod)[-1]
yvars <- all.vars(mod)[1]
```

#### `xmerit::qx.psm`: aligned + split

[`xmerit::qx.psm()`](https://huhuaping.github.io/xmerit/reference/qx.psm.md)函数默认形式为嵌套结构`\begin{aligned}`内含`\begin{split}`。

- R chunk
- source style
- render effect

``` r
qx.out <- xmerit::qx.psm(
  x = xvars, y = yvars,
  begin = 0,
  #greek.n = length(xvars)+1,
  n.row = 3,
  lm.label = "lx-psm",
  lm.tag = "lx.psm",
  no_dollar = FALSE)
```

    $$
    \begin{aligned}
    \begin{split}
    mpg_i=&+\beta_{0}+\beta_{1}cyl_i+\beta_{2}disp_i\\&+\beta_{3}wt_i+\beta_{4}gear_i+u_i
    \end{split}
    \quad \text{(lx.psm)}\quad
    \end{aligned}
    $$ {#eq-lx-psm}

    引用语法`见 @eq-lx-psm` 显示为见 @eq-lx-psm 。

$$\begin{array}{r}
{\begin{aligned}
{mpg_{i} =} & {+ \beta_{0} + \beta_{1}cyl_{i} + \beta_{2}disp_{i}} \\
 & {+ \beta_{3}wt_{i} + \beta_{4}gear_{i} + u_{i}}
\end{aligned}\quad\text{(lx.psm)}\quad}
\end{array}$$ {#eq-lx-psm}

引用语法`见 @eq-lx-psm` 显示为见 @eq-lx-psm 。

#### `xmerit::qx.est`: alignedat + split

[`xmerit::qx.est()`](https://huhuaping.github.io/xmerit/reference/qx.est.md)函数默认形式为嵌套结构`\begin{alignedat}{999}`内含`\begin{split}`。

- R chunk
- source style
- render effect

``` r
qx.out <- qx.est(
  lm.mod = mod, lm.dt = df,
  lm.n = 3, lm.label = "lx-est", 
  lm.tag = "lx.est",
  no_dollar = FALSE)
```

    $$
    \begin{alignedat}{999}
    \begin{split}
    &\widehat{mpg}=&&+43.54&&-1.78cyl_i&&+0.01disp_i\\ 
    &(s)&&(4.8601)&&(0.6139)&&(0.0120)\\ 
    &(t)&&(+8.96)&&(-2.91)&&(+0.58)\\ 
    &(cont.)&&-3.79wt_i&&-0.49gear_i &&\\ 
    &(s)&&(1.0818)&&(0.7903) &&\\ 
    &(t)&&(-3.51)&&(-0.62) &&
    \end{split}
    \quad \text{(lx.est)}\quad
    \end{alignedat}
    $$ {#eq-lx-est}

    引用语法`见 @eq-lx-est` 显示为见 @eq-lx-est 。

$$\begin{array}{r}
{\begin{aligned}
 & {\widehat{mpg} =} & & {+ 43.54} & & {- 1.78cyl_{i}} & & {+ 0.01disp_{i}} \\
 & (s) & & (4.8601) & & (0.6139) & & (0.0120) \\
 & (t) & & ( + 8.96) & & ( - 2.91) & & ( + 0.58) \\
 & (cont.) & & {- 3.79wt_{i}} & & {- 0.49gear_{i}} & & \\
 & (s) & & (1.0818) & & (0.7903) & & \\
 & (t) & & ( - 3.51) & & ( - 0.62) & & 
\end{aligned}\quad\text{(lx.est)}\quad}
\end{array}$$ {#eq-lx-est}

引用语法`见 @eq-lx-est` 显示为见 @eq-lx-est 。

## Other utilities of functions
