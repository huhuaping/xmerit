# opt_line(): 设计问卷题项选项行

``` r
library(xmerit)
```

## 简介

[`opt_line()`](https://huhuaping.github.io/xmerit/reference/opt_line.md)
用于在 **Quarto**（.qmd）中生成问卷题项的一行或多行选项，支持 **HTML**
与 **Word (docx)** 双格式。输出格式会根据当前 Quarto
渲染目标自动选择：HTML 下可输出真实勾选框或字母选项，Word
下输出适合打印填写的符号与间距。

适用场景：问卷、量表、选择题的选项行，需要同时输出网页版和 Word
打印版时。

## 基本用法

在 .qmd 中，题干单独一行，选项行用内联 R 调用
[`opt_line()`](https://huhuaping.github.io/xmerit/reference/opt_line.md)，例如：

- 题干：`1. 您的性别：`
- 选项行：`<span class="q-opt"><input type="checkbox" class="q-opt-cb"> 男</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="q-opt"><input type="checkbox" class="q-opt-cb"> 女</span>`

函数返回一整行（或多行）的选项字符串；在 HTML 勾选框模式下会通过
[`knitr::asis_output()`](https://rdrr.io/pkg/knitr/man/asis_output.html)
输出原始 HTML，其它情况为普通字符，可直接嵌入正文。

## 两种样式

### 勾选框样式（默认）

`style = "checkbox"`（或通过
`options(questionnaire.opt_style = "checkbox")` 设为默认）：

- **HTML**：输出 `<input type="checkbox">`，可在网页上勾选。
- **Word**：输出 Unicode 方框符号 ☐ (U+2610)，便于打印后手写勾选。

``` r
opt_line(c("男", "女"))
```

男        

女

``` r
opt_line(c("非常了解", "比较了解", "一般", "不太了解", "完全不了解"))
```

非常了解        

比较了解        

一般        

不太了解        

完全不了解

### 字母选项样式

`style = "letter"`：选项前加 A.、B.、C. 等标签，便于阅读与填答。

``` r
opt_line(c("男", "女"), style = "letter")
```

\[1\] “A. 男        B. 女”

``` r
opt_line(c("是", "否"), style = "letter", letter_sep = "、")
```

\[1\] “A、是        B、否”

可在 .qmd 的 setup 中统一设置默认样式，例如：

``` r
options(questionnaire.opt_style = "letter")
```

之后所有 `opt_line(...)` 未显式指定 `style` 时都会使用字母样式。

## 换行与 A4 适配

当选项较多或文字较长时，可开启按宽度换行，使打印到 A4 时更整齐。

- `wrap = TRUE`：按显示宽度换行。
- `max_width`：单行最大宽度（半角单位，默认 42）。中文字符计 2
  单位，英文/数字计 1 单位。
- `indent_wrap`：换行后的缩进字符串（默认 3
  个空格），使续行与首行选项对齐。

``` r
opt_line(
  c("价格过高", "不了解相关服务/产品", "觉得没必要", "身体状况良好，无需康养"),
  wrap = TRUE,
  max_width = 42
)
```

价格过高        

不了解相关服务/产品  

觉得没必要  

身体状况良好，无需康养

## 在 Quarto 中的使用步骤

1.  **Setup 块**（可选）：若需全局样式或默认选项，在文档开头加入：

    ``` r
    library(xmerit)
    options(questionnaire.opt_style = "letter")  # 可选：默认字母选项
    ```

2.  **题干 + 选项**：题干一行，选项行用内联 R：

    ``` markdown
    1. 您的性别：

    <span class="q-opt"><input type="checkbox" class="q-opt-cb"> 男</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="q-opt"><input type="checkbox" class="q-opt-cb"> 女</span>
    ```

3.  **需要换行时**：在
    [`opt_line()`](https://huhuaping.github.io/xmerit/reference/opt_line.md)
    中设置 `wrap = TRUE` 并可按需调整 `max_width`、`indent_wrap`。

## 参数速查

| 参数          | 含义                              | 默认                                                       |
|---------------|-----------------------------------|------------------------------------------------------------|
| `options`     | 选项文字向量                      | —                                                          |
| `gap_n`       | 选项间间隔单位数                  | 8                                                          |
| `style`       | `"checkbox"` 或 `"letter"`        | 由 `getOption("questionnaire.opt_style", "checkbox")` 决定 |
| `letter_sep`  | 字母与文字间的分隔符（仅 letter） | `". "`                                                     |
| `wrap`        | 是否按宽度换行                    | `FALSE`                                                    |
| `max_width`   | 单行最大宽度（半角单位）          | 42                                                         |
| `indent_wrap` | 换行缩进字符串                    | `" "`                                                      |

更多细节见函数帮助：[`?opt_line`](https://huhuaping.github.io/xmerit/reference/opt_line.md)。
