# Tests
## Simple expressions
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `        𝑎 + 𝑏 * 𝑐 ` | ⟹  `a+b{\cdot}c` | ⟹  ![pdflatex-rendered version of `a+b{\cdot}c`](Symbolic-math-HaTeX/test/PdfSnippets/a%2Bb%7B%5Ccdot%7Dc.png) |
| `      (𝑎 + 𝑏) * 𝑐 ` | ⟹  `\left(a+b\right){\cdot}c` | ⟹  ![pdflatex-rendered version of `\left(a+b\right){\cdot}c`](Symbolic-math-HaTeX/test/PdfSnippets/%5Cleft%28a%2Bb%5Cright%29%7B%5Ccdot%7Dc.png) |
| `(𝑎 + 𝑏) / (𝑥 - 𝑦) ` | ⟹  `\frac{a+b}{x-y}` | ⟹  ![pdflatex-rendered version of `\frac{a+b}{x-y}`](Symbolic-math-HaTeX/test/PdfSnippets/%5Cfrac%7Ba%2Bb%7D%7Bx-y%7D.png) |
| ` (𝑎 + 𝑏)**(𝑥 - 𝑦) ` | ⟹  `\left(a+b\right)^{x-y}` | ⟹  ![pdflatex-rendered version of `\left(a+b\right)^{x-y}`](Symbolic-math-HaTeX/test/PdfSnippets/%5Cleft%28a%2Bb%5Cright%29%5E%7Bx-y%7D.png) |
| `          𝑎**𝑏**𝑐 ` | ⟹  `a^{b^{c}}` | ⟹  ![pdflatex-rendered version of `a^{b^{c}}`](Symbolic-math-HaTeX/test/PdfSnippets/a%5E%7Bb%5E%7Bc%7D%7D.png) |
| `        (𝑎**𝑏)**𝑐 ` | ⟹  `\left(a^{b}\right)^{c}` | ⟹  ![pdflatex-rendered version of `\left(a^{b}\right)^{c}`](Symbolic-math-HaTeX/test/PdfSnippets/%5Cleft%28a%5E%7Bb%7D%5Cright%29%5E%7Bc%7D.png) |
| `      sin (sin 𝑥) ` | ⟹  `\sin{\left(\sin{x}\right)}` | ⟹  ![pdflatex-rendered version of `\sin{\left(\sin{x}\right)}`](Symbolic-math-HaTeX/test/PdfSnippets/%5Csin%7B%5Cleft%28%5Csin%7Bx%7D%5Cright%29%7D.png) |
