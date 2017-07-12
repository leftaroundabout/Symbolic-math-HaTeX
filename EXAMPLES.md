_This file was generated automatically from [MkSnippets.hs](test/PdfSnippets/MkSnippets.hs). Run `cabal test` to refresh it._
# Tests
## Simple expressions
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `        𝑎 + 𝑏 * 𝑐 ` | `a+b{\cdot}c` | ![pdflatex-rendered version of `a+b{\cdot}c`](test/PdfSnippets/aPLUSbOBRACEBACKSLASHcdotCBRACEc.png) |
| `      (𝑎 + 𝑏) * 𝑐 ` | `\left(a+b\right){\cdot}c` | ![pdflatex-rendered version of `\left(a+b\right){\cdot}c`](test/PdfSnippets/BACKSLASHleftOPARENaPLUSbBACKSLASHrightCPARENOBRACEBACKSLASHcdotCBRACEc.png) |
| `(𝑎 + 𝑏) / (𝑥 - 𝑦) ` | `\frac{a+b}{x-y}` | ![pdflatex-rendered version of `\frac{a+b}{x-y}`](test/PdfSnippets/BACKSLASHfracOBRACEaPLUSbCBRACEOBRACExMINUSyCBRACE.png) |
| ` (𝑎 + 𝑏)**(𝑥 - 𝑦) ` | `\left(a+b\right)^{x-y}` | ![pdflatex-rendered version of `\left(a+b\right)^{x-y}`](test/PdfSnippets/BACKSLASHleftOPARENaPLUSbBACKSLASHrightCPARENTOTHEOBRACExMINUSyCBRACE.png) |
| `         (𝑝/𝑞)**γ ` | `\left(\frac{p}{q}\right)^{\gamma{}}` | ![pdflatex-rendered version of `\left(\frac{p}{q}\right)^{\gamma{}}`](test/PdfSnippets/BACKSLASHleftOPARENBACKSLASHfracOBRACEpCBRACEOBRACEqCBRACEBACKSLASHrightCPARENTOTHEOBRACEBACKSLASHgammaOBRACECBRACECBRACE.png) |
| `          𝑎**𝑏**𝑐 ` | `a^{b^{c}}` | ![pdflatex-rendered version of `a^{b^{c}}`](test/PdfSnippets/aTOTHEOBRACEbTOTHEOBRACEcCBRACECBRACE.png) |
| `        (𝑎**𝑏)**𝑐 ` | `\left(a^{b}\right)^{c}` | ![pdflatex-rendered version of `\left(a^{b}\right)^{c}`](test/PdfSnippets/BACKSLASHleftOPARENaTOTHEOBRACEbCBRACEBACKSLASHrightCPARENTOTHEOBRACEcCBRACE.png) |
| `      sin (sin 𝑥) ` | `\sin{\left(\sin{x}\right)}` | ![pdflatex-rendered version of `\sin{\left(\sin{x}\right)}`](test/PdfSnippets/BACKSLASHsinOBRACEBACKSLASHleftOPARENBACKSLASHsinOBRACExCBRACEBACKSLASHrightCPARENCBRACE.png) |
