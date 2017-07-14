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
| `      abs(𝑝/𝑞)**ξ ` | `\left\|\frac{p}{q}\right\|^{\xi{}}` | ![pdflatex-rendered version of `\left\|\frac{p}{q}\right\|^{\xi{}}`](test/PdfSnippets/BACKSLASHleftPIPEBACKSLASHfracOBRACEpCBRACEOBRACEqCBRACEBACKSLASHrightPIPETOTHEOBRACEBACKSLASHxiOBRACECBRACECBRACE.png) |
| `          𝑎**𝑏**𝑐 ` | `a^{b^{c}}` | ![pdflatex-rendered version of `a^{b^{c}}`](test/PdfSnippets/aTOTHEOBRACEbTOTHEOBRACEcCBRACECBRACE.png) |
| `        (𝑎**𝑏)**𝑐 ` | `\left(a^{b}\right)^{c}` | ![pdflatex-rendered version of `\left(a^{b}\right)^{c}`](test/PdfSnippets/BACKSLASHleftOPARENaTOTHEOBRACEbCBRACEBACKSLASHrightCPARENTOTHEOBRACEcCBRACE.png) |
| `      sin (sin 𝑥) ` | `\sin{\left(\sin{x}\right)}` | ![pdflatex-rendered version of `\sin{\left(\sin{x}\right)}`](test/PdfSnippets/BACKSLASHsinOBRACEBACKSLASHleftOPARENBACKSLASHsinOBRACExCBRACEBACKSLASHrightCPARENCBRACE.png) |
## Operators
### Arithmetic
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| ` 𝑎 + 𝑏 ` | `a+b` | ![pdflatex-rendered version of `a+b`](test/PdfSnippets/aPLUSb.png) |
| ` 𝑎 - 𝑏 ` | `a-b` | ![pdflatex-rendered version of `a-b`](test/PdfSnippets/aMINUSb.png) |
| ` 𝑎 * 𝑏 ` | `a{\cdot}b` | ![pdflatex-rendered version of `a{\cdot}b`](test/PdfSnippets/aOBRACEBACKSLASHcdotCBRACEb.png) |
| ` 𝑎 × 𝑏 ` | `a\times{}b` | ![pdflatex-rendered version of `a\times{}b`](test/PdfSnippets/aBACKSLASHtimesOBRACECBRACEb.png) |
| ` 𝑎 ± 𝑏 ` | `a\pm{}b` | ![pdflatex-rendered version of `a\pm{}b`](test/PdfSnippets/aBACKSLASHpmOBRACECBRACEb.png) |
| ` 𝑎 ∓ 𝑏 ` | `a\mp{}b` | ![pdflatex-rendered version of `a\mp{}b`](test/PdfSnippets/aBACKSLASHmpOBRACECBRACEb.png) |
| ` 𝑎 ⊕ 𝑏 ` | `a\oplus{}b` | ![pdflatex-rendered version of `a\oplus{}b`](test/PdfSnippets/aBACKSLASHoplusOBRACECBRACEb.png) |
| ` 𝑎 ⊗ 𝑏 ` | `a\otimes{}b` | ![pdflatex-rendered version of `a\otimes{}b`](test/PdfSnippets/aBACKSLASHotimesOBRACECBRACEb.png) |
### Logical
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| ` 𝑝 ∨ 𝑞 ` | `p\vee{}q` | ![pdflatex-rendered version of `p\vee{}q`](test/PdfSnippets/pBACKSLASHveeOBRACECBRACEq.png) |
| ` 𝑝 ∧ 𝑞 ` | `p\wedge{}q` | ![pdflatex-rendered version of `p\wedge{}q`](test/PdfSnippets/pBACKSLASHwedgeOBRACECBRACEq.png) |
| ` 𝑝==>𝑞 ` | `p\Longrightarrow q` | ![pdflatex-rendered version of `p\Longrightarrow q`](test/PdfSnippets/pBACKSLASHLongrightarrowSPACEq.png) |
| ` 𝑝<==𝑞 ` | `p\Longleftarrow q` | ![pdflatex-rendered version of `p\Longleftarrow q`](test/PdfSnippets/pBACKSLASHLongleftarrowSPACEq.png) |
| ` 𝑝<=>𝑞 ` | `p\Longleftrightarrow q` | ![pdflatex-rendered version of `p\Longleftrightarrow q`](test/PdfSnippets/pBACKSLASHLongleftrightarrowSPACEq.png) |
| ` 𝑝==>𝑞==>𝑟 ` | `p\Longrightarrow q\Longrightarrow r` | ![pdflatex-rendered version of `p\Longrightarrow q\Longrightarrow r`](test/PdfSnippets/pBACKSLASHLongrightarrowSPACEqBACKSLASHLongrightarrowSPACEr.png) |
### Relations
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| ` 𝑎 ⩵ 𝑏 ` | `a=b` | ![pdflatex-rendered version of `a=b`](test/PdfSnippets/aEQUALSb.png) |
| ` 𝑎 ≥ 𝑐 ` | `a\geq{}c` | ![pdflatex-rendered version of `a\geq{}c`](test/PdfSnippets/aBACKSLASHgeqOBRACECBRACEc.png) |
| ` 𝑎 ⪡ ρ ` | `a<\rho{}` | ![pdflatex-rendered version of `a<\rho{}`](test/PdfSnippets/aLESSERBACKSLASHrhoOBRACECBRACE.png) |
| ` 𝑥 ⩵ 𝑦 ⩵ 𝑧 ` | `x=y=z` | ![pdflatex-rendered version of `x=y=z`](test/PdfSnippets/xEQUALSyEQUALSz.png) |
| ` 𝑠 ⊂ 𝑡 ⊆ 𝑢 ` | `s\subset{}t\subseteq{}u` | ![pdflatex-rendered version of `s\subset{}t\subseteq{}u`](test/PdfSnippets/sBACKSLASHsubsetOBRACECBRACEtBACKSLASHsubseteqOBRACECBRACEu.png) |
