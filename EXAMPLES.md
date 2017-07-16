_This file was generated automatically from [MkSnippets.hs](test/PdfSnippets/MkSnippets.hs). Run `cabal test` to refresh it._
# Tests
## Simple expressions
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑎 + 𝑏 * 𝑐 ` | `a+b{\cdot}c` | ![pdflatex-rendered version of `a+b{\cdot}c`](test/PdfSnippets/a⼦bⶈᓭcdotⶉc.png) |
| `𝐴 * 𝐵 + 𝐶 ` | `A{\cdot}B+C` | ![pdflatex-rendered version of `A{\cdot}B+C`](test/PdfSnippets/AⶈᓭcdotⶉB⼦C.png) |
| `(𝑎 + 𝑏) * 𝑐 ` | `\left(a+b\right){\cdot}c` | ![pdflatex-rendered version of `\left(a+b\right){\cdot}c`](test/PdfSnippets/ᓭleftᑕa⼦bᓭrightᑐⶈᓭcdotⶉc.png) |
| `(𝑎 + 𝑏) / (𝑥 - 𝑦) ` | `\frac{a+b}{x-y}` | ![pdflatex-rendered version of `\frac{a+b}{x-y}`](test/PdfSnippets/ᓭfracⶈa⼦bⶉⶈx⼀yⶉ.png) |
| `(𝑎 + 𝑏)**(𝑥 - 𝑦) ` | `\left(a+b\right)^{x-y}` | ![pdflatex-rendered version of `\left(a+b\right)^{x-y}`](test/PdfSnippets/ᓭleftᑕa⼦bᓭrightᑐᐞⶈx⼀yⶉ.png) |
| `(𝑝/𝑞)**γ ` | `\left(\frac{p}{q}\right)^{\gamma{}}` | ![pdflatex-rendered version of `\left(\frac{p}{q}\right)^{\gamma{}}`](test/PdfSnippets/ᓭleftᑕᓭfracⶈpⶉⶈqⶉᓭrightᑐᐞⶈᓭgammaⶈⶉⶉ.png) |
| `abs(𝑝/𝑞)**ξ ` | `\left\|\frac{p}{q}\right\|^{\xi{}}` | ![pdflatex-rendered version of `\left\|\frac{p}{q}\right\|^{\xi{}}`](test/PdfSnippets/ᓭleftᛁᓭfracⶈpⶉⶈqⶉᓭrightᛁᐞⶈᓭxiⶈⶉⶉ.png) |
| `𝑎**𝑏**𝑐 ` | `a^{b^{c}}` | ![pdflatex-rendered version of `a^{b^{c}}`](test/PdfSnippets/aᐞⶈbᐞⶈcⶉⶉ.png) |
| `(𝑎**𝑏)**𝑐 ` | `\left(a^{b}\right)^{c}` | ![pdflatex-rendered version of `\left(a^{b}\right)^{c}`](test/PdfSnippets/ᓭleftᑕaᐞⶈbⶉᓭrightᑐᐞⶈcⶉ.png) |
| `sin (sin 𝑥) ` | `\sin{\left(\sin{x}\right)}` | ![pdflatex-rendered version of `\sin{\left(\sin{x}\right)}`](test/PdfSnippets/ᓭsinⶈᓭleftᑕᓭsinⶈxⶉᓭrightᑐⶉ.png) |
| `(𝑖⩵0,3)∑ 𝑖 ` | `\sum_{i=0}^{3} i` | ![pdflatex-rendered version of `\sum_{i=0}^{3} i`](test/PdfSnippets/ᓭsum⣀ⶈi〧0ⶉᐞⶈ3ⶉᐧi.png) |
| `matrix[[ 0,1]` `,[-1,0]] ` | `\begin{pmatrix}0&1\\-1&0\end{pmatrix}` | ![pdflatex-rendered version of `\begin{pmatrix}0&1\\-1&0\end{pmatrix}`](test/PdfSnippets/ᓭbeginⶈpmatrixⶉ0ತ1ᓭᓭ⼀1ತ0ᓭendⶈpmatrixⶉ.png) |
## Operators
### Arithmetic
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑎 + 𝑏 ` | `a+b` | ![pdflatex-rendered version of `a+b`](test/PdfSnippets/a⼦b.png) |
| `𝑎 - 𝑏 ` | `a-b` | ![pdflatex-rendered version of `a-b`](test/PdfSnippets/a⼀b.png) |
| `𝑎 * 𝑏 ` | `a{\cdot}b` | ![pdflatex-rendered version of `a{\cdot}b`](test/PdfSnippets/aⶈᓭcdotⶉb.png) |
| `𝑎 × 𝑏 ` | `a\times{}b` | ![pdflatex-rendered version of `a\times{}b`](test/PdfSnippets/aᓭtimesⶈⶉb.png) |
| `𝑎 ± 𝑏 ` | `a\pm{}b` | ![pdflatex-rendered version of `a\pm{}b`](test/PdfSnippets/aᓭpmⶈⶉb.png) |
| `𝑎 ∓ 𝑏 ` | `a\mp{}b` | ![pdflatex-rendered version of `a\mp{}b`](test/PdfSnippets/aᓭmpⶈⶉb.png) |
| `𝑎 ⊕ 𝑏 ` | `a\oplus{}b` | ![pdflatex-rendered version of `a\oplus{}b`](test/PdfSnippets/aᓭoplusⶈⶉb.png) |
| `𝑎 ⊗ 𝑏 ` | `a\otimes{}b` | ![pdflatex-rendered version of `a\otimes{}b`](test/PdfSnippets/aᓭotimesⶈⶉb.png) |
### Sub/superscripts
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑎◞𝑏 ` | `a_{b}` | ![pdflatex-rendered version of `a_{b}`](test/PdfSnippets/a⣀ⶈbⶉ.png) |
| `𝑎◞◝(𝑏,𝑐) ` | `a_{b}^{c}` | ![pdflatex-rendered version of `a_{b}^{c}`](test/PdfSnippets/a⣀ⶈbⶉᐞⶈcⶉ.png) |
| `ψ◞"Foo" ` | `\psi{}_{\mathrm{Foo}}` | ![pdflatex-rendered version of `\psi{}_{\mathrm{Foo}}`](test/PdfSnippets/ᓭpsiⶈⶉ⣀ⶈᓭmathrmⶈFooⶉⶉ.png) |
| `ψ◞𝐹⁀𝑜⁀𝑜 ` | `\psi{}_{Foo}` | ![pdflatex-rendered version of `\psi{}_{Foo}`](test/PdfSnippets/ᓭpsiⶈⶉ⣀ⶈFooⶉ.png) |
### Function application
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑓°𝑥 ` | `f\left(x\right)` | ![pdflatex-rendered version of `f\left(x\right)`](test/PdfSnippets/fᓭleftᑕxᓭrightᑐ.png) |
| `𝑓°(𝑥،𝑦) ` | `f\left(x,y\right)` | ![pdflatex-rendered version of `f\left(x,y\right)`](test/PdfSnippets/fᓭleftᑕx،yᓭrightᑐ.png) |
### Logical
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑝 ∨ 𝑞 ` | `p\vee{}q` | ![pdflatex-rendered version of `p\vee{}q`](test/PdfSnippets/pᓭveeⶈⶉq.png) |
| `𝑝 ∧ 𝑞 ` | `p\wedge{}q` | ![pdflatex-rendered version of `p\wedge{}q`](test/PdfSnippets/pᓭwedgeⶈⶉq.png) |
| `𝑝==>𝑞 ` | `p\Longrightarrow q` | ![pdflatex-rendered version of `p\Longrightarrow q`](test/PdfSnippets/pᓭLongrightarrowᐧq.png) |
| `𝑝<==𝑞 ` | `p\Longleftarrow q` | ![pdflatex-rendered version of `p\Longleftarrow q`](test/PdfSnippets/pᓭLongleftarrowᐧq.png) |
| `𝑝<=>𝑞 ` | `p\Longleftrightarrow q` | ![pdflatex-rendered version of `p\Longleftrightarrow q`](test/PdfSnippets/pᓭLongleftrightarrowᐧq.png) |
| `𝑝==>𝑞==>𝑟 ` | `p\Longrightarrow q\Longrightarrow r` | ![pdflatex-rendered version of `p\Longrightarrow q\Longrightarrow r`](test/PdfSnippets/pᓭLongrightarrowᐧqᓭLongrightarrowᐧr.png) |
| `cases[(1, "Today"), (2, "Else")] ` | `\begin{cases}1&\text{Today}\\2&\text{Else}\end{cases}` | ![pdflatex-rendered version of `\begin{cases}1&\text{Today}\\2&\text{Else}\end{cases}`](test/PdfSnippets/ᓭbeginⶈcasesⶉ1ತᓭtextⶈTodayⶉᓭᓭ2ತᓭtextⶈElseⶉᓭendⶈcasesⶉ.png) |
### Relations
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑎 ⩵ 𝑏 ` | `a=b` | ![pdflatex-rendered version of `a=b`](test/PdfSnippets/a〧b.png) |
| `𝑎 ≥ 𝑐 ` | `a\geq{}c` | ![pdflatex-rendered version of `a\geq{}c`](test/PdfSnippets/aᓭgeqⶈⶉc.png) |
| `𝑎 ⪡ ρ ` | `a<\rho{}` | ![pdflatex-rendered version of `a<\rho{}`](test/PdfSnippets/aᐸᓭrhoⶈⶉ.png) |
| `𝑥 ⩵ 𝑦 ⩵ 𝑧 ` | `x=y=z` | ![pdflatex-rendered version of `x=y=z`](test/PdfSnippets/x〧y〧z.png) |
| `𝑠 ⊂ 𝑡 ⊆ 𝑢 ` | `s\subset{}t\subseteq{}u` | ![pdflatex-rendered version of `s\subset{}t\subseteq{}u`](test/PdfSnippets/sᓭsubsetⶈⶉtᓭsubseteqⶈⶉu.png) |
| `𝑝 ∈ ℚ ⊂ ℝ ` | `p\in{}\mathbb{Q}\subset{}\mathbb{R}` | ![pdflatex-rendered version of `p\in{}\mathbb{Q}\subset{}\mathbb{R}`](test/PdfSnippets/pᓭinⶈⶉᓭmathbbⶈQⶉᓭsubsetⶈⶉᓭmathbbⶈRⶉ.png) |
## Calculus
### Integration
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `(-1,1)∫d 𝑥 (𝑥**2) ` | `\int\limits_{-1}^{1}\mathrm{d}x\ x^{2}` | ![pdflatex-rendered version of `\int\limits_{-1}^{1}\mathrm{d}x\ x^{2}`](test/PdfSnippets/ᓭintᓭlimits⣀ⶈ⼀1ⶉᐞⶈ1ⶉᓭmathrmⶈdⶉxᓭᐧxᐞⶈ2ⶉ.png) |
| `ω◞∫d 𝑥 (exp $ -(𝑥**2)) ` | `\int_{\omega{}}\!\!\!\mathrm{d}x\ \exp{\left(-x^{2}\right)}` | ![pdflatex-rendered version of `\int_{\omega{}}\!\!\!\mathrm{d}x\ \exp{\left(-x^{2}\right)}`](test/PdfSnippets/ᓭint⣀ⶈᓭomegaⶈⶉⶉᓭ⢘ᓭ⢘ᓭ⢘ᓭmathrmⶈdⶉxᓭᐧᓭexpⶈᓭleftᑕ⼀xᐞⶈ2ⶉᓭrightᑐⶉ.png) |
| `(0,1)∫d 𝑥 ((0,1)∫d 𝑦 (𝑥*𝑦)) ` | `\int\limits_{0}^{1}\mathrm{d}x\ \int\limits_{0}^{1}\mathrm{d}y\ \left(x{\cdot}y\right)` | ![pdflatex-rendered version of `\int\limits_{0}^{1}\mathrm{d}x\ \int\limits_{0}^{1}\mathrm{d}y\ \left(x{\cdot}y\right)`](test/PdfSnippets/ᓭintᓭlimits⣀ⶈ0ⶉᐞⶈ1ⶉᓭmathrmⶈdⶉxᓭᐧᓭintᓭlimits⣀ⶈ0ⶉᐞⶈ1ⶉᓭmathrmⶈdⶉyᓭᐧᓭleftᑕxⶈᓭcdotⶉyᓭrightᑐ.png) |
## Algebraic manipulation
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `𝑎 + 𝑏 + 𝑐 &~~ [𝑏 ⩵ 𝑦] ` | `a+b+c=a+y+c` | ![pdflatex-rendered version of `a+b+c=a+y+c`](test/PdfSnippets/a⼦b⼦c〧a⼦y⼦c.png) |
| `𝑎 + 𝑏 + 𝑐 &~~ [𝑏+𝑐 ⩵ 𝑐+𝑏, 𝑎+𝑐 ⩵ ξ] ` | `a+b+c=\xi{}+b` | ![pdflatex-rendered version of `a+b+c=\xi{}+b`](test/PdfSnippets/a⼦b⼦c〧ᓭxiⶈⶉ⼦b.png) |
| `𝑎 - 𝑏 &~~ [𝑏 ⩵ 𝑦] &~~ [𝑎 ⩵ 𝑧] ` | `a-b=a-y=z-y` | ![pdflatex-rendered version of `a-b=a-y=z-y`](test/PdfSnippets/a⼀b〧a⼀y〧z⼀y.png) |
