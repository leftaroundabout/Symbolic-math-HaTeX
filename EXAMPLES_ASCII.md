_This file was generated automatically from [MkSnippets.hs](test/PdfSnippets/MkSnippets.hs). Run `cabal test` to refresh it._
# Tests
## Simple expressions
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `a + b * c ` | `a+b{\cdot}c` | ![pdflatex-rendered version of `a+b{\cdot}c`](test/PdfSnippets/a⼦bⶈᓭcdotⶉc.png) |
| `A * B + C ` | `A{\cdot}B+C` | ![pdflatex-rendered version of `A{\cdot}B+C`](test/PdfSnippets/AⶈᓭcdotⶉB⼦C.png) |
| `(a + b) * c ` | `\left(a+b\right){\cdot}c` | ![pdflatex-rendered version of `\left(a+b\right){\cdot}c`](test/PdfSnippets/ᓭleftᑕa⼦bᓭrightᑐⶈᓭcdotⶉc.png) |
| `(a + b) / (x - y) ` | `\frac{a+b}{x-y}` | ![pdflatex-rendered version of `\frac{a+b}{x-y}`](test/PdfSnippets/ᓭfracⶈa⼦bⶉⶈx⼀yⶉ.png) |
| `(a + b)**(x - y) ` | `\left(a+b\right)^{x-y}` | ![pdflatex-rendered version of `\left(a+b\right)^{x-y}`](test/PdfSnippets/ᓭleftᑕa⼦bᓭrightᑐᐞⶈx⼀yⶉ.png) |
| `(p/q)**gamma ` | `\left(\frac{p}{q}\right)^{\gamma{}}` | ![pdflatex-rendered version of `\left(\frac{p}{q}\right)^{\gamma{}}`](test/PdfSnippets/ᓭleftᑕᓭfracⶈpⶉⶈqⶉᓭrightᑐᐞⶈᓭgammaⶈⶉⶉ.png) |
| `abs(p/q)**xi ` | `\left\|\frac{p}{q}\right\|^{\xi{}}` | ![pdflatex-rendered version of `\left\|\frac{p}{q}\right\|^{\xi{}}`](test/PdfSnippets/ᓭleftᛁᓭfracⶈpⶉⶈqⶉᓭrightᛁᐞⶈᓭxiⶈⶉⶉ.png) |
| `a**b**c ` | `a^{b^{c}}` | ![pdflatex-rendered version of `a^{b^{c}}`](test/PdfSnippets/aᐞⶈbᐞⶈcⶉⶉ.png) |
| `(a**b)**c ` | `\left(a^{b}\right)^{c}` | ![pdflatex-rendered version of `\left(a^{b}\right)^{c}`](test/PdfSnippets/ᓭleftᑕaᐞⶈbⶉᓭrightᑐᐞⶈcⶉ.png) |
| `sin (sin x) ` | `\sin{\left(\sin{x}\right)}` | ![pdflatex-rendered version of `\sin{\left(\sin{x}\right)}`](test/PdfSnippets/ᓭsinⶈᓭleftᑕᓭsinⶈxⶉᓭrightᑐⶉ.png) |
| `matrix[[ 0,1]` `,[-1,0]] ` | `\begin{pmatrix}0&1\\ -1&0\end{pmatrix}` | ![pdflatex-rendered version of `\begin{pmatrix}0&1\\ -1&0\end{pmatrix}`](test/PdfSnippets/ᓭbeginⶈpmatrixⶉ0ತ1ᓭᓭᐧ⼀1ತ0ᓭendⶈpmatrixⶉ.png) |
## Operators
### Arithmetic
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `a + b ` | `a+b` | ![pdflatex-rendered version of `a+b`](test/PdfSnippets/a⼦b.png) |
| `a - b ` | `a-b` | ![pdflatex-rendered version of `a-b`](test/PdfSnippets/a⼀b.png) |
| `a * b ` | `a{\cdot}b` | ![pdflatex-rendered version of `a{\cdot}b`](test/PdfSnippets/aⶈᓭcdotⶉb.png) |
| `a `times` b ` | `a\times{}b` | ![pdflatex-rendered version of `a\times{}b`](test/PdfSnippets/aᓭtimesⶈⶉb.png) |
| `a +- b ` | `a\pm{}b` | ![pdflatex-rendered version of `a\pm{}b`](test/PdfSnippets/aᓭpmⶈⶉb.png) |
| `a -+ b ` | `a\mp{}b` | ![pdflatex-rendered version of `a\mp{}b`](test/PdfSnippets/aᓭmpⶈⶉb.png) |
| `a `oplus` b ` | `a\oplus{}b` | ![pdflatex-rendered version of `a\oplus{}b`](test/PdfSnippets/aᓭoplusⶈⶉb.png) |
| `a `otimes` b ` | `a\otimes{}b` | ![pdflatex-rendered version of `a\otimes{}b`](test/PdfSnippets/aᓭotimesⶈⶉb.png) |
### Sub/superscripts
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `a!:b ` | `{a}_{b}` | ![pdflatex-rendered version of `{a}_{b}`](test/PdfSnippets/ⶈaⶉ⣀ⶈbⶉ.png) |
| `a!^(b,c) ` | `{a}_{b}^{c}` | ![pdflatex-rendered version of `{a}_{b}^{c}`](test/PdfSnippets/ⶈaⶉ⣀ⶈbⶉᐞⶈcⶉ.png) |
| `psi!:"Foo" ` | `{\psi{}}_{\mathrm{Foo}}` | ![pdflatex-rendered version of `{\psi{}}_{\mathrm{Foo}}`](test/PdfSnippets/ⶈᓭpsiⶈⶉⶉ⣀ⶈᓭmathrmⶈFooⶉⶉ.png) |
| `psi!:(F<>o<>o) ` | `{\psi{}}_{Foo}` | ![pdflatex-rendered version of `{\psi{}}_{Foo}`](test/PdfSnippets/ⶈᓭpsiⶈⶉⶉ⣀ⶈFooⶉ.png) |
### Logical
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `p `vee` q ` | `p\vee{}q` | ![pdflatex-rendered version of `p\vee{}q`](test/PdfSnippets/pᓭveeⶈⶉq.png) |
| `p `wedge` q ` | `p\wedge{}q` | ![pdflatex-rendered version of `p\wedge{}q`](test/PdfSnippets/pᓭwedgeⶈⶉq.png) |
| `cases[(1, "Today"), (2, "Else")] ` | `\begin{cases}1&\text{Today}\\2&\text{Else}\end{cases}` | ![pdflatex-rendered version of `\begin{cases}1&\text{Today}\\2&\text{Else}\end{cases}`](test/PdfSnippets/ᓭbeginⶈcasesⶉ1ತᓭtextⶈTodayⶉᓭᓭ2ತᓭtextⶈElseⶉᓭendⶈcasesⶉ.png) |
### Relations
| Haskell | LaTeX | pdf |
| ---: | --- | :--- |
| `s `subset` t `subseteq` u ` | `s\subset{}t\subseteq{}u` | ![pdflatex-rendered version of `s\subset{}t\subseteq{}u`](test/PdfSnippets/sᓭsubsetⶈⶉtᓭsubseteqⶈⶉu.png) |
| `h `approx` i `sim` j `simeq` k `cong` l ` | `h\approx{}i\sim{}j\simeq{}k\cong{}l` | ![pdflatex-rendered version of `h\approx{}i\sim{}j\simeq{}k\cong{}l`](test/PdfSnippets/hᓭapproxⶈⶉiᓭsimⶈⶉjᓭsimeqⶈⶉkᓭcongⶈⶉl.png) |
| `p `in_` mathbb Q `subset` mathbb R ` | `p\in{}\mathbb{Q}\subset{}\mathbb{R}` | ![pdflatex-rendered version of `p\in{}\mathbb{Q}\subset{}\mathbb{R}`](test/PdfSnippets/pᓭinⶈⶉᓭmathbbⶈQⶉᓭsubsetⶈⶉᓭmathbbⶈRⶉ.png) |
| `mathbf u `perp` (vec%$>v) `parallel` (underline%$>w) ` | `\mathbf{u}\perp{}\vec{v}\parallel{}\underline{w}` | ![pdflatex-rendered version of `\mathbf{u}\perp{}\vec{v}\parallel{}\underline{w}`](test/PdfSnippets/ᓭmathbfⶈuⶉᓭperpⶈⶉᓭvecⶈvⶉᓭparallelⶈⶉᓭunderlineⶈwⶉ.png) |
