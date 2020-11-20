
<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build
status](https://github.com/mrc-ide/epicyst/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/epicyst/actions)
[![codecov](https://codecov.io/gh/mrc-ide/epicyst/branch/master/graph/badge.svg)](https://codecov.io/gh/mrc-ide/epicyst)
<!-- badges: end -->

<img src='man/figures/logo.png' align="right" height="211" />

<br />

### A deterministic *Taenia solium* and Cysticercosis dynamic model

-----

### Overview

A deterministic, compartmental model of the Cysticercosis disease
system.

For full model details please see: [Winskill, P., Harrison, W.E.,
French, M. D., Dixon, M., Abela-Ridder, B. & Basáñez, M.-G. (2017)
Assessing the impact of intervention strategies against Taenia solium
cysticercosis using the epicyst transmission model. Parasites &
Vectors, 10:73,
DOI: 10.1186/s13071-017-1988-9](https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-017-1988-9)

Other publications include:

[Dixon MA, Braae UC, Winskill P, Walker M, Devleesschauwer B, Gabriël S,
et al. Strategies for tackling Taenia solium taeniosis/ cysticercosis: A
systematic review and comparison of transmission models, including an
assessment of the wider Taeniidae family transmission models. PLoS Negl
Trop
Dis. 2019;13(4):1–24](https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0007301)

[Dixon MA, Winskill P, Harrison WE, Whittaker C, Schmidt V, Sarti E, et
al. Force ‑ of ‑ infection of Taenia solium porcine cysticercosis: a
modelling analysis to assess global incidence and prevalence trends. Sci
Rep. 2020;1–14](https://doi.org/10.1038/s41598-020-74007-x)

[Dixon MA, Braae UC, Winskill P, Devleesschauwer B, Trevisan C, Van
Damme I, et al. Modelling for Taenia solium control strategies
beyond 2020. Bull World Health
Organ. 2020;98(3):198–205](https://apps.who.int/iris/handle/10665/331383)

-----

### Installation

To use the package, please install package devtools and install epicyst:
`remotes::install_github("mrc-ide/epicyst")`

You will need an [appropriate
compiler](https://cran.r-project.org/bin/windows/Rtools/) installed.

The package ODEs are all driven by the [odin
package](https://mrc-ide.github.io/odin/)

-----

### Use

For a detailed practical guide please see the [epicyst introduction
vignette](https://mrc-ide.github.io/epicyst/articles/Introduction.html)

-----

Details on the model functionality and use can be found on the [epicyst
Webpage](https://mrc-ide.github.io/epicyst/)  
Source code is available from the [epicyst
Github](https://github.com/mrc-ide/epicyst/)  
Seen an issue? Please [Report a
bug](https://github.com/mrc-ide/epicyst/issues)
