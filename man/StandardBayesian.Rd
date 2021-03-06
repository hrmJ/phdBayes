% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_bayesian.R
\name{StandardBayesian}
\alias{StandardBayesian}
\title{Tutkimuksessa standardisti käytetty bayesiläinen malli
 
Mallissa selitettävänä muuttujana sijainti. Lisäksi tutkitaan kielen ja
muiden prediktoreiden välisiä interaktioita.}
\usage{
StandardBayesian(original.data = data.frame(), filename = character(),
  predictors = vector(), yamlpath = character(), dumppath = character())
}
\arguments{
\item{original.data}{alkuperäinen dataframe, joka on mallin aineistona}

\item{filename}{nimi tai nimen alku rds-tiedostoille, joihin tallennetaan aineisto ja posteriorijakauma}

\item{predictors}{kielen lisäksi käytettävien x-muuttujien nimet}

\item{yamlpath}{jos käytetään olemassaolevaa mallia, niin polku tämän määrittämän yaml-tiedostoon kotikansioon}

\item{dumppath}{}
}
\description{
Tutkimuksessa standardisti käytetty bayesiläinen malli
 
Mallissa selitettävänä muuttujana sijainti. Lisäksi tutkitaan kielen ja
muiden prediktoreiden välisiä interaktioita.
}
\examples{

# Example 1
model = StandardBayesian(my_dataframe,"model_name",c('hasPverb','subjtype','corpustype'))
SaveOriginalData(model)
CreateAndRunModel(model, list("n.chains" = 2, "sample"=70000, "adapt" = 10000, "burnin" = 9000, "thin" = 1, "method" = "parallel"), T)
model@datalist = LoadResults(model)
PrintSvg(model)


}
