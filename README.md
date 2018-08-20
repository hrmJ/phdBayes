

## Help installing on Ubuntu

### Packages to install:

    sudo apt-get install jags

## Usage


### Creating a model


```
library(phdBayes)
model = StandardBayesian(d, "testmodel", c('subjtype','corpustype'))
SaveOriginalData(model)
CreateAndRunModel(model, list("n.chains" = 2, "sample"=70000, "adapt" = 10000, "burnin" = 9000, "thin" = 1, "method" = "parallel"))
model@datalist = LoadResults(model)
PrintSvg(model)
```

### Extracting data from a saved model


#### Getting all the data

```

library(phdBayes)
model = StandardBayesian(yamlpath="/home/juho/phd_data/data/model_specifications/l8b/",
                         dumppath="/tmp/")
model@datalist = LoadResults(model,lang="fi")

```

#### Saving plot data


```

library(phdBayes)
model = StandardBayesian(yamlpath="/home/juho/phd_data/data/model_specifications/l8b/",
                         dumppath="/tmp/")
model@datalist = LoadResults(model,lang="fi")

```

