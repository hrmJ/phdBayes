

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
model = StandardBayesian(yamlpath="/root/", dumppath="/root/")
model@datalist = LoadResults(model,lang="fi")


```

Here's an example script for using on a remote server:

```
library(stats)                                                                                            
library(methods)
library(phdBayes)                                                                                         
model = StandardBayesian(yamlpath="./",dumppath="./")                                                                
model@datalist = LoadResults(model,lang="fi")                                                             
saveRDS(model@datalist, "path/to/datalist.rds") 

```


#### Extracting plots to pdfs or svgs

The plots produced by ggmcmc require a huge amount of memory, so it is probably 
a good idea to just use pdfs or svgs instead of actual ggplot objects.

For this purpose, the function `SavePdf` can be used, e.g.: 

```

general <- ReadData("general_new")
SavePdf(general,"std.all",2,5)
SavePdf(general,"std.interact")
SavePdf(general,"general_new", "funct")

```
