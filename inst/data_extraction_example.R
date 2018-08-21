library(stats)                                                                                            
library(methods)
library(phdBayes)                                                                                         
model = StandardBayesian(yamlpath="/tmp/general_new/",                                                    
                         dumppath="/tmp/")                                                                
model@datalist = LoadResults(model,lang="fi")                                                             
saveRDS(model@datalist, "/tmp/testmodel_plots.rds") 
