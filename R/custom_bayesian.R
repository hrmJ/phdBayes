
#' S3 object stuff

setClass("StandardBayesian", slots = c(original.data="data.frame",filename="character",dump.path="character",model.path="character",
                                       predictors="vector", predictorlist="list",
                                       source.file="character",post.file="character",datalist="list"))


#'  
#' Tutkimuksessa standardisti käytetty bayesiläinen malli
#'  
#' Mallissa selitettävänä muuttujana sijainti. Lisäksi tutkitaan kielen ja
#' muiden prediktoreiden välisiä interaktioita.
#'
#' @param original.data alkuperäinen dataframe, joka on mallin aineistona
#' @param filename nimi tai nimen alku rds-tiedostoille, joihin tallennetaan aineisto ja posteriorijakauma
#' @param predictors kielen lisäksi käytettävien x-muuttujien nimet
#' @param yamlpath jos käytetään olemassaolevaa mallia, niin polku tämän määrittämän yaml-tiedostoon kotikansioon
#' @param dumppath
#' 
#' @importFrom yaml yaml.load_file
#' 
#' @export
#' 
#' @examples
#' 
#' # Example 1
#' model = StandardBayesian(my_dataframe,"model_name",c('hasPverb','subjtype','corpustype'))
#' SaveOriginalData(model)
#' CreateAndRunModel(model, list("n.chains" = 2, "sample"=70000, "adapt" = 10000, "burnin" = 9000, "thin" = 1, "method" = "parallel"), T)
#' model@datalist = LoadResults(model)
#' PrintSvg(model)
#' 
#' 

StandardBayesian  <-  function(original.data=data.frame(),filename=character(), predictors=vector(), yamlpath=character(), dumppath=character()){
    if(hasArg(yamlpath)){
        #Jos ladataan jo olemassa olevaa
       spec <- yaml.load_file(paste0(yamlpath,"model.yml"))
       original.data <- readRDS(spec$datafile)
       dump.path=dumppath
       model.path=yamlpath
       predictors <- c()
       for(x in spec$x){
           predictors <- c(predictors, x$name)
       }
       post.file = spec$outputfile
       source.file = spec$datafile
       filename=gsub(".rds", "", basename(spec$datafile),ignore.case=T)
    }
    else{
        #Jos luodaan kokonaan uusi
        dump.path="modeldata/"
        model.path="model_specifications/"
        post.file = paste0(dump.path,filename,"_post.rds")
        source.file = paste0(dump.path,filename,".rds")
        system(paste("mkdir -p ", dump.path))
        system(paste("mkdir -p ", model.path))
    }
    #Muutetaan prediktorit ja responssi faktoreiksi ja poistetaan muut muuttujat
    allvars <- c("lang","location3",predictors)
    original.data[allvars] <- lapply(original.data[allvars], as.factor)
    original.data <-original.data[,allvars]
    #tee prediktoreista lista, jossa erikseen attribuutteina name ja n eli nimi ja kuinka monta mahdollista arvoa
    predictorlist <- lapply(predictors,function(predictor,df)return(list(name=predictor,n=length(unique(original.data[[predictor]])))),df=original.data)

    new ("StandardBayesian",original.data=original.data, filename=filename,
         dump.path=dump.path, model.path=model.path,
         source.file=source.file,
         post.file=post.file,
         predictors=predictors, predictorlist=predictorlist)

}


#' S3 object stuff
#' 
#' @export
#' 

setGeneric(name="SaveOriginalData", def=function(object) { standardGeneric("SaveOriginalData") })


#' S3 object stuff
#' 
#' @export
#' 

setGeneric(name="ResetAllData", def=function(object, forcethis,...) { standardGeneric("ResetAllData") })

#' S3 object stuff
#' 
#' @export
#' 

setGeneric(name="CreateAndRunModel", def=function(object, jags_settings, dontrun, ...) { standardGeneric("CreateAndRunModel") })

#' S3 object stuff
#' 
#' @export
#' 

setGeneric(name="LoadResults", def=function(object,force_reload, lang, ...) { standardGeneric("LoadResults") })

#' S3 object stuff
#' 
#' @export
#' 

setGeneric(name="PrintSvg", def=function(object,...) { standardGeneric("PrintSvg") })

#' Poista kaikki tallennettu tieto tästä mallista, jotta se olisi esimerkiksi helppo ladata uudestaan muutetuilla parametreilla
#' 
#' @export
#' 

setMethod("ResetAllData", "StandardBayesian",
            function(object, forcethis=F) {
                cont <- readline("Oletko aivan varma, että haluat tuhota kaiken vanhan datan tästä mallista? (yes/no)")
                if(cont=="yes" | forcethis){
                    system(paste("rm -f -r", paste0(object@model.path, object@filename)))
                    system(paste("rm -f -r", paste0(object@dump.path, object@filename,"*")))
                    system(paste("rm -f -r", object@source.file))
                }
            })


#' Testaa, onko jo tallennettuna tämän mallin pohjana olevasta datasta rds-tiedostoa. Jos ei ole, tallentaa sellaisen.
#' 
#' @export
#' 

setMethod("SaveOriginalData", "StandardBayesian",
            function(object) {
                if(!file.exists(object@source.file)){
                    saveRDS(object@original.data,object@source.file)
                }
            })


#' Testaa, onko jo tallennettuna tämän mallin pohjana olevasta datasta rds-tiedostoa. Jos ei ole, tallentaa sellaisen.
#'
#' @param jags_settings lista jagsille annettavista asetuksista (adapt,n.chains,burnin,thin,method)
#' @param dontrun estää jagsin käynnistämisen
#' 
#' @importFrom yaml as.yaml
#' 
#' @export
#' 

setMethod("CreateAndRunModel", "StandardBayesian",
            function(object=StandardBayesian(), jags_settings=list(), dontrun=F) {
                modelfolder <- paste0(object@model.path, object@filename)
                diagfolder <- paste0("mcmc_diagnostics/",object@filename)
                system(paste("mkdir -p ", diagfolder))
                #Jos ei vielä luotuna bugs-mallia, luo se 
                #HUOM! olettaa, että jos .yml-tiedosto tuhottu, luodaan ja ajetaan uusi malli
                if(!file.exists(paste0(modelfolder, "/model.yml"))){
                    #Rakenna yaml-muotoinen kuvaus tehtävästä tilastollisesta mallista 
                    yamlstr <- as.yaml(list(y=list(name="location3",n=3), upper_x=list(name="lang",n=2),
                                 x=object@predictorlist, datafile = object@source.file , outputfile = object@post.file,
                                 jags_settings = list(list("adapt"=jags_settings$adapt),
                                                      list("n.chains"=jags_settings$n.chains),
                                                      list("burnin"=jags_settings$burnin),
                                                      list("thin"=jags_settings$thin),
                                                      list("sample"=jags_settings$sample),
                                                      list("method"=jags_settings$method)
                                                      ), diagnostics_directory = diagfolder))
    
                #Luo mallille oma kansionsa
                com = paste0("mkdir -p ", modelfolder)
                cat(system(com, intern=TRUE))
                write(yamlstr, paste0(modelfolder,"/model.yml"))

                cat(system(paste("python3",system.file("python","model_builder.py",package="phdBayes"), paste0(modelfolder,"/model.yml")), intern=TRUE))
                if(!dontrun){
                    cat("Running JAGS, this may take a while...","\n")
                    source(paste0(modelfolder,"/script.R"))
                }
                }

            })




#' Hakee rds-muodossa tallennetun posterioridatan perusteella tietyt usein tarvittavat kuviot ja 
#' oliot yhdeksi listaksi. Jos listaa ei ole vielä olemassa, tallentaa tämän rds-muodossa.
#' 
#' @import rjags coda reshape ggplot2 ggmcmc gridExtra
#' 
#' @param force_reload whether or not to force reloading the plots and all the other stuff
#' @param lang minkäkielisiä kuvioita tehdään
#' @export
#' 

setMethod("LoadResults","StandardBayesian",
    function(object, force_reload, lang="fi"){
        if(missing(force_reload)) force_reload =FALSE;
        object@datalist  <- list()
        datalist.file <- paste0(object@dump.path, object@filename,"_datalist.rds")
        if(file.exists(datalist.file) & !force_reload){
            #cat("Ladataan tallennetua dataa Bayes-malliin nimeltä",object@filename,"\n")
            object@datalist <- readRDS(datalist.file)
        }
        else{
            cat("Luodaan dataa Bayes-malliin nimeltä",object@filename,"\n")
            cat("Ladataan posteriorijakauma","\n")
            datalist <- list(post=readRDS(object@post.file))
            datalist$post.matr <- as.matrix(datalist$post)
            cat("Ladataan yhteenvetoa posteriorijakaumasta","\n")
            datalist$sumstats <- summary(datalist$post)$statistics
            cat("Luodaan keskihajontakuviota","\n")
            datalist$plots <- list()
            library(dplyr)
            mylabs_gen <- data.frame(Parameter=c(),Label=c())
            mylabs_int <- data.frame(Parameter=c(),Label=c())
            for (predictor in object@predictorlist){
                mylabs_gen <- mylabs_gen  %>% add_row(Parameter=paste0("std.",predictor$name), Label=predictor$name)
                mylabs_int <- mylabs_int  %>% add_row(Parameter=paste0("std.lang.",predictor$name), Label=predictor$name)
            }
            datalist$plots$std.all <- ggs_caterpillar(ggs(datalist$post, family="^std.[^\\.]+$", par_labels=mylabs_gen)) + theme_bw() +  geom_vline(xintercept = 0, linetype="dotted")
            datalist$plots$std.interact <- ggs_caterpillar(ggs(datalist$post, family="^std\\.lang\\.", par_labels=mylabs_int)) + theme_bw() +  geom_vline(xintercept = 0, linetype="dotted")
            #Hae jokaisesta prediktorista kuviot
            cat("Aletaan luoda prediktorikohtaisia kuvioita")
            for (predictor in object@predictorlist){
                datalist$plots[[predictor$name]] <- GetAllPlots(predictor$name, predictor$name, object@original.data, datalist$post, datalist$sumstats, lang)
            }
            #saveRDS(datalist,datalist.file,compress=F)
            object@datalist <- datalist
        }
        #HACKY!
        return (object@datalist)
    })


#' Tulostaa html-esityksen kuvioista
#'
#' 
#' @export
#' 

setMethod("PrintSvg","StandardBayesian",
          function(object, ...){
              location <- paste0("output/model_plots/")
              htmlstring  <-  paste("<html><head><meta charset='utf-8'></head>
                                    <style>
                                    main{
                                    /*
                                        display:flex;
                                        flex-flow:row wrap;
                                        width:80vw;
                                        margin:auto;
                                    */

                                    display: grid;
                                    grid-gap: 10px;
                                    grid-template-columns: repeat(3,31vw);

                                    }

                                    </style>
                                    <body><h1>Kuviot mallille ",object@filename,"</h1>
                                    <main>
                                    ")
              com = paste0("mkdir -p ", paste0(location,object@filename))
              cat(system(com, intern=TRUE))
              for(this.plot in names(object@datalist$plots)){
                  cat(this.plot,"\n")
                  if(grepl("std",this.plot)){
                    filename <- paste0(location,object@filename,"/",this.plot,".svg")
                    ggsave(filename,object@datalist$plots[[this.plot]] + labs(title= this.plot), device="svg")
                    htmlstring <- paste(htmlstring,paste0("<div class='first-div'><img object-fit='contain' width='100%'  src='",paste0(this.plot,".svg"),"'></div>"))
                    #ggsave(paste0(location,object@filename,this.plot,".svg"),object@datalist$plots[[this.plot]] + labs(title= this.plot), device="svg")
                  }
                  else{
                      for(plot_type in names(object@datalist$plots[[this.plot]])){
                          for(actual.plot.name in names(object@datalist$plots[[this.plot]][[plot_type]])){
                            filename.end <- paste0(paste(this.plot,plot_type,actual.plot.name,sep="_"),".svg")
                            filename <- paste0(location,object@filename,"/",filename.end)
                            object@datalist$plots[[this.plot]][[plot_type]][[actual.plot.name]] 
                            ggsave(filename,object@datalist$plots[[this.plot]][[plot_type]][[actual.plot.name]] + labs(title= filename.end), device="svg")
                            htmlstring <- paste(htmlstring,paste0("<div><img object-fit='contain' width='100%'  src='",filename.end,"'></div>"))
                          }
                      }
                  }
              }
            htmlstring  <- paste(htmlstring,"</main></body></html>")
            write(htmlstring,paste0(location,object@filename,"/index.html"))
          }
          )
