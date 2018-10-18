#' On the basis of a coda object, retrieve human readable names for the variables
#' 
#' @param VarName tutkittava muuttuja
#' @param VarValues lista, jossa arvot
#' @param MeanGuide vektori keskiarvoista
#' @param MeanGuide vektori keskiarvoista
#' @param MeanTreshold minkä alle jäävät arvot väritetään vaaleammalla värillä
#' @param lang tulostetaanko suomen- vai englanninkielisiä kuvioita
#' 
#' @export

GetLabelNamesForGgMcmc <- function(VarName, VarValues, MeanGuide, MeanTreshold=0.1, lang="fi"){
    VarLengths <- lengths(VarValues)
    replist <- list()
    for(i in 1:length(VarValues)){
        repeach <- 1
        reptimes <- 1
        if(i==1){
            repeach <- 1
            reptimes <- prod(VarLengths[2:length(VarValues)])
        }
        else if(i<length(VarValues)){
            repeach <- prod(VarLengths[1:(i-1)])
            reptimes <- prod(VarLengths[(i+1):length(VarValues)])
        }
        else{
            repeach <- prod(VarLengths[1:(i-1)])
        }
        replist[[i]] <- list(reptimes=reptimes,repeach=repeach, repvals=c(1:length(VarValues[[i]])))
    }
    tobepasted <- list()
    idx <- 1
    for(thislist in replist){
        tobepasted[[idx]] <- rep(thislist$repvals,times=thislist$reptimes,each=thislist$repeach)
        idx <- idx +1
    }
    rawlabels <- do.call("paste", c(tobepasted, sep = ","))
    tobepastednames <- list()
    idx <- 1
    for (thislist in tobepasted){
        thesenames <- c()
        for(val in thislist){
            thesenames <- c(thesenames, VarValues[[idx]][val])
        }
        tobepastednames[[idx]] <- thesenames
        idx <- idx +1
    }
    labels <- do.call("paste", c(tobepastednames, sep = ","))
    labels <- gsub("std.","",labels)
    Parameter=paste(VarName, "[", rawlabels, "]", sep="")
    return (data.frame(Parameter=Parameter, Label=labels))

}

#' Luo caterpillarkuviot kaikista sijainneista sekä lisäksi interaktiosta 
#' kielen kanssa
#'
#' @export

GetAllPlots <- function(varname.in.model, varname.in.df,  vaihe1.df, post, sumstats, lang="fi"){
    cat("\nGenerating plots for", varname.in.df, " -- this may take a while..","\n") 
    ##1. ilman interaktioita
    gen.ints <- GetLabelNamesForGgMcmc(varname.in.model,
                                       list(
                                            levels(as.factor(vaihe1.df[[varname.in.df]])),
                                            levels(as.factor(vaihe1.df$location3))),
                                       MeanGuide=sumstats, 
                                       lang=lang)
    genplots <- list()
    if(lang == "fi"){
        locnames <- c("S1","S2_S3","S4")
    }
    else{
        locnames <- c("L1","L2_L3","L4")
    }
    for(s in c(1:3)){
        cat(varname.in.df, " -- Ilman interaktioita -- ", locnames[s],"\n")
        regex <- paste("^",gsub("\\.","\\\\.",varname.in.model),"\\[\\d+,",s,sep="")
        genplots[[locnames[s]]] <- StyleGgmcmcComp(ggs_caterpillar(ggs(post, family=regex, par_labels=gen.ints), horizontal=T)  )
    }
    #2. interaktiot
    intvarname <- ifelse(grepl("b\\.",varname.in.model),paste("b.lang.",varname.in.df,sep=""),paste("lang.",varname.in.df,sep=""))
    ia.ints <- GetLabelNamesForGgMcmc(intvarname,
                                      list(
                                           levels(as.factor(vaihe1.df$lang)), 
                                           levels(as.factor(vaihe1.df[[varname.in.df]])),
                                           levels(as.factor(vaihe1.df$location3))
                                           ),
                                      MeanGuide=sumstats,
                                      lang=lang)
    iaplots <- list()
    for(s in c(1:3)){
        cat(varname.in.df, " -- interaktioilla kielen kanssa -- ", locnames[s],"\n")
        #regexpin alussa oleva 1 tarkoittaa, että vertaillaan suhteessa kielimuuttujan arvoon 1 ("fi")
        regex <- paste("^",gsub("\\.","\\\\.",intvarname),"\\[1,\\d+,",s,sep="")
        iaplots[[locnames[s]]] <- StyleGgmcmcComp(ggs_caterpillar(ggs(post, family=regex, par_labels=ia.ints), thin_ci = c(0.05, 0.95), thick_ci = c(0.25, 0.75), horizontal=T))
    }

    return(list("general"=genplots,"interaction"=iaplots))
}

#' Muokkaa ggmcmc-kuvion ulkoasua
#'
#' @param ggo ggmcmc-kuvio 
#' 
#' @export

StyleGgmcmcComp <- function(ggo){
greycols <- c("#a6a6a6","#404040")
return(ggo + theme_bw() + scale_color_manual(guide=F,values = greycols) + geom_vline(xintercept = 0, linetype="dotted"))
}
