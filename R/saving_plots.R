
#'  Lukee tilastollista mallia koskevan datan rds-tiedostosta
#' 
#' 
#' @param name mallin nimi
#' 
#' @export

ReadData <- function(name){
    datapath = getOption("modeldatapath")
    datalist <- readRDS(paste0(datapath,paste0(name,"_datalist.rds")))
    return (datalist)
}

#'  Luo pdf-tiedoston kuvasta
#' 
#' @param obj ggplot-kuvio
#' @param fname tallennettava tiedostonimi
#' @param thiswidth kuvan leveys
#' @param thisheight kuvan korkeus
#' @param coord_start x-akselin alkupää
#' @param coord_end x-akselin loppupää
#' 
#' @export

CreatePdfFile <- function(obj, fname, thiswidth, thisheight, coord_start, coord_end){
        system(paste("mkdir -p ", dirname(fname)))
        if(thiswidth & thisheight){
            pdf(fname, width=thiswidth,height=thisheight)
        }
        else{
            pdf(fname)
        }
        show(
             obj + coord_cartesian(xlim = c(coord_start, coord_end)) + 
             xlab("Vaikutuksen voimakkuus logaritmisella asteikolla") +
             ylab("") + 
             theme(
                   axis.title=element_text(size=7)
                   #axis.text=element_text(size=6)
                   ) 
         ) 
        dev.off()
}

#'  Tallentaa pdf-tiedostot yhden til. mallin kuvioista. 
#' 
#' @param datalist StandardBayesian-olion datalista
#' @param modelname mallin nimi
#' @param plotname kuvion nimi tai muuttujan nimi
#' @param thiswidth kuvan leveys
#' @param thisheight kuvan korkeus
#' @param coords x-akselin raja-arvot muodossa c(general.start,general.end,int.start,int.end)
#' 
#' @export

SavePdf <- function(datalist, modelname, plotname, thiswidth=F, thisheight=F, coords=c(-2,2,-2,2)){
    if(grepl("std",plotname)){
        pl <- datalist$plots[[plotname]]
        fname <- paste0(getOption("phdpath"),"figure/modelplots/",modelname,"/std/",plotname,".pdf")
        CreatePdfFile(pl, fname, thiswidth, thisheight, coords[1], coords[2])
    }
    else{
        i = 1
        for(type in c("general","interaction")){
            for(loc in c("S1","S2_S3","S4")){
                pl <- datalist$plots[[plotname]][[type]][[loc]]
                fname <- paste0(getOption("phdpath"),
                                "figure/modelplots/",
                                modelname,"/",
                                plotname,"/",
                                type, "/",
                                loc, ".pdf")
                CreatePdfFile(pl, fname, thiswidth, thisheight, coords[i], coords[i+1])
            }
            i  <- i+2
        }
    }
}
