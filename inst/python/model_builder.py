import yaml
import sys
import os
import subprocess
import numbers
import textwrap

class Model():

    def __init__(self, yamlfile):
        with open(yamlfile,"r") as f:
            params = yaml.safe_load(f)

        self.y = YVar(params["y"])
        self.upper_x = UxVar(params["upper_x"],self.y)
        self.x=list()
        self.datafile = params["datafile"]
        self.outputfile = params["outputfile"]
        self.jags={}
        for setting in params["jags_settings"]:
            for key, item in setting.items():
                self.jags[key] = item if isinstance(item, numbers.Number) else "'{}'".format(item)
        self.diagnostics_directory = params["diagnostics_directory"]

        for xitem in params["x"]:
            self.x.append(XVar(xitem, self.upper_x, self.y))

        self.folder = yamlfile[:yamlfile.rfind("/")+1]
        if not self.folder.strip():
            self.folder = "./"

        #Jos annettu jokin kolmas parametri, käynnistä R-skripti
        self.launch = True if len(sys.argv)>2 else False

    def PrintLikelihood(self):
        output = ""
        output += "\n\n#LIKELIHOOD\n\n"

        uxloop = ForLoop(self.upper_x.name)
        for x in self.x:
            uxloop.NestForLoop(ForLoop(x.name))

        probs=Matrix("probs",[self.upper_x.name] + [x.name for x in self.x] + [self.y.name])

        phi=Matrix("phi",[self.upper_x.name] + [this_x.name for this_x in self.x] + [self.y.name])
        probs_assign = probs.Output() + " <- "  + phi.Output() + " / " + "sum({})".format(phi.Output(self.y.name))

        
        phi_assign = "log({}) <- {} \n".format(phi.Output(), " + ".join([self.y.matrix.Output(), self.upper_x.matrix.Output()] + [" + ".join([x.bmatrix.Output(), x.bintmatrix.Output()]) for x in self.x]))

        yloop = ForLoop(self.y.name)
        yloop.NestLine(" "*4 + probs_assign, " "*4 + phi_assign)

        observations = Matrix("observations", [self.upper_x.name] + [this_x.name for this_x in self.x] + [self.y.name])
        totals = Matrix("totals", [self.upper_x.name] + [this_x.name for this_x in self.x])
        obs_assign = observations.Output(self.y.name) + " ~ dmulti(" + probs.Output(self.y.name) + ", " + totals.Output() + ")"

        uxloop.NestLine(obs_assign, yloop.Output())

        output += uxloop.Output()
        return output

    def PrintPriors(self):
        output = ""
        output += "\n\n#PRIORS\n\n" 
        #Sigmat ja taut
        for idx, thisvar in enumerate([self.y, self.upper_x] + self.x):
            output += "sigma.{var} ~ dt(0,1,1) T(0,)\ntau.{var} <- pow(sigma.{var},-2)\n \n".format(var=thisvar.name)
            if idx>1:
                #x vars
                output += "sigma.{uxvar}.{var} ~ dt(0,1,1) T(0,)\ntau.{uxvar}.{var} <- pow(sigma.{uxvar}.{var},-2)\n \n".format(var=thisvar.name, uxvar=self.upper_x.name)
        #y-muuttujan jakaumat
        yloop = ForLoop(self.y.name)
        yloop.NestLine("{y}[{y}.idx] ~ dnorm(0, tau.{y})".format(y=self.y.name))
        output += yloop.Output() + "\n \n"
        #Varsinaiset priorit
        #Ensin upper x
        yloop = ForLoop(self.y.name)
        yloop.NestLine("{ux}.raw[{y}.idx] ~ dnorm(0, tau.{ux})".format(ux=self.upper_x.name, y=self.y.name))
        yloop.NestLine("{ux}[1, {y}.idx] <- {ux}.raw[{y}.idx] - mean({ux}.raw[1:N{y}])".format(ux=self.upper_x.name, y=self.y.name))
        yloop.NestLine("{ux}[2, {y}.idx] <- -{ux}[1, {y}.idx]\n \n".format(ux=self.upper_x.name, y=self.y.name))
        #Sitten kukin x:stä
        for x in self.x:
            if x.n > 2:
                xloop = ForLoop(x.name)
                xloop.NestLine("    {x}.raw[{x}.idx, {y}.idx] ~ dnorm(0, tau.{x})".format(x=x.name,y=self.y.name))
                xloop.NestLine("    {x}[{x}.idx, {y}.idx] <- ".format(x=x.name,y=self.y.name))
                xloop.NestLine("        {x}.raw[{x}.idx, {y}.idx] ".format(x=x.name,y=self.y.name))
                xloop.NestLine("        - mean({x}.raw[1:N{x}, {y}.idx]) ".format(x=x.name,y=self.y.name))
                xloop.NestLine("        - mean({x}.raw[{x}.idx, 1:N{y}]) ".format(x=x.name,y=self.y.name))
                xloop.NestLine("        + mean({x}.raw[1:N{x}, 1:N{y}]) ".format(x=x.name,y=self.y.name))
                xloop.NestLine("    #INTERACTION")
                xloop.NestLine("    {ux}.{x}.raw[{x}.idx, {y}.idx] ~ dnorm(0, tau.{ux}.{x})".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                xloop.NestLine("    {ux}.{x}[1, {x}.idx, {y}.idx] <- ".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                xloop.NestLine("        {ux}.{x}.raw[{x}.idx, {y}.idx] ".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                xloop.NestLine("        - mean({ux}.{x}.raw[1:N{x}, {y}.idx]) ".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                xloop.NestLine("        - mean({ux}.{x}.raw[{x}.idx, 1:N{y}]) ".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                xloop.NestLine("        + mean({ux}.{x}.raw[1:N{x}, 1:N{y}]) ".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                xloop.NestLine("    {ux}.{x}[2,{x}.idx,{y}.idx] <- -{ux}.{x}[1,{x}.idx,{y}.idx]".format(ux=self.upper_x.name,x=x.name,y=self.y.name))
                yloop.NestLine(xloop.Output())
            else:
                yloop.NestLine("#A BINARY PREDICTOR:\n\n")
                yloop.NestLine("{x}.raw[{y}.idx] ~ dnorm(0, tau.{x})".format(x=x.name, y=self.y.name))
                yloop.NestLine("{x}[1, {y}.idx] <- {x}.raw[{y}.idx] - mean({x}.raw[1:N{y}])".format(x=x.name, y=self.y.name))
                yloop.NestLine("{x}[2, {y}.idx] <- -{x}[1, {y}.idx]\n \n".format(x=x.name, y=self.y.name))
                yloop.NestLine("{ux}.{x}.raw[{y}.idx] ~ dnorm(0, 0.001)".format(ux=self.upper_x.name, x=x.name,y=self.y.name))

                yloop.NestLine("{ux}.{x}[1, 1, {y}.idx] <- {ux}.{x}.raw[{y}.idx] - mean({ux}.{x}.raw[1:N{y}]) ".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                yloop.NestLine("{ux}.{x}[1, 2, {y}.idx] <- -{ux}.{x}[1,1,{y}.idx]".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                yloop.NestLine("{ux}.{x}[2, 1, {y}.idx] <- -{ux}.{x}[1,1,{y}.idx]".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
                yloop.NestLine("{ux}.{x}[2, 2, {y}.idx] <- {ux}.{x}[1,1,{y}.idx]".format(ux=self.upper_x.name, x=x.name,y=self.y.name))
        output += yloop.Output() + "\n \n"

        return output

    def PrintSumToZero(self):
        output = ""
        output += "\n\n#SUM-TO-ZERO\n\n" 
        yloop = ForLoop(self.y.name)
        hascontent=False
        for this_x in self.x:
            if this_x.n > 2:
                hascontent = True
                uxloop = ForLoop(self.upper_x.name)
                xloop = ForLoop(this_x.name)
                xloop.NestLine("   {mxm} <- {ym} + {intm} + {xm} + {intxm}".format(mxm=this_x.meanmatrix.Output(), ym=self.y.matrix.Output(), intm=self.upper_x.matrix.Output(), xm=this_x.matrix.Output(), intxm=this_x.intmatrix.Output()))
                uxloop.NestForLoop(xloop)
                yloop.NestLine(uxloop.Output())

        yloop.NestLine("\n")

        for this_x in self.x:
            if this_x.n > 2:
                yloop.NestLine("    {b0m} <- mean({mxm})".format(b0m=this_x.b0matrix.Output(),mxm=this_x.meanmatrix.Output([self.upper_x.name,this_x.name])))

        yloop.NestLine("\n")

        for this_x in self.x:
            if this_x.n > 2:
                uxloop = ForLoop(self.upper_x.name)
                xloop = ForLoop(this_x.name)
                xloop.NestLine("    {bmx} <- mean({mxm}) - {b0m}".format(bmx=this_x.bmatrix.Output(),mxm=this_x.meanmatrix.Output([self.upper_x.name]), b0m=this_x.b0matrix.Output()))
                uxloop.NestLine("    {bimx} <- {mxm} - ({b0m} + {uxm} + {bmx})".format(bimx=this_x.bintmatrix.Output(), b0m=this_x.b0matrix.Output(), bmx=this_x.bmatrix.Output([self.upper_x.name]),mxm=this_x.meanmatrix.Output(),uxm=self.upper_x.matrix.Output(),bxm=this_x.bmatrix.Output()))
                xloop.NestLine(uxloop.Output())
                yloop.NestLine(xloop.Output())


        output += yloop.Output() 
        if hascontent:
            return output
        else:
            return ""

    def PrintSdVars(self):
        output = "#STANDARD DEVIATIONS\n"
        output += "std.{uxname} <- sd({uxmatr})".format(uxname=self.upper_x.name,uxmatr=self.upper_x.matrix.Output(omitall=True)) + "\n"
        for this_x in self.x:
            output += "std.{xname} <- sd({xmatr})".format(xname=this_x.name,xmatr=this_x.bmatrix.Output(omitall=True)) + "\n"
        for this_x in self.x:
            output += "std.{uxname}.{xname} <- sd({xmatr})".format(xname=this_x.name, uxname=self.upper_x.name, xmatr=this_x.bintmatrix.Output(omitall=True))  + "\n"
        return output

    def CreateScript(self):
        xvarlist = [self.upper_x] + self.x
        output = "#!/usr/bin/env Rscript\n\n"
        output += "library(runjags)\nlibrary(coda)\nlibrary(mcmcplots)\n\n"
        output += "aineisto <- readRDS('{}')\n\n".format(self.datafile)

        output += "for(varname in c({}))".format(",".join(["'" + x.name + "'" for x in xvarlist]))
        output += "{\n"
        output += "    aineisto[[varname]] <- as.factor(aineisto[[varname]])\n"
        output += "}\n\n"

        output += "observations <- xtabs(~ {}, data=aineisto)\n".format(" + ".join([x.name for x in xvarlist] + [self.y.name]))
        output += "totals <- xtabs(~ {}, data=aineisto)\n\n".format(" + ".join([x.name for x in xvarlist]))

        n = "N{0} = length(unique(aineisto${0}))"
        output += "dataList <- list(observations=observations, totals=totals, {})\n\n".format(", ".join([n.format(x.name) for x in xvarlist] + [n.format(self.y.name)]))

        output += "monitor <- c({})\n\n".format(", ".join(["'{}'".format(x.bname) for x in xvarlist] +
                                                      ["'std.{}'".format(x.name) for x in xvarlist] + 
                                                      ["'{}'".format(x.bintname) for x in self.x] +
                                                      ["'std.{}.{}'".format(self.upper_x.name, x.name) for x in self.x]))

        output += "RunJagsModel <- run.jags(data=dataList, monitor=monitor, model='{}', {})\n\n".format(self.folder + "model.bugs", ", ".join(["{}={}".format(key, item) for key, item in self.jags.items()]))
        output += "post <- as.mcmc.list(RunJagsModel)\n"
        output += 'saveRDS(post,"{}")\n'.format(self.outputfile)
        output += 'mcmcplot(post,dir="{}")\n'.format(self.diagnostics_directory)
        output += 'summary(post)'

        return output

    def Run(self):
        #bugs = "model{ \n" + self.PrintLikelihood() + "\n" + self.PrintPriors() + "\n" + self.PrintSumToZero() + "\n" + self.PrintSdVars() + "\n}"
        bugs = "model{ \n" + self.PrintLikelihood() + "\n" + self.PrintPriors() + "\n" + self.PrintSdVars() + "\n}"
        bugs = os.linesep.join([s for s in bugs.splitlines() if s])
        script = self.CreateScript()
        with open(self.folder + "model.bugs", "w") as f:
            f.write(bugs)
        with open(self.folder + "script.R", "w") as f:
            f.write(script)
        if self.launch:
            #ps = subprocess.Popen(("Rscript", "script.R"),  cwd=self.folder)
            ps = subprocess.Popen(("ntfy", "-b" ,"telegram", "done", "Rscript", "script.R"),  cwd=self.folder)

class ModelVar():
    def __init__(self, item):
        self.name = item["name"]
        self.bname = self.name
        self.n = item["n"]
        #if self.n > 2:
        #    self.bname = "b." + self.name

class YVar(ModelVar):
    def __init__(self, item):
        super().__init__(item)
        self.matrix = Matrix(self.name,[self.name])

class UxVar(ModelVar):
    def __init__(self, item, y):
        super().__init__(item)
        self.matrix = Matrix(self.name,[self.name, y.name])
        self.rawmatrix = Matrix(self.name + ".raw",[y.name])

class XVar(ModelVar):
    def __init__(self, item, ux, y):
        super().__init__(item)
        self.bintname = "{}.{}".format(ux.name,self.name)
        #if self.n > 2:
        #    self.bintname = "b.{}.{}".format(ux.name,self.name)
        self.matrix = Matrix(self.name,[self.name, y.name])
        self.rawmatrix = Matrix(self.name + ".raw",[y.name])
        self.intmatrix = Matrix(ux.name + "." + self.name,[ux.name, self.name, y.name])
        self.rawintmatrix = Matrix(ux.name + "." + self.name + ".raw",[y.name])
        self.bmatrix=self.matrix
        self.bintmatrix=self.intmatrix
        if self.n > 2:
            #self.b0matrix = Matrix(self.name + "b0",[y.name])
            #self.bmatrix = Matrix(self.bname,[self.name, y.name])
            #self.bintmatrix = Matrix("b." + ux.name + "." + self.name,[ux.name, self.name, y.name])
            self.meanmatrix = Matrix("m" + self.name,[ux.name,self.name,y.name])

class ForLoop():
    def __init__(self, varname):
        self.header = "for ({var}.idx in 1:N{var})".format(var=varname)
        self.nestedlines = []
        self.nestedforloops = []

    def NestLine(self,*arg):
        for a in arg:
            self.nestedlines.append(a)

    def NestForLoop(self,*arg):
        for a in arg:
            self.nestedforloops.append(a)

    def Output(self):
        s = self.header + "{ "
        for loop in self.nestedforloops:
            s += loop.header + "{ "
            if loop.nestedlines:
                s += "\n" + "\n".join([" "*4 + nl for nl in loop.nestedlines]) 
        s += "\n" + "\n".join([" "*4 + nl for nl in self.nestedlines]) + "\n" +"}"*(len(self.nestedforloops)+1)

        return s

class Matrix():
    def __init__(self, myname, varnames):
        self.name = myname
        self.vars = varnames

    def Output(self, omits=[], noindex=False, omitall=False):
        if noindex:
            s = self.name + "[]"
        else:
            s = self.name + "["
            for idx, varname in enumerate(self.vars):
                if omitall:
                    printedidx = ""
                    spacechar = ""
                else:
                    spacechar= " "
                    printedidx = varname + ".idx" 
                    if isinstance(omits, dict):
                        for key, item in omits.items():
                            if varname == key:
                                printedidx = item
                    elif varname in omits: 
                        printedidx = "1:N" + varname
                s += "{}{}".format(printedidx, "," + spacechar if idx+1<len(self.vars) else "]")
        return s

def PrintPrior(thisx, y, interaction=None):
    yloop = ForLoop(y.name)
    xloop.NestLine("{ux}.raw[{y}.idx] ~ dnorm(0, tau.{ux})".format(ux=intera))


    if thisx.n == 2:
        yloop = ForLoop(y.name)
        yloop.NestLine("{rawxm} ~ dnorm(0, tau.{name})".format(name=thisx.name, rawxm=thisx.rawmatrix.Output()))
        yloop.NestLine("{xm} <- {rawxm} - mean({rawxm2})".format(xm=thisx.matrix.Output({thisx.name:1}),rawxm=thisx.rawmatrix.Output(), rawxm2=thisx.rawmatrix.Output(noindex=True)))
        yloop.NestLine("{xm} <- -{xm2}".format(xm=thisx.matrix.Output({thisx.name:2}),xm2=thisx.matrix.Output({thisx.name:1})))
        yloop.NestLine("\n")
        if interaction:
            s += "sigma.{int}.{name} ~ dt(0,1,1) T(0,)\ntau.{int}.{name}  <- pow(sigma.{int}.{name},-2)".format(name=thisx.name,int=interaction.name)
            yloop.NestLine("{intrawm} ~  dnorm(0, tau.{int}.{name})".format(intrawm=thisx.rawintmatrix.Output(),name=thisx.name, int=interaction.name))
            yloop.NestLine("{intm} <- {intrawm} - mean({intrawm2})".format(intm=thisx.intmatrix.Output({interaction.name:1,thisx.name:1}),intrawm=thisx.rawintmatrix.Output(),intrawm2=thisx.rawintmatrix.Output(noindex=True)))
            yloop.NestLine("{intm} <- -{intm2}".format(intm=thisx.intmatrix.Output({interaction.name:1,thisx.name:2}),intm2=thisx.intmatrix.Output({interaction.name:1,thisx.name:1})))
            yloop.NestLine("{intm} <- -{intm2}".format(intm=thisx.intmatrix.Output({interaction.name:2,thisx.name:1}),intm2=thisx.intmatrix.Output({interaction.name:1,thisx.name:1})))
            yloop.NestLine("{intm} <- {intm2}".format(intm=thisx.intmatrix.Output({interaction.name:2,thisx.name:2}),intm2=thisx.intmatrix.Output({interaction.name:1,thisx.name:1})))
        return s + "\n\n" + yloop.Output()

    if thisx.n > 2:

        xloop = ForLoop(thisx.name)
        xloop.NestLine("    {xm} ~ ddirich(alpha[])".format(xm=thisx.matrix.Output(y.name)))
        if interaction:
            intloop = ForLoop(interaction.name)
            intloop.NestLine("    {intm} ~ ddirich(alpha[])".format(intm=thisx.intmatrix.Output(y.name)))
            xloop.NestLine(intloop.Output())
        return xloop.Output()



model = Model(sys.argv[1])
model.Run()

