
############################################################

setClass(
		Class="menuitem", 
		representation(
			nombre= "character",
			previo = "function",
			ayuda = "function",
			opciones = "character",
			acciones = "list",
			fi= "logical"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("menuitem","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			nombre		= return(x@nombre),
			previo		= return(x@previo),
			ayuda 		= return(x@ayuda),
			opciones 	= return(x@opciones),
			acciones 	= return(x@acciones),
			fi 			= return(x@fi)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("menuitem","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			nombre		= x@nombre <- value,
			previo		= x@previo <- value,
			ayuda 		= x@ayuda <- value,
			opciones 	= x@opciones <- value,
			acciones 	= x@acciones <- value,
			fi 			= x@fi <- value
		)
		return(x)
	}
)

############################################################

setClass(
		Class="menu", 
		representation(
			items = "list"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("menu","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			items	= return(x@items)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("menu","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			items = x@items <- value
		)
		return(x)
	}
)

############################################################

setClass(
		Class="report", 
		representation(
			report 		= "logical",
			graph 		= "numeric",
			trueVal 	= "logical",
			actRep 		= "function",
			desGraph 	= "function",
			desRep 		= "function",
			commentt	= "logical",
			files		= "logical",
			contRep 	= "list"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("report","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			report		= return(x@report),
			graph		= return(x@graph),
			trueVal		= return(x@trueVal),
			actRep		= return(x@actRep),
			desGraph	= return(x@desGraph),
			desRep		= return(x@desRep),
			commentt	= return(x@commentt),
			files		= return(x@files),
			contRep		= return(x@contRep)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("report","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			report		= x@report <- value,
			graph		= x@graph <- value,
			trueVal		= x@trueVal <- value,
			actRep		= x@actRep <- value,
			desGraph	= x@desGraph <- value,
			desRep		= x@desRep <- value,
			commentt	= x@commentt <- value,
			files		= x@files <- value
		)
		return(x)
	}
)

############################################################

setClass(
		Class="cami", 
		representation(
			cami = "character"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("cami","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			cami	= return(x@cami)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("cami","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			cami	= x@cami <- value,
			
		)
		return(x)
	}
)

############################################################

setClass(
		Class="lineal", 
		representation(
			crit = "numeric",
			ls = "logical",
			atip = "ANY"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("lineal","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			crit		= return(x@crit),
			ls		= return(x@ls),
			atip 		= return(x@atip)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("lineal","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			crit		= x@crit <- value,
			ls		= x@ls <- value,
			atip 		= x@atip <- value
		)
		return(x)
	}
)

############################################################

setClass(
		Class="serie",
		representation(
			nom = "character",
			serie = "ts",
			orig = "numeric",
			sact = "logical",
			trans = "numeric",
			est = "numeric",
			reg = "numeric",
			stac = "logical",
			lin = "lineal"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("serie","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			nom		= return(x@nom),
			serie	= return(x@serie),
			orig 	= return(x@orig),
			sact 	= return(x@sact),
			trans 	= return(x@trans),
			est 	= return(x@est),
			reg 	= return(x@reg),
			stac 	= return(x@stac),
			lin 	= return(x@lin)
			
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("serie","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			nom		= x@nom <- value,
			serie	= x@serie <- value,
			orig 	= x@orig <- value,
			sact 	= x@sact <- value,
			trans 	= x@trans <- value,
			est 	= x@est <- value,
			reg 	= x@reg <- value,
			stac 	= x@stac <- value,
			lin 	= x@lin <- value
		)
		return(x)
	}
)

############################################################

setClass(
		Class="modelo", 
		representation(
			modelo = "ANY",
			mact = "logical",
			ser = "numeric",
			int = "logical",
			valid = "logical",
			est = "logical",
			eqm = "numeric",
			best = "logical"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("modelo","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			modelo	= return(x@modelo),
			mact	= return(x@mact),
			ser 	= return(x@ser),
			int 	= return(x@int),
			valid 	= return(x@valid),
			est 	= return(x@est),
			eqm 	= return(x@eqm),
			best 	= return(x@best)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("modelo","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			modelo	= x@modelo <- value,
			mact	= x@mact <- value,
			ser 	= x@ser <- value,
			int 	= x@int <- value,
			valid 	= x@valid <- value,
			est 	= x@est <- value,
			eqm 	= x@eqm <- value,
			best 	= x@best <- value
		)
		return(x)
	}
)

############################################################

setClass(
		Class="datos",
		representation(
			lserie = "list",
			sident = "numeric",
			lmodelo = "list",
			mident = "numeric",
			modif = "logical"
			)
)

##### Get
setMethod(
	f="[",
	signature=c("datos","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			lserie	= return(x@lserie),
			sident	= return(x@sident),
			lmodelo	= return(x@lmodelo),
			mident 	= return(x@mident),
			modif 	= return(x@modif)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("datos","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			lserie	= x@lserie <- value,
			sident	= x@sident <- value,
			lmodelo	= x@lmodelo <- value,
			mident 	= x@mident <- value,
			modif 	= x@modif <- value
		)
		return(x)
	}
)
	
############################################################	

setClass(
		Class="sessio.ts",
        representation(
			menu = "menu",
			cami = "cami",
			datos = "datos",
			student = "logical",
			report = "report"
		)
)

##### Get
setMethod(
	f="[",
	signature=c("sessio.ts","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			menu		= return(x@menu),
			cami		= return(x@cami),
			datos 		= return(x@datos),
			student 	= return(x@student),
			report		= return(x@report)
		)
	}
)

###### Set
setMethod(
	f="[<-",
	signature=c("sessio.ts","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			menu		= x@menu <- value,
			cami		= x@cami <- value,
			datos 		= x@ayuda <- value,
			student 	= x@student <- value,
			report 		= x@report <- value
		)
		return(x)
	}
)
	
############################################################
	
setClass(
		Class="tex",
        representation(
			menu = "matrix",
			opciones = "matrix",
			previo = "matrix",
			ayuda = "matrix",
			ini  = "matrix",
			gesser = "matrix",
			trans = "matrix",
			iden = "matrix",
			gesmod = "matrix",
			estim = "matrix",
			valid = "matrix",
			cap = "matrix",
			atip = "matrix",
			prev = "matrix",
			salir = "matrix",
			plot = "matrix",
			drawser = "matrix",
			drawmod = "matrix",
			writelistser = "matrix",
			writemod = "matrix",
			writelistmod = "matrix",
			writecoef = "matrix",
			drawarma = "matrix",
			atipics = "matrix",
			drawatip = "matrix",
			previsiones = "matrix",
			enterComment = "matrix",
			latexCntrl = "matrix"
		)
)

###### Get
setMethod("[",
	signature=c("tex","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			menu		= return(x@menu),
			opciones	= return(x@opciones),
			previo		= return(x@previo),
			ayuda		= return(x@ayuda),
			ini			= return(x@ini),
			gesser		= return(x@gesser),
			trans		= return(x@trans),
			iden		= return(x@iden),
			gesmod		= return(x@gesmod),
			estim		= return(x@estim),
			valid		= return(x@valid),
			cap			= return(x@cap),
			atip		= return(x@atip),
			prev		= return(x@prev),
			salir		= return(x@salir),
			plot		= return(x@plot),
			drawser		= return(x@drawser),
			drawmod		= return(x@drawmod),
			writelistser	= return(x@writelistser),
			writemod	= return(x@writemod),
			writelistmod= return(x@writelistmod),
			writecoef	= return(x@writecoef),
			drawarma	= return(x@drawarma),
			atipics		= return(x@atipics),
			drawatip	= return(x@drawatip),
			previsiones = return(x@previsiones),
			enterComment = return(x@enterComment),
			latexCntrl = return(x@latexCntrl)
		)
	}
)

###### Set
setMethod("[<-",
	signature=c("tex","character","missing","ANY"),
	def = function(x,i,j,value){
		switch(EXP=i,
			menu		=x@menu <- value,
			opciones	= x@opciones <- value,
			previo		= x@previo <- value,
			ayuda		= x@ayuda <- value,
			ini			= x@ini <- value,
			gesser		= x@gesser <- value,
			trans		= x@trans <- value,
			iden		= x@iden <- value,
			gesmod		= x@gesmod <- value,
			estim		= x@estim <- value,
			valid		= x@valid <- value,
			cap			= x@cap <- value,
			atip		= x@atip <- value,
			prev		= x@prev <- value,
			salir		= x@salir <- value,
			plot		= x@plot <- value,
			drawser		= x@drawser <- value,
			drawmod		= x@drawmod <- value,
			writelistser	= x@writelistser<- value,
			writemod	= x@writemod <- value,
			writelistmod= x@writelistmod <- value,
			writecoef	= x@writecoef <- value,
			drawarma	= x@drawarma <- value,
			atipics		= x@atipics <- value,
			drawatip	= x@drawatip <- value,
			previsiones = x@previsiones <- value,
			enterComment = x@enterComment <- value,
			latexCntrl = x@latexCntrl <- value
		)
		return(x)
	}
)
	
setGeneric("makeMenu", function(x, opciones, ...) standardGeneric("makeMenu"))
setGeneric("initializerRep", function(report, contRep, ...) standardGeneric("initializerRep"))
setGeneric("initializer", function(object, student, report, contRep, ...) standardGeneric("initializer"))
setGeneric("ender", function(session, ...) standardGeneric("ender"))
setGeneric("activate", function(x, ...) standardGeneric("activate"))
setGeneric("addcami", function(x, ...) standardGeneric("addcami"))
setGeneric("getcami", function(x, ...) standardGeneric("getcami"))
setGeneric("nextGraphic", function(x, name, twoplot, pos, contRep, ...) standardGeneric("nextGraphic"))
setGeneric("getserie", function(x, ...) standardGeneric("getserie"))
setGeneric("addserie", function(x, ...) standardGeneric("addserie"))
setGeneric("getsident", function(x, ...) standardGeneric("getsident"))
setGeneric("addsident", function(x, ...) standardGeneric("addsident"))
setGeneric("getmodelo", function(x, ...) standardGeneric("getmodelo"))
setGeneric("addmodelo", function(x, ...) standardGeneric("addmodelo"))
setGeneric("getmident", function(x, ...) standardGeneric("getmident"))
setGeneric("addmident", function(x, ...) standardGeneric("addmident"))
setGeneric("getmodif", function(x, ...) standardGeneric("getmodif"))
setGeneric("addmodif", function(x, ...) standardGeneric("addmodif"))
setGeneric("resumen", function(x, ...) standardGeneric("resumen"))
setGeneric("texinitializer", function(x, ...) standardGeneric("texinitializer"))



#######Funciones de realizacion del informe#######
setMethod("nextGraphic",signature(x = "numeric",name="character",twoplot="ANY",pos="ANY", contRep="list"),
          function(x,name,twoplot,pos,contRep,...)
		{
	if(missing(twoplot)) twoplot=F
    x <- x+1
    aux <-paste("Grafh",x,".jpg", sep="")
    jpeg(file=aux)
	if(twoplot){
		if(pos==1){
			cat("\n\\begin{figure}[H]")
			cat("\n\\centering")
			cat("\n\\captionsetup{listformat=empty}")
			
			cat(paste("\n\\subfloat[","*",name,"]{\\includegraphics[width=",contRep$twograph,"\\linewidth]{",aux,"}}",sep=""))
		}else{
			cat(paste("\n\\subfloat[","*",name,"]{\\includegraphics[width=",contRep$twograph,"\\linewidth]{",aux,"}}",sep=""))			
			cat("\n\\end{figure}\n")
		}
	}else{
		cat("\n\\begin{figure}[H]")
		cat("\n\\centering")
		cat(paste("\n\\includegraphics[width=",contRep$maingraph,"\\linewidth]{",aux,"}\n",sep=""))
		cat(paste("\n\\caption{",name,"}",sep=""))
		cat("\n\\end{figure}\n")	
	}
	return(x)

  }
)
###########

setMethod("addcami",signature(x = "cami"),
          function(x, id=NULL,...)
		{
		if(is.null(id)){
			length(x@cami)=length(x@cami)-1
		}else{
			x@cami=c(x@cami,id)
		}
		x
	}
)


setMethod("getcami",signature(x = "cami"),
          function(x,...)
		{
		a=x@cami[length(x@cami)]
		a
	}
)

setMethod("getserie", signature(x = "datos"),
          function(x, id=NULL,...)
		{
		if(is.null(id)){
			x@lserie[[x@sident]]
		}else{
			x@lserie[[id]]
		}
	}
)

setMethod("addserie", signature(x = "datos"),
          function(x, s,id=NULL,...)
		{
		if(is.null(id)){
			x@lserie[[length(x@lserie)+1]]=s
			x=addsident(x,length(x@lserie))
		}else{
			x@lserie[[id]]=s
		}
		x
	}
)



setMethod("getsident", signature(x = "datos"),
          function(x,...)
		{
		x@sident
	}
)

setMethod("addsident", signature(x = "datos"),
          function(x, a,...)
		{
		x@sident=a
		x
	}
)


setMethod("getmodelo", signature(x = "datos"),
          function(x, id=NULL,...)
		{
		if(is.null(id)){
			x@lmodelo[[x@mident]]
		}else{
			x@lmodelo[[id]]
		}
	}
)

setMethod("addmodelo", signature(x = "datos"),
          function(x, m, id=NULL,...)
		{
		if(is.null(id)){
			if (x@mident == 0) {
				x@lmodelo[[1]]=m
			} else {
				x@lmodelo[[length(x@lmodelo)+1]]=m
			}
			x=addmident(x,length(x@lmodelo))
		}else{
			x@lmodelo[[id]]=m
		}
		x
	}
)


setMethod("getmident", signature(x = "datos"),
          function(x,...)
		{
		x@mident
	}
)

setMethod("addmident", signature(x = "datos"),
          function(x, a,...)
		{
		x@mident=a
		x
	}
)


setMethod("getmodif", signature(x = "datos"),
          function(x,...)
		{
		x@modif
	}
)

setMethod("addmodif", signature(x = "datos"),
          function(x, a,...)
		{
		x@modif=a
		x
	}
)


setMethod("resumen", signature(x = "datos"),
          function(x, a,...)
		{
		for(n in 1:length(x@lserie)){
			s=getserie(x,n)
			s@sact=T
			x=addserie(x,s,n)

		}
		if(getmident(x) != 0){
			for(n in 1:length(x@lmodelo)){
				s=getmodelo(x,n)
				s@mact=T
				x=addmodelo(x,s,n)
			}
		}
		x
	}
)








slotcreator=function(tex,pos,nlines,name){
	aux=matrix(NA,nrow=max(nlines),ncol=length(pos),dimnames=list(NULL,name))
	for(i in 1:length(pos)){
		aux[,i]=c(tex[(pos[i]+1):(pos[i]+nlines[i])],rep(NA,max(nlines)-nlines[i]))
	}
	return(aux)
}


findtext=function(keynames,tex){
	m=matrix(c(keynames,rep(NA,length(keynames)*2)),nrow=length(keynames),ncol=3)
	j=1
	for(i in 1:(length(keynames)-1)){
		find=F
		while(!find){
			if(m[i,1]==tex[j]) find=T
			j=j+1
		}
		m[i,2]=j-1
	}
	m[length(keynames),2]=length(tex)
	aux1=as.numeric(m[,2][-1])
	aux2=as.numeric(m[,2][-length(m[,2])])
	a=aux1-aux2-1
	a[length(a)+1]=0
	m[,3]=a
	return(m)
}


setMethod(	"texinitializer",
			signature="tex",
			function(x, ...){
				for(i in 1:length(.libPaths())){
					fil=try(file(paste(.libPaths()[i],"/TSTutorial/","english",".txt",sep="")),silent=T)
					af=suppressWarnings(try(readLines(fil),silent=T))
					if(!is(af,"try-error")) tex=af
					close(fil)
				}
				keynames=c("--menu",
							paste("--opciones.",1:10,sep=""),
							paste("--previo.",1:3,sep=""),
							paste("--ayuda.",1:10,sep=""),
							paste("--ini.",1,sep=""),
							paste("--gesser.",1:2,sep=""),
							paste("--trans.",4:6,sep=""),
							paste("--iden.",2:3,sep=""),
							paste("--gesmod.",1:2,sep=""),
							paste("--estim.",2:4,sep=""),
							paste("--valid.",2:5,sep=""),
							paste("--cap.",2:4,sep=""),
							paste("--atip.",2,sep=""),
							paste("--prev.",2,sep=""),
							"--salir",
							"--plot",
							"--drawser",
							"--drawmod",
							"--writelistser",
							"--writemod",
							"--writelistmod",
							"--writecoef",
							"--drawarma",
							"--atipics",
							"--drawatip",
							"--previsiones",
							"--enterComment",
							"--latexCntrl",
							"--end"
				)
				m=findtext(keynames,tex)
				slotpos=c(1,10,3,10,1,2,3,2,2,3,4,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
				slotname=c("menu","opciones", "previo", "ayuda", "ini", "gesser", "trans", "iden", "gesmod", "estim", "valid", "cap", "atip",
							"prev", "salir","plot", "drawser", "drawmod", "writelistser", "writemod", "writelistmod", "writecoef", "drawarma",
							"atipics", "drawatip", "previsiones", "enterComment", "latexCntrl"
				)
				listname=list(
					c(1),c("ini","trans","gesser","iden","estim","gesmod","valid","cap","atip","prev"),c("previotit1","previotit2","previo"),
					c("ini","gesser","trans","iden","gesmod","estim","valid","cap","atip","prev"),c(1), c(1:2), c(4:6), c(2:3), c(1:2),c(2:4),c(2:5), c(2:4), c(2),
					c(2), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1), c(1)
				)
				aux=1
				pos=as.numeric(m[,2][aux:slotpos[1]])
				nlines=as.numeric(m[,3][aux:slotpos[1]])
				name=listname[[1]]
				x[slotname[1]]<-slotcreator(tex,pos,nlines,name)
				aux=slotpos[1]+1
				for(i in 2:length(slotpos)){
					pos=as.numeric(m[,2][aux:(aux+slotpos[i]-1)])
					nlines=as.numeric(m[,3][aux:(aux+slotpos[i]-1)])
					name=listname[[i]]
					x[slotname[i]]<-slotcreator(tex,pos,nlines,name)
					aux=aux+slotpos[i]
				}
				return(x)
			}
)







setMethod(	"makeMenu", 
			signature(x = "menu", opciones = "list"),
			function(x, opciones, ...){
			co=1
			for (nom in names(opciones)){
				k=length(opciones[[nom]])
				x@items[[co]]=new(Class="menuitem",
					nombre=nom,
					previo = get(paste(nom,"previo",sep=".")),
					ayuda = get(paste(nom,"ayuda",sep=".")),
					opciones=opciones[[nom]],
					acciones=apply(matrix(paste(nom,1:k,sep=".")),1,function(el) get(el,pos=environment(TSTutorial))),
					fi=FALSE)
				co=co+1
				}
			names(x@items)=names(opciones)
			return(x)
		}
)

latexCntrl=function(...){
	latex=F
	for(i in 1:length(.libPaths())){
			fil=try(file(paste(.libPaths()[i],"/TSTutorial/test/LatexTest.tex",sep="")),silent=T)
			af=suppressWarnings(try(readLines(fil),silent=T))
			if(!is(af,"try-error")) text=af
			close(fil)
	}
	suppressWarnings(try(close(fil),silent=T))
	aux="LatexTest.tex"
	sink(aux)
	for(i in 1:length(text)){
		cat(paste(text[i],"\n",sep=""))
	}
	sink()
	suppressWarnings(sink())					
	b=suppressWarnings(try(ifelse(test=as.logical(Sys.info()["sysname"] == "Windows"),
		shell("pdfLatex LatexTest.tex",intern=T),
		system("pdflatex LatexTest.tex",intern=T)),silent=T))
	div=apply(as.matrix(rep(1:nchar(b[1]))),1,function(el) substr(b[1],el,el))
	if(paste(div[1],div[2],div[3],div[4],sep="")=="This") latex=T
	suppressWarnings(try(unlink(paste("LatexTest",c(".pdf",".aux",".log",".tex"),sep="")),silent=T))
	if(!latex){
		prettyprint(tex["latexCntrl"][1,"1"])
		cat("\n")
		return(F)
	}else{
		return(T)
	}
}

reportCntrl=function(report=T, comment=F, files=F){
	stopifnot(is.logical(report),is.logical(comment),is.logical(files))
	if(report){
			report=latexCntrl()
	}
	if(!report){
		comment=F
		files=F
	}
	list(
		report = as.logical(report),
		comment = as.logical(comment),
		files = as.logical(files)
	)
}

contRepCntrl=function(fil=F, tex=NULL, name=NULL, maingraph=0.7, twograph=0.47){
	stopifnot(is.logical(fil),is.numeric(maingraph),is.numeric(twograph))
	if((fil)&(is.null(tex))) stop("tex needs to have a file .tex conection")
	list(
		fil = fil,
		tex = tex,
		name = name,
		maingraph = maingraph,
		twograph = twograph
	)
}

nameser=function(name){
	print(name)
	if(substr(name, start=11, stop=17)=="series"){
		cont=1
		nam=""
		while((substr(name, start=17+cont, stop=17+cont)!=",")|(substr(name, start=11+cont, stop=11+cont)!=")")){
			print(substr(name, start=17+cont, stop=17+cont))
			nam=paste(nam,substr(name, start=17+cont, stop=17+cont),sep="")
			cont=cont+1
			print(cont)
		}
	}else{
		cont=1
		nam=""
		while((substr(name, start=11+cont, stop=11+cont)!=",")|(substr(name, start=11+cont, stop=11+cont)!=")")){
			print(substr(name, start=11+cont, stop=11+cont))
			nam=paste(nam,substr(name, start=11+cont, stop=11+cont),sep="")
			cont=cont+1
			print(cont)
		}		
	}
	return(nam)
}

setMethod(	"initializerRep", 
			signature(report = "list", contRep = "list"),
			function(report, contRep, ...){
					
				actRep=function(...){
					sink(paste(contRep$name,".tex",sep=""),append=T)	
					return(invisible())
				}	
				desGraph=function(...){
					try(dev.off(),silent=T)
				}
				desRep=function(...){
					sink()
					suppressWarnings(sink())
					return(invisible())
				}
				aux=new(Class="report",report=report[["report"]],graph=0,trueVal=report[["report"]],actRep=actRep,desGraph=desGraph,
					desRep=desRep,commentt=report[["comment"]],files=report[["files"]],contRep=contRep)
				return(aux)
			}
)

setMethod(	"initializer",
			signature(object = "ts", student="logical",  report="list", contRep="list"),
			function(object, student, report, contRep, ...){
				
				menus=new(Class="menu",items=list(NULL))
				menus=makeMenu(menus,opciones)
				creport=initializerRep(report,contRep)
				cami=new(Class="cami",cami="ini")
				lineal=new(Class="lineal",crit=0,ls=F,atip=NULL)
				serie=new(Class="serie",nom=contRep$name ,serie=(object),orig=1,sact=T,trans=1,est=0,reg=0,stac=F,lin=lineal)
				datos=new(Class="datos",lserie=list(serie),sident=1,lmodelo=list(NULL),mident=0,modif=T)

				session=new(Class="sessio.ts",
					menu=menus,
					cami=cami,
					datos=datos,
					student = student,
					report=creport
				)
				if(report[["report"]]){
					for(i in 1:length(.libPaths())){
						fil=try(file(paste(.libPaths()[i],"/TSTutorial/Sweave.sty",sep="")),silent=T)
						af=suppressWarnings(try(readLines(fil),silent=T))
						if(!is(af,"try-error")) text=af
						close(fil)
					}
					suppressWarnings(try(close(fil),silent=T))
					aux="Sweave.sty"
					sink(aux)
					for(i in 1:length(text)){
						cat(paste(text[i],"\n",sep=""))
					}
					sink()
					suppressWarnings(sink())	
					for(i in 1:length(.libPaths())){
						fil=try(file(paste(.libPaths()[i],"/TSTutorial/fancyvrb.sty",sep="")),silent=T)
						af=suppressWarnings(try(readLines(fil),silent=T))
						if(!is(af,"try-error")) text=af
						close(fil)
					}
					suppressWarnings(try(close(fil),silent=T))
					aux="fancyvrb.sty"
					sink(aux)
					for(i in 1:length(text)){
						cat(paste(text[i],"\n",sep=""))
					}
					sink()
					suppressWarnings(sink())	
					if(!contRep$fil){
						sink(paste(contRep$name,".tex",sep=""))	
						cat("\\documentclass{article}\n")
						cat("\n\\usepackage[colorlinks=true,urlcolor=blue]{hyperref}\n")
						cat("\n\\usepackage{color}\n")
						cat("\n\\usepackage{Sweave}\n")
						cat("\n\\usepackage[cp1252]{inputenc}\n")
						cat("\n\\usepackage{amscd}\n")
						cat("\n\\usepackage{graphics}\n")
						cat("\n\\usepackage{float}\n")
						cat("\n\\usepackage{subfig}\n")				
						cat("\n\\addtolength{\\oddsidemargin}{-.875in}\n")
						cat("\n\\addtolength{\\evensidemargin}{-.875in}\n")
						cat("\n\\addtolength{\\textwidth}{1.75in}\n")
						cat("\n\\addtolength{\\topmargin}{-.875in}\n")
						cat("\n\\addtolength{\\textheight}{1.75in}\n")
						cat("\n\\makeindex\n")
						cat("\n\\begin{document}\n")
						cat(paste("\n\\title{",tex["previsiones"][1,"1"],": ",contRep$name,"}\n",sep=""))
						user=Sys.info()["user"]
						cat(paste("\n\\author{",user,"}\n",sep=""))
						cat("\n\\maketitle\n")
						cat("\n\\tableofcontents\n")
						cat("\n\\newpage\n")
						creport["desRep"]()
					}else{
						tex=suppressWarnings(readLines(contRep$tex,n=-1))
						sink(paste(contRep$name,".tex",sep=""))
						cat(tex,sep="\n")
						creport["desRep"]()
					}
				}
				return(session)
		}
)

setMethod(	"ender",
			signature(session = "sessio.ts"),
			function(session, ...){
				session["report"]["actRep"]()
				cat("\n\\section{ References }\n")
				cat("\nThe report is made with the library 'TSTutorial' from the software R.\n")
				cat("\n\\end{document}\n")
				session["report"]["desRep"]()

				b=try(ifelse(test=as.logical(Sys.info()["sysname"] == "Windows"),
				shell(cmd=paste("pdfLatex",session["report"]@contRep$name,sep=" "),intern=T),
				system(paste("pdflatex ",session["report"]@contRep$name,".tex",sep=""),intern=T)),silent=T)	
				#Se vuelve a realizar ya que con sólo una ejecución, el índice, en la gran mayoría de ocasiones, no aparece.

				b=try(ifelse(test=as.logical(Sys.info()["sysname"] == "Windows"),
				shell(cmd=paste("pdfLatex",session["report"]@contRep$name,sep=" "),intern=T),
				system(paste("pdflatex",session["report"]@contRep$name,sep=" "),intern=T)),silent=T)	
			

				b=try(ifelse(test=as.logical(Sys.info()["sysname"] == "Windows"),
				shell.exec(paste(session["report"]@contRep$name,".pdf",sep="")),
				system(paste("xpdf ",session["report"]@contRep$name,".pdf&",sep=""))),silent=T)
				
				if(!session["report"]["files"]){
						LatexFiles=c(".tex",".aux",".idx",".log",".out",".toc")
						deleteList=c(paste("Grafh",1:session["report"]["graph"],".jpg",sep=""),paste(session["report"]@contRep$name,LatexFiles,sep=""),
									"Sweave.sty","fancyvrb.sty"
						)
						unlink(deleteList)
				}
				prettyprint(tex["previsiones"][2,"1"])				
				return(invisible())
		}
)

setMethod(	"activate", 
			signature(x = "sessio.ts"),
			function(x, ...){
				men=x@menu@items[[getcami(x@cami)]]
				n=length(men@opciones)
				valores=men@previo(men=list(cami=x@cami,datos=x@datos,student=x@student,report=x@report))
				x@datos=valores$datos
				res=0
				res=menu(c(men@opciones,tex["menu"][1,"1"],tex["menu"][2,"1"]),graphics=F)
				while((res<=0)|(res>(n+2))){
					res=menu(c(men@opciones,tex["menu"][1,"1"],tex["menu"][2,"1"]),graphics=F)
				}
				if (res<=n)	valores=men@acciones[[res]](men=list(cami=x@cami,datos=x@datos,student=x@student,report=x@report))
				if (res==n+1) valores=men@ayuda(men=list(cami=x@cami,datos=x@datos,student=x@student,report=x@report))
				if (res==n+2) valores=salir(men=list(cami=x@cami,datos=x@datos,student=x@student,report=x@report))
				
				x@cami=valores$cami
				x@datos=valores$datos
				x@report=valores$report
				return(x)
			}
)



tex=new(Class="tex",opciones=matrix(NA),previo=matrix(NA),ayuda=matrix(NA),ini=matrix(NA),gesser=matrix(NA),trans=matrix(NA),iden=matrix(NA),gesmod=matrix(NA),
			estim=matrix(NA),valid=matrix(NA),cap=matrix(NA),atip=matrix(NA),prev=matrix(NA),salir=matrix(NA),plot=matrix(NA),drawser=matrix(NA),drawmod=matrix(NA),
			writelistser=matrix(NA),writemod=matrix(NA),writelistmod=matrix(NA),writecoef=matrix(NA), drawarma=matrix(NA),atipics=matrix(NA),drawatip=matrix(NA),
			enterComment=matrix(NA),latexCntrl=matrix(NA)
)
tex=texinitializer(tex)

