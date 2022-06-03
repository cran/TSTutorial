\name{TSTutorial}
\alias{TSTutorial}
\alias{TSTutorial-method}
\alias{TSTutorial,ts-method}


\title{
  Interactive Laboratory of Time Series
}
\description{
\code{TSTutorial} is an interactive laboratory for learning fitting and getting predictions from Time Series. 

It consist on an ordered collections of menus where it is explained the Box-Jenkins methodology. The principal objective of the function is to be a useful tool in the process of learning this methodology.

Howerver, if you are an expert user who knows the Box-Jenkins methodology, you can use \code{TSTutorial} to work with your time series because it avoids you to write all the code.

To know more how to use \code{TSTtutorial} you can read the vignette "Tutorial" of this package or directly read it executing the function and selecting the third option \code{(3. Tutorial)} of the first menu. 
}
\usage{
TSTutorial(series, student, report, contRep)
}

\arguments{
  \item{series}{
     Object of clase .ts (Time Series)
}
  \item{student}{
     A logical which indicates if you want the student mode (T) or expert mode (F). See "Details".
}
  \item{report}{
     A logical list which indicates if you want to get a report of the session after finish it, and more options. See "Details".
}

  \item{contRep}{
     A list containing some parameters to modify parameters of the report, introduce an .tex file modified by the user, etc. See "Details" for more information.
}

}

\details{
The \code{student} has two options:
	
	\code{T}{: the laboratory is for students. It shows help texts and suggestions to teach the Box-Jenkins method.}
	
	\code{F}{: the laboratory is for expert users. Only shows the basic information avoiding all the helps and suggestions. It is an useful tool for Time Series advanced users to avoid to write their own scripts.}
	
The \code{report} has three options:
	
	\code{report}{: a logical value indicating (with the value \code{T}) to the program to create a .pdf file with the name of the time series introduced in \code{TStutorial} and it is saved in the current working directory.}
	
	\code{comment}{: a logical value inidicating (with the value \code{T}) to let to the user write comments during the session which will appear in the report. By default, and when report is \code{F}, its value is \code{F}.}
	
	\code{files}{: a logical value inidicating (with the value \code{T}) to not delete the files which are created during the report building process. In this case, the only file created will be the .pdf file . By default, and when report is \code{F}, its value is \code{F}.}

The \code{contRep} has three options:
	
	\code{fil}{: a logical value indicating (with the value \code{T}) whether the user set a new .tex file introduced in \code{name}. By default is \code{F}.}
	
	\code{name}{: the name of the .tex file which the head of the report is to be read from. By default is \code{NULL}.}
	
	\code{maingraph}{: The size of the graphics that are plotted in one column.}

	\code{twograph}{: The size of the graphics that are plotted in two columns.}

	
}

\value{
	None(invisible NULL).
}

\references{
 Box Jenkins
}

\author{
	Author and maintainer: Alberto Lopez Moreno\cr
}

\seealso{
Overview: \code{\link{TSTutorial-package}}.\cr
Methods	: \code{\link{TSTutorial}}.\cr
}

\examples{
#data(AirBcn)
#TSTutorial(AirBcn)

}
