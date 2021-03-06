% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/altWrapper-tools.R
\name{getAltClassSetting}
\alias{getAltClassSetting}
\alias{setAltClassSetting}
\title{Get/Set altClass settings}
\usage{
getAltClassSetting(className = NULL, settingName = NULL, x = NULL)

setAltClassSetting(className = NULL, ..., x = NULL)
}
\arguments{
\item{className}{Character, the name of an altWrapper class}

\item{settingName}{A character vector. The name of the setting you want to query}

\item{x}{An altWrapper object}

\item{...}{Named arguments. It is used to change the setting.}
}
\value{
The settings of an altWrapper class
}
\description{
The function get or set altClass settings. The setting include
\code{autoExportClassDef}, \code{autoDuplicate} and \code{autoSerialize}.
}
\details{
\code{autoExportClassDef} determines whether the definition of a class will be exported to
other processes along with the exported variable(Default \code{TRUE}). If the setting
is FALSE, users are responsible to export the class definition to the other processes before
exporting an altWrapper variable in order to make sure the exported variable works properly.

\code{autoDuplicate} determines whether an altWrapper class can use a default duplication method
(Default \code{TRUE}).
The default duplication method will copy the underlying data that an altWrapper variable
is using to duplicate the altWrapper variable and resulting a new altWrapper object.
The default duplication is useful when
the data is not a reference of the other data sources. In case that the underlying data
is a reference(eg. file handle), the default duplication will fail to duplicate the
variable since only the handle will be duplicated. Users should define the
duplication function to overwrite the default behavior. An error will be thrown out
If \code{autoDuplicate} is FALSE and no duplication method is provided.

\code{autoSerialize} determines whether an altWrapper class can use a default serialize method
(Default \code{TRUE}). The default serialize method will serialize the underlying data of an
altWrapper object and send it to the other processes. If the altWrapper object relys on the
other data that is only available in the current processes, users should provide their customized
serialize function to overwrite the default serialize function. An error will be thrown out
If \code{autoSerialize} is FALSE and no serialize method is provided.
}
\examples{
## Define the ALTREP functions
length_func <- function(x) length(x)
get_ptr_func <- function(x,writeable) x


## Define the altWrapper class and its functions
setAltClass(className = "example", classType = "integer")
setAltMethod(className = "example", getLength = length_func)
setAltMethod(className = "example", getDataptr = get_ptr_func)

## Create an altWrapper object by providing the class name and data.
A <- newAltrep(className = "example", x = 1L:10L)
A

##Get altWrapper class settings by class name
getAltClassSetting(className = "example")

##Get altWrapper class settings by altWrapper object
getAltClassSetting(x = A)

}
