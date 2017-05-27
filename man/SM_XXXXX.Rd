\name{SM_XXXXX}
\alias{SM_XXXXX}
\alias{SM_GlobInit}
\alias{SM_Build}
\alias{SM_Mapper}
\title{Internal Functions to utilize boundary data and create maps.}
\description{Provides support for \var{SeerMapper} and other packages that can 
  use the \var{SeerMapper} core functions and boundary datasets to map 
  state, registry, county or tract sub-areas in the U. S.  
  Based on the location IDs provided loads into memory the U. S. state and 
  registry boundary data, determines the sub-area level being mapped,
  then loads the county and (if needed) census tract boundary 
  data for any state containing a sub-area with data and maps the data and 
  requested boundaries.
}
\usage{SM_Mapper(rPM,MV)} 
\arguments{
  \item{rPM}{This a named list of \var{SeerMapper} variables and
    call parameter values used by the internal functions of \var{SeerMapper}.
   }
  \item{MV}{This a named list of \var{SeerMapper} boundary data and location
    IDs to support the requested map.
   }
 }
\details{
    Three functions are exported by \var{SeerMapper} for use by 
    other packages offered by this author.  These functions are internal
    functions and not designed for general use.  The three exposed functions are: 
    \var{SM_GlobInit}, \var{SM_Build}, and \var{SM_Mapper}.  Each uses the 
    \var{rPM} and \var{MV} named lists to receive and return information
    to the caller.  \var{SM_GlobInit} builds the initial \var{rPM} named list
    and populates it with default values. \var{SM_Build} receives the \var{rPM} 
    and \var{MV} named lists, validates the location ID supplied, sets the level
    of the boundaries needed, loads the boundaries from the datasets contained
    in the packages of \var{SeerMapper}, implements the expanded boundaries to 
    be drawn beyond the data sub-areas, and returns the loaded boundaries in the 
    \var{MV} named list.  \var{SM_Mapper} receives the \var{rPM} and \var{MV}
    and based on the information provided, creates the map.
    The functions are very depended on the information in the \var{rPM} and 
    \var{MV} begin correct, the use of these functions or modification to 
    there data structures is not recommended.
    } 
\value{The functions returns named list containing the \var{rPM} named list 
  (\var{SM_GlobInit}, \var{SM_Build}), the \var{MV} named list (all three functions)
  and a named list of the graphic x and y limits used in the mapping.
}
\author{
 Jim Pearson \email{jpearson@statnetconsulting.com}\cr 
 Maintainer: "Jim Pearson" \email{jpearson@statnetconsulting.com}\cr 
}
\keyword{SeerMapper}
