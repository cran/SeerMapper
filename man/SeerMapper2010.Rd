\name{SeerMapper2010}
\alias{SeerMapper2010}
\alias{SeerMapper2000}
\title{Quick Data Mapper at State, County, Census Tract or Seer Registry levels}
\description{
Provides a call to the \var{SeerMapper} main function in the package to create
maps of data using the U. S. 2010 census boundary identifiers and data.  While the \var{SeerMapper}
function can map against either the 2000 or 2010 census boundary data by using the \var{censusYear} call
parameter, the 
\var{SeerMapper2010} and \var{SeerMapper2000} function are provided minimize the 
call parameters required and also simplify the overall process. 
All of the \var{SeerMapper} call parameters can be specified and are passed along to 
\var{SeerMapper}.
}
\usage{SeerMapper2010(...)}
\arguments{
  \item{...}{any of the \var{SeerMapper} call parameters (see section on the \var{SeerMapper} function for details).
     All parameters are passed through to \var{SeerMapper}.
   }
}
\details{
For details on the supported call parameters, see the \var{SeerMapper} section of this
doccument.
}
\author{
James B Pearson and Linda Pickle, StatNet Consulting, LLC, Gaithersburg, MD
}