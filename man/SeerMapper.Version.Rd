\name{SeerMapper.Version}
\alias{SeerMapper.Version}
\title{Display SeerMapper Package Version and Build Date}
\description{
  Provides a means of retreiving SeerMapper's or Seer2010Mapper's 
  version number and build date and time.
}
\usage{SeerMapper.Version()}

\details{
 The \var{SeerMapper} package provide a simple 
 means of mapping data for the U. S. at several levels:  State,  State/County,  
 State/County/Census Tract, and U. S. Seer Registry areas. The package automatically
 determines the mapping level based on the data and the area identifiers provide at the time
 of the call.  The data is categorised and each area is drawn and colored appropriately
 based on its data value.
 
 The SeerMapper.Version function return the version 
 number and build date and time for package.
 
} 
\value{The return value is "SeerMapper Version X.X.X YY/MM/DD HH:MM Xm"}

\author{
James B Pearson and Linda Pickle, StatNet Consulting, LLC, Gaithersburg, MD
}