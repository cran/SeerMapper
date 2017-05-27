\name{sa99_dXX}
\alias{sa99_dXX}
\alias{sa99_d00}
\docType{data}
\title{The boundary dataset for the 20 U.S. Seer Registries.}
\description{
   This dataset contains a Spatial Polygons data frame for the 20 U. S. Seer registry areas 
   boundaries as of the U. S. 2000 or 2010 Census years.  Since the Seer Registries boundaries
   are the same for the 2000 and 2010 census, a single \var{sa99_d00} dataset is used by the 
   \var{SeerMapper} mapping packages for the NCI Seer Registry boundary data and information.
   The boundaries are used to map data by U. S. Seer registry area, to provide an
   overlay to state, county and census tract maps with the Seer registry areas boundaries, and 
   provide a intermediate boundary for drawing census tract or county boundaries.
   When doing counties or census tracts level mapping, the \var{SeerMapper} 
   package can be used to map the sub-areas within a registry that only have data or 
   can expand the county or census tract boundary's drawn to include all sub-areas up to a 
   Seer Registry or State boundary.
   This feature is controlled by the \var{countyB} and \var{tractB} call parameters. For more details
   see below.
   The Seer registry area boundaries mapping is controlled by the \var{seerB} call
   parameter.  When set to "NONE", no Seer registry area boundaries are drawn on county 
   or census tract maps.  If set to "DATA", the Seer registry boundaries will 
   be drawn for any Seer registry area containing sub-areas with data.
   If set to "ALL", all Seer registry boundaries will be drawn whether they contain 
   counties or census tracts with data.
 }
\usage{data(sa99_d00)}
\format{
  The dataset contains a SpatialPolygon data.frame for the 20 U. S. Seer registry areas
  based on the 2000 and 2010 U. S. Census county boundary data.  The \var{SeerMapper} package 
  uses the 2000 boundaries and the \var{SeerMapper2010} package uses the 2010 boundaries.
  Make sure to use the package that matches the appropriate census year.
  
  An registry area may contain multiple polygons.  For more details, refer to the 
  SpatialPolygon structure description in the "sp" package documentation.
  The 20 U. S. Seer registry areas are identify by character abbreviations:
 
  \tabular{lll}{
      saID    \tab stID \tab      Name\cr
      AK-NAT\tab	02\tab	Alaska Natives\cr
      AZ-NAT\tab	04\tab	Arizona Indians\cr
      CA-OTH\tab	06\tab	California Other (not SF-Oakland, SJ-Monterey, LA areas)\cr
      CA-SF\tab	06\tab	California San Fran/Oakland\cr
      CA-SJ\tab	06\tab	California San Jose/Monterey\cr
      CA-LA\tab	06\tab	California Los Angeles\cr
      CT\tab 09\tab	Connecticut\cr
      GA-OTH\tab	13\tab	Georgia Other (not Atlanta or Rural areas)\cr
      GA-ATL\tab	13\tab	Georgia Atlanta\cr
      GA-RUR\tab	13\tab	Georgia Rural\cr
      HI\tab	15\tab	Hawaii\cr
      IA\tab	19\tab	Iowa	\cr
      KY\tab        21\tab	Kentucky\cr
      LA\tab	22\tab	Louisiana\cr
      MI-DET\tab	26\tab	Michigan Detroit\cr
      NJ\tab	34\tab	New Jersey\cr
      NM\tab	35\tab	New Mexico\cr
      OK-CHE\tab    40\tab	Oklahoma Cherokee Nation\cr
      UT\tab	49\tab	Utah\cr
      WA-SEA\tab	53\tab	Washington Seattle-Puget Sound\cr
    }    
  
When mapping Seer registry area level data, the Seer registry area abbreviated name 
(\var{saID}) provides the linkage between Seer registry area level data provided by the user
and the Seer registry area boundary data.  These abbreviations must be used.

A Seer registry area may be composed of one or more polygon structures.  These are grouped by 
the Seer registry area in the SpatialPolygons structure.

The data slot of the spatialpolygonsdataframe contains two lists:  \var{saID}, \var{stID}, and \var{region}.
The \var{saID} is the Seer registry area abbreviation associated with the area.  The \var{stID} is the 2 digit fips code
for the state containing the Seer registry area.  One state may be a Seer registry area 
or contain multiple Seer registry areas.  But no Seer registry area spans multiple states.

The data slot can be used to identify the states associated with the active Seer registry areas.

}

\details{
When Seer Registry boundary be used to draw all of the county and tract boundaries up to the 
registry boundary to be able to provide a complete county or tract mapping within the registry. 
This is cone by setting either the \var{countyB} or \var{tractB} call parameter to "SEER".  This 
feature works the same as setting these call parameters to "STATE", but the boundary is the 
Registry boundaries.

The \var{sa99_d00} dataset is a spatial polygon data.frame (SPDF).
The spatial polygons represent the Seer Registry boundary data. The data
slot of the SPDF contains the following information about Seer Registries:
 \describe{
   \item{row.names}{The row.names attribute of the SPDF are character vectors 
     and are set to the Seer Registry abbreviations as listed above.
    }
   \item{stID}{a 2 digit character vector representing the U. S. State FIPS code
     where the Seer Registry is located.
    }
   \item{coCnt}{is an integer value of the number of counties in the Seer Registry.
    }
  }
Once the SPDF is loaded, the data slot is expanded to contain the following additional fields:
 \describe{
   \item{ID}{a copy of the row.names values of the Seer Registry abbreviations.
    }
   \item{saID}{a copy of the row.names values of the Seer Registry abbreviations.
    }
   \item{rgID}{the U. S. region ID containing the Seer Registry.
    }
 }
}
\author{
Jim Pearson \email{jpearson@statnetconsulting.com}\cr 
Maintainer: "Jim Pearson" \email{jpearson@statnetconsulting.com}\cr 
}
\keyword{Census2000}
\keyword{Census2010}
\keyword{Registries}
\keyword{Seer}
\keyword{datasets}
\keyword{SeerMapper}

