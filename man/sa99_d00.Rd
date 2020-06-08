\name{sa99_d00}
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
   can expand the hsa, county or census tract boundary's drawn to include all 
   sub-areas up to a Seer Registry or State boundary.
   This feature is controled by the \var{hsaB}, \var{countyB} and \var{tractB} 
   call parameters. For more details see below.
   The Seer registry area boundaries mapping is controlled by the \var{seerB} call
   parameter.  When set to "NONE", no Seer registry area boundaries are drawn on county 
   or census tract maps.  If set to "DATA", the Seer registry boundaries will 
   be drawn for any Seer registry area containing sub-areas with data.
   If set to "STATE", all Seer registry boundaries will be drawn in a state that 
   contains a sub-area with data.
   If set to "ALL", all Seer registry boundaries will be drawn.
 }
\usage{data(sa99_d00)}
\format{
  The dataset contains a SpatialPolygon data.frame of the 20 U. S. Seer registry areas
  based on the 2000 and 2010 U. S. Census county boundary data and index information 
  for use by \var{SeerMapper}.  The Seer Registry boundary and information are the same
  for both census years.
  
  An registry area may contain multiple polygons.  For more details, refer to the 
  SpatialPolygon structure descripted in the "sp" package documentation.  
  The SpatialPolygons is constructed to have all of the polygons for a single 
  registry contained under a single SpatialPolygon instead of several. 
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
      NJ\tab	34\tab	New Jersery\cr
      NM\tab	35\tab	New Mexico\cr
      OK-CHE\tab    40\tab	Oklahoma Cherokee Nation\cr
      UT\tab	49\tab	Utah\cr
      WA-SEA\tab	53\tab	Washington Seattle-Puget Sound\cr
    }    
  
When mapping Seer registry area level data, the Seer registry area abbreviated name 
must be used.

The data slot of the SpatialPolygonsDataFrame (sa99_d00) contains information
on the associated state, region identifiers, the centroid of the registry,
and the number of counties and tracts in the registry.


The data slot can be used to identify the states associated with the active 
Seer registry areas.
}

\details{
When Seer Registry boundary be used to draw all of the county and tract boundaries up to the 
registry boundary to be able to provide a complete county or tract mapping within the registry. 
This is cone by setting either the \var{countyB} or \var{tractB} call parameter to "SEER".  This 
feature works the same as setting these call parameters to "STATE", but the boundary is the 
Registry boundaries.

The PROJ.4 projection used for these boundaries is:

"+proj=aea +lat_1=33 +lat_2=49 +lat_0=39 +lon_0=96W +ellps=WGS84"

The \var{sa99_d00} dataset is a SpatialPolygonsDataFrame (SPDF).
The spatial polygons represent the Seer Registry boundary data. The data
slot of the SPDF contains the following information about Seer Registries:
 \describe{
  \item{row.names}{The row.names attribute of the SPDF are character vectors 
     and are set to the Seer Registry abbreviations as listed above.
    }
  \item{ID}{ - character string representing the Seer Registry ID, the Seer Registry area
     abbreviation. \var{saID} and \var{ID} values provides the linkage between 
     Seer registry area level data provided by the user and the Seer registry area 
     boundary data.  These abbreviations must be used.  This field is the 
     same as the row.names of the SPDF.
   }
  \item{saID}{ - same as ID.  The Seer Registry Identifier.
   }
  \item{stID}{ - the state ID containing the Seer Registry.  The \var{stID} is 
    the 2 digit fips code for the state containing the Seer registry area.  
    One state may be a Seer registry area or contain multiple Seer registry areas.  
    But no Seer registry area spans multiple states.
   }
  \item{stName}{ - character vector of the state name containing the Seer Registry.
   }
  \item{rgID}{ - the U. S. census region ID containing the Seer Registry.
   }
  \item{c_X}{ - a numeric value of the centroid's X coordinates.
   }
  \item{c_Y}{ - a numeric value of the centroid's Y coordinates.
   }
  \item{county00}{ - a numeric value of the number of tracts in the Seer Registry in 
    the 2000 census.
   }
  \item{county10}{ - a numeric value of the number of counties in the Seer Registry in 
    the 2010 census.
   }
  \item{tracts00}{ - a numeric value of the number of counties in the Seer Registry in 
    the 2000 census.
   }
  \item{tracts10}{ - a numeric value of the number of tracts in the Seer Registry in 
    the 2010 census.
   }
 }
}
\keyword{Census2000}
\keyword{Census2010}
\keyword{Registries}
\keyword{Seer}
\keyword{datasets}
\keyword{SeerMapper}

