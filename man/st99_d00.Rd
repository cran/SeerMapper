\name{st99_d00}
\alias{st99_d00}
\docType{data}
\title{US State Boundary Data and Information}
\description{
  US State and Territory Boundary datasets from the 2000 or 2010 census
  for use with the Seer Mapper packages (\var{SeerMapper} 
  and the SatScan Mapping Program.
}
\usage{data(st99_d00)}
\format{
SpatialPolygons class of object. Keyed on the State FIPs code.
Each list element is a "polygons" class object contains a list 
of "Polygons" class objects describing the state boundaries and 
several attributes: Boundary Box, Label Point, Area, and ID.  
}
\details{
  \preformatted{
  The state FIPS code are:
      01 - Alabama
      02 - Alaska
      04 - Arkansas
      06 - California
      08 - Colorado
      09 - Connecticut
      10 - Delaware
      11 - District of Columbia
      12 - Florida
      13 - Georgia
      15 - Hawaii
      16 - Idaho
      17 - Illinois
      18 - Indiana
      19 - Iowa
      20 - Kansas
      21 - Kentucky
      22 - Louisiana
      23 - Maine
      24 - Maryland
      25 - Massachusetts
      26 - Michigan
      27 - Minnesota
      28 - Mississippi
      29 - Missouri
      30 - Montana
      31 - Nebraska
      32 - Nevada
      33 - New Hampshire
      34 - New Jersey
      35 - New Mexico
      36 - New York
      37 - North Carolina
      38 - North Dakota
      39 - Ohio
      40 - Oklahoma
      41 - Oregon
      42 - Pennsylvania
      44 - Rhode Island
      45 - South Carolina
      46 - South Dakota
      47 - Tennessee
      48 - Texas
      49 - Utah
      50 - Vermont
      51 - Virginia
      53 - Washington 
      54 - West Virginia
      55 - Wisconsin
      56 - Wyoming
      72 - Puerto Rico
    }

As of Version 1.0 of \var{SeerMapper} package, 
the .rda files are compressed using the "xz" method to reduce the disk space requirements.

The \var{SeerMapper} package contains the state boundary files from the U. S. 2000 Census 
state boundary data. Since the U. S. 2000 state boundaries are very similar to the 
U. S. 2010 state boundaries, the U. S. 2000 state boundaries are used for mapping data
for both census years.

The \var{censusYear} call parameter or using the \var{SeerMapper2010} function call
informs the package which census year boundaries to use during the mapping.
In the case of the state boundaries, they are the same.  Three states changed counties 
and have additional 2010 counties boundary files.  Most of the change
was with the tract boundaries, so there are two set of three supplemental packages 
containing the tract boundaries for each state.  

The state, county and tract boundary shape file data was downloaded from 
the U. S. census website on July 3, 2016.  The shapefiles were uploaded
to the "www.MapShaper.org" website and using the modified Visvalingam method 
with intersection repair enabled, prevent shape removal enabled, and 
coordinate precision set to 0.0, the boundaries were simplified from 100% down to 13% of
the original.  This reduced the space requirements 
by 90% while maintaining a visually usable boundary image and edge alignments.
The shapefiles were downloaded and converted to SpatialPolygons structures and saved as
one county dataset per state, one tract dataset per state and census year 
and the state boundary set (st99_d00).  The coordinates started as 
lat/long values and have been transformed to a cartisian coordinates system based
on proj4 string "+proj=eqdc +lat_1=33 +lat_2=49 +lat_0=39 +lon_0=96w +units=m" 
for mapping.  All boundary data used by the \var{SeerMapper} package is pre-transformed
to this project for better visualization and to reduce the time required to build each 
map.

The \var{st99_d00} is used to support both census years (2000 and 2010).  It contains the 
information to identify the three states that had county boundary changes and triggers 
the program to use different county boundary files when mapping census year 2010.

The boundary spatial data 

The @data structure of the saved \var{st99_d00} SpatialPolygonsDataFrame 
contains the following information for use by the \var{SeerMapper} function:
\describe{
  \item{row.names}{the row.names of the SPDF are the 2 digit state FIPS codes.
   }
  \item{ID}{the state 2 digit FIPS code.
   }
  \item{stID}{the state 2 digit FIPS code.
   }
  \item{abbr}{ = A character vector of the 2 letter state abbreviation.
   }
  \item{stName}{ = A character vector of the state full name.
   }
  \item{rgID}{ = a value representing the U. S. census region containing the state.
   }
  \item{rgName}{ = A character vector of the name of the region containing the state.
   }
  \item{dvID}{ = An integer representing the U. S. census regional division containing
     the state.
   }
  \item{dvName}{ = A character vector of the name of the regional division 
     containing the state.
   }
  \item{loc}{ = A character vector of the last part of the name of the supplemental  
     package containing the census tract boundary data for this state.
   }
  \item{DoAdj}{ = a value indicating if adjustments are required to the original 
    shapefile boundary data.  (e.g. moveX, moveY, scale) (Not used by \var{SeerMapper}.)
   }
  \item{moveX}{ = A numeric value of the number of degrees latitude the centroid of the 
     state was moved to create the boundary data for this state. (Not used by \var{SeerMapper}.)
   }
  \item{moveY}{ = A numeric value of the number of degrees longitude the centroid of the 
     state was moved to create the boundary data for this state. (Not used by \var{SeerMapper}.)
   }
  \item{scale}{ = A numeric value represntating the scaling factor used on the 
     original shapefile data to create the boundary data in this dataset.  The 
     value of 1 indicates no scaling change. (Not used by \var{SeerMapper}.)
   }
  \item{proj}{ = A character vector representing the proj4 values of the 
     boundary data projection.  All boundaries are transformed to an equal area
     projection for the mapping. (Not used by \var{SeerMapper}.)
   }
  \item{county00}{ = An integer value of the number of counties in the state
     during the 2000 census year.
   }
  \item{county10}{ = An integer value of the number of counties in the state
     during the 2010 census year. 
   }
  \item{tracts00}{ = An integer value of the number of 2000 census tracts 
     contained in the state.
   }
  \item{tracts10}{ = An integer value of the number of 2010 census tracts 
     contained in the state.
   }
  \item{change10}{ = A logical value (T/F) indicateing (if TRUE), the states 
     had county boundary changes in the U. S. 2010 Census.  If TRUE,
     the backages uses the coXX_d10 datasets for the states instead of 
     the standard cXX_d00 datasets for the county boundaries in the state for 
     mapping.
   }
  \item{c_X}{ = A numeric value of the X coordinate of the centroid of the state.
   }
  \item{c_Y}{ = A numeric value of the Y coordinate of the centroid of the state.
   }
 }

 To ensure the boundary datas correctly align when census tracts are mapped
 within counties and counties within states, the state boundaries based on 
 their census tract boundaries data.
 The state census tract data was imported from the U. S. census website and
 uploaded into URL:{www.mapshaper.org} and reduced as stated above to 10%.  
 The resulting shapefile for tracts in a state are saved as 
 the census tract baseline shapefile for the state.  The census 
 tract boundaries are joined together into counties to create the state's 
 county boundary dataset.  The county boundaries are then joined together
 to create the state boundary and added to this state boundary dataset.
}
\source{
The US State boundary SpatialPolygon structures are generated by collapsing 
the census tract bountaries from the U. S. Census website  
url{http://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html} for the 
appropriate census year - 2000 or 2010. 
The shapefiles were downloaded on July 3, 2016.
}
\examples{
  data(st99_d00)
}
\keyword{datasets}
