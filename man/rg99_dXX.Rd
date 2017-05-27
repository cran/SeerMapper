\name{rg99_dXX}
\alias{rg99_dXX}
\alias{rg99_d00}
\docType{data}
\title{U. S. 2000 or 2010 Census Regional Boundary Definitions}
\description{This dataset contains the boundary data for the 4 U. S. 2000 or 2010 Census Regions for use by 
  the NCI \var{SeerMapper} package.
}
\usage{data(rg99_d00)}
\format{
 This dataset contain a single SpatialPolygonsDataFrame type object that contains
 the boundary polygon definitions for the 5 U. S. Census Regions for the year 2000 or 2010 and 
 an extra region covering Alaska, Hawaii and Puerto Rico. 
 The U. S. Census web site defines the continental regions with values of 1 through 4 as
 NorthEast, South, MidWest and West.  The states of Alaska, Hawaii, and the territory of Puerto
 Rico were assign regional values of 0, effectively a 5th region - offshore.
 The regional boundary data is the same for both the 2000 and 2010 census.
 This dataset maintains the codes as published by the U. S. Census Bureau.  
 The regions are keyed using the U. S. 2000 or 2010 Census region codes based on which package
 being used. The \var{SeerMapper} package supports the 2000 and 2010 boundaries. Only one dataset is 
 required: rg99_d00.
} 
\details{
  \preformatted{
  The U. S. 2000 and 2010 Census Region codes:
      0 - Offshore
      1 - NorthEast
      2 - South
      3 - MidWest
      4 - West
  
  The U. S. States/Territories in the Offshore region (code = 0) are"
      02 - Alaska
      15 - Hawaii
      72 - Puerto Rico
  
  The U. S. States in the NorthEast region (code = 1) are:
      FIPS Name
      09 - Connecticut
      23 - Maine
      25 - Massachusetts
      33 - New Hampshire
      34 - New Jersey
      36 - New York
      42 - Pennsylvania
      44 - Rhode Island
      50 - Vermont
  
  The U. S. States in the South region (code = 2) are:
      FIPS Name
      01 - Alabama
      04 - Arkansas
      10 - Delaware
      11 - District of Columbia
      12 - Florida
      13 - Georgia
      21 - Kentucky
      22 - Louisiana
      24 - Maryland
      28 - Mississippi
      37 - North Carolina
      40 - Oklahoma
      45 - South Carolina
      47 - Tennessee
      48 - Texas
      51 - Virginia
      54 - West Virginia
  
  The U. S. States in the Midwest region (code = 3) are:
      17 - Illinois
      18 - Indiana
      19 - Iowa
      20 - Kansas
      26 - Michigan
      27 - Minnesota
      29 - Missouri
      31 - Nebraska
      38 - North Dakota
      39 - Ohio
      46 - South Dakota
      55 - Wisconsin
  
  The U. S. States in the West region (code = 4) are:
      06 - California
      08 - Colorado
      16 - Idaho
      30 - Montana
      32 - Nevada
      35 - New Mexico
      41 - Oregon
      49 - Utah
      53 - Washington 
      56 - Wyoming
  }

As of Version 1.0 of the \var{SeerMapper} package,  
The ".rda" datasets are compressed using the "xz" method to reduce the disk space requirements.

The region boundary data was created from the state boundary data used in 
this package to ensure the borders lined up.  The state boundary data
was created from county and census tract boundary data processed 
by "www.MapShaper.org" web site version 01.15 using modified Visvalingam 
method with intersection repair enabled and prevent shape removal enabled and 
simplified to 13%. This reduced the space requirements by about 90% 
while maintaining a visually usable boundary image and edge alignments.

The dataset provide a good functional characterization of the 5 regions.

The @data structure of the saved SpatialPolygonsDataFrame contains the following 
information for use by the SeerMapper function:
\describe{
  \item{rgName}{a character vector containing the region name.
   }
  \item{rgID}{a character vector containing the 1 digit U. S. region number
   from the U. S. 2000 or 2010 census state boundary datasets. (0 to 4)  This field 
   is also used as the row.names for the SpatialPolygons.
   }
  \item{tracts}{an integer value of the number of census tracts in the region.
   }
 }
}
\source{
The US 2000 and 2010 Regional Census boundaries are based on the CENSUS.GOV web site 
state, county and census tract boundary shapefiles for the 2000 and 2010 Census.
}
\author{
Jim Pearson \email{jpearson@statnetconsulting.com}\cr 
Maintainer: "Jim Pearson" \email{jpearson@statnetconsulting.com}\cr 
}
\examples{
   data(rg99_d00)
}
\keyword{datasets}
\keyword{SeerMapper}