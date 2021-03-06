#
#
#  Function to map seer areas and color them based on ratings 
#  of the data.  The areas can also be hatched based on their 
#  reliability based on P_value.
#
#  This is an extension of the SeerTractMapper, but Seer Areas 
#  instead of counties for the entire US.
#

#####
#
#   Quick Mapper Function for R data analysis of Seer Areas, 
#   States, Health Service Areas, State/Counties and 
#   State/County/Census Traces over US.
#
#   The package can use either the 2000 or 2010 census boundary 
#   files.  By default it will use the 2000 census information.
#   The "censusYear" call parameter is available to specify 
#   which census year's boundary and fips code files to use.  
#
#   The package contains the 2000 and 2010 information at the 
#   4 census regional, 51 states and D.C. and PR, 
#   21 U. S. Seer Registry, 942 U. S. Health Service Area, 
#   and county level.  
#   The information for the census tracts mapping 
#   has been made available as 6 supplemental boundary data 
#   packages (3 for each census year).  The 6 packages are
#   automatically downloaded when "SeerMapper" is installed.
#   They are dynamically loaded into memory based on the 
#   areas of the U. S. being mapped at the tract level.
#
#   The boundary data for the U. S. census tracts are divided 
#   into three (3) supplemental packages for each year: 
#   SeerMapperRegs, SeerMapperEast, and SeerMapperWest.
#
#   The SeerMapperRegs package contains the 2000 census tract 
#   boundary data for the states containing a Seer Registries:
#      Alaska (Native), Arizona (Indian), California, 
#      Connectticut, Georgia, Hawaii, Iowa, Kentucky, 
#      Lousiana, Michigan (Detroit), New Mexico, New Jersey,  
#      Oklahoma (Cherokee Nation), Utah, 
#      Washington (Seattle-Puget) 
#      (15)
#
#   Two other packages contains the 2000 census tract 
#   boundary data for the other states that are east of the 
#   east and west of the Mississippi river.
#
#   The SeerMapperEast package contains 2000 tract boundary 
#   data for:
#      Alabama, Delaware, DC, Florida, Indiana, Illinois, 
#      Maine, Massachusettes, New Hampshire, New York, 
#      North Carolina, Ohio, Pennsylvania, PR, Rhode Island, 
#      Maryland, Mississippi. South Carolina, Tennessee, 
#      Vermont, Virginia, West Virginia, Wisconsin 
#      (23)
#
#   The SeerMapperWest package contains 2000 tract boundary 
#   data for:
#      Arkansas, Colorado, Idaho, Kansas, Minnisota, Missouri, 
#      Montana, Nebraska, Neveda, North Dakota, Oregan,  
#      South Dakota, Texas, Wyoming 
#      (14)
#
#   Implement as a function with the following parameters:
#
#   ndf  = data frame of the data.  The key columns that must be
#           provided in the ndf data.frame are the identifer column (idCol)
#           and the data column (dataCol).
#           The idCol must contain one of the following geographical identifers
#           based on the U. S. Fips codes or the NCI Seer Area name abbreviations.
#
#           State mapping -> 2 digit - U. S. state fips codes.
#           State/County mapping -> 5 digit - U. S. state and county fips codes.
#           State/County/Census Tract mapping -> 11 digit - U. S. state, county and
#                 census tract fips codes.
#           Seer Area mapping - up to 6 characters matching the NCI Seer Area
#                 name abbreviations.
#
#           See detail documentation for more on these codes.
#
#   proj4 = a Proj.4 string describing the projection to be used
#          when drawning the maps.  It must be an acceptable
#          string for the CRS R function in the sp and maptools 
#          packages.
#
#   censusYear = the census year boundary data to be used with the rate data.
#           The only supported values are 2000 or 2010. This is an internal
#           parameter and cannot be generally not set by the caller.  It is
#           set to correspond to the year of the boundaries datasets contained in
#           the package.
#
#   idCol = a character vector or a integer number of the column in
#           the ndf data.frame containing the spatial area identifier.
#           Default column names is "FIPS"
#
#   dataCol = a character vector or numeric value.  It specifies the
#           numver or name of the column in the ndf data.frame that contains
#           data to be classified and colored on the map.
#
#           The data can be either a value/rates to be categorized (real number) 
#           or the integer category value for the sub-area.
#           The "categ" parameter must be set to "DATA" or "data". 
#           The number of categories is limited by the
#           number of colors provided by RColorBrewer for the specified palette
#           in "palColors" call parameter.
#
#           If categ="colors" or "COLORS", then the dataCol values are the color to fill each area.
#           The default column name is "Rate".
#
#   categ = can have three forms: a single numeric (3 to 12), a vector of
#           break points values, or the character "DATA" or "data".
#         = when a single value is provided, it is the number of categories
#           the package should calculate the break points and then
#           classify the data. The value can range from 3 to 12.  The maximum
#           depends on the value of the palColors call parameter.
#         = The vector of break points can contain from 2 to "n" values.
#           "n" is equal to the maximum number of colors supported by the
#           selected color palette in the "palColors" call parameter.
#           For the default color palette of "RdYlBu", the maximum number of
#           values is 10.  See palColors for more details.  The vector
#           is used to classify the data for mapping.
#         = when categ is set to "DATA" or "data", this indicates the
#           dataCol contains the actual category integer values and not
#           count or rate data. When "DATA" is specified, the range of the
#           integer category values is limited by the number of colors
#           RColorBrewer can provide for the selected palette in the
#           "palColors" call parameter.  The default palColors value is "RdYlBu"
#           which can support up to 11 categories.
#         - when categ is set to "COLORS" or "colors", this indicates the 
#           dataCol contains the actual color value to be used to fill the sub-area.
#           The value must be a valid color name (e.g. colors()), or a character
#           vector string starting with a "#" and containing 6 or 8 hexidecimal 
#           characters (0-9,a-f,A-F).  No other checking is done against the colors.
#           When colors are specified, the legend will be constructed to list the colors
#           in numerical order with the number of observations using that color. 
#           All of the colors supplied are checked to make sure they are good color 
#           values or names.
#         = the default value for categ is 5.  
#
#   mTitle = title for the map.  Must be a character vector and can consist
#           of up to two elements for a two line title.  Any more elements
#           will be ignored.
#
#   mTitle.cex = specified font size multiplier - default = 1 x font size.
#
#
#  Future - make "title" as alternate.
#
#   us48Only = is a logical variable - TRUE/FALSE, (us48OnlyFlag).
#           If set to TRUE, only the continental 48 states will be mapped.
#           Hawaii, Alaska and PR will not be mapped and any provided data is ignored.
#           If set to FALSE, all of the states and DC are mapped.
#           The default value is FALSE.
#
#   includePR = is a logical variable - TRUE/FALSE,  If set to TRUE, the PR
#           territory is included in all maps where us48Only=FALSE.
#           When set to FALSE, the PR territory is not mapped.
#           The default value is FALSE.
#
#   regionB = "NONE", "DATA", "ALL".
#           "NONE" -> no region boundaries are drawn (default)
#           "DATA" -> Only region boundaries are drawn when a subarea within the region has data.
#           "ALL"  -> Draw all region boundaries within the us48Only and includePR limitations.
#
#   regionB_lwd = numeric value for the line weight for Regional Boundaries. Acceptable range
#            is 1 to 72.  Default value = 2.5 
#
#
#   stateB = "NONE", "DATA", "REGION", or "ALL".
#            "NONE" -> no state/DC boundaries are drawn (default for non-state level data)
#            "DATA" -> Only state/DC boundaries are drawn if data is provided
#                      for the state/district or a sub-area. (default for state level data)
#            "REGION" -> Map all state boundaries within a region that contain some sub-area with data.
#            "ALL"  -> All state/DC boundaries are drawn.
#
#   stateB_lwd = numeric value for the line weight for State Boundaries.  Acceptable range
#            is 1 to 72.  Default value = 2.5 
#
#
#   seerB  = "NONE", "DATA", "STATE", "REGION", or "ALL".
#            "NONE" -> No Seer Registry boundaries are drawn
#            "DATA" -> Only Seer Registry boundaries are drawn if the Registry or 
#                      sub-areas have data values.
#            "STATE"-> Draw ALL Seer Registry boundaries for states
#                      that containing data values at any level.
#            "REGION" -> Draw All Seer Registry boundaries in any region 
#                      with a registry with data.
#            "ALL"  -> All US Seer Registry boundaries are drawn.
#                      *** Exception: if state data, in stateB="DATA", "ALL" is limited 
#                      to the states with data.
#
#   seerB_lwd = numeric value for the line weight for Seer Registry Boundaries.  Acceptable range
#            is 1 to 72.  Default value = 2.0 
#
#
#  NEW
#   hsaB = "NONE", "DATA", "SEER", or "STATE".
#            "NONE" -> No Health District boundaries are drawn
#            "DATA" -> Only Health District boundaries are drawn if the Health District or 
#                      a sub-areas have data values.
#            "SEER" -> All HSA boundaries are drawn in a Seer Registry, if the registry contains
#                      a sub-area with data.
#            "STATE"-> Draw ALL Health District boundaries for states
#                      that containing data values at any level.
#
#   hsaB_lwd = numeric value for the  line weight for HSA Registry Boundaries.  Acceptable range
#            is 1 to 72.  Default value = 1.5 
#
#   countyB = "NONE", "DATA", "HSA", "SEER", or "STATE". Only valid when county data or tract data is used.
#            "NONE" -> No County boundaries are drawn
#            "DATA" -> Only County boundaries are drawn if the country has a data value.
#  NEW       "HSA"  -> All county boundaries are drawn within a health service area, if any 
#                      county within the HSA has a data value.
#            "SEER" -> All county boundaries are drawn within a registry, if any county or tract 
#                      within the Registry has a data value.
#            "STATE"-> All county boundaries are drawn within a state, if any county or tract 
#                      within the state has a data value.
#
#   countyB_lwd = numeric value for the  line weight for County Boundaries.  Acceptable range
#            is 1 to 72.  Default value = 1.0 
#
#
#   tractB = "NONE", "DATA", "COUNTY", "SEER", or "STATE",  Only valid when tract data is used.
#            "NONE" -> No census tract boundaries are drawn
#            "DATA" -> Only census tract boundaries are drawn that have data values.
#            "COUNTY"->All census tract boundaries in a county are drawn if the county 
#                      containing a tract with data value.
#  NEW       "HSA"  -> All tract boundaries within a Health Service Area are drawn, if the HSA
#                      contains a tract with a data value.
#            "SEER" -> All tract boundaries within a Seer Registry are drawn, if the registry 
#                      contains a tract with a data value.
#            "STATE"-> All tract boundaries within a state are drawn, if the state contains
#                      a tract with a data value.
#
#   tractB_lwd = numeric value for the  line weight for Census Tract Boundaries.  Acceptable range
#            is 1 to 72.  Default value = 0.75 
#
#
#   clipTo = "NONE", "DATA", "HSA", "SEER", "STATE", or "REGION"  Valid 
#                      for all mapping levels. For the explanation 
#                      of "clipTo" assume stateB="ALL" is set
#                      so normally the entire US (all state 
#                      boundaries) would be drawn.  This feature is not 
#                      working as expected with the use of the boundary
#                      options. As more boundaries are drawn, the 
#                      clipTo area enlarges.  The NONE and DATA 
#                      work fine.  When the SEER, HSA, STATE and REGION boundary
#                      options are used, the area is expanded to include 
#                      boundaries that don't have data.
#                      The new thought is to do the clipTo to the 
#                      HSA, SEER, STATE and REGION areas that contain data.
#                      This will require additional code to 
#                      keep track of the data areas at each level.
#            "NONE" -> No boundary clipping, all requested 
#                      boundaries are drawn. In the example above: 
#                      all of the U.S. states is mapped regardless 
#                      of how little of an area is being mapped.
#            "DATA" -> Only the sub-areas are drawn, the spatial 
#                      box for the sub-areas with data is used for 
#                      the overall plot scaling. If any boundaries 
#                      are request to be drawn (like states), they 
#                      will be drawn but clipped at the box size set 
#                      by the sub-areas with data.  If data was 
#                      mapped Maryland counties, the graphic would 
#                      show the boundaries for other states 
#                      until they reach the spatial box limits.
#            "HSA" ->  The spatial box for the graphics is taken
#                      from the space occupied by the HSAs 
#                      containing data or sub-areas with data.
#                      Other boundaries may be drawnn up to 
#                      when they exit the spatial box.
#                      This value is not valid with SEER or STATE
#                      level data.
#            "SEER" -> The spatial box for the graphics is taken 
#                      from the space occupied by the Seer Registry 
#                      areas containing the data or sub-areas with 
#                      data.  Other boundaries may be drawn up to 
#                      when they exit the spatial box.
#                      This value is not valid with STATE level data.
#            "STATE"-> The spatial box for the graphic is taken 
#                      from the space defined by all of the states 
#                      that contain data or sub-areas with data.  
#                      Again, other state boundaries may be drawn, 
#                      but will be clipped when they reach the 
#                      spatial box limits.
#            "REGION"-> The spatial box for the graphic is taken 
#                      from the space defined by all of the U.S. 
#                      regions that contain sub-areas with data.  
#                      Again, other state boundaries may be drawn, 
#                      but will be clipped when they reach the 
#                      spatial box limits.
#             The default for this call parameter is "NONE"
#
#
#   dataBCol = is a character vector containing the color to be 
#             used for the border of the areas at the data level.  
#             The default value is "black".  The value is 
#             validated against the color names listed in 
#             colors().  It is only applied if supplied by 
#             caller - it's default if the default color 
#             of the levels boundaries.
#
#
#   hatch   = TRUE, FALSE or List of options:
#             If TRUE, hatch is done using the default settings.
#             If FALSE (default), hatching will not be preformed.
#             If a list, hatching will be enabled and the list 
#             evaluated for overriding values.
#             The default value is FALSE.
#
#         dataCol = a character vector or a single integer number 
#             containing the name of the data column or number 
#             for use to determine is hatch will be done on 
#             this area. The default  value is "pValue"
#
#         ops - a character vector containing one of the 
#             following values:
#             "eq", "ne", "lt", "le", "gt", "ge", "EQ", "NE", 
#             "LT", "LE", "GT", "GE", "=", "==", "!=", "<>", 
#             "<", "<=", "=<", ">", ">=", and "=>".  
#             This value specifies the comparison operator 
#             be used to test for hatching.  The formula is:
#
#             <hatch "dataCol" column values> <operator> <"value" parameter>
#
#             For pValue testing the default values for ops (gt) 
#             and value (0.05) can be used. The resulting formula 
#             would be:
#                <hatch "dataCol" values> gt 0.05
#
#         value - a numeric value used with the criteria to 
#             compare against the user's data to determine if 
#             the area should be hatch. The default value 
#             is 0.05  (as used with pValues)
#
#         range - a vector of two numeric values.  The user 
#             provided data column for the hatch test is 
#             validted and must be within this range (inclusively.)
#             If range=FALSE or NA, the range check feature 
#             is disabled. If range=TRUE, the default range 
#             vector of c(0,1) is used. The default value 
#             for the range option is NA, disabled. 
#
#      The following options are general options for all hatching.
#
#         col = a character vector containing the color to be use 
#             for the hatching lines. The default value is grey(0.66)
#
#         lwd = a numeric representing the line weight (thickness) 
#             of the hatching lines. The default value is 0.65.
#
#         density (or den) = a numeric representing the number 
#             of lines per inch in the hatching line pattern.  
#             The default is 25 lines per inch.  The valid range
#             for this value is 5 to 64.
#
#         angle = a numeric representing the angle in 
#             degrees of the lines in the hatching pattern.  
#             The default value is 45 degree CCW.  The valid 
#             range of this value is -360 to 360.
#
#         incAngle = a numeric representing the amount the 
#             angle of the hatching will be increased for hatch2.  
#             The value must be between -120 and 120. The default 
#             value is 60 CCW.  (Added - 18/03/14)
#
#   hatch2  = List of options:    (Added - 18/03/14)
#             If a list, hatching will be enabled and the list 
#             evaluated for overriding values. The default 
#             value is NULL.
#
#         dataCol = a character vector or a single integer 
#             number containing the name of the data column or 
#             number for use to determine is hatch will be done 
#             on this area. The default  value is NULL
#
#         ops - a character vector containing one of the 
#             following values:
#             "eq", "ne", "lt", "le", "gt", "ge", "EQ", "NE", 
#             "LT", "LE", "GT", "GE", "=", "==", "!=", "<>", 
#             "<", "<=", "=<", ">", ">=", and "=>".  This value 
#             specifies the comparison operator be used to 
#             test for hatching.  The formula is:
#
#            <hatch "dataCol" column values> <operator> <"value" parameter>
#
#             For pValue testing the default values for ops (gt) 
#             and value (0.05) can be used. The resulting formula 
#             would be:
#                <hatch "dataCol" values> gt 0.05
#
#         value - a numeric value used with the criteria to 
#             compare against the user's data to determine if 
#             the area should be hatch. The default value is 0.05  
#             (as used with pValues)
#
#
#  Note: A legend will be drawn by default using the default settings..
#
#  mLegend = a logical value of TRUE or FALSE or a list of legend parameters.
#           If NA or FALSE, the legend will not be drawn. If TRUE, the default Legend 
#           settings will be used to create the legend.
#
#           mLegend can also be set to a list of named values.  Each named value 
#           provides controls over how the legend will be drawn.  
#           For example:  mLegend=list(pos="center",size=0.5)
#           The following options are available:
#
#         numCols = (ncol or legendColn) is an integer and specifies 
#              the number of columns in the legend. The value must 
#              be in the range of 1 to 8.  The default value is 3.
#
#         pos = (legendPos) is the string  "left", "center", or 
#              "right" indicating the position of the legend in 
#              the bottom of the graphic. The default value is "left".
#
#         size = (legendCex) a numeric value to use as a multiplier 
#              for legend font size. The default value is 0.85 
#              times the par("ps") value.
#
#         counts = TRUE/FALSE,  (legendCnt) If TRUE, display the 
#              number of areas in each category in the legend 
#              after the label.  If FALSE (the default), no counts 
#              are displayed in the legend.
#
#         noValue = is a logical parameter. If TRUE, any category 
#              in the legend with no entries is tagged with "NV" 
#              after the category. The default value is FALSE.
#
#
#   bktPtDigits = number of digits to allow in the break point list
#               and resulting categorization intervals.  Default is the calculated
#               number of digits based on the interval between break points.
#               If categ is set to "color" or "data", this parameter is ignored.
#
#   palColors   = is a character string used to specify the RColorBrewer 
#               palette to use to color the categories.  The default 
#               is "RdYlBu" reversed.  The value provided by the user 
#               is verified against the list of RColorBrewer acceptable 
#               palette names.   Dependent on the palette selected, 
#               the number of categories may be limited based on 
#               the number of colors available from RColorBrewer.
#               if categ is set to "color", then this call parameter 
#               is ignored.
#
#   debug       = is a logical variable.  Set to \var{TRUE} to enable 
#               printing a lot of debug output prints and logic traces.  
#               This is a lot of output and should not be used unless 
#               requested.
#
#  Future: 
#     shapeFile = allows a user to specify their own shapefile.
#               This parameter is a list of several options:
#                dir = directory where the shapefile is stored.
#                dsn = file name of the shapefile (.shp, .dbf, etc.)
#                idCol = name of the Location ID in the shapefile@data 
#               structure.
#               The use of shapeFile=list will disable the following
#               regionB, stateB, seerB, hsaB, countyB, tractB, clipTo,
#               us48only, includePR, censusYear parameters.
#               
#
#
#  The type of map is dynamically determined by the area ids used by
#  the caller.
#
#  If the id is a 2 digit state fips code, the data and mapping
#  is done at the state Level. No county, census tract or Seer
#  registry area boundaries will be drawn.  By default only state
#  boundaries are drawn for states with data (stateB="DATA".)
#  To draw the state boundaries for all states, set stateB = "ALL".
#
#  If the id is a 5 digit state/county fips code, the data and
#  mapping is done at the state and county level.  No census
#  tract boundaries are drawn.  Seer registry area boundaries
#  may be drawn if the seerB parameter is set to "DATA" or "ALL".
#  By default (countyB="DATA") the county boundaries are only drawn 
#  for counties with data values. The boundaries for counties without 
#  data values can be drawn to their registry or state boundaries by 
#  using the countyB = "HSA", "SEER" and "STATE" options.
#  State and registry boundaries can be added by using the stateB and seerB
#  options set to "DATA" or "ALL".  The "DATA" value will draw 
#  the associated boundary around any county with data values.
#
#  If the id is a 11 digit state/county/tract fips code, the
#  data and mapping is done at the census tract level.
#  The tractB parameter works in the same manner for tracts as the 
#  countyB parameter worked for counties.  tractB has one additional
#  value of "COUNTY".  This askes the package to draw all tracts in any
#  county containing a tract with data values, similar to the countyB="SEER".
#  The tractB="NONE", "DATA", "HSA", "COUNTY", "SEER" and "STATE" 
#  work the same way.  
#  The stateB and seerB parameter also work the same.
#
#  The base package only contains the census tracts for states
#  containing Seer Registries.  To be able to map census tracts
#  for other states, one or more additional boundaries packages
#  must be loaded to provide the boundary data.  There is one
#  supplemental tract boundaries package for the Eastern states
#  not included in the package and another package for the
#  Western states.  (See below for the list of states in each
#  package.) All of the county boundaries in an state with
#  data are always drawn.  A county is colored white if no
#  data exists for the county.  To draw the state boundaries
#  for states without data, set the StateOverlay parameter
#  to TRUE.
#
#  All of the FIPS code based id's are adjusted to add back
#  the leading zero to form character string versions of the
#  codes to match the boundary data files.
#
#  The HSA numbers are 1 to 3 digits in the range from 1 to 999.
#  The idMode = HSA will be set if any of the location IDs 
#  are 3 digits, do not match a state FIPS code.  If a 
#  state ID is entered incorrectly, the location id may 
#  be classified as HSA.
#
#  If the id is the Seer registry area abbreviation or a
#  recognized string outputed by government Seer Registry
#  support programs.  Only the Seer registry abbreviations
#  or strings that can be alias matched.  See table below.
#  Only the state and Seer Registry bounaries are drawn
#  based on the stateB and seerB call parameters.
#
#  If the id does not match any known pattern, a wildcard
#  match will be attempted to try and determine the Seer
#  Registry abbreviation for the row.  The wildcard match
#  is a "contains" match. If the alias string is found in
#  the id phrase, then the associated Seer Registry
#  abbreviation is assigned to the row.
#
#  Any ids that do not match a boundary FIPS code (at state,
#  county or tract level), the HSA number range
#  or the alias or abbreviation of
#  the Seer Registry, will be reported to the user via
#  warning messages.  The data for the unmatched
#  row will not be mapped.
#
#  If a state/county or state/county/tract data is provided
#  for just areas in the Seer Areas and the SeerOnly
#  parameter is set to TRUE, then only the county or
#  county/tract boundaries in the Seer Areas are drawn.
#  Any county or county/tract boundaries outside of the
#  Seer Area but within in the state and states without
#  seer areas are not drawn.
#
#  If a state is drawn (with or without county or census
#  tract boundaries) and the SeerOverlay parameter is set
#  to TRUE, the boundaries of the Seer Areas are mapped
#  in the states drawn.
#
#  New options:
#
#  censusYear   = "2000"    default = "2000" or "2010"
#                          (other value = "2010")
#
#  The boundaries datasets included in this package are:
#
#    The boundaries for all 50 states and DC.
#    The boundaries for all counties in the 50 states.
#    The boundaries for all 20 Seer Areas
#                  (AK-NAT, AZ-NAT, CA-OTH, CA-LA, CA-SF,
#                   CA-SJ, CT, GA-OTH, GA-ATL, GA-RUR,
#                   HI, IA, KY, LA, MI-DET,
#                   NJ, NM, OK-CHE, WA-SEA, UT)
#
#    Seer Registry Abbreviations and alias strings
#
#
#  Change Log:
#     9/23/14 - Fixed hatching from <= to > 0.05 areas.
#             - Added legend.
#
#     9/24/14 - Change break points to a predetermined set (.6, .8, 1, 1.2, 1.4)
#             - Added option to be able to specify the break point list
#               (only 5 values are supported.) When a break point list is
#               provided, the end points used are -Inf and Inf.
#
#             - Corrected and changed the calculated break points based
#               on the number of categories to use the min*0.9 to max
#               data values for the end points and drop the group from
#               the max value to Inf (always empty.)
#
#             - Added option to include or not include legend in window
#
#             - Change default prior data column name as "RateRatio".
#
#     9/25/14 - Lightened lines in hatching and added as option.
#             - Use a general format for the legend values.
#             - decrease precision in legend of categories.
#             - Could not change line width of hatch at this time.
#             - Tested hatching line as dashes and dots, stayed
#               with solid.
#
#     9/26/14 - Fix rounding on calculated break points. Floor and
#               Ceiling returned integers - not a good representation
#               of the Min and Max data values. Also dropped the
#               0.9 * Min adjustment. Since rounding is to two
#               decimals (1/100), the min and max values were adjusted
#               by -0.01 and + 0.01, then all values rounded to two
#               decimal places.  This ensured the low value was always
#               lower then the min and high value greater than the max,
#               by just enough. This provides an accurate representation
#               of the low and high values.
#
#    10/16/14 - The calculated breakpoints may have the lowest values
#               and/or highest values duplicated. A good example is a
#               set of data that has a large number of "1" values at
#               the high end. If 5 categories are requested then 20%
#               is in each category. If the number of "1" values exceed
#               20% of the total -> then all of these values will appear
#               in the next to last category. This is true of the low
#               end as well where there may be possibly a large number
#               of "0" values. The middle points may also be duplicated,
#               but not as frequently as the high and low ends. The
#               resolution is to adjust the 2nd to low end value to
#               be 0.0001 above the miniumum value and set the second
#               to high value 0.0001 below the maximum value. A set
#               of break points like:
#                  c(0, 0, 0.4, 0.6, 1, 1) would become
#                        c(0, 0.0001, 0.4, 0.6, 0.9999, 1)
#               This would force a large number of 0's or 1's in this
#               example to the extreme ends. A message documenting the
#               adjustment will be outputed. If an internal breakpoint
#               is duplicated, example:
#                  c(0,  0.4,  0.6,  0.6,  1).
#               This is 4 categories (25% each) with large number of
#               0.6 values and 25% of the values in the range from 0.6+
#               to 1 for the high category. When this is noted, a
#               message generated and the lower duplicate adjusted
#               by -0.0001 to cause all of the 0.6 values to land in
#               the 3 category in this example. If the set of breakpoints
#               is provided by the user contains duplicate values,
#               a message will be generated and the function execution
#               stopped.
#
#             - The function must be able to run when wrapped with TIFF,
#               PNG, JPEG, PS, and PDF device setups. At the current
#               time, the TIFF. JPEG, and PNG do not work. They only show
#               the state boundary and legend are shown, we think. Need to
#               find what is being ploted successfully and why. Windows, PS
#               and PDF appear to work correctly.
#               Answer: Tested to see if the line width or colors were
#               cause lines (polygons) to not be drawn, then attempted to
#               isolate which plots were working and which were not working.
#               The last plot always worked, but appears to erases the
#               previous plots or completely obscure them. As it turns out
#               each type of output graphics handles the plot() differently.
#               The par(new=T) was added to help with overplotting.
#               However, the devices need more information. A patch was
#               implemented. R would like you to do PLOT then polygons
#               or lines as means to overplot in R. To permit plot()
#               overlays an option in the plot call "add=TRUE" was added
#               in an patch to tell the devices this plot is an overlay.
#               This options does not appear in the general documentation.
#               This appears to affect the backgrounds and area fills to
#               allow them to be effectively transparent.
#
#             - Error found- when us48Only option is set, the codes does
#               not exclude the data from Alaska and HI. When the maps are
#               later drawn, the existance of the data causes the
#               boundaries to be drawn anyway. Data is scanned and any
#               data, at state, county or tract level, for AK, HI, and PR
#               are excluded. The state FIPS code will be added to each
#               data record and used to subset the data at plotting time.
#
#    10/21/14 - The adjustements for the duplicate break points broke
#               the break point rounding logic and was not dynamic to
#               handle future cases. Changes made to use small adjustment
#               intervals based on the difference between the calculated
#               break points. Also logic added to ensure the rounding
#               process did not recreate the duplicate break point
#               problems. Also noted the logic for the interior duplicates
#               skewed the categorization high. Logic changed to try
#               and center the interior categorization to the middle
#               of the range of duplicate values.
#
#               End result:  High end skewed to high category.
#                            Low end skewed to low category
#                            Middle groups skewed to the middle
#                               category of the duplicates.
#
#             - Added code to remove data rows with no
#                       FIPS code ("" or NA)
#             - Added code to remove data rows with no
#                       values ("" or NA) in the dataCol.
#
#    10/23/14 - Minor tuning of the RateRound and RateLabel
#                 sub-functions.
#
#    12/31/14 - Added Seer Areas as an alternative to county areas.
#
#    01/20/15 - Add parameters to help control the legend:
#                 legendColn = force the number of columns used
#                       in the legend (def = 3)
#                 legendCex  = force the font size multiplier
#                       (def = 1)
#             - Add parameter to help control the number of digits
#                 in the break point values and the break point
#                 labels (bktPtDigits).
#                 WARNING if you set the number of digits to low,
#                 you may get an error indicating you have
#                 duplicate break point values.
#
#                 brkPtDigits = force the number of digits after
#                       the decimal point (def = 2)
#             - Add logic to determine the significant's of the
#               break points values.
#             - Balanced the size of the legend key to the text.
#    01/22/15 - Add parameter to control location of legend - left,
#               center. or right - translated to legend "position" values.
#    03/19/15 - Packaged MiniNCIMapper to permit distribution and
#               use by NCI researcher.  Updated the documentation
#               with the new options added over the last couple
#               of months.
#    03/30/15 - Document package does not support Alaska-Native,
#               Arizona-Native and HI Seer Areas in this release.
#             - Changed the us48Only default to TRUE and the
#               pValueHatch default to FALSE.
#    03/01/16 - Changed package name to NCIMapper.
#             - Updated boundary file references to use new design
#               aimed at making this a releasable CRAN package.
#               All state and counties boundaries will be included
#               in package.
#               Only tract boundaries will be included for seer areas.
#               Seer area boundaries will be included in package.
#               Supplimental tract boundary files will be accessible,
#               if referenced and loaded.  If not, throu error.
#             - Rebuild boundary data into simplier SPDFs with no indexes,
#               no placename data, and no SaTScan used information.
#             - Reduced the boundary data to 15% from 33%.
#             - Added new features (SeerOnly and SeerOverlay)
#             - Changed type of map identification logic to do it
#               automatically.
#             - Changed the Legend parameter to a named list to
#               simplify function call.
#   03/11/16  - Lazydata and Lazyload is disabled to save about
#               1 megabyte of disk space to get release < 5 megabytes.
#             - Reimplemented hatch and Legend parameters to use
#               named lists instead of separate call parameters
#             - implemented hatch and Legend parameters as TRUE/FALSE
#               or a list of settings.
#             - Reorder operations to better handle new overlay features.
#             - Added stateB, seerB, and fillTo call parameters.
#               These allow inspection of data and filling at the
#               data level to Seer or State levels based on the
#               areas with data.  Extra areas are mapped in white.
#             - Completed generating boundaries reduced to 15% for
#               all states, seer Areas, counties and censustracts.
#             - Created SeerTractEast and SeerTractWest packages
#               to carry the census tracts for non-Seer Area states
#               to the east and west of the Mississippi.
#               Packages built and verified.
#             - Updated code to handle oddities in R 3.2+ that
#               do not handle xxx[yy] properly - now you have to
#               code xxx[yy,].  Updated code as debugged.
#             - Changed name of package to SeerTractMapper.
#   03/15/16  - Rewrote the how the package determines what
#               areas to map and what overlays to draws.  It now
#               had the areas with data, the extention of the
#               boundaries at the data level to the Seer Area
#               boundaries or the State boundaries (controlled
#               by the fillTo call parameter and possible
#               overlays at the County, Seer and State levels.
#             - Control was added to manage which overlays
#               would be drawn. seerB, stateB, and countyB
#               can be used to indicate:  "NONE" - don't draw
#               the boundary, "DATA" - draw only around
#               lower level areas with data, and "ALL" - draw
#               all.  countyB is only "NONE" or "DATA".
#             - A bug was discovered in the RateRound function
#               that created a categorization range that did
#               not cover the entire data range.  The result
#               is a NA category and areas colored black.
#               The RateRound function was recoded to provide
#               a better rounding process.
#             - Corrected the loadBoundary function to properly
#               handle reading tract boundary dataset that are
#               SpatialPolygons and not SpatialPolygonsDataFrames
#               like the county boundary datasets.
#             - Added countyB and fillTo="COUNTY" options.
#   03/17/16  - Updated code to handle new 2000 Census files
#                - rg99_d00 for region boundaries
#                - sa99_d00 for Seer area boundaries
#                - eliminate indexes - now included in SPDF structures
#                - County, State, Seer and Region boundaries now
#                  derived from tract boundaries to ensure edges
#                  overlap properly.
#                  May still have minor issue at state boundarie
#             - Expanded examples to include Georgia census tracts
#                  and do partial maps - ATL, ATL+partial and
#                  show off options.
#   03/20/16  - Changed default value of us48Only to FALSE.
#             - Changed package name to SeerMapper for 2000 data and
#               SeerMapper2010 for 2010 data.
#   03/24/16  - Completed the hatching lty logic, added character
#               values equal to par(lty) values.
#   05/24/16  - Allow categ to get set to "DATA", to indicate
#               the dataCol contains the actual category values,
#               not data.  Initial code assume categories are
#               from 1 to "n".
#               Additional code must be added later to compensate
#               for other ranges. ***
#             - Fix problem with FIPS identifiers - not matching
#               geo base correctly when leading zero is missing.
#             - Open up the maximum number of categories from 9
#               to 10 to support the Spectral palette and category
#               values in the data from 1 to 10.
#             - Add palColors parameter without any checks.  Used on
#               call to RColorBrewer. Need to validate palette name
#               and then enforce the limited number of colors
#               supported by each palette.
#   08/31/16  - added check for the palColors palette name. Changed
#               cSchema to palette, then to palColors.
#             - Edited documentation for replacement dataset for
#               Georgia Census Tract data.
#   09/23/16  - Replace all datasets used by examples and test with
#               releasable versions.
#             - update and test examples.
#   09/26/16  - Corrects made to the data validate code to
#               properly handle character type data for the dataCol.
#               It was creating T/F values instead of numerical
#               and causing the categorization code to crash.
#             - The area outlines are not working as expected.
#               Added the option for seerB="STATE". This setting
#               will draw all Seer Registry borders within a state
#               where an area has data. No other Seer Registery
#               borders are drawn as in the seerB="ALL" case.
#               seerB="ALL" is not designed to draw all Seer
#               Registries within the US even if they do not contain
#               any data.
#             - The limits of the graphs have been corrected to
#               draw the state or empty Seer Registry borders.
#               It currently clips the borders in these situatlions.
#             - Protection needs to be added to the categ="n" logic
#               incase the data only contains a few areas (<"n").
#               The interval can not be 0.
#             - Updated examples to use the new datasets for state,
#               state/county and state/county/tract, and state/seer
#               reg. data. All examples had to be re-written.
#             - The regular expression used to validate character
#               form of numbers has been enhanced to handle leading
#               whitespace and a space between leading - or + and
#               the number. Commas may be included, but must
#               be every 3 digits from the decimal point or end of
#               number (standard format). The commas are now
#               removed after the validate check to allow the
#               strings to be converted to numeric values (using
#               as.numeric).
#    09/28/16 - Added seerB="STATE" option to draw All Seer
#               Registries boundaries in a state with data.
#             - Added tractB call parameter to control the
#               drawing of census tract boundaries. The values of
#               the option are: "DATA", "SEER", and "STATE".
#               The option is only used when tract data is provided.
#             - The countyB call parameter is expanded to include:
#               "SEER" and "STATE" values.
#             - Added wild card match on Seer Registry Names -
#               see which is better Abbr Match or Alias Match -
#               pick the best.
#    10/02/16 - Reorder call parameter checking to allow idCol
#               processing and level determination code to set
#               defaults for the stateB, seerB, countyB, and
#               tractB parameters.
#             - Remove catRange call parameter - will not use,
#               will not accomplish objective.
#             - Corrected base module documentation.
#             - Started updating categ logic to handle c(a,d,b,d,e)
#               up to 10 values. Verification, elements in
#               ascending order. Do not check interval. Values
#               must be numeric. Also check number of value+1 must
#               be =< number  supported by Color Brewer palette.
#             - Changed the palette parameter to palColors, to
#               avoid confusion with a lot of palette variables.
#             - Moved palColors parameter check up in order to
#               have information for later tests.
#    10/18/16 - Fix tractB and countyB partial code - not handling "COUNTY", "SEER" and "STATE"
#               values correctly.
#    10/23/16 - Added code to allow the idCol, dataCol, and hatching dataCol to
#               be specified as a column name or number.
#    11/16/16 - Changed hatching range checking parameter default from c(0,1) to NA, disabled.
#             - Corrected Hatching code to support the operations and value fields properly.
#             - Corrected the Legend code to allow a single option. 
#             - Expanded legend code to aupport all of the top and bottom position names.
#               Have conflict with the original names and the "middle" names used by the 
#               legend function.
#             - Reordered the hatching logic to:
#                a) validate the hatch options in the list.
#                b) validate the dataCol as an option
#                c) validate the dataCol value as a column name or number
#                d) validate the dataCol contents as numbers.
#                e) execute comparison logic and set hatching flag for each area.
#                f) remove comparison logic from ploting section.  If hatching, plot
#             - changed the default value for the categ from c(0.6, 0.8, 1.0, 1.2, 1.4) to "5".
#               This provides a quick map for any type of data.  User can then refine the 
#               breakpoints when needed.
#             - Since users come from different backgrounds and know different computer languages,
#               the hatch operator value has been expanded to access the following operation
#               names: eq, ne, lt, le, gt, ge.  The operation symbols allowed are:
#               ==,  =     for equal
#               !=,  <>    for not equal
#               >=,  =>    for greater than or equal
#               <=,  =<    for less than or equal
#               <          for less than
#               >          for greater than
#
#               The first symbols are R operator symbols.  The second symbol are values
#               supported by other languages.  The package will access the character symbol
#               (based on fortran), the R based operators or the variation symbol operators.
#             - Completed density and angle options for hatching.  Added verification code
#               defaults and parameters to plot call.
#             - Needed about 200k of space in package.  Removed the rg99_dXX.rda data sets
#               and replaced with code to generate same table from st99_dXX table if regions=TRUE.  
#               Hard coded table of region numbers to names.
#    01/10/17 - change Legend and Title call parameter names to mLegend and mTitle.
#             - Add dataBCol call parameter to allow the color of the data level border to be 
#               be changed.  Default is black.  This is added to support requirements for 
#               using SeerMapper by SaTScanMapper.
#             - Added return value by SeerMapper of the x and y limits used to draw then 
#               map.  This is used by SaTScanMapper to position the cluster outline shapes 
#               over the drawn map.
#             - Add the option categ="colors" (upper or lower case).  This allows the caller to pass the actual
#               colors for filling the areas instead of a category or rate value.  This is 
#               required to support SaTScanMapper as a caller.  The colors are check to ensure they
#               are really color values to the R routines.  An upper limit of the number of colors used
#               needs to be set to help the legend code handle the list. The feature may require the addition of the 
#               legend label override new feature being considered.
#             - changed "US48Only" call parameter name to "us48Only".
#             - changed the default of the categ parameter to 5.
#             - changed the return list of xlim and ylim to an invisible return.
#    01/16/17 - Subdivide logic into segments to all SaTScanMapper to call key elements and
#               void repeat work to improve performance.  Key elements 1) parameter 
#               validation - not required, 2) Categorization and Color assignment - not required.
#               3) building SPDFs for mapping based on Location IDs. - required.
#               4) plotting map. The data structures produced or required by each 
#               element must be able to be saved and passed along. This would allow
#               SeerMapper to setup the SPDF structures for a map, then re-use it 
#               with different colors for multiple maps as SaTScanMapper does.
#    01/22/17 - reorganized for better preformance when working with SaTScanMapper.  
#             - Take out regions call parameter
#             - Add regionB call parameter as a better user interface.
#             - Clean up code - extensively.
#    01/30/17 - Updated documentation - added error messages
#             - added mLegend$yAdj option to modify position of legend box.
#             - Added mTitle.cex call parameter
#             - Provides SM_GlobInit, SM_Build, and SM_Mapper to help satscanMapper map.
#             - Passed build, check --as-cran tests.  Need to reduce example run times.
#             - Updated several datasets with default columns of FIPS and Rate for class.
#             - Started release testing and satscanMapper testing.
#             - Fixed bug in handling hatch=T/F and mLegend=T/F logic.
#             - Included dataCol variable in rPM list.
#             - Added "REGION" option to stateB and seerB call parameters.  This sets the 
#               drawing limit to the region boundaries when the region has a sub-area containing
#               data.
#    02/03/17 - Added new call parameter - ClipTo="NONE", "DATA", "SEER", "STATE"
#               This overrides the x and y limit calculation and allows the Seer and State
#               boundaries to be included, but clipped at a level to keep the data areas
#               larger.
#    02/03/17 - Fxed xxxB='SEER' problem.  Required a type of add to PList function
#               instead of get IDs function that lost old information when 
#               an area is not a Registry in a state. The xxxB="COUNTY" is affected
#               in the same manner and had to be fixed.
#    02/15/17 - Changed dateset structures - coXX_dXX is only polygons,  trXX_dXX is only polygons.
#               Restored co99_dXX to contain all of the county information from coXX_dXX.
#               Corrected loadboundary function to handle SpatialPolygons or SpatialPolygonsDataFrame
#               structures.
#               Corrected example code to correctly pull out the WA-SEA registry with saID equal NA.
#               Added code to handle missing (NA) location IDs.  Added error messages and deleted rows.
#               Clarified error message when the locations IDs are a mix of numbers and characters - 
#               thus can't be FIPS codes or Registry abbreviations.
#    02/17/17 - Restructured coXX_dYY and trXX_dYY to SpatialPolygons only.
#               Collected all county information into co99_d00.
#               Since all but 3 states have the same counties in 2000 and 2010,
#               implemented way to not have to have coXX_d10 dataset for all states.
#               Marked states requiring _d10 in st99_d00 and do special load
#               when states and census year match. ("change10" flag)
#               Updated dataset images to include all state, county, and tract
#               counts and centroid X and Y coordinates used by satscanMapper. All of this
#               is now re-calculated to same run time.
#               Transformed all polygon structures from Lat/Long coordinates
#               to cartisian - equal area projection.  This eliminates the 
#               need to do it during the function execution.
#               Expanded co99_d00 to include 2000 and 2010 areas that may overlap, but
#               are not used in the same census year.  This eliminated the need
#               for a co99_d10 version of the dataset.
#    02/18/17 - Fixed hatch:lty option. Not assigning the correct value for the plot call and
#               was limiting numeric range to 0-5 instead of 0-6.  Also did not ensure
#               the value was an integer.
#    03/13/18 - Add Heatlh Service Areas to the boundary maps.  This impacts: dynamic location 
#               ID detection, add new hsaB and HSA parameters, new hs99_d00 table, new set of 
#               hsXX_d00 and hsXX_d10 datasets, updates the co99_d00 to include HSA number,
#               updates st99_d00 to include number of HSAs in state, modifies 
#               added plotting of HSA layer, roll up of counties and tracts to HSA level, 
#               add one example and same data for HSA in the Washington/Baltimore CMA, HSA boundaries are 
#               included for all US states.
#             - Boundary Options support for HSA is enhanced: hsaB = { NONE, DATA, SEER, STATE }, 
#               expanded countyB = HSA ( all counties in HSA that contains data), and 
#               expanded tractB = HSA ( all tracts in HSA that contains data.)
#             - Add the "proj4" call parameter for a caller specified projection definition for 
#               the final mapping. Value is converted to CRS and any errors trapped and reported
#               to the caller.  This is the only validation of the string provided.
#             - Accept "proj4" parameter string from satscanMapper in the rPM vector. Must be 
#               provided immediately after the SM_GlobInit call is completed.
#             - Add to documention the process and transformations done to all coordinates.
#             - Removed the ltyp hatching option.  
#             - Add the ability to do a second hatch (hatch2).  The first hatch and all 
#               control options are specified via the "hatch" call parameter.  
#               The second hatch is specified via the "hatch2" call parameter, but 
#               only the dataCol, ops, and value options are allowed.
#               The rest of the options for the second hatch are copies from the hatch=list(options).
#             - Update documentation for new features and parameters.
#                   proj = "string"
#                   countyB, and tractB modifications = HSA
#                   hsaB = { NONE | SEER | STATE | REGION | ALL
#                   hatch2 = list()
#             - Add return value at end of SeerMapper to provide information required to 
#               allow overlay printing on maps created by SeerMapper.
#             - Remove ltyp options in hatching.
#             - Added "lab" option to Hatching (1 and 2) incase we need to label them 
#               or do a small legend.
#   02/27/19  - turn off debug output.
#
#   Future:
#             - Add GROUP feature to allow user to define there own logical groups (like districts
#               or HSAs) based on counties.  Input is "name/ID" and county (5 digit fips).
#               package constucts name-group-county table.  Reads needed counties and 
#               combines counties as needed.  Group entities can not span state boundaries.
#               GROUP = "data.frame" of (locID, county FIPS).
#             - xxxxxB_lwd  - boundary line weight override.
#             - User specified shapefiles with names tables.
#
#   Immedidate: - rebalance census tract files to meet CRAN size requirements.
#   10/07/19 Fanni Zhang
#             - add # to comment out functions cat and print when debug=0 to get rid of long output
#             - modify st99_d00 with loc2 included for the new tract data arrangement in version 1.2.2
#               (see R program "modify_st99_d00.R")
#             - check the data package version and replace loc with loc2 in st99_d00 if 
#               packageVersion("SeerMapperRegs")>="1.2.2" & packageVersion("SeerMapper2010Regs")>="1.2.2"
#   
#   01/06/20 Fanni Zhang
#             - fix the proj4 problems by replacing the default proj4 string with user-defined proj4
#               for region, state, registry.
#             - add # to comment out function str when debug=0 to get rid of long output
#
#   02/11/20 Fanni Zhang
#             - resolve the discrepancy of centroid points between st99_d00@data and st99_d00@polygons
#               (see R program "modify_st99_d00.R")
#
#   03/26/20 Fanni Zhang
#             - resolve the discrepancy of centroid points between sa99_d00@data and sa99_d00@polygons
#               (see R program "modify_package_rdafiles.R")
#   
#   04/22/20 Fanni Zhang
#             - update centroids for states, seer registries and regions based on the user-specified proj if any  
#
#   05/06/20 Fanni Zhang
#             - resolve the discrepancy of centroid points for co99_d00 and coXX_dXX@polygons
#             - resolve the discrepancy of centroid points for hs99_d00 and hsXX_dXX@polygons
#               (see R program "modify_package_rdafiles.R")
#             - update centroids for county-level and hsa-level data based on the user-specified proj if any  
#
#   06/17/20 Fanni Zhang
#             - remove code for transforming the centroids for regions data files
#             - add code to address the rgdal and sp corrections
#   
# Plans:
#   1) convert legend to list vector format
#   2) use same code structure for legend and hatch
#
#
#  Libraries required for function.  Make sure you have them installed.
#
#  The focus is to develop on R 3.5.  (as of March 2018, development focus is R 3.4)
#
#  IMPLEMENTATION NOTES:
#    a) LazyData and LazyLoad were disabled. It appears by
#       having them enables, the size of the installed package
#       increases from 4.4 to 5.7 megabytes. to meet the CRAN
#       limit - they were disabled. Overall, does not appear
#       to effect the implementation and removed the possible
#       issues with deleting/removing boundary data structures
#       once they are loaded and combined.
#
#    b) Somewhere in time, indexing into SpatialPolygoms and
#       data.frames have changed.  xxx[yyy] used to work,
#       now R requires xxx[yyy,] for the subsetting to work.
#
#    c) Providing a complete set of code to validate each user provide parameter is difficult.
#       Found using a generalized indexing of xxx[[1]][1], Almost always the first element
#       of any data structure is returned: vector, matrix, array, list, data.frame, etc. with
#       out having to check for typsof or classes.  When a single value is expected, this 
#       coding technique is used to retreive the value from the data form provided.
#
#  Referenced packages:
#
#library(foreign)      - BUILD processes
#library(grDevices)    - embedded in R
#library(graphics)     - confirmed
#library(data.table)   - ???
#library(stats)        - confirmed  (quantile)
#
#library(stringr)      - confirmed  (str_sub, str_trim, str_match)
#library(RColorBrewer) - confirmed  (brewer.pal, brewer.pal.info)
#library(sp)           - confirmed  (all)
#library(maps)         - NO
#library(maptools)     - confirmed  (spRbind)
#library(mapproj)      - NO
#library(rgdal)	       - BUILD processes
#
#  Data contained in package:
#     state boundaries (all)
#     county boundaries (all)
#     Seer area boundaries (all)
#     Health Service Area boundaries (all)
#     Index of Seer to State (2 digits)
#     Index of HSA (1-3 digits) to Seer (abbr) and State (2 digits)
#     Index of county (5 digits) to Seer (abbr) and HSA (1-3 digits)
#     test/example data
#
#####

#####
#
#  General Functions  (Common to all routines)
#

   #####
   #
   # is.Color takes a hex string, the name of a color (from grDevices::colors()), or palette number
   #   and validates it as a color variable.  TRUE - is a valid color, FALSE - not a color.
   #   
   # Inputs:  values can by any color names that matches the grDevices::colors() name list, 
   #    a 6 or 8 character hex string starting with a "#" character, or 
   #    the palette color number (1 to 8) as integer or character.
   #
   #    Examples:   "white", "red", "lightgreen", "#232323", "#234Ad3", or "#FFDDCC80"
   #                1, or "1"
   #
   #    On hex strings, the alpha value is optional (last 2 hex digits)
   #    Vectorize the is.Color function provided by is.Color2
   #
 
   ####
   #
   #  Color string to hex string conversion (handles vectors of values)
   #    col2hex(<name>) -> hex
   #
   #  Uses col2rgb (grDevices)
   #
   col2hex <- function(cname) {
   
      res <- try(colMat <- col2rgb(cname), silent=TRUE)
      
      if (class(res)!="try-error") {
          rgb(red=colMat[1,]/255, green=colMat[2,]/255, blue=colMat[3,]/255)
      } else {
          res
      }
    }
   #
   ####

   ####
   #
   #   is.Color(xxx)   verifies if all of items in xxx are colors (vectorized)
   #
   
   is.Color  <- function(x) {
       # handle a vector of colors
       vapply(x, is.Color2, logical(1))
   }
   #
   ####
   
   ####
   #
   #  Test a single value test as colors - is.Color2
   #
   #  The test is done against the standard color list and the micromapST color list.
   #  The value can be a color name or a color pallet value.
   #  The returned value is TRUE or FALSE.
   #
   #  Uses col2rgb  (grDevices)
   #
   
   is.Color2 <- function(x) {
       ErrFnd <- FALSE     
       # check one color "x" string
       if (is.numeric(x)) {
       
          # numeric color value - if so its a relative color number within the pallet.
          if (x < 0) {
             # can not be a negative value..
             ErrFnd    <- TRUE
             #xmsg      <- paste0("The color value provided in the data or parameter must be a positive number. Value seen:",x,"\n")
             #warning(xmsg,call.=FALSE)
                     
          } else {
             # if value is numeric, convert to integer character string.
             x  <- as.character(x)
          } 
       }
       
       if (!ErrFnd) {
          # convert factor to character
          if (is.factor(x)) x <- as.character(x)
    
          if (is.character(x)) {
             #   character string, check for palette number or color name.
             if (!is.na(match(x,c(as.character(c(1:8)),grDevices::colors())))) {  # test name and/or number
                
                TRUE   # good color name that matches the colors() list.
                
             } else {
                
                # No match with character version of palette number or grDevices::colors(),
                # so try conversion from color to rgb, if it works, got a color - return TRUE 
                # if it fails, it will return error - catch and return "FALSE"
                # This tests if the "hex" value character string was provided.
    
                res     <- try(col2rgb(x),silent=TRUE)
                
                #  if class of res is not "try-error", return TRUE, 
                #  if class of res is 'try-error", then return FALSE (not a color)
                
                return(!"try-error"%in%class(res))   #  TRUE or FALSE based on col2rgb's result.
             }
          } else {
             # not a integer or character or valid palette value (>0)
             FALSE  # not a color
          }
       }
    }
   #
   #  end of is.Color and is.Color2
   #
   ####
      
   ####
   #
   #  print Names List 
   #
   printNamedList <- function(n,x) {
      cat("Named List - ",n,"\n")
      
      if (!is.list(x)) {
         xmsg <- paste0("Variable passed to print named list ia not a list structure. Printed in raw format.")
         warning(xmsg, call.=FALSE)
         print(x)
      } else {
         xN <- sort(names(x))
         #cat("xN:",xN,"\n")
         if (is.null(xN)) {  
            # if not names, print raw
            print(x)
         } else {
            xNMax <- max(nchar(xN))
            xSp   <- paste0(rep(" ",xNMax),collapse="")
            for (N in xN) {
                wN <- stringr::str_sub(paste0(N,xSp),1,xNMax)
                wM <- paste0(wN,":",paste0(x[N],collapse=", "))
                cat(wM,"\n")
            }
            cat("\n")     
         }
      }
   }
   #
   # End of  printNamedList 
   # 
   ###
   
   
   ###
   #
   #  function to convert PROJ4 string into CRS format, catch any errors and warnings, report them
   #  and return CRS to caller.
   #
   
   convertPROJ4 <- function (x) {
   
   #  function is designed to convert a proj4 string into CRS format
   #  and catch any errors or warnings.
   #
   #   x - user provided PROJ4 string
   #
   #   value = CRS of x if no errors or warnings
   #         = FALSE if errors or warnings.
   #
    save_x <- x
    y <- NULL
    #cat("input proj4:",x,"\n")
 
    ErrFnd <- FALSE
    if (!is.character(x)) {
       # not a character vector - error
       ErrFnd <- TRUE
       xmsg <- paste0("***903 The proj4 call parameter is not a valid character vector.  Must be a valid proj4 argument character string to be converted.")
       warning(xmsg,call.=FALSE)
       return(FALSE)     
    } else {
       # character vector - OK try the convert
 
       res <- tryCatch({
          y <- sp::CRS(x)
          }, warning = function(war) {
             #print(paste0("My Warning: ",war))
             return(paste0("WARNING:",war))
          }, error = function(err) {
        
             #print(paste0("My Error:  ",err))
             return(paste0("ERROR:",err))
          }, finally = {
          }
       )
     
       if (class(res) == "CRS") {
          # its a CRS class - convert to string to print status of call.
          #xres <- sp::CRSargs(res)
          #cat("results:",xres,"\n")
          return(res)    # return CRS class version.
       } else {
          # its not a CRS..  Most likely an error. Should be character.
          res <- as.character(res)
          #cat("results:",res,"\n")
       
          if (stringr::str_sub(res,1,6) == "ERROR:" ) {
             # an error occurred during the conversion to CRS
             xmsg <- paste0("***900 The provided proj4 string encountered an error when converted to the \n",
             "      internal CRS parameter. The following error was reported, plesae correct and rerun.\n",
             "     ",stringr::str_sub(res,7,200),"\n")
             warning(xmsg,call.=FALSE)
             ErrFnd = TRUE
          } else {
    
             if (stringr::str_sub(res,1,8) == "WARNING:") {    
                # an warning occurred during the conversion to CRS
                xmsg <- paste0("***901 The provided proj4 string encountered an warning when converted to the \n",
                "      internal CRS parameter.  The following warning was reported, plesae correct and rerun.\n",
                "     ",stringr::str_sub(res,9,200),"\n")
                warning(xmsg,call.=FALSE)
                ErrFnd = TRUE
             } else {
                xmsg <- paste0("***902 Unpredicted results when proj4 was translated to CRS format. Unknown problem.\n",
                "     ",res,"\n")
                warning(xmsg,call.=FALSE)
                ErrFnd = TRUE
             }
          }
          return(FALSE)
       }
    }
 }
   #
   #
   ###


#  End of Common Functions
#
#####

#####
#
#  Master Functions for SeerMapper
#
   ###
   #
SM_GlobInit <- function() {

      rPM            <- NULL
      rPM$debugFlag  <- FALSE                # set to TRUE if running outside of package
      rPM$debug      <- FALSE    
      
      #
      #  Colors
      #
      #    RColorBrewer Palette Name List and limit color
      #
      RCBrewerDF <- RColorBrewer::brewer.pal.info   # get palatte information from RColorBrewer
      #       columns:  row.names = palette name
      #                 maxcolors = maximum number of colors
      #                 category  = div, qual, seq
      #                 colorblind = T/F
      #
      RCBrewerDF$PName     <- row.names(RCBrewerDF)       # get palette names as brewer wants them.
      RCBrewerDF$category  <- as.character(RCBrewerDF$category)
      RCBrewerDF$Name      <- toupper(RCBrewerDF$PName)   # get upper case version for matching.
      #
      rPM$RCBrewerDF       <- RCBrewerDF
     
      rPM$palColors        <- "RdYlBu"
      rPM$palColorsMaxNum  <- 11
      
      #
      #  Boundary Colors
      #
      rPM$ColorB_O_Region  <- "grey10"      # Top Layer  - 90% black
      rPM$ColorB_O_State   <- "grey14"      # Top Layer
      rPM$ColorB_O_Seer    <- "grey18"      # Seer Level
      rPM$ColorB_O_Hsa     <- "grey22"      # Health Service Area Level
      rPM$ColorB_O_Group   <- "grey23"      # User Group Definition Level
      rPM$ColorB_O_County  <- "grey26"      # When tracts are mapped.
      rPM$ColorB_O_Tract   <- "grey30"
      rPM$ColorB_hatching  <- grey(0.66)    # hatch overlay (hatch and hatch2)  66% -> grey34
      rPM$ColorB_Data      <- "black"       # black
      
      rPM$palColors        <- "RdYlBu"      # default Color Brewer palette
      rPM$CB_Rate_Mid      <- rev(RColorBrewer::brewer.pal(5,rPM$palColors))  # place holder.
      rPM$CB_Rate_Mid2     <- rPM$CB_Rate_Mid # back up value if needed for categMode = 4
      
      #
      #  Constants
      #
      #  regular expression to validate numeric data formats.  Handles the following formats:
      #   n     +n    -n    nnnn.n  +nnnn.n   -nnnn.n
      #   n,nnn   +n,nnn -n,nnn n,nnn.nnn  nnnn.nE+nn  nnn.nE-n
      #
      #  values should be trimmed of blanks and tabs (whitespace) (str_trim) on both ends.
      #
  
      rPM$numberTestRegExpr <- "^[:space:]*[+-]?[:blank:]?((([0-9]{1,3}[,])?([0-9]{3}[,])*[0-9]{3})|([0-9]*))?(([.][0-9]*)|)([eE][-+]?[0-9]+)?[:space:]*$"
  
      #
      # Verification of patterns
      # numberTestRegExpr    <- "^ [:space:]*[+-]?[:blank:]*    ( ( ([0-9]{1,3}[,])? ([0-9]{3}[,])*  [0-9]{3} ) | ([0-9]*) )? ( ([.][0-9]*)  | )  ( [eE][-+]?[0-9]+)?[ \t]*$"
      #                                                          ***************  *************
      #                                                        +++++++++++++++++++++++++++++++++++++++++++++   ++++++++      +++++++++++   +      ++  ++   +++
      #                                                      ------------------------------------------------------------  ------------------  ------------------
      #                                                                    with commas                         no commas    decimal fraction    possible Scientific or not

      #
      #  Set up project 4 strings  -  Original and Projected
      #

      rPM$OrigCRS      <- sp::CRS("+proj=longlat +datum=NAD83")

      #
      #  Transform the State, State/County, State/County/Census Tract
      #  boundary polygons from long/lat to Equidistance Conic projection.
      #
      #   Projection = Alber equal area  => simpleconic
      #   Lat Parallel 1   = 33
      #   Lat Parallel 2   = 45
      #   Origin of Lat    = 39
      #   central Meridian = -96   (96W)
      #

      rPM$ProjCRS      <- sp::CRS("+proj=aea +lat_1=33 +lat_2=49 +lat_0=39 +lon_0=96w +units=m")
      
      #
      # As of 2/20/17 - all datasets were converted from lat/long to equal area coordinates 
      #  by doing it ahead of time, it same in setup time when the package is run.
      #
      
      #
      #   User provided Proj4 projection definition for final mapping.
      #
      
      rPM$proj4        <- NA
      rPM$CRSproj4     <- NA
      
      
      #
      #
      #  rg99_dXX table labels - static data table.
      #
      
      rPM$rg99Data     <- data.frame(region=c("1","2","3","4"),
                                      rgName=c("NorthEast","South","Midwest","West"),
                                      stringsAsFactors=FALSE)

      #
      #  Local Constant - Tables
      #
      #    The boundaries for all 20 Seer Areas
      #                  (AK-NAT, AZ-NAT, CA-OTH, CA-LA, CA-SF,
      #                   CA-SJ, CT, GA-OTH, GA-ATL, GA-RUR,
      #                   HI, IA, KY, LA, MI-DET,
      #                   NJ, NM, OK-CHE, WA-SEA, UT)
      #
      #      Seer Registry Abbreviations and alias strings  - used in Stage 2, 3
      
      SeerNames <-matrix(c(
               # Abbr     Alias string     stAbbr stID  coCnt  rgID
                "AK-NAT","Alaska",         "AK",  "02", 27,  "4",
                "AZ-NAT","Arizona",        "AZ",  "04", 15,  "4",
                "CA-LA", "Los Angeles",    "CA",  "06",  1,  "4",
                "CA-OTH","California excl","CA",  "06", 48,  "4",
                "CA-OTH","Greater Calif",  "CA",  "06", 48,  "4",
                "CA-SF", "San Fran",       "CA",  "06",  5,  "4",
                "CA-SJ", "San Jose",       "CA",  "06",  4,  "4",
                "CT",    "Connecticut",    "CT",  "09",  8,  "1",
                "GA-ATL","Atlanta",        "GA",  "13",  5,  "2",
                "GA-OTH","Georgia other",  "GA",  "13",144,  "2",
                "GA-OTH","Greater Geor",   "GA",  "13",144,  "2",
                "GA-RUR","Rural Georg",    "GA",  "13", 10,  "2",
                "HI",    "Hawaii",         "HI",  "15",  5,  "4",
                "IA",    "Iowa",           "IA",  "19", 99,  "3",
                "KY",    "Kentucky",       "KY",  "21",120,  "2",
                "LA",    "Louisiana",      "LA",  "22", 64,  "2",
                "MI-DET","Detroit",        "MI",  "26",  3,  "3",
                "NJ",    "New Jersey",     "NJ",  "34", 21,  "1",
                "NM",    "New Mexico",     "NM",  "35", 33,  "4",
                "OK-CHE","Cherokee",       "OK",  "40", 14,  "2",
                "UT",    "Utah",           "UT",  "49", 29,  "4",
                "WA-SEA","Puget",          "WA",  "53", 13,  "4",
                "WA-SEA","Seattle",        "WA",  "53", 13,  "4"
                ),
                ncol = 6, byrow=TRUE)
      
      SeerNames           <- as.data.frame(SeerNames,stringsAsFactors=FALSE)
      colnames(SeerNames) <- c("ab","alias","stAbbr","stID","coCnt","rgID")
      SeerNames$ab        <- toupper(SeerNames$ab)
      SeerNames$alias     <- toupper(SeerNames$alias)
      SeerNames$stAbbr    <- toupper(SeerNames$stAbbr)
      SeerNames$stID      <- as.character(SeerNames$stID)
      
      rPM$SeerNames       <- SeerNames
      
      #
      #   Run Parameters and Variables
      #     names and space has been reserved for all variables referencing the rPM list.
      #
     
      #
      #  Fill out basic structure
      #
      rPM$cYear           <- "00"     # the default   - 2000
      rPM$censusYear      <- "2000"
      rPM$cY              <- ""

      rPM$ndf             <- data.frame(FIPS=c("01","02"),Rate=c(1.1,1.2),stringsAsFactors=FALSE)
      rPM$ndfName         <- "ndf"       # undetermined
      rPM$ndfColNames     <- c("FIPS","Rate")
      rPM$ndfColMax       <- 2
  
      rPM$idMode          <- 0          # undetermined
      
      rPM$idCol           <- "FIPS"
      rPM$idColName       <- "FIPS"
      rPM$idColNum        <- 0

      rPM$dataCol         <- "pValue"
      rPM$dataColName     <- "pValue"
      rPM$dataColNum      <- 0

      rPM$categMode       <- 1        # dataCol data is rates to be categorized - default mode.
      rPM$categ           <- 5
      rPM$wCateg          <- 5
      rPM$CatNumb         <- 5
      
      rPM$CatR            <- c("") 
      rPM$CatRAdj         <- c("")
      rPM$CatRwCnt        <- c("")
      
      rPM$brkPtDigits     <- 2

      rPM$stateSelDel     <- c("")    #   ??? short term variable
      rPM$AspRatio        <- 1        #  aspect ratio of map.

      rPM$HatchFlag       <- FALSE
      rPM$hatch_caller    <- FALSE
      rPM$hatch           <- list(hDataCol=character(),hDataColName=character(),hDataColNum=numeric(),
                                  hData=numeric(),
                                  hOps=character(),
                                  hValue=numeric(), 
                                  hRange=numeric(),
                                  hLab=character(),
                                  hAngle=numeric(),
                                  hRes=logical(),
                                  # general options
                                  hCol=character(),
                                  hLwd=numeric(),
                                  hDen=numeric(),
                                  incAngle=numeric()
                                 )
                                  
      rPM$Hatch2Flag      <- FALSE
      rPM$hatch2_caller   <- FALSE
      rPM$hatch2          <- list(hDataCol=character(),hDataColName=character(),hDataColNum=numeric(),
                                  hData=numeric(),
                                  hOps=character(),
                                  hValue=numeric(), 
                                  hRange=numeric(), 
                                  hLab=character(),
                                  hAngle=numeric(),
                                  hRes=logical(),
                                  # general options - inherited
                                  hCol=character(),
                                  hLwd=numeric(),
                                  hDen=numeric()
                                 )
			      
      
      rPM$mLegendFlag     <- FALSE
      rPM$mLegend_caller  <- FALSE
      rPM$mLegend         <- list(lCounts=logical(),lSize=numeric(),lNumCols=numeric(),
                                   lPos=character(),lPosv=character(),
                                   lNoValue=logical(),lPch=numeric(),lLabels=character())
      
      rPM$dataBCol        <- "grey90"
      rPM$dataBCol_caller <- FALSE
      
      rPM$data_lwd        <- 0.75
      rPM$tr_lwd          <- 0.75
      rPM$co_lwd          <- 1.0
      rPM$hs_lwd          <- 1.5
      rPM$sa_lwd          <- 2.0
      rPM$st_lwd          <- 2.5
      rPM$rg_lwd          <- 2.5
    
      
      rPM$mTitle          <- c()
      rPM$mTitle.cex      <- 1
      
      rPM$HSA             <- FALSE
      rPM$GROUPS          <- FALSE
      
      rPM$regionB         <- "NONE"
      rPM$regionB_caller  <- FALSE
      rPM$stateB          <- "NONE"
      rPM$stateB_caller   <- FALSE
      rPM$seerB           <- "NONE"
      rPM$seerB_caller    <- FALSE
      rPM$hsaB            <- "NONE"
      rPM$hsaB_caller     <- FALSE
      rPM$countyB         <- "NONE"
      rPM$countyB_caller  <- FALSE
      rPM$tractB          <- "NONE"
      rPM$tractB_caller   <- FALSE
      rPM$fillTo          <- "NONE"
      rPM$fillTo_caller   <- FALSE
      rPM$clipTo          <- "NONE"
      rPM$clipTo_caller   <- FALSE
       
      rPM$dataMapDF       <- data.frame(ID=c("01","02"),data=c("1.2","1.3"),hData=c(1,2),h2Data=c(1,2),stringsAsFactors=FALSE)  # and more. 
       
      rPM$NumErrors       <- 0
      rPM$NumWarnings     <- 0
      
      return(rPM)      
   }

   #
   #  End of SM_GlobInit function
   #
   #####


   #####
   #
   #   SM_Hatching - setup the hatching variables and dataMapDF for the plotting.
   #    The data is in xxxxx$hData vector under rPM.
   #    The control information are under $hRes and $hDen vectors in 
   #    the hatch and hatch2 lists.
   #    To be able to sync with the dataMapDF, the hatching lists also carry
   #    the sub-area ID.
   #
SM_Hatching  <- function(rPM) {
      #   
      #   Local Functions
      #
      SetUpHatch <- function(whatch,lhatch) {
     
         #  whatch is the hatch list with all parameters and data
         #  lhatch is the literal - "hatch" or "hatch2"
         #  Use whatch$flag for the "HatchFlag"
     
         # get data into working vector
         
         maphData   <- whatch$hData   # get data
 
         # re-written to generalize for both hatch and hatch2
         
         #####
         #
         #  Step HG.1 - Step 1 - Hatching - Get data and validate
         #
         #  Do if hatching is still enabled.
         #  By default maphData is set to NAs
         #
         #  RULE: can't get here with HatchFlag = TRUE if no dataCol exists.
         #
      
         # check to see if data provided is valid (numeric)
         
         numberTestRegExpr <- rPM$numberTestRegExpr
         if (is.factor(maphData)) {
            # convert factors to character
            maphData <- as.character(maphData)

         }
         #
         
         if (!is.numeric(maphData)) {
            # Not numeric vector.
            
            if (!is.character(maphData)) {
               # data is not numeric or character vector
               xmsg       <- paste0("***144 The ",lhatch," data column ",whatch$hDataColName,
                                           " does not contain numbers. Parameter ",lhatch," disabled.")
               warning(xmsg, call.=FALSE)
               whatch$flag  <- FALSE
         
            } else {
            
               # should be characters - test for numeric characters
               
               hDataT     <- gregexpr(numberTestRegExpr,maphData)    #  1 OK, NA missing, -1 not number
               HNumOK     <- (hDataT == 1)
               
               if (all(HNumOK)) {
                  # also clean up pValue data column   (may kill if character data is allowed?
                  #    OK to convert to numbers.
                  maphData <- as.numeric(maphData) # convert to numeric pValue
               
                  if (any(is.na(maphData))) {
                     xmsg <- paste0("***145 The ",lhatch," data is not numeric.  Parameter ",lhatch," disabled.")
                     warning(xmsg,call.=FALSE)
                     whatch$flag <- FALSE
                  }
                  # should no have any NAs..
               
               } else {
                  xmsg       <- paste0("***146 The ",lhatch," data column ",whatch$hDataColName,
                                           " does not contain valid numbers. Parameter ",lhatch," is disabled.")
                  warning(xmsg, call.=FALSE)
                  whatch$flag  <- FALSE
               
               }
               
            }  # end of character check
         } # end of numeric check
         
         #  data now is in maphData instead of NAs
         #
         #  we have turned off hatching or really believe it a good number.
         #
         #####
         
         #####   
         #
         #  Step HG.2 - Data Range Check if RANGE = TRUE or c(l,h)
         #
         #  If still hatching # 1 and range exists
         #   
         #  Assumption if HatchFlag is TRUE at this point - hData is known to be numeric.
         
         if (whatch$flag) {  # still doing hatching.
            #  Check range of hData.
            H_range <- whatch$hRange
            
            if (!is.na(H_range)) {
      
               #  have range, check data against it.
               x1 <-  maphData < H_range[1]
               x2 <-  maphData > H_range[2]
               if (any(x1 | x2)) {
                  xmsg <- paste0("***147 ",lhatch," data provided is not within the allowed range :",
                                     whatch$hRange[1]," to ",whatch$hRange[2],". Parameter ",lhatch," disabled.")
                  warning(xmsg, call.=FALSE)
                  whatch$flag <- FALSE    # turn it off.
               }
            } # end of range check
      
            # don't do numeric check on this column, could be character test.
         }
         #
         #####
         
         #####
         #
         # Step HG.3 - Have valid data, do comparison and set hatch flag 
         #
         #  maphData - the data to compare
         #  result of comparision -> $hRes vector is set to FALSE to start.
         #  When the string is evaluated, all entries the match the equation 
         #    are set TRUE
         #
         #    $hRes <-  ( <dataValue>  <hOps>  <hValue> )
         #
         H_res  <- rep(FALSE,length(maphData))          # initialize.   Result of test.
      
         if (whatch$flag) {  # still hatching,,,
         
            #  build comparison is by row.   <data> <H_ops> <H_value>
            wstr        <- paste0("maphData ",whatch$hOps," ",whatch$hValue)
            #cat("hatch comparison command:",wstr,"\n")
   
            # execute
            H_res        <- eval(parse(text=wstr))   #  true/false   # perform the test
     
            #cat("hatch op results:",H_res,"\n")
    
            whatch$hRes   <- H_res
           
         }
   
         #
         #  if the validate check on "value" is changed to allow a vector with
         #  a length > 1, then the H_ops operaton can be executed between 
         #  different values (one per row).  If the H_ops limitation of one 
         #  value is removed, then different operations could be specified 
         #  for each row.  Why you would do this is beyond me.
         #
    
         return(whatch)
 
      }  # end of all ifs to check each option
      
      #
      #  End of hatch List checking and Processing common function.
      #
      #####

      #####
      #
      #   SM_Hatching main code
      #
      #   Local Variables
      #
      debug      <- rPM$debug      # get debug flags
      debugFlag  <- rPM$debugFlag
      
      dataMapDF  <- rPM$dataMapDF  # get access to hatching data
      
      HatchFlag  <- rPM$HatchFlag  # get HatchFlag
      
      # hatch # 1
      
      if (HatchFlag) {

         hatch           <- rPM$hatch         # get hatch list options
         hatch$flag      <- HatchFlag
         hatch$hData     <- dataMapDF$hData   # get hatch data # 1
         #str(hatch) ## FZ 01/06/2020
         
         hatch      <- SetUpHatch(hatch,"hatch")  # do hatching calculations

         # save variables    
         HatchFlag       <- hatch$flag         # HatchFlag - could be disabled.
         dataMapDF$hRes  <- hatch$hRes         # save test results.
         
         #str(dataMapDF) ## FZ 01/06/2020
         
         if (debug) {
    
            cat("Hatching FINAL settings Z-1814 Flag:",HatchFlag,"\n")
            cat("   parameters -- dataCol:",hatch$hDataColName," #:",hatch$hDataColNum,"  ops:",hatch$hOps,"  value:",hatch$hValue,"\n")
            cat("  range:",hatch$hRange,"\n")
            cat("  col  :",hatch$hCol,"  lwd:",hatch$hLwd,
            #"  lty:",hatch$hLty,
            "  den:",hatch$hDen,"  angle:",hatch$hAngle," incAngle:",hatch$hIncAngle,"\n")
            cat("dataMapDF:\n")
            print(str(dataMapDF))
            
         }
         
         rPM$HatchFlag <- HatchFlag
         rPM$hatch     <- hatch
      }  
 
      Hatch2Flag  <- rPM$Hatch2Flag
      
      # hatch # 2
      
      if (Hatch2Flag) {

         hatch2       <- rPM$hatch2
         hatch2$flag  <- Hatch2Flag
         hatch2$hData <- dataMapDF$h2Data
         
         #str(hatch2) ## FZ 01/06/2020
         
         hatch2       <- SetUpHatch(hatch2,"hatch2")
         
         Hatch2Flag       <- hatch2$flag   # hatch2flag may be false if errors.
         dataMapDF$h2Res  <- hatch2$hRes

         #str(dataMapDF) ## FZ 01/06/2020

         if (debug) {
         
            cat("Hatching # 2 FINAL settings Z-1850 Flag:",Hatch2Flag,"\n")
            cat("   parameters -- dataCol:",hatch2$hDataColName," #:",hatch2$hDataColNum,"  ops:",hatch2$hOps,"  value:",hatch2$hValue,"\n")
            cat("  range:",hatch2$hRange,"\n")
         }
         rPM$Hatch2Flag   <- Hatch2Flag
         rPM$hatch2       <- hatch2
      }
            
      rPM$dataMapDF <- dataMapDF
      
      #
      #
      #####

      return(rPM)

   }
   #  End of SM_Hatching.
   #
   #####
   

   #####
   #
   #  SM_Build - complete loading of county and tract boundary data
   #         setup regionB, stateB, seerB, countyB, and tractB variables
   #         projection was done in advance - not done here.
   #         build xxxx_proj and xxxx_data  at each level
   #         build xxxxPList at each level.
   #         The id column is pulled, idMode is setup, etc. (rPM)
   #
   #         Input:   rPM -> dataMapDF$ID, idMode, categMode
   #                  rPM -> debug, debugFlag
   #                  rPM -> OrigCRS, ProjCRS
   #                  rPM -> censusYear, cYear, cY
   #                  rPM -> ndfName, stateSelDel
   #                  rPM -> CRSproj4  (modification if not NULL or NA or "")
   #
   #
   #         Returnes:   MV -> xxxx_proj, xxxx_data, xxxxListAll, xxxxListData
   #                     more to be added.
   #
   #  Must apply new projections to SPDF before this function returns.
   #
   #
   
   SM_Build <- function(rPM) {
      
      ####
      #   
      #    The boundary data for the regions, states, Seer Registries, 
      #    HSA, and counties are contained in the "SeerMapper" package.
      #
      #    The boundary data for the census tracts for 2000 and 2010 are
      #    located in six additional packages with a sets for 2000 and 2010.
      #
      #    The ...Regs packages contain the census tract boundaries
      #    for the 19 states containing Seer Registiries:
      #         AK, AZ, CA, CT, GA, HI, ID, IA, KY, LA, 
      #         MA, MI, NJ, NM, NY, OK, UT, WA, WI  ## changed by FZ 10/07/2019
      #
      #    The ...East packages contain the census tracts for 2000 and 2010
      #    for the 20 states without Seer Registries east of the Mississippi 
      #    river.  ## number changed by FZ 10/07/2019
      #
      #    The ...West packages contain the census tracts for 2000 and 2010
      #    for the 13 states without Seer Registries west of the Mississippi
      #    river.  ## number changed by FZ 10/07/2019

      #   
      #    Added - March, 2018 - Health Service Areas (HSA)
      #
      ####
   
      ###
      #
      #  Local SM_Build functions
      #
      
      ###
      #
      #  Load a list of boundary files
      #
      #  rPM is the variable list for the package
      #  DSList is a data.frame with the "DSN" column containing the dataset (file)
      #   to be loaded/data'd and the "Pkg" column indicating the 
      #   package name containing the dataset.
      #
      #  This version uses the "Import:" feature in the DESCRIPTION
      #  file to make sure the packages are loaded along with lasy-data loaded.
      #  While the datasets may not show up on a data() call, they do
      #  when you do a data(package="xxxx") call. To load, the data call must
      #  specify data("dataset-name",package="package-name",environ=environment())
      #
      #  Primarily for hsa, county and tract
      #
      #  
      #
      loadBoundary2 <- function(rPM, DSList) {
      
         #   rPM - parameter lists and variables
         #   DSList - Data set list  (DSN and Pkg)
         #
         #  Check to see if datasets are available because the appropriate libraries (packages)
         #  were loaded.  This mostly applies to census tract datasets.  
         #
         #cat("Loading boundary list:\n")
         #print(DSList)
         #DDir   <- "c:/projects/statnet/r code/"
         #DDir<-"/spin1/users/zhangf10/GIS/Rpackage/V1.2.2-New/"  ## biowulf folder
         DDir   <- "H:/work/GIS/Rpackage/V1.2.2/"
         DVer   <- "-1.2.2/data/"
      
         # validate the requested load list (dataset and package)
         if (!rPM$debugFlag) {
            #  check DSN names against list provided.
            loadMatch         <- match(DSList$DSN,rPM$loadedDataSetList)   # are the dataset available?
            loadMissing       <- is.na(loadMatch)
         } else {
            # in debug mode nothing is missing.
            loadMissing       <- FALSE
         }
         
         # report any missing required datasets.
         if (any(loadMissing)) {
         
            #  one or more of the datasets needed are missing, need extra package loaded.
            ErrFnd       <- TRUE
            xmsg         <- paste0("***196 The following boundary datasets are missing. Make sure the appropriate SeerMapper",rPM$cY," supplement packages have been installed and loaded.")
            warning(xmsg, call.=FALSE)
         
            loadListMiss <- DSList$DSN[loadMissing]
            xmsg         <- paste0("***197 Missing:",paste0(loadListMiss,collapse=", "),"\n")
            stop(xmsg, call.=FALSE)
            rm(loadListMiss)
         }
      
         #  DSList$DSN contains the list of .rda datasets to load
         #    Data level      is  State,   Seer,  HSA, State/County, or Tract.
         #    Overlay Levels are  Region,  State, State/Seer,  HSA, State/Seer/County
      
         work_set        <- NULL     # start of the accumulation of the DF
         
         for (inx in seq(dim(DSList)[1]))  { #  loadImage in loadList) { # loop and load.
      
            loadImage <- DSList[inx,"DSN"]
            loadImPkg <- DSList[inx,"Pkg"]
         
            if (rPM$debugFlag) {
               # manually load froom data directory "DDir"
               DSN_FN <- paste0(DDir,loadImPkg,DVer,loadImage,".rda")
               #cat("Loading boundary file:",DSN_FN,"\n")
               load(file=DSN_FN,envir=environment())
            } else {
               #cat("data(",loadImage," from package=",loadImPkg,")\n")
               data(list=loadImage,envir=environment(),package=loadImPkg)
            }
       
            new_bnd    <- get(loadImage)   # get newly loaded spdf
                        
            new_idList     <- row.names(new_bnd)
            #cat("idList Z-2000 :",new_idList,"\n")
            new_spdf       <- new_bnd 
            
            if (class(new_spdf) == "SpatialPolygons") {
               # if a Spatial Polygon structure, build it into SPDF
               idDF             <- data.frame(ID=new_idList,row.names=new_idList,stringsAsFactors=FALSE)
               new_spdf         <- SpatialPolygonsDataFrame(new_bnd,idDF)   # build SPDF
            }
            if (class(new_spdf) == "SpatialPolygonsDataFrame" ) {
               # have a Spatial Polygon Data Frame (assume)
               new_spdf@data$ID <- new_idList  # set up one element in @data
            } else {
               # not the correct format.
               xmsg <- paste0("***980 Internal Error.  Boundary DS :",loadImage," is not a SpatialPolygonsDataFrame.")
               stop(xmsg,call.=FALSE)
            }
            #  new_spdf is definitely a SPDF.
    
            #  Combine single state into multiple state data
            if (length(work_set) == 0)  {
               # if first structure loaded - just copy to base structure
               work_set        <- new_spdf
            } else {
               # else append to existing structure *assumption - no duplicate IDs.
               # add following line as workaround  by FZ 06172020
               work_set@proj4string   <-  new_spdf@proj4string   # rgdal and sp workaround. ****
               work_set        <- spRbind(work_set,new_spdf)
            }
            
            #  Now erase imported structures to save space.
            eStr2              <- paste0("suppressWarnings(rm(",loadImage,"))")
            eval(parse(text=eStr2))  # remove imported data.
            #cat("eStr2 command Z-2037 :",eStr2,"\n")
    
         }
         #  boundaries have been read and combined.
      
         #
         #  The boundaries have already been projected into an Alber Equal Area/Distance
         #  projection when the datasets were built.
         #
         
         return(work_set)
      }
      
      # 
      #  end of loadBoundaries2
      #
      #####
    
    
    
      ##### Main code of SM_Build

      ##### Stage 2 - would be used by other processes.
      #
      #  Key variable:  rPM$dataMapDF$ID -> idList -> list of data location IDs for the data.
      #
      #  take information and build id List, 
      #      st99, sa99, rg99, co99, hs99 and associated hsXX, coXX, and maybe trXX
      #
      #      the xxxxListAll vectors
      #      Then construct xxxxPLists related to the data.
      #
      
      #####
      #
      #   We know the names in idCol and dataCol are value.  Use them 
      #   to validate the contents.
      #
      #####
      #cat("initialize - SM_Build Z-2076 \n")

      ErrFnd        <- FALSE
      StopFnd       <- FALSE
      
      #  pull out variables from rPM>
      
      debugFlag     <- rPM$debugFlag
      debug         <- rPM$debug
      
      censusYear    <- rPM$censusYear
      cYear         <- rPM$cYear
      cY            <- rPM$cY

      #cat("censusYear:",censusYear,"  cYear:",cYear,"  cY:",cY," Z-2083 \n")
      
      stateSelDel   <- rPM$stateSelDel    # from us48Only and includePR.
      #OrigCRS      <- rPM$OrigCRS
      #ProjCRS      <- rPM$ProjCRS
      
      ndfName       <- rPM$ndfName    
      
      idMode        <- rPM$idMode         # id validation
      categMode     <- rPM$categMode      # categ validation
      
      dataMapDF     <- rPM$dataMapDF      # get data info.
   
      MV            <- NULL               # initialize new return vector for data.frames
      
      #####
      #
      #  Step 20 - load region, state and seer registry information
      #     and boundary data
      #     Load county information, but not boundary data. There is no 
      #     real tract information to load, just ID.
      #     This is the top level that is always used.
      #     Build xxxxxListAll for the above.
      #
      #  Step 20.1  - Setup the references to the correct census year and handle debug mode.
      #
      rg99_d00        <- NULL
      st99_d00        <- NULL
      sa99_d00        <- NULL
      hs99_d00        <- NULL
      co99_d00        <- NULL
      
                     # to test code. Changes data into loads
      rgDataSet       <- paste0("rg99_d00")   # regional info
      stDataSet       <- paste0("st99_d00")   # state info
      saDataSet       <- paste0("sa99_d00")   # seer  info
      hsDataSet       <- paste0("hs99_d00")   # HSA information
      coDataSet       <- paste0("co99_d00")   # state/county to seer mapping

      #
      #  As of 2/20/17, these files cover both 2000 and 2010 census years.  
      #  They contain a super set of information covering both census years.
      #
      #  Step 20.2 - Setup and load region, states, seer registry,
      #       HSA and county information (and boundaries).
      #       Complete table linkage and information.
      #       co99_d00 table now serves as the original Seer stcoID 
      #         to saID table.
      #
      #   Have censusYear - Load data structures
      #
     
      #cat("Load index files Z-2142 \n")
     
      #cat("Reading/Loading:",stDataSet,"  ",saDataSet,"  ",coDataSet,"\n")
      #DDir          <- "/spin1/users/zhangf10/GIS/Rpackage/V1.2.2-New/"
      DDir   <- "H:/work/GIS/Rpackage/V1.2.2/"
      DVer   <- "-1.2.2/data/"
      
      #cat("debug Z-2148 = ",debug,"\n")
      
      if (debugFlag) {
         cat("datasets via load().\n")      
         load(file=paste0(DDir,"SeerMapper",DVer,rgDataSet,".rda"),envir=environment())  # region
         load(file=paste0(DDir,"SeerMapper",DVer,stDataSet,".rda"),envir=environment())  # state
         load(file=paste0(DDir,"SeerMapper",DVer,saDataSet,".rda"),envir=environment())  # seer registry
         load(file=paste0(DDir,"SeerMapper",DVer,hsDataSet,".rda"),envir=environment())  # hsa
         load(file=paste0(DDir,"SeerMapper",DVer,coDataSet,".rda"),envir=environment())  # county
      } else {
         #cat("datasets via data().\n")
         data(list=rgDataSet,envir=environment(),package="SeerMapper")
         data(list=stDataSet,envir=environment(),package="SeerMapper")
         data(list=saDataSet,envir=environment(),package="SeerMapper")
         data(list=hsDataSet,envir=environment(),package="SeerMapper")
         data(list=coDataSet,envir=environment(),package="SeerMapper")
      }

      #
      #  Step 20.3 - clear the region, data, county and tract level information areas.
      #
      
      #cat("initialize - proj and data areas Z-2170 \n")
      
      # US Regions level (combinations of states)
      regions_set       <- NULL
      regions_data      <- NULL
      regions_proj      <- NULL
      
      rg_proj_sel       <- NULL
      
      # US State Level (includes DC)
      states_set        <- NULL
      states_data       <- NULL
      states_proj       <- NULL
      
      st_proj_sel       <- NULL
      
      # US Seer Registry Level (combinations of counties within a state)
      SeerRegs_set      <- NULL
      SeerRegs_data     <- NULL
      SeerRegs_proj     <- NULL
      
      sa_proj_sel       <- NULL
      hs99_mapr         <- NULL
      co99_mapr         <- NULL
      
      ####
      #
      #  Step 20.4 - State Boundaries Information - adjust for us48Only and includePR
      #
      #cat("Process ",stDataSet,"  into states_data\n")
      
      states_set <- get(stDataSet)
      #eStr                <- paste0("states_set <- ",stDataSet)  #  move load to common variable.
      #eval(parse(text=eStr))
      #cat("cmd:",eStr,"\n")
      
      #cat("dim-states_data:",dim(states_set@data),"\n")
      #print(str(states_set@data))
      
      StateListFull       <- as.character(row.names(states_set))
      MV$StateListFull    <- StateListFull #  not edited by stateSelDel.
      #cat("StateListFull:",StateListFull,"\n")
      
      StateListAll        <- StateListFull
      
      
      #  Adjust state boundaries list based on us48Only and includePR parameters
      
      if (length(stateSelDel) > 0 ) {
         #  Adjust the State Lists
         #  Find index to states to be removed.
         sLInxAllKp       <- is.na(match(StateListAll,stateSelDel))   # T/F of entries to keep
                                                                      # keep entries that do not match (NA)
         StateListAll     <- StateListAll[sLInxAllKp]                 # reduced to only the US 48 states.
         #cat("reduced size of StateListAll by:",stateSelDel,"\n")
      }
      
      MV$StateListAll     <- StateListAll
      #cat("StateListAll:",StateListAll,"\n")
      
      states_set          <- states_set[StateListAll,]      
      
      # Project lat/long coordinates to equal area.
      
      #states_set@proj4string  <- OrigCRS
      #states_proj             <- sp::spTransform(states_set,CRSobj=rPM$ProjCRS) # already done..
      states_proj              <- states_set
      
      states_data         <- states_proj@data                     #  pick up data section of SPDF
      #print(states_data) 
      #saveRDS(states_data, file = "test_states_data_before.rds")
      
      if (!is.null(rPM$CRSproj4)) {
        states_proj <- sp::spTransform(states_set,CRSobj=rPM$CRSproj) ## FZ 01/06/2020
        c_XY<-t(sapply(slot(states_proj,"polygons"), function(x) c(x@ID,x@labpt[1],x@labpt[2])))
        colnames(c_XY)<-c("ID","c_X","c_Y")
        states_proj@data[,c("c_X","c_Y")]<-c_XY[,c("c_X","c_Y")]
      
      } ## FZ 02/24/2020 update centroids based on the user-specified proj
      
      states_data         <- states_proj@data  
      #print(states_data) 
      #saveRDS(states_data, file = "test_states_data_after.rds")
      
      #cat("states_proj row.names RN Z-2231 :",paste0(row.names(states_proj),collapse=", "),"\n")   # get row names.
      
      #  states information dataset.   clean up incase it's not just right
      rm(stDataSet)
    
      #print("initial states_data and StateListAll Z-2239 :")
      #print(str(states_data))
      #print(states_data)
      
      MV$states_proj      <- states_proj          # all states.
      MV$states_data      <- states_data
    
    
      #  saID can't be in the states_data table, since more than one can be in a state.
     
      #
      #     20.5 - Seer Area Boundaries Information
      #
      #cat("Proccess ",saDataSet," into SeerRegs_data.\n")
      
      SeerRegs_set <- get(saDataSet)
        
      #SeerRegs_set@proj4string <- OrigCRS
      #SeerRegs_proj            <- sp::spTransform(SeerRegs_set,CRSobj=rPM$ProjCRS)
      SeerRegs_proj             <- SeerRegs_set

      if (!is.null(rPM$CRSproj4)) {
        SeerRegs_proj <- sp::spTransform(SeerRegs_set,CRSobj=rPM$CRSproj) ## FZ 01/06/2020
        
        ## FZ 02/24/2020 update centroids based on the user-specified proj
        c_XY<-t(sapply(slot(SeerRegs_proj,"polygons"), function(x) c(x@ID,x@labpt[1],x@labpt[2])))
        colnames(c_XY)<-c("ID","c_X","c_Y")
        SeerRegs_proj@data[,c("c_X","c_Y")]<-c_XY[,c("c_X","c_Y")]   
      } 
      
      SeerRegs_data        <- SeerRegs_proj@data
      
      #cat("SeerRegs_data:\n")
      #print(str(SeerRegs_data))
      
      # one row in table per registry (no need to look for unique)
      SeerRegListAll       <- SeerRegs_data$saID
      SeerRegListAll       <- na.omit(SeerRegListAll)   # new file - no NAs.
      #attr(SeerRegListAll,"na.action") <- NULL  # no NAs in the section.
      MV$SeerRegListAll    <- SeerRegListAll
      
      #cat("SeerRegListAll:\n")
      #print(SeerRegListAll)
      
      rm(saDataSet)
      
      MV$SeerRegs_proj     <- SeerRegs_proj     # all Registries
      MV$SeerRegs_data     <- SeerRegs_data
   
      #print(str(SeerRegs_data))
      #print(SeerRegs_data)
      
      # project Seer Registry boundaries from Lat/Long to equal area.
      
      if (debug) {
         cat("SeerRegs_proj RN Z-2285 :",paste0(row.names(SeerRegs_proj),collapse=", "),"\n")   # get row names.
      }
      
      #
      #     20.6 - generate regions Boundaries Information
      #
      #cat("Reading Region_data...\n")
      
      regions_set          <- get(rgDataSet)
      
      regions_data         <- regions_set@data
      #regions_proj         <- sp::spTransform(regions_set,CRSobj=rPM$ProjCRS)
      regions_proj         <- regions_set
      
      if (!is.null(rPM$CRSproj4)) {
        regions_proj <- sp::spTransform(regions_set,CRSobj=rPM$CRSproj) ## FZ 01/06/2020
        
        ## FZ 03/26/2020 update centroids based on the user-specified proj
        #c_XY<-t(sapply(slot(regions_proj,"polygons"), function(x) c(x@ID,x@labpt[1],x@labpt[2])))
        #colnames(c_XY)<-c("ID","c_X","c_Y")
        #regions_proj@data[,c("c_X","c_Y")]<-c_XY[,c("c_X","c_Y")]   ## FZ 06/17/2020 remove c_X and c_Y
      } 
       
      RegionListAll        <- as.character(row.names(regions_proj))
      
      MV$regions_proj      <- regions_proj        # all regions
      MV$regions_data      <- regions_data
      
      MV$RegionListAll     <- RegionListAll
      
      #cat("SM_Build-regional data info:\n")
      #print(regions_data)
      #print(str(regions_data))
      
      #cat("RegionListAll:",RegionListAll,"\n")
      
      
      #
      #  HSA index
      #
      #cat("Process ",hsDataSet," into hs99_mapr Z-2316 \n")
      hs99_mapr            <- get(hsDataSet)
      MV$hs99_mapr         <- hs99_mapr            # represents all HSA
      
      # hs99_mapr is now our reference for HSA information - not any of the hsXX_d00 files.
      #cat("hs99_mapr Z-2328 : \n")
      #print(head(hs99_mapr,30))
      #print(str(hs99_mapr))
       
      #
      #  county index
      #
       
      #cat("Process ",coDataSet," into co99_mapr.\n")
      
      co99_mapr            <- get(coDataSet)
      
      #print(co99_mapr) ### FZ 05/06/2020
      # rebuild parts
      co99_mapr$ID         <- row.names(co99_mapr)
      co99_mapr$stID       <- stringr::str_sub(co99_mapr$ID,1,2)
      co99_mapr$stcoID     <- co99_mapr$ID
      co99_mapr$stcotrID   <- NA
      
      co99_mapr$stName   <- states_data[co99_mapr$stID,"stName"]
      co99_mapr$stAbbr   <- states_data[co99_mapr$stID,"abbr"]
      
      co99_mapr <- co99_mapr[,c("ID","stID","stAbbr","stName","stcoID","coName","stcotrID","saID","HSAID","c_X_00","c_Y_00","c_X_10","c_Y_10","tracts_00","tracts_10","y")]
      # co99 index now restored.
      
      MV$co99_mapr         <- co99_mapr              # represents all CO
      
      # co99_mapr is now our reference for county information - not the coXX_d00 files.
      #cat("co99_mapr Z-2355 : \n")
      #print(head(co99_mapr,30))
      #print(str(co99_mapr))
      
      ####
      #
      #  The data tables for each map layer and data.
      #
      #  states_data   (from st99_d00)
      #            Expanded to handle 2000 and 2010 boundaries - states did not change.
      #     row.names      - state 2 digit FIPS code
      #     ID             - state 2 digit fips code (also row.names)
      #     stID           - state 2 digit fips code (also row.names)
      #     abbr           - state 2 character postal abbreviation
      #     stName         - state name (remove)
      #     rgID           - census region #  (1-4)
      #     rgName         - census region name (Northeast, South, Midwest, West)
      #     dvID           - census division #
      #     dvName         - census division name
      #     loc            - location of state (east or west of Mississippi river)
      #     c_X, c_Y       - centroid X, Y build on load and after transform
      #     county_00      - number of 2000 counties in state
      #     county_10      - number of 2010 counties in state
      #     tracts_00      - number of 2000 census tracts in state
      #     tracts_10      - number of 2010 census tracts in state
      #     change10       - T/F indicator whether coXX_d00 is valid for 2010 mapping.
      #                      If T, package should use coXX_d10 dataset.
      #
      #    dropped after read:
      #     ## done when building dataset.
      #     scale          - Scaling of original coordinates of state
      #     moveX          - X adjustment to state coordinates (offset_x)
      #     moveY          - Y adjustment to state coordinates (offset_y)
      #     proj           - projection used on the state  (contained in _proj)
      #     DoAdj          - are adjustments required?  (shift)
      #
      #
      #  SeerRegs_data     (only true Seer Regs - with boundaries)
      #     row.names      - Seer Registry abbreviation
      #     ID             - built from row.names
      #     saID           - Seer area abbreviation (build from row.names)
      #     stID           - state 2 digit fips code
      #     stName         - state Name
      #     county_00      - number of counties in Seer Registry in 2000
      #     county_10      - number of counties in Seer Registry in 2010
      #     tracts_00      - number of tracts in Seer Registry in 2000
      #     tracts_10      - number of tracts in Seer Registry in 2010
      #     rgID           - key to US regions. (generated from stID match when loaded)
      #     c_X_00, c_Y_00 - built from proj - area centroid (after transform) for 2000
      #     c_X_10, c_Y_10 - built from proj - area centroid (after transform) for 2010
      #
      #
      #  regions_data
      #     row.names      - key to US census regions
      #     rgID           - key to US regions.
      #     rgName         - region name
      #     county_00      - integer number of counties in region in 2000
      #     county_10      - Integer number of counties in region in 2010
      #     tracts_00      - integer number of tracts in region in 2000
      #     tracts_10      - integer number of tracts in region in 2010
      #
      #         from hs99_d00.rda
      #  hs99_mapr
      #     row.names      - character3 digit HSA number (001-999)
      #     ID             - * character 3 digit HSA ID (number) 
      #     HSA            - * integer numeric HSA number
      #     HSAID          - * same as ID
      #     HSA_Name       - character - HSA name
      #     stID           - 2 digit state ID (FIPS) associated with HSA
      #     stAbbr         - * 2 character state abbreviation
      #     stName         - * character full state name
      #     saID           - character assocated Seer Registry ID (2-6 characters) or NA
      #     y              - numeric - census year indicater - 1 = 2000, 2 = 2010, 3 = both
      #     Chg10          - indicates the boundaries changed during 2010.
      #     county00       - integer number of counties in HSA during 2000 census
      #     county10       - integer number of counties in HSA during 2010 census
      #     c_X_00, c_Y_00 - centroid of HSA during 2000 census
      #     c_X_10, c_Y_10 - centroid of HSA during 2010 census
      #
      #
      #         from co99_d00.rda  - partial read from disk and rest re-built.
      #  co99_mapr        (replacement for Seer_stcoIDtosaID to minimize space.)
      #     row.names      - character vector of 5 digit FIPS coped
      #     ID             - * county fips id - 5 digit same as stcoID   #built
      #     stID           - * state fips
      #     stAbbr         - * state abbreviation
      #     stName         - * state Name
      #     stcoID         - * state/county fips   
      #     stcotrID       - * NA
      #     coName         - county name                         
      #     saID           - associated Seer Registry abbr.
      #     HSAID          - associated HSA number (3 characters)
      #     tracts_00      - Number of tracts within county in 2000
      #     tracts_10      - Number of tracts within county in 2010
      #     c_X_00, c_Y_00 - centroid for country for 2000 census
      #     c_X_10, c_Y_10 - centroid for country for 2100 census
      #     y              - use year (1 = 2000, 2 = 2010, 3 = 2000 and 2010
      #
      #
      #  No tr99_d00 index.
      #
      #  Links between state, seer and region = "region"
      #  Links between state and seer = stID
      #  Links between county and HSA = HSAID
      #    Seer Registry can only be in one state, but multiple Seer Registries
      #    can be in a state.
      #
      #  Loaded the Seer and State level information and boundaries.  Used in all case.
      #
      #  Have set up:  _data table with information and identification fields.
      #                _proj SPDF with boundery informaton for area projected to equal area.
      #
      #
      #  co99_proj
      #
      #  co99_data         - counties for states with data.
      #     row.names      - 5 digit FIPS code
      #    added:
      #     ID             - 5 digit FIPS code
      #     stID           - State ID (2 digit FIPS)
      #     saID           - Seer Registry abbreviation (is associated, else NA)
      #     HSAID          - HSA Number
      #     stcoID         - 5 digit FIPS code
      #     stcotrID       - 11 digit FIPS for tract <= NA
      #     
      # As many fields are pre-calculated and in the fields to avoid
      #   processing time.  Assume reading is faster.
      #
      #####
      
      ##  regional, state, and Seer Registry boundaries for the entire US are loaded.
      
      
      #####  Basic boundary tables loaded
      #cat("basic boundary tables loaded - Z-2491 \n")
      
      #####
      #
      #   Validate contents of idCol (passed as idList), create idList and 
      #   set idMode to the right value for the data.
      #   The id list is already in the dataMapDF data.frame.  
      #   In the call parameter validation, the id, data, and hData 
      #   columns were identified and copied into the dataMapDF. 
      #
      #  Step 21 - process idCol values, detect type of run 
      #            (state, seer, county or tract) and clean up values
      #            Deal with content of idCol.
      #
      #  idCol data pasted in via idList parameter.
      #
      #  Clean up ID and save.
      #
      
      dataMapID       <- dataMapDF$ID         # make character and trim blanks
      
      #cat("dataMapDF Z-2512 :\n")
      #print(str(dataMapDF))
      rPM$dataMapDF <- dataMapDF
      
      #
      #  possible ID values:
      #       1-2  state fips  1-56 and 72  (digits)
      #       1-3  HSA numbers  (not state value) if 3 digits force HSA type (digits)
      #       4-5  state/county fips
      #      10-11 state/county/tract fips
      #     non-numeric seer registry abbreviations or names
      #
      
      #   Test for NA values in location IDs.
      idNA            <- is.na(dataMapID)                        # any NA in list of Location IDs.
      if (any(idNA)) {
         # some IDs are NA.
         xmsg         <- paste0("***220 Some of the data rows in the ",rPM$ndfName," data.frame have location IDs with missing values (NA). These rows will be removed. Correct and rerun.")
         warning(xmsg, call.=FALSE)
         NAList       <- dataMapDF$rSeq[idNA]             # get list of NA rows.
         xmsg         <- paste0("***222 The following rows will not be mapped: ",paste0(NAList,collapse=", "))
         warning(xmsg, call.=FALSE)
         dataMapDF    <- dataMapDF[!idNA,]    # remove ID=NA
         dataMapID    <- dataMapDF$ID         # get new copy of IDs.
      }
      
      #  get range of number of characters in location ID
      idLenR          <- range(nchar(dataMapID)) #,na.rm=TRUE)      # get range of length of idCol values
      
      #  validate all location IDs are numeric ONLY
      idType          <- grepl("^[0-9]*$",dataMapID)             # inspect list for numbers or characters
          # values can only be integer (0-9) or characters.  So simple verification.
          # TRUE - NUMBER,    FALSE - NOT-NUMBER or NA or SEER-Abbr
          # Possible NAs already been removed.
      
      idLenMax        <- idLenR[2]    # upper range of the number of character in ID.
      idMode          <- NA           # 1-state, 2-county, 3-tract, 4-seer  , 5-HSA   , 6-   , 7- 
      idGroup         <- ""
      
      data_proj        <- NULL
      data_data        <- NULL
      dataListAll      <- NA
      dataListData     <- NA   

      loadDetails      <- FALSE
      #
      # Classify ID.  - and fill out dataMapDF as needed.
      #
      
      #cat("classify run - idLenR(ange):",idLenR,"\n idLenMax:",idLenMax,"\n")
      #cat("  idMode:",idMode,"\n idGroup:", idGroup,"\n")
      #cat("idType:",idType,"\n")
      
      if (all(idType)) {
         # all values are numeric integers.  (=1 TRUE) (or NA, to be edited out.)
         #
         # The data is at the state, HSA, state/county, or state/county/tract level.
         #
         if (idLenMax == 2 || idLenMax == 1) {  # 1 or 2 characters => STATE
            # State Level data
            #cat("state level Z-2572 \n")
            
            # data info - make sure it's 2 digits.
            dataMapID       <- as.character(stringr::str_sub(paste0("0",dataMapID),-2,-1))  # get right two characters   stID
            
            xm              <- match(dataMapID,StateListAll)   # see if any does not match the list (clue for HSA)
            xmNA            <- is.na(xm)
            xmCnt           <- sum(xmNA)              # number of non-matches.
            
            # if there are non-matches, then classify the Location ID as HSA.
            if (xmCnt > 0) {
               # bad state IDs - assume it's HSA
               xmsg <- paste0("***206 The location IDs contain invalid state IDs.  Assuming the Location IDs are HSA numbers.")
               warning(xmsg, call.=FALSE)
               idLenMax <- 3   # for to HSA
            } else {
               # treat as State IDs
            
               loadDetails     <- FALSE
      
               idGroup         <- NA
               idMode          <- 1                # State mode
      
               dataMapDF$ID       <- dataMapID
               dataMapDF$stID     <- dataMapID
               dataMapDF$stcoID   <- NA
               dataMapDF$stcotrID <- NA
      
               dataMapDF$saID     <- NA              # Seer Reg not at state level
               dataMapDF$HSAID    <- NA
               #  saID, stcoID, stcotrID and HSAID will remain NA.
            
               data_proj          <- states_proj
               data_data          <- states_data
                    
               #
               # Special Note: when state data is being mapped, Registries have no
               #   data, so SeerRegListData will be empty.
               #
               # At state level, when seerB="DATA" or "STATE", we want the 
               # registries in the state to be outlined.
               #     SeerRegListData &  SeerRegListInStateData
               #     must be set up - later (See SM_Impl_B)
               #
            }
         }
         
         #   HSA IDs
         if (idLenMax == 3) {
            # at least one location ID has a length of 3, it must be HSA IDs.
            #cat("HSA Level Z-2622 \n")
            #print(dataMapID)
            #print(dataMapDF)
            
            # Health Service Area Level data
            loadDetails        <- TRUE
            
            # type
            idGroup            <- "hs"
            idMode             <- 5        # hsa mode
         
            # make sure it's 3 digits in all cases.
            dataMapID          <- stringr::str_sub(paste0("00",dataMapID),-3,-1)  # get right 3 characters - HSAID

            dataMapDF$ID       <- dataMapID                      # HSA ID field
            dataMapDF$stID     <- hs99_mapr[dataMapID,"stID"]    # Pick state ID   ###
            dataMapDF$stcoID   <- NA
            dataMapDF$stcotrID <- NA
      
            dataMapDF$saID     <- hs99_mapr[dataMapID,"saID"]    # Pick up Registry ID ###
            dataMapDF$HSAID    <- dataMapID
            # set data_proj later when boundary datasets read. (hs99_d00 and boundaries)
            
            #cat("HSA Classification Z-2645 \n")
            #print(str(hs99_mapr))
            #print(str(dataMapDF))
            
            #   proj to be gathered later.
         
         }
      
         if (idLenMax == 5 || idLenMax == 4) {
            #cat("County Level  Z-2647 \n")
            # State / County Level data
            loadDetails        <- TRUE     # need to load county/tract data.
            
            # type
            idGroup            <- "co"
            idMode             <- 2             # State/County mode
      
            # make sure it's 5 digits.
            dataMapID           <- stringr::str_sub(paste0("000",dataMapID),-5,-1)  # get right 5 characters  stcoID
            dataMapDF$ID        <- dataMapID
            dataMapDF$stID      <- stringr::str_sub(dataMapID,1,2)         
            dataMapDF$stcoID    <- dataMapID                # get state ID which is equal to XXXid
            dataMapDF$stcotrID  <- NA
          
            dataMapDF$saID      <- co99_mapr[dataMapDF$stcoID,"saID"]
            dataMapDF$HSAID     <- co99_mapr[dataMapDF$stcoID,"HSAID"]
        
            #  Set data_proj later when boundary datasets read. (co99_d00 and boundaries)
            
         }
         
         if (idLenMax == 11 || idLenMax == 10) {
            #cat("Census Tract Level Z-2677 \n")
            # State / County / Tract Level data
            loadDetails        <- TRUE
            
            #  indicate type
            idGroup            <- "tr"
            idMode             <- 3             # State/County/Tract (or State/Tract) mode
      
            # make sure it's 11 digits.
            dataMapID           <- as.character(stringr::str_sub(paste0("0",dataMapID),-11,-1))  # get right two characters   stID
            dataMapDF$ID        <- dataMapID
            dataMapDF$stID      <- stringr::str_sub(dataMapID,1,2)
            dataMapDF$stcoID    <- stringr::str_sub(dataMapID,1,5)   # get state ID which is equal to XXXid
            dataMapDF$stcotrID  <- dataMapID
      
            dataMapDF$saID      <- co99_mapr[dataMapDF$stcoID,"saID"]
            dataMapDF$HSAID     <- co99_mapr[dataMapDF$stcoID,"HSAID"]
            
            dataListAll         <- NA                     # fill in later.  (Tract)
        
            #  Set data_proj later when boundary datasets read. (tr99_d00 and boundaries)
       
         }
       
         #  In all FIPS based Location IDs - get stID for state, county and tract runs.
         if (idMode != 5) dataMapDF$stID      <- stringr::str_sub(dataMapDF$ID,1,2)   # or add to each processing group.
      
      } else {
         # possible Seer Registry Abbr.
         # check to see if all character ??
         if (all(!idType)) {
            # The value is not all numeric or character numeric
            # Assume it's a string name or abbreviation
            #
            # match against the abbreviations first.  If any rows have
            # not matched, try the alias match.  Then report on any misses.
            #
            #
            #  ### Future, may want to implement HSA names as Location IDs.  If so, the logic would become part of the Seer.
            #
      
            SeerNames       <- rPM$SeerNames
            
            SeerAbbr        <- toupper(dataMapID)   # get label strings
            numEntries      <- length(SeerAbbr)                   # get number of rows.
            #cat("numEntries:",numEntries,"\n")
            
            SeerAbbrRes     <- rep(NA,numEntries)                 # results list.
            #cat("Seer Reg ID-SeerAbbr:",paste0(SeerAbbr,collapse=", "),"\n")
      
            # First try matching the Seer Registry Abbreviations
            #     Mark entries that match abbreviations (completely)
      
            SeerAbbrMatch   <- match(SeerAbbr,SeerRegListAll)
                    # have index into SeerRegListAll or NA (no match)
            SeerAbbrMiss    <- is.na(SeerAbbrMatch)     # TRUE = Miss - no match
      
            #
            # SeerAbbrMatch is an index into the SeerAbbr table for each ndf entry ID.
            #   Entries that did not match have indexes of NA.
            #
            # SeerAbbrMiss is a T/F list, one entry for each ndf ID row.
            #
      
            SeerAbbrRes     <- SeerRegListAll[SeerAbbrMatch]       # have matches.
      
            #
            #  SeerAbbrRes is a list of Seer Reg. Abbreviations. One for each
            #    row in the ndf table.   It contains the match abbreviation,
            #    the abbreviation translation for aliases or NA.
            #
      
            if (any(SeerAbbrMiss)) {
               #
               #  If any entry is not an abbreviation (a miss), then
               #  try to partial match to alias list to get abbrev.
               #
               #  If there are any abbr matches earlier, they will not match
               #  the alias list.
               #
               #cat("SeerReg Misses after abbr:",paste0(SeerRegMiss,collapse=", "),"\n")
      
               SeerAliasMatch   <- rep(NA,numEntries)       # NA results index
      
               # wild card match of input character vector (SeerAbbr)
               #   to alias and abbreviations in Seer Registry name table.
      
               SeerROuta     <- t( sapply(c(1:length(SeerNames$alias)),
                                     function(x) {
                                        y=grep(SeerNames$alias[x],SeerAbbr,ignore.case=TRUE)
                                        ifelse(length(y)==0,return(c(NA,NA)),return(c(x,y)))
                                     }
                                  )
                                 )
               # result - matrix of 2 columns:
               #      1st -> position in SeerNames of the match
               #      2nd -> results of the match (grep) (index into the string of the match.)
      
               #  SeerROutb is the results that matched,
               SeerROutb                     <- SeerROuta[!is.na(SeerROuta[,1]),]
                  # Set match vector element for the match entry [,2] to the index to the SeerNames [,1]
               SeerAliasMatch[SeerROutb[,2]] <- SeerROutb[,1]  # set rows that matched with index to SeerNames
                  # any entry not set (NA), did not match the alias list.
               SeerAliasMiss            <- is.na(SeerAliasMatch)  # get list of non-matches.
               
               #
               # SeerAliasMatch contains indexes into SeerAbbr for each entry in the ndf table ID column
               #   that matched.  If no match index = NA.
               #
               # SeerAliasMiss is T/F. One entry for each row in the ndf data.frame.
               #
      
               #cat("SeerAbbrRes-Before Alias:",SeerAbbrRes,"\n")
      
               #  Translate the matched alias to Abbr. and update the results list.
               SeerAliasRes            <- SeerNames$ab[SeerAliasMatch]
               
               #  Merge valid matches from Alias matches.
               SeerAbbrRes[!SeerAliasMiss] <- SeerAliasRes[!SeerAliasMiss] 
               
               #cat("SeerAbbrRes-After merge:",SeerAbbrRes,"\n")
      
               # combine miss lists from Abbr and Alias - TRUE = neither was found.
               SeerRegMiss                <- SeerAbbrMiss & SeerAliasMiss
               #cat("SeerRegMiss:",SeerRegMiss,"\n")
               
               dataMapDF$good <- !SeerRegMiss
               
               # alias converted to abbreviations.
               
               dataMapID[!SeerRegMiss]  <- SeerAbbrRes[!SeerRegMiss]  # update the ID list.
               dataMapDF$ID <- dataMapID
             
               #  report on no matches.
               if (any(SeerRegMiss)) {
                  # we still have some rows that are not valid.
                  
                  badList <- SeerAbbr[SeerRegMiss]   # get list of string not matched.
                  xmsg <- paste0("***300 The following Seer Registry idenifiers do not match the abbreviations or aliases:")
                  ymsg <- paste0("***301   ",paste0(badList,collapse=", "))
                  zmsg <- paste0("***302 These data rows will be ignored in the mapping.")
                  warning(xmsg,call.=FALSE)
                  warning(ymsg,call.=FALSE)
                  warning(zmsg,call.=FALSE)
                  #  remove bad ID rows.
               }
      
               #  SeerAbbr has list of Seer Reg strings.
               #  SeerRegMiss indicates which entries are bad.
               #
               ####
            }
      
            #cat("SR-dataMapID Z-2830 :",dataMapID,"\n")
            
            #cat("Seer Registry name matching completed.\n")
            loadDetails       <- FALSE   # no more loads (SeerReg) - we have everything.
       
            #   set type  
            idGroup            <- NA
            idMode             <- 4       # Seer Area Mode
      
            # Set Seer Registry Abbr ID
            dataMapDF$ID       <- dataMapID
            dataMapDF$saID     <- dataMapID
      
            # Set state ID         
            srMatch            <- match(dataMapID,SeerRegs_data$saID)
            dataMapDF$stID     <- SeerRegs_data[srMatch,"stID"]
            
            dataMapDF$stcoID   <- NA
            dataMapDF$stcotrID <- NA
            
            dataMapDF$HSAID    <- NA
      
            #   use preloaded seer datasets.
            data_proj          <- SeerRegs_proj
            data_data          <- SeerRegs_data             
      
            #print(str(dataMapDF))
      
         } else {
            # mixture of integers and non-integers.
            ErrFnd             <- TRUE
            xmsg               <- paste0("***011 The Location IDs in column ",rPM$idCol," in the ",rPM$ndfName,
                                             " data.frame are a mix of numbers and characters.\n",
                                             " They must be all digits for for FIPS codes or characters for Seer Registry Abbreviations.")
            stop(xmsg, call.=FALSE)
         }
      } # end classification
      # everyone is done. fix up reference values
      
      
      row.names(dataMapDF) <- dataMapDF$ID  # update incase fixed.
      
      rPM$dataMapDF       <- dataMapDF        # save checkpoint of dataMapDF
      rPM$idMode          <- idMode
      
      rPM$BK0.dataMapDF   <- dataMapDF
      
      #  all set including idMode
      #  boundary data at state, region and registry level have been edited to stateSelDel.
      #
      ####
      
   
      ####
      #
      #   Step 23 - back fill in additional information into ndf - like region (080-082)
      #
      #  Translate stID to region ID
      #
      #cat("Step 23 - Z-2889 \n")
       
      #
      #   Check that all states used are valid.
      #
      stMatch           <- match(dataMapDF$stID,StateListFull)           # map data to statesIndex.
      #print(stMatch)
      #print(StateListFull)
      
      if (debug) {
         cat("stMatch Z-2899 :",paste0(stMatch,collapse=", "),"\n")
      }
      
      # State check must be done now to save loading bad county and tract data.
      if (any(is.na(stMatch))) {
          # one or more IDs in the data not valid Fips codes for states.
          ErrFnd     <- TRUE
          MisList    <- dataMapDF[is.na(stMatch),"ID"]
          xmsg       <- paste0("***046 The following ID values are not valid FIPs codes. Check the first two digits for the proper state codes:\n",
                                   paste0(MisList, collapse=", "))
                                   
          stop(xmsg, call.=FALSE)
      }   
      # are the states in the boundary data.
      stMatch              <- match(dataMapDF$stID, states_data$stID)
      if (any(is.na(stMatch))) {
          # we have states not in the mapping list because of us48Only and includePR.
          # mark the record to be dropped later.
          # This should not happen, since we checked the ID's earlier.
          
          DropList         <- is.na(stMatch)
          dataMapDF[DropList,"good"] <- FALSE
          dataListData     <- dataMapDF$ID
      }
      
      # second use of stMatch - move rgID over to data
      
      dataMapDF$rgID       <- as.character(states_data[stMatch,"rgID"])       # then pull out the regions for this data entry.
      
      #####
      #
      #  Build all of the StateListData vectors regardless of ID type.  
      #    state ID is pulled off the ID if state, county or tract level.
      #
      
      StateListData     <- unique(dataMapDF$stID)
      
      #####
      #
      #  Check point save into MV
      #
      
      rPM$dataMapDF     <- dataMapDF
      rPM$StateListData <- StateListData
      
      #####debug
      
      rPM$BK1.dataMapDF <- dataMapDF
 
      #cat("end of state checks Z-2948 \n")
      #cat("dataMapDF - \n")
      #print(str(dataMapDF))
      #cat("StateListData\n")
      #print(StateListData)
      
 
      #
      #####
      
      #####
      #
      #  Step 24 - Load needed HSA/COUNTY/TRACT DATA
      #         Based on ID type setup, load county and tract boundary datasets as needed.
      #
      # Three tiers: HSA, County, Tract
      #    a) T2=HSA,   T1=County, Data=Tract
      #    b)           T1=HSA,    Data=County
      #    c)                      Data=HSA
      
      #
      #   Later expansion would add loading of the Health Districts for each state.
      #
      #cat("Step 24 - load needed details boundaries Z-2971 loadDetails:",loadDetails,"\n")
      
      if (debug) {
         cat("Check if hs99, co99 or tr99 datasets need loading. Z-2974  idMode:",idMode,
                "  idGroup:",idGroup,"  loadDetails:",loadDetails,"\n")
      }
      
      #cat("Get ready for load details - idMode",idMode,"  loaddetails:",loadDetails,"\n")
      
      #
      #  hsXX_dXX fields -> hs99_data
      #
      #     row.names  <- hs99_data$ID  <- $HSAID
      #     @data$ID   <- row.names(co99_proj)
      #     polygons   (for the HSA)
      #
      #  for detail fields see hs99_mapr
      #
      #       ID        -> build on load from row.names
      #       stID      -> built on load (from hs99_d00$stID)
      #       saID      -> built on load (from hs99_d00$asID)
      #       HSAID     -> built on load from row.names
      #       stcoID    -> NA           (present for parallel structure)
      #       stcotrID  -> NA           (present for parallel structure)
      #
      #
      #   coXX_dXX fields -> co99_data
      #
      #       row.names -> co99_data$ID  <- $stcoID
      #       @data$ID   <- row.names(co99_proj)
      #       polygons  (for the counties)
      #
      #  for detail fields see co99_mapr
      #       ID        -> build on load from row.names
      #       stID      -> built on load co99_d00$stID
      #       saID      -> built on load co99_d00$saID
      #       HSAID     -> build on load co99_d00$HSAID
      #       stcoID    -> built on load from row.names
      #       stcotrID  -> NA           (present for parallel structure)
      #
      #   trXX_dXX fields -> tr99_data     (build on fly)   
      #
      #       row.names -> tr99_data$stcotrID  <- $ID
      #       polygons (for the tracts)
      
      #   added based on row.names  in tr99_data  with row.names = $ID.
      #       ID        -> built on load from row.names
      #       stID      -> built on load (1,2) of ID
      #       saID      -> built on load (co99_d00[tr$stcoID,"saDI")
      #       HSAID     -> built on load (co99_d00(tr$stcoID,"HSAID")
      #       stcoID    -> built on load (1,5) of ID
      #       stcotrID  -> built on load is ID
      #       
      #
      #  Added code to check to see if SeerMapperEast and SeerMapperWest packages were installed on this 
      #  system and if they are loaded and the census tract datasets are available has been 
      #  determined are NEEDED.
      #
      #
      #  idMode = 5: loc ID = HSA    : load HSA as Data Level, no county or tract
      #  idMode = 2: loc ID = County : load county as Data Level, HSA as overlay # 1
      #  idMode = 3: loc ID = tract  : load tract as Data Level, County as overlay # 2, HSA as overlay # 1
      #
      #  As before county was always loaded - NOW - HSA is always loaded.
      #      County is loaded for modes 2 and 3.
      #      tract is loaded for modes 3
      #
      hs99_proj        <- NULL
      hs99_data        <- NULL
      HSAListAll       <- NA
      HSAListData      <- NA
              
      co99_proj        <- NULL
      co99_data        <- NULL
      CountyListAll    <- NA
      coListData       <- NA
              
      tr99_proj        <- NULL
      tr99_data        <- NULL
      TractListAll     <- NA
      trListData       <- NA
            
      #
      if (loadDetails) {
         
         #cat("loadDetails Z-3056 :\n")
         
         # take care of getting county and tract boundary datasets read in.
         #  This is only set in idMode 2 (county)  or 3 (tract) or 5 (hsa).
         
         # build load list for states with data.
         #          idGroup = "hs", "co" or "tr"
         #          StateListData is a list of 2 digit state FIPS codes
         #
         
         #
         # In building the load list for counties, check for 2010 requests.  Most
         # are filed by 2000 datasets, but 3 changed enough to require separate files 
         # under the _d10 names.
         
         #cat("censusYear:",censusYear,"   cYear:",cYear,"  cY:",cY," Z-3064 \n")
         #cat("Required States  :",StateListData,"\n")
         
         # Build list of hsXX_dXX, coXX_dXX and trXX_dXX files and packages.  
         #    hsXX_dXX is always in "SeerMapper"
         #    coXX_dXX is always in "SeerMapper"
         #     based on the change10 flag, d00 may be used for d10.
         #
         #    trXX_dXX are in six packages based on the state and census year.
         #
         #    StateListData -> is a list of the states, we need boundaries for.
         #
         if (censusYear == "2000") {
            cY = ""        # part of package name
            DSext ="_d00"  # part of dataset name
         } else {
            cY = "2010"
            DSext ="_d10"
         }
         #cat("cY:",cY,"  DSext:",DSext,"\n")
         
         # build empty data frames.
         
         hsDSList <- data.frame(DSN=character(), Pkg=character(), stringsAsFactors=FALSE)
         coDSList <- data.frame(DSN=character(), Pkg=character(), stringsAsFactors=FALSE)
         trDSList <- data.frame(DSN=character(), Pkg=character(), stringsAsFactors=FALSE)
         
         #
         #  HSA, County and Tract Boundary Data Load List (name, pkg)
         #
         for (stID in StateListData) {
            #   
            #  counties  - all are 2000 based, except 3.
            #
            wDSN2    <- paste0("hs",stID,"_d00")
            wDSN     <- paste0("co",stID,"_d00")
            wPkg     <- "SeerMapper"
             
            if (censusYear == "2010") {
               if (states_data[stID,"change10"]) {
                  # right year and it's an exception.
                  wDSN2 <- paste0("hs",stID,"_d10")
                  wDSN <- paste0("co",stID,"_d10")
               }
            }
            wEnHs  <- data.frame(DSN=wDSN2,Pkg="SeerMapper",stringsAsFactors=FALSE)
            wEnCo  <- data.frame(DSN=wDSN,Pkg="SeerMapper",stringsAsFactors=FALSE)
            #print(wEnHs)
            #print(wEnCo)
            
            hsDSList <- rbind(hsDSList,wEnHs)
            coDSList <- rbind(coDSList,wEnCo)
         
            #	    
            #  tracts - based on year
            # 
            stLoc    <- states_data[stID,"loc"]
            ## check the data package version and replace loc with loc2 in st99_d00 ## FZ 10/07/2019
            if(packageVersion("SeerMapperRegs")>="1.2.2" & packageVersion("SeerMapper2010Regs")>="1.2.2"){
              stLoc    <- states_data[stID,"loc2"]
              states_data$loc<-stLoc
            }else{
              warning("The version of the data packages is older than 1.2.2.")
            }
            wDSN     <- paste0("tr",stID,DSext)
            wPkg     <- paste0("SeerMapper",cY,stLoc)
            wEnTr    <- data.frame(DSN=wDSN,Pkg=wPkg,stringsAsFactors=FALSE) 
            #print(wEnTr)
            
            trDSList <- rbind(trDSList,wEnTr)
         }
         
         #cat("hsDSList, coDSList and trDSList Z-3136:\n")
         #print(hsDSList)
         #print(coDSList)
         #print(trDSList)
         
         #
         # load HSA layer - later use as HSA or data Layer
         # load county layer - later use as county or data layer
         #
         
         #
         #  get list of loaded datasets
         #
         
         wPkg      <- unique(c(trDSList$Pkg,"SeerMapper")) # get list of packages needed.
         #cat("List of tr Pkgs Z-3151 :\n")
         #print(wPkg)
         
         #   Get list of datasets in packages
         rPM$loadedDataSetList <- data(package=wPkg)$results[,"Item"]  
         
         #cat("List of datasets in pkgs:\n")
         #print(rPM$loadedDataSetList)
         
         #cat("StateListData   :",StateListData,"\n")
         #cat("states_data$stID:",states_data$stID,"\n")
         
         #
         #   load HSA information and boundaries   (always loaded) idModes = 2, 3, 5
         # 
         #cat("loadBoundary - hs99_dXX Z-3166 \n")
         
         hs99_proj           <- loadBoundary2(rPM,hsDSList)    # load all HSAs needed as SPDF
         
         xM                 <- match(hs99_proj@data$ID, hs99_mapr$HSAID)
        
         if (!is.null(rPM$CRSproj4)) {
           hs99_proj <- spTransform(hs99_proj,rPM$CRSproj4)  # apply user projection 
           ## FZ 05/06/2020 update centroids based on the user-specified proj
           c_XY<-t(sapply(slot(hs99_proj,"polygons"), function(x) c(x@ID,x@labpt[1],x@labpt[2])))
           colnames(c_XY)<-c("ID","c_X","c_Y")
           hs99_mapr_xM<-hs99_mapr[xM,]
           c_XY_match<-match(hs99_mapr_xM$ID,c_XY[,"ID"])
           if(censusYear=="2000"){
             hs99_mapr_xM[,c("c_X_00","c_Y_00")]<-c_XY[c_XY_match,c("c_X","c_Y")]
           }else if(censusYear=="2010"){
             hs99_mapr_xM[,c("c_X_10","c_Y_10")]<-c_XY[c_XY_match,c("c_X","c_Y")]
           }
           hs99_mapr[xM,]<-hs99_mapr_xM
         } 
         
         #cat("length of hs99 SPDF:", length(hs99_proj),"\n")
         #print("loading HSA borders for overlay Z-3173 ")
         
         hs99_data       <- hs99_proj@data
         hs99_data$HSAID <- hs99_data$ID
         
         #  fill in HSA name  (???)  what to back fill into hs99_d00 from 
         hs99_data$HSA_Name   <- hs99_mapr[xM,"HSA_Name"]
     
         #  Each HSA index record contains the state and seer registry information 
         hs99_data$stID       <- hs99_mapr[xM,"stID"]
         hs99_data$saID       <- hs99_mapr[xM,"saID"]    # build when loaded.
         
         hs99_data$stcoID     <- NA                # no tract ID at county level
         hs99_data$stcotrID   <- NA                # no tract ID at county level
         
         #cat("hs99_data:\n")
         #str(hs99_data)
         
         MV$hs99_proj         <- hs99_proj         # all HSA in states with Data
         MV$hs99_data         <- hs99_data
         
         HSAListAll           <- hs99_data$HSAID    # all HSA in states with Data
         MV$HSAListAll        <- HSAListAll
         
         HSAListData          <- unique(dataMapDF$HSAID)
         MV$HSAListData       <- HSAListData
         
         
         #cat("HSAListAll      :\n")
         #print(HSAListAll)
         
         if (idMode == 5) {
         
            data_proj     <- hs99_proj      # all HSAs in states with data.
            data_data     <- hs99_data
            
            CountyListAll <- NULL
            coListData    <- NULL
            
            TractListAll  <- NULL
            trListData    <- NULL
         
         } else {
            # mode 2, 3 
            #
            #   load County information and boundaries.  Load if idModes 2 or 3.
            #
            co99_proj           <- loadBoundary2(rPM, coDSList)   # load all counties needed.
        
            xM                 <- match(co99_proj@data$ID, co99_mapr$stcoID)

            if (!is.null(rPM$CRSproj4)) {
              co99_proj <- spTransform(co99_proj,rPM$CRSproj4)  # apply user projection 
              ## FZ 05/06/2020 update centroids based on the user-specified proj
              c_XY<-t(sapply(slot(co99_proj,"polygons"), function(x) c(x@ID,x@labpt[1],x@labpt[2])))
              colnames(c_XY)<-c("ID","c_X","c_Y")
              co99_mapr_xM<-co99_mapr[xM,]
              c_XY_match<-match(co99_mapr_xM$ID,c_XY[,"ID"])
              if(censusYear=="2000"){
                co99_mapr_xM[,c("c_X_00","c_Y_00")]<-c_XY[c_XY_match,c("c_X","c_Y")]
              }else if(censusYear=="2010"){
                co99_mapr_xM[,c("c_X_10","c_Y_10")]<-c_XY[c_XY_match,c("c_X","c_Y")]
              }
              co99_mapr[xM,]<-co99_mapr_xM
            } 
            
       
            #cat("length of co99 SPDF:",length(co99_proj),"\n")
            #  co99_proj contains saID with "xx-NON" references.
            #  17/02/03 - removed saID and saHere from dataset - must rebuild here.
         
            #print("loading County borders for overlay Z-3224 ")
            
            co99_data          <- co99_proj@data
         
            co99_data$stcoID   <- co99_data$ID                                    # redundent
            co99_data$stID     <- stringr::str_sub(co99_data$stcoID,1,2)         # build when loaded.
            
            #  fill in county name  (???)
            co99_data$coName   <- co99_mapr[xM,"coName"]
            #  since states may have multiple registries, the registries must be looked up at the county level.
            co99_data$saID     <- co99_mapr[xM,"saID"]    # build when loaded.
         
            co99_data$stcotrID <- NA                # no tract ID at county level
            
            #cat("co99_data:\n")
            #str(co99_data)
            
            CountyListAll      <- co99_data$stcoID     # list of all counties in states with data.
            
            coListData         <- unique(dataMapDF$stcoID)
            
            #cat("CountyListAll   :\n")
            #print(CountyListAll)
                  
            if (idMode == 2) {
                        
               data_proj    <- co99_proj
               data_data    <- co99_data
               
               TractListAll <- NULL
               trListData   <- NULL
            
            } else {
               if (idMode == 3) {
         
                  # DATA Level = TR
                  #
                  # if tr is needed, then load it and make it data layer.
                  #
                  # All of the census tract boundary information are now in supplemental packages.
                  # Additional processing is needed to determine if the packages were properly 
                  # installed and then to get them loaded, if not.
                  #
                  # 1) Get list of installed packages on host machine.
                  # 2) Get list of required packages for the states listed in the data.
                  # 3) Issue require for each package needed.
                  #
                  # If errors - warn user and stop.
                  #
                  #  The st99_dxx data.frame contains the location of each states' census tract
                  #    as "Regs", "East", or "West".
                  #
                  #  The base name is always "SeerMapper".
                  #
                  #  Get list of required boundary dataset packages.
                
                  #### Have list of needed packages.
                  
                  #
                  #  Get list of loaded packages (in namespace). By now they should be in Namespaces
                  #
                  loadNSPkg         <- loadedNamespaces()        # list of loaded packages in Namespace
                  missingPkg        <- is.na(match(trDSList$Pkg, loadNSPkg))
                  #cat("loadNSPkg:",loadNSPkg,"\n")
                  
                  if (any(missingPkg)) {
                     MissingList   <- trDSList$Pkg[missingPkg]   # get list of missing packages
                     #cat("package needed:",MissingList,"\n")
                  
                     #  one or more of the packages are missing, need extra package loaded.
                     ErrFnd       <- TRUE
                     xmsg         <- paste0("***198 The following supplemental SeerMapper Census Tract boundary packages are missing and must be installed and loaded:")
                     warning(xmsg, call.=FALSE)
               
                     xmsg         <- paste0("***199 Missing:",paste0(MissingList,collapse=", "),"\n")
                     stop(xmsg, call.=FALSE)
                     rm(MissingList)
                  }
                  
                  if (ErrFnd) stop()
                  
                  #cat("trDSList.\n")
                  #print(head(trDSList))
                  
                  tr99_proj          <- loadBoundary2(rPM,  trDSList )
         
                  if (!is.null(rPM$CRSproj4)) tr99_proj <- spTransform(tr99_proj,rPM$CRSproj4)  # apply user projection 
         
                  #cat("LB-class(tr99_proj):",class(tr99_proj),"\n")
                  
                  tr99_proj@data$ID  <- as.character(row.names(tr99_proj))
                  
                  tr99_data          <- tr99_proj@data     # nothing to get, just the row.names
         
                  #cat("LB-tr99_proj@data:\n")
                  #print(str(tr99_proj@data))
         
                  #  Assumption - row.names(tr99_data) is set to the tr99_data$ID  (stcotrID)
         
                  # tr99 data does not have any @data fields, must build fron scratch
         
                  tr99_data$stID     <- as.character(stringr::str_sub(tr99_data$ID,1,2))    # build when loaded - get stID from tract fips code.
                  tr99_data$stcoID   <- as.character(stringr::str_sub(tr99_data$ID,1,5))    # build when loaded - get stcoID
                  tr99_data$stcotrID <- tr99_data$ID                               # build when loaded
         
                  tr99_data$saID     <- as.character(co99_mapr[tr99_data$stcoID,"saID"]) # pull saID from county and add to tract DF.
         
                  trMatch            <- match(tr99_data$stID,StateListAll)         # Do effective statesSelDel operation.
                  TractListAll       <- tr99_data[!is.na(trMatch),"stcotrID"]      #  
         
                  #cat("LB-tr99_data:\n")
                  #print(str(tr99_data))
         
                  # now build subL table for Census Tract Data.
         
                  data_data            <- tr99_data         # all tracts in states with data.
                  data_proj            <- tr99_proj
         
                  dataMapDF$saID       <- tr99_data[dataMapDF$ID,"saID"]  # match ID to boundary and get saID.
         
                  #
                  #  At this point we have tract in subL_ and tr99_ structures  and county in co99_ structure.
                  #
                  
                  TractListAll <- tr99_data$stcotrID
                  trListData   <- unique(dataMapDF$stcotrID)
         
               }
            
            }
         
         }         
      }  # end of load details        
      #cat("End of load details tables - hs, co and tr, if any!.\n")
      
      rPM$BK2.dataMapDF   <- dataMapDF
      
      # save all of the xxxx_proj's 
      
      dataListAll         <- row.names(data_proj)  # all of the area at the data level.  Up to states with data.
      dataListData        <- dataMapDF$ID
      
      MV$data_proj        <- data_proj
      MV$data_data        <- data_data
      MV$dataListAll      <- dataListAll
      rPM$dataListAll      <- dataListAll
      MV$dataListData     <- dataListData
      rPM$dataListData     <- dataListData
      
      MV$hs99_proj        <- hs99_proj
      MV$hs99_data        <- hs99_data
      MV$HSAListAll       <- HSAListAll
      rPM$HSAListAll       <- HSAListAll
      HSAListData          <- unique(dataMapDF$HSAID)
      MV$HSAListData       <- HSAListData
      rPM$HSAListData      <- HSAListData
      
      MV$co99_proj        <- co99_proj
      MV$co99_data        <- co99_data
      MV$CountyListAll    <- CountyListAll
      rPM$CountyListAll   <- CountyListAll
      coListData          <- unique(dataMapDF$stcoID)
      MV$coListData        <- coListData
      rPM$coListData       <- coListData
          
      MV$tr99_proj         <- tr99_proj
      MV$tr99_data         <- tr99_data
      MV$TractListAll      <- TractListAll
      rPM$TractListAll     <- TractListAll
      trListData           <- unique(dataMapDF$stcotrID)
      MV$trListData        <- trListData
      rPM$trListData       <- trListData
      
      #  update changed dataMapDF
      
      rPM$dataMapDF       <- dataMapDF
      rPM$idMode          <- idMode
      
      #### debug
      rPM$Backup.dataMapDF <- dataMapDF
      
      #
      # Report on the built lists for ALL  
      #
      #cat("end of build part 1 - loaded.\n")
      
      #cat("idMode         :",idMode,"\n")
      #cat("dataListAll    :",dataListAll,"\n")
      #cat("RegionListAll  :",RegionListAll,"\n")
      #cat("StateListAll   :",StateListAll,"\n")
      #cat("SeerRegListAll :",SeerRegListAll,"\n")
      #cat("HSAListAll     :",HSAListAll, "\n")
      #cat("CountyListAll  :",CountyListAll,"\n")
      #cat("TractListAll   :",TractListAll,"\n")
      
      ####
      #
      #  Build dataListData list of IDs in dataMapDF
      #
      #   Go and validate the ID and adjust the rows.
      #
      #print("Call SM_ValID")     
      
      rPM <- SM_ValID(rPM,MV)
      
      #
      #  end of SM_ValID
      #
      ####
      
      ####
      #cat("Adjust the clipTo values and Check - Z-3443 \n")
      #	  Adjust the clipTo values
      #   idMode now set to type of Location ID
      #
      #   Test values of clipTo: 
      #     if clipTo = "NONE" - no clipping                       (num=1)
      #     if clipTo = "DATA" - use data_proj for bbox            (num=2)
      #     if clipTo = "HSA"  - find bbox of HSAs with data.      (num=7)
      #     if clipTo = "SEER" = find bbox of SEER with data.      (num=4) 
      #     if clipTo = "STATE" = find bbox of STATE with data.    (num=5)
      #     if clipTo = "REGION" = find bbox of REGIONs with data. (num=6)
      #
      #  SList         <- c("NONE", "DATA", NA, "SEER", "STATE", "REGION", "HSA", "TRUE","FALSE")
      #                      1       2      3     4       5       6         7      8      9
      #  if clipTo="HSA" can't use with data levels of HSA(5), SEER(4) and STATE(1) = set to "DATA"
      #  if clipTo="SEER" can't use with data levels of SEER(4), STATE(1) = set to "DATA"
      #  if clipTo="STATE" can't use with data levels of STATE(1) = set to "DATA"
      #  
      # The reason for this logic is we don't have spatial polygons structure for any 
      # boundaries lower than the data layer's geography.  If set lower, reset to "DATA"
      #
      clipReset <- FALSE
      clipTo    <- rPM$clipTo
      clipToNum <- rPM$clipToNum
      
      if (clipTo == "HSA" && ( idMode == 1 || idMode == 5 || idMode == 4)) {
           clipTo = "DATA"
           clipToNum = 2
           if (idMode != 5) clipReset <- TRUE
      }
      if (clipTo == "SEER" && ( idMode == 1 || idMode == 4)) {
           clipTo = "DATA"
           clipToNum = 2
           if (idMode != 4) clipReset <- TRUE
      }
      if (clipTo == "STATE" && ( idMode == 1 )) {
           clipTo = "DATA"
           clipToNum = 2
      }
      
      if (clipReset) {
         xmsg <- paste0("***096 The clipTo value specifies a geographic level lower than the data level. The clipTo value set to 'DATA'.\n")
         warning(xmsg, call.=FALSE)
      }
      #cat("clipTo:",clipTo," Z-3487  clipToNum:",clipToNum,"\n")
      
      rPM$clipTo <- clipTo
      rPM$clipToNum <- clipToNum
      
      ####
            
      # Build xxxxxListData after the SM_ValID in case the 
      # data is reduced because of an error.
      
      dataMapDF  <- rPM$dataMapDF
      
      # Build ....Data list of what the caller provided.
      dataListData       <- dataMapDF$ID
      RegionListData     <- unique(dataMapDF$rgID)
      StateListData      <- unique(dataMapDF$stID)
      SeerRegListData    <- sort(unique(dataMapDF$saID))   # sort killed possible NA.
      HSAListData        <- unique(dataMapDF$HSAID)
      CountyListData     <- unique(dataMapDF$stcoID)
      TractListData      <- unique(dataMapDF$stcotrID)
      
      #cat("dataListData   :",dataListData,"\n")
      #cat("RegionListData :",RegionListData,"\n")
      #cat("StateListData  :",StateListData,"\n")
      #cat("SeerRegListData:",SeerRegListData,"\n")
      #cat("HSAListData    :",HSAListData,"\n")
      #cat("CountyListData :",CountyListData,"\n")
      #cat("TractListData  :",TractListData,"\n")
      
      #cat(" Z-3516 length(dataListData):",length(dataListData),"\n")
      #cat("        length(data_proj)   :",length(data_proj),"\n")
      #cat("        length(dataMapDF)   :",dim(dataMapDF)[1],"\n")
      
      MV$dataListData    <- dataListData
      MV$RegionListData  <- RegionListData
      MV$StateListData   <- StateListData
      MV$SeerRegListData <- SeerRegListData
      MV$HSAListData     <- HSAListData
      MV$CountyListData  <- CountyListData
      MV$TractListData   <- TractListData
      
      #cat("xxxxListData are build and saved in MV.\n")
      
      #cat("Calling SM_SetDef\n")
      rPM       <- SM_SetDef(rPM)
      
      #cat("Calling SM_Impl_B\n")
      MV        <- SM_Impl_B(rPM, MV)
      
      #cat("Calling SM_box_sel\n")
      xRes      <- SM_box_sel(rPM, MV)
      rPM       <- xRes$rPM
      MV        <- xRes$MV
      
      #cat("Exiting SM_Build... \n")
      
      return(list(rPM=rPM,MV=MV))
      
   }  # end of SM_Build
   #
   #
   #####
      
   #####
   #
   #  SM_ValID  - validate the ID information 
   #      Delete bad rows discovered during earlier validation.
   #      Check ID as NA
   #      Check ID against state list
   #      Check ID against boundary data at same level.
   #
      
 SM_ValID <- function(rPM, MV) {      
      
      #####
      #
      #  Input: rPM - dataMapDF  -> ID and stID of data
      #         MV  - StateListAll -> state ID
      #         MV  - data_proj  -> ID of boundaries at data level
      #
      #  The states_data dataset (table) contains the mapping of the
      #  state abbreviations to the state fips codes and a key to the
      #  location of the census tract boundary data if needed.
      #
      #  message number 200-219
      #
      #####
      
      debug     <- rPM$debug
      
      dataMapDF <- rPM$dataMapDF
      
      #cat("ID List:",dataMapDF$ID,"\n")
      
      #####
      #
      #  5.02   - Clean up dataMapDF(new RateTable) - bad entries - data/location
      #
      
      #  5.02.1 - Delete rows with bad DATA. data = NA
      dataMapDF[is.na(dataMapDF$data),"good"] <- FALSE
      
      #  5.02.1.5 - Delete Rows with bad IDs, ID = NA
  
      dataMapDF[is.na(dataMapDF$ID),"good"]   <- FALSE
      
      #
      #  5.02.2 - Check for Duplicate IDs - identify, warn, and delete.
      #
      #cat("checking for duplicates\n")
      
      idListData       <- dataMapDF$ID
     
      dupList          <- duplicated(idListData)
      
      if(any(dupList)) {
         # Duplicate rows found.
         xmsg          <- paste0("***202 The ",rPM$ndfName," data.frame has duplicate rows with the same location IDs. The duplicate rows will be removed.")
         warning(xmsg, call.=FALSE)   
               
         dPos          <- dataMapDF[dupList,"rSeq"]   # get relative row number in data.
         idL           <- idListData[dupList]
         
         xlines        <-  paste0(formatC(dPos,width=5,)," ",stringr::str_sub(paste0("   ",idL),-5,-1),"\n")
         xlines        <-  c("  row#   ID\n",xlines)
         
         xmsg          <- paste0("***204 The duplicate IDs are:")
         warning(xmsg, call.=FALSE)
         warning(xlines, call.=FALSE)
         #  remove duplicate rows.
         dataMapDF[dupList,"good"]  <- FALSE
      }
      
      
      #
      # Rate Table (dataMapDF) now only contains the area to be mapped (controlled by us48Only and includePR)
      #
      
      #
      #  5.02.04 - Validate data location verse boundary locations
      #
      #   The other boundaries at the same level are in the appreate level boundary data
      #     region, state, seer reg, county, tract - contain the "ALL" boundary information.
      #
      #  Compare data's IDs with the same level's boundary IDs (dataListAll)
      #
      
      #  Match data areas to space_proj
      
      areaMatch          <- match(MV$dataListData, MV$dataListAll)        # index of rate id match to spatial area selected.
                       # should be within this collection.
      
      areaMatchNAs       <- is.na(areaMatch)
      #cat("areaMatch:",areaMatch,"\n")
      
      if (any(areaMatchNAs))  {
         # if any entry in dataListData is not in the boundary group
         
         areaMissing     <- MV$dataListData[areaMatchNAs]  # get list of missing polygons
         xmsg            <- paste0("***290 The following area(s) in the data do not match the list of boundaries:")
         ymsg            <- paste0("***291   >",paste0(areaMissing,collapse=", "))
         warning(xmsg, call.=FALSE)
         warning(ymsg, call.=FALSE)
         zmsg            <- paste0("***292 Please check to make sure your data matches the 20",rPM$cYear, " census area identifiers and boundaries.")
         warning(zmsg, call.=FALSE)
      
         #cat("areaMissing Z-3653 len:",length(areaMatch)," content:",areaMatch,"\n")
         #  remove bad rows
         
         dataMapDF[areaMatchNAs,"good"] <- FALSE
          
      }
     
      # 
      #  5.02.5 - Only good rows from validation (ID validation) and data validation.
      #    Last validation of ID step.
      #
      dataMapDF            <- dataMapDF[dataMapDF$good,]         # clear bad records in the data.
      #cat("Why reset row.names? - ",row.names(dataMapDF),"\n")
      #row.names(dataMapDF) <- dataMapDF$ID     # reset row.names

      rPM$dataMapDF        <- dataMapDF    # update data frame.
      rPM$dataListData     <- dataMapDF$ID # save a step - directly to the answer.
      
      #####debug
      rPM$BK3.dataMapDF    <- dataMapDF
      
      if (debug) {
         print(str(dataMapDF))
         print(head(dataMapDF,10))
         print(tail(dataMapDF,10))
      }  

      lenDataMapDF         <- dim(dataMapDF)[1]                   # get length of rate table.

      if (lenDataMapDF <= 0 ) {
         xmsg     <- paste0("***200 After cleaning up the ",rPM$ndfName," data.frame to remove detected errors, there are no rows of data to process.")
         stop(xmsg)
      }
      
      #cat("Exiting SM_ValID\n")
      return(rPM)
   }
   #
   #
   #####

   #####
   #
   #   SM_SetDef  = Adjust xxxxB based on idMode and original cVL values.
   #
   SM_SetDef  <- function(rPM) {
        
        #####
        #
        #  Now we have idMode.  
        #
        
        debug  <- rPM$debug
        idMode <- rPM$idMode
   
        #cat("Entry SetDef -\n")
        #cat("  regionB:",rPM$regionB,"  regionB_c:",rPM$regionB_caller,"\n")
        #cat("  stateB :",rPM$stateB, "  stateB_c :",rPM$stateB_caller, "\n")
        #cat("  seerB  :",rPM$seerB,  "  seerB_c  :",rPM$seerB_caller,  "\n")
        #cat("  hsaB   :",rPM$hsaB,   "  hsaB_c   :",rPM$hsaB_caller,   "\n")
        #cat("  countyB:",rPM$countyB,"  countyB_c:",rPM$countyB_caller,"\n")
        #cat("  tractB :",rPM$tractB, "  tractB_c :",rPM$tractB_caller, "\n")
        #cat("  clipTo :",rPM$clipTo, "  clipTo_c :",rPM$clipTo_caller, "\n")
        #
        #  Apply merge defaults and caller provided xxxxB parameters based on idMode.
        #  If caller did not set value - set the default value based on idMode.
        #
        #  SM_GlobInit set all xxxxB values to "NONE" in rPM$.  If not modified
        #  by caller - set the default dependent on idMode.
        # 
        
        if (!rPM$regionB_caller) {
           rPM$regionB <- switch( idMode,
                              "NONE",    # 1 - state
                              "NONE",    # 2 - county
                              "NONE",    # 3 - tract
                              "NONE",    # 4 - seer reg
                              "NONE"     # 5 - hsa
                           )
        }
        if (!rPM$stateB_caller) {
           rPM$stateB <- switch( idMode,
                              "ALL",     # 1 - state
                              "NONE",    # 2 - county
                              "NONE",    # 3 - tract
                              "NONE",    # 4 - seer reg
                              "NONE"     # 5 - hsa
                           )
        }
        if (!rPM$seerB_caller) {
           rPM$seerB <- switch( idMode,
                              "NONE",    # 1 - state
                              "NONE",    # 2 - county
                              "NONE",    # 3 - tract
                              "DATA",    # 4 - seer reg
                              "NONE"     # 5 - hsa
                           )
        }
        if (!rPM$hsaB_caller) {
           rPM$hsaB <- switch( idMode,
                              "NONE",    # 1 - state
                              "NONE",    # 2 - county
                              "NONE",    # 3 - tract
                              "NONE",    # 4 - seer reg
                              "DATA"     # 5 - hsa
                           )
        }
        if (!rPM$countyB_caller) {
           rPM$countyB <- switch( idMode,
                              "NONE",    # 1 - state
                              "DATA",    # 2 - county
                              "NONE",    # 3 - tract
                              "NONE",    # 4 - seer reg
                              "NONE"     # 5 - hsa
                           )
        }
        if (!rPM$tractB_caller) {
           rPM$tractB <- switch( idMode,
                              "NONE",    # 1 - state
                              "NONE",    # 2 - county
                              "DATA",    # 3 - tract
                              "NONE",    # 4 - seer reg
                              "NONE"     # 5 - hsa
                           )
        }

        if (!rPM$clipTo_caller) {
           # The default for clipTo is always "NONE"
           rPM$clipTo <- "NONE"
           rPM$clipToNum <- 1
        }
        #
        #  Merge default based on idMode and caller value for dataBCol.
        #
        
        if (!rPM$dataBCol_caller) {
           # caller has not specified the dataBCol parameter, so 
           # we can reset it to how the defaults would work based on the idMode.
           rPM$dataBCol  <- switch(idMode,
                             rPM$ColorB_O_State,      # idMode = 1 STATE
                             rPM$ColorB_O_County,     # idMode = 2 COUNTY
                             rPM$ColorB_O_Tract,      # idMode = 3 TRACT
                             rPM$ColorB_O_Seer,       # idMode = 4 Seer Registry
                             rPM$ColorB_O_Hsa,        # idMode = 5 Health Service Areas
                             rPM$ColorB_O_Tract       # default.
                           )
        
        }
        
        #
        #####
        #cat("Exit SetDef regionB:",rPM$regionID,"  stateB:",rPM$stateB,"  seerB:",rPM$seerB,
        #     "  hsaB:",rPM$hsaB,"  countyB:",rPM$countyB,"  tractB:",rPM$tractB,"  clipTo:",rPM$clipTo,"\n")
        
        return(rPM)
   }
   #
   #  end of SM_SetDef
   #
   #####

      
   #####
   #
   #  SM_Impl_B  - take xxxxB parameters and the xxxxListData, xxxxListAll, and dataMapDF and generate 
   #    the xxxx_sel_proj boundary files for mapping.
   #
   #    ssssB validation and default setting has already been done.
   #
   #    creates and returns xxxxPList of the active areas at each level
   #
      
SM_Impl_B <- function(rPM, MV) {
      
      debug <- rPM$debug
      #cat("Entering SM_Impl_B - \n")

      #####
      #
      #  Build intermediate boundary sets - SeerB=STATE, etc.
      #
      #  Dependes on xxxxListAll and xxxxListData vectors and 
      #  the xxxx_data tables for each level with $stID, $stcoID, $rgID, and $saID tags.
      #
      #   Normal default values for StateSeerListData and SeerStateListData
      #  These are list of sub-ares to be drawn.  The may be all or none or data or inbetween.
      #
      #     dataPList    --  The data levels boundaries (with data)
      #     regionPList  --  Region Boundaries
      #     statePList   --  State boundaries
      #     seerPList    --  Seer Registry boundaries
      #     hsaPList     --  Health Service Area boundaries
      #     countyPList  --  county boundaries
      #     tractPList   --  tract boundaries
      #
      #  Lists:
      #     data   -                                                                           DATA
      #     region - ALL,                                                                      DATA, NONE
      #     state  - ALL, REGION w/DATA,                                                       DATA, NONE
      #     seer   - ALL, REGION w/DATA, STATE w/DATA,                                         DATA, NONE
      #
      #     hsa    -                     STATE w/DATA, SEER w/DATA                             DATA, NONE
      #     county -                     STATE w/DATA, SEER w/DATA, HSA w/DATA                 DATA, NONE
      #     tract  -                     STATE w/DATA, SEER w/DATA, HSA w/DATA, COUNTY w/DATA, DATA, NONE
      #
          
      ####
      #
      # local functions
      #
      #  Warning: With this code, originally it replaced the old list of elements
      #  at the level.  In most cases this is fine.  However, when there are
      #  not registries covering the entire state (countyB="SEER" or
      #  tractB="SEER") or tractB="COUNTY", or multiple state data. and 
      #  countyB="STATE" on tract data ==> general YOU never really replace 
      #  or delete the original.  YOU ADD the extra.
      #  xxxPList always starts with the original xxxxListData list..
      #  Then add any extra items matched.
      #
      #
      #  Get functions find all xxIDs in full loaded boundaries
      #   that match a criteria, independent of data.
      #   The assumption is it will cover the data since it starts with
      #   a higher level's data list and fills to it.
      #
      #   We have full loads of Region, State, and Seer.
      #
      GetIDListByrgID <- function(xx_data, LrgLD) {
           xM    <- !is.na(match(xx_data$rgID, LrgLD))
           PList <- xx_data[xM,"ID"]
           return (PList)
      }
      GetIDListBystID <- function(xx_data, LstLD) {
           xM    <- !is.na(match(xx_data$stID, LstLD))
           PList <- xx_data[xM,"ID"]
           #cat("BystID return:",PList,"\n")
           return (PList)
      }
      GetIDListBysaID <- function(xx_data, LsaLD) {    
           # for this function the xx_data is the data's 
           xM    <- !is.na(match(xx_data$saID, LsaLD))
           PList <- xx_data[xM,"ID"]
           #cat("BysaID return:",PList,"\n")
           return (PList)
      }
      GetIDListByhsID <- function(xx_data, LhsLD) {           # future
           # for this function the xx_data is the data's 
           xM    <- !is.na(match(xx_data$HSAID, LhsLD))
           PList <- xx_data[xM,"ID"]
           #cat("ByhsaID return:",PList,"\n")
           return (PList)
      }
      GetIDListBystcoID <- function(xx_data, LstcoLD) {       # not used
           xM    <- !is.na(match(xx_data$stcoID, LstcoLD))
           PList <- xx_data[xM,"ID"]
           #cat("BystcoID return:",PList,"\n")
           return (PList)
      }
      #
      #   For HSA, County and Tract, we only have partial loads and
      #   they are lower in the hierarchy then Seer Registry.
      #   One of the problems is Registries are not a subset of 
      #   states and do not cover all counties and tracts.
      #
      
      AddIDsBysaID <- function(xx_data, LsaLD, xxxLD) {
           # Find additional counties or tracts by saID and 
           #  add them to the xxxxListData list.  The list contains NA.
           xM    <- !is.na(match(xx_data$saID, LsaLD))
           PList <- sort(unique(c(xx_data[xM,"ID"],xxxLD)))
           #cat("BysaID return:",PList,"\n")
           return (PList)
      }
      #
      AddIDsByhsID <- function(xx_data, LhsLD, xxxLD) {
           # Find additional counties or tracts by HSAID and 
           #  add them to the xxxxListData list.
           xM    <- !is.na(match(xx_data$HSAID, LhsLD))
           PList <- sort(unique(c(xx_data[xM,"ID"],xxxLD)))
           #cat("ByhsaID return:",PList,"\n")
           return (PList)
      }
      #
      AddIDsBycoID <- function(xx_data, LstcoLD, xxxLD) {
           # Find additional tracts by stcoID and 
           #  add them to the xxxxListData list.
           xM    <- !is.na(match(xx_data$stcoID, LstcoLD))
           PList <- sort(unique(c(xx_data[xM,"ID"],xxxLD)))
           #cat("BysaID return:",PList,"\n")
           return (PList)
      }
      #
      # end if local functions
      ####
      
      #cat("SM_Impl_B - idMode:",rPM$idMode,"\n")
      #cat("regionB:",rPM$regionB,"  stateB:",rPM$stateB,"  seerB:",rPM$seerB,"   hsaB:",rPM$hsaB,"  countyB:",rPM$countyB,"  tractB:",rPM$tractB,"\n")
 
      MV$dataPList   <- MV$dataListData
      
      MV$regionPList <- switch(rPM$regionB,
                         NONE   = NULL,                 # no region boundaries
                         DATA   = MV$RegionListData,    # regional boundaries around data
                         ALL    = MV$RegionListAll,     # all regional boundaries
                         NULL
                       )
      
      MV$statePList <- switch(rPM$stateB,
                         NONE   = NULL,
                         DATA   = MV$StateListData,     # states around data
                         REGION = GetIDListByrgID(MV$states_data,MV$RegionListData),  # states in region around data
                         ALL    = MV$StateListAll,
                         NULL
                       )
      
      MV$seerPList  <- switch(rPM$seerB,
                         NONE   = NULL,
                         DATA   = MV$SeerRegListData,    # registries around data
                         STATE  = GetIDListBystID(MV$SeerRegs_data,MV$StateListData),  # registries in states around data
                         REGION = GetIDListByrgID(MV$SeerRegs_data,MV$RegionListData), # registries in regions around data
                         ALL    = MV$SeerRegListAll,
                         NULL
                       )
      # handle SeerReg exception.
      idMode    <- rPM$idMode
      #cat("SM_Impl_B-idMode:",idMode,"\n")
    
      if (idMode == 1) {
         # state mode
         if (rPM$stateB == "DATA" && rPM$seerB == "ALL") {
            # exception for STATE data, with stataB="DATA" and seerB="ALL", don't do 
            # seer registries in states that will not have boundaries.  So, limit the
            # seerB="ALL", to seer registries with in drawn states.
            
            MV$seerPList <- GetIDListBystID(MV$SeerRegs_data,MV$StateListData)
         }
         if (rPM$stateB == "NONE" && rPM$seerB == "ALL") {
            # exception for STATE data, with stataB="NONE" and seerB="ALL", don't do 
            # seer registries in states that will not have boundaries.  So, limit the
            # seerB="ALL", to seer registries with in drawn states.
            
            MV$seerPList <- GetIDListBystID(MV$SeerRegs_data,MV$StateListData)
         }
      }
      #
      #   SeerPList is the only list that could be empty!!!  If none exist in the states being mapped.
      #
      
      #   HSAs boundaries apply to HSAs, Counties, Tracts
      if (idMode == 5 || idMode == 3 || idMode == 2 ) {
         #cat("hs99_data DF Z-3996 :\n")
         #print(str(MV$hs99_data))
         #cat("StateListData : \n")
         #print(MV$StateListData)
         
         # Health Service Areas
         MV$hsaPList  <- switch(rPM$hsaB,
                          NONE  = NULL,
                          DATA  = MV$HSAListData,   # health service areas around data
                          SEER  = AddIDsBysaID(MV$hs99_data,MV$SeerRegListData,MV$HSAListData),
                                             # HSAs in registries around data
                                             # uses all HSAs list in state(s)
                                             # may have HSAs not in Registries - keep.
                                             # we did load all of the registry HSAs
                                             #  within the SeerRegListData because loaded all within StateListData.
                          STATE = GetIDListBystID(MV$hs99_data,MV$StateListData),
                                             # we loaded all HSAs with the StateListData set.
                          NULL
                               )
      }
      
      #  County boundaries apply to Counties and Tracts
      if (idMode == 2 || idMode == 3) {
         #cat("Setting countyPList\n")
         MV$countyPList <- switch(rPM$countyB,
                          NONE  = NULL,
                          DATA  = MV$CountyListData,    # counties around data
                          HSA   = GetIDListByhsID(MV$co99_data,MV$HSAListData,MV$CountyListData),
                                             # we have all HSAs that include the counties,
                                             # this fills in to the HSA level, no county
                                             # is lost, because the counties are draw within
                                             # the HSAs with counties with data.
                          SEER  = AddIDsBysaID(MV$co99_data,MV$SeerRegListData,MV$CountyListData), 
                                             # counties in registries around data
                                             # uses all county list in state(s)
                                             # may have counties not in Registries - keep.
                                             # we did load all of the registry counties 
                                             #  within the SeerRegListData because loaded all within StateListData.
                          STATE = GetIDListBystID(MV$co99_data,MV$StateListData),
                                             # we loaded all counties with the StateListData set.
                          NULL
                        )
      }
  
      # Tract boundaries only apply to tracts
      if (idMode == 3) {
         #cat("Setting tractPList\n")
         MV$tractPList  <- switch(rPM$tractB,
                          NONE  = NULL,
                          DATA  = MV$TractListData,               # tracts with data
                          COUNTY= GetIDListBystcoID(MV$tr99_data,MV$CountyListData),  
                                            # we have all counties that include the 
                                            # tracts, this fills to the county level
                                            # no tract is lost, because tracts are 
                                            # draw within the counties with tracts with data.
                          HSA   = GetIDListByhsID(MV$tr99_data,MV$HSAListData,MV$TractListData),
                                            # we have all the HSAs that include the 
                                            # tracts, this fills to the HSA level
                                            # no tract is lost, because tracts are 
                                            # draw within the HSAs with tracts with data.
                          SEER  = AddIDsBysaID(MV$tr99_data,MV$SeerRegListData,MV$TractListData),
                                            # Must find the extra tracts in the Seers with data
                                            # but also keep any tracts not in Seers.
                          STATE = GetIDListBystID(MV$tr99_data,MV$StateListData),
                                            # states with data have tract boundaries loaded
                                            # if we find tracts in states with data, it's all inclusive.
                          NULL
                        )
      
      }
      #
      #
      #####
      
      if (debug) {
         cat("setup up proj_data and proj_mapped for states and seer Z-4071  END of SM_Impl_B","\n")
         cat("idMode:",rPM$idMode," regionB:",rPM$regionB,"   stateB:",rPM$stateB, "  seerB:",rPM$seerB,
                                  "    hsaB:",rPM$hsaB,   "  countyB:",rPM$countyB," tractB:",rPM$tractB,
                                  "  fillTo:",rPM$fillTo, " dataMapDF Size:",dim(rPM$dataMapDF),"\n")
         cat("dataPList   :",length(MV$dataPList)  ," ",paste0(MV$dataPList  ,collapse=", "),"\n")
         if (!is.null(MV$regionPList)) {
            cat("regionPList :",length(MV$regionPList)," ",paste0(MV$regionPList,collapse=", "),"\n")
         }
         if (!is.null(MV$statePList)) {
            cat("statePList  :",length(MV$statePList) ," ",paste0(MV$statePList ,collapse=", "),"\n")
         }
         if (!is.null(MV$seerPList)) {
            cat("seerPList   :",length(MV$seerPList)  ," ",paste0(MV$seerPList  ,collapse=", "),"\n")
         }
         if (!is.null(MV$hsaPList)) {
            cat("hsaPList    :",length(MV$hsaPList)   ," ",paste0(MV$hdaPList  ,collapse=", "),"\n")
         }
         if (!is.null(MV$countyPList)){
           cat("countyPList :",length(MV$countyPList)," ",paste0(MV$countyPList,collapse=", "),"\n")
         }
         if (!is.null(MV$tractPList)){
           cat("tractPList  :",length(MV$tractPList) ," ",paste0(MV$tractPList ,collapse=", "),"\n")
         }
      }
      #cat("Exiting SM_Impl_B-\n")
  
      return(MV)
   }
   #
   #  end if SM_Impl_B
   #
   ####

   #########
   #
   #    SM_box_sel - 
   #      Takes the xxxx_proj and xxxxPList and creates a xxxx_proj_sel SPDF 
   #      and a bbox for the space.
   #      Calculates the overall size of the bboxes for all xxxx_proj_sel
   #      and returns the xLim and yLim values.
   #
   #   Input:  xxxxxPList,  xxxxx_proj
   #
   #   Output:   MV$ -> xxxxPList adjusted, xxxx_proj_sel 
   #
      
SM_box_sel <- function(rPM, MV) {

      #cat("Entering SM_box_sel...\n")
      
      #####
      debug     <- rPM$debug
      idMode    <- rPM$idMode
      dataPList <- MV$dataPList  # dataPList is equal to dataMapDF$ID list.

      ####
      #
      #  Step 6 - Setup to find the size of the mapping (bbox) and selective boundary sets. (200-209)
      #
      #  The xxxxPList vectors have been setup to indicate what boundaries or areas 
      #  will be drawn at each level.  If NULL, then nothing at that level.
      #
      #  Applying xxxxPList to xxxx_proj yields xxxx_proj_sel for the mapping.
      #  Then the box size of each level can be calculated and the box size of 
      #  the plotting space can be calculated.
      #
      
      ErrFnd <- FALSE
      
      dataMapDF <- rPM$dataMapDF
      
      if (debug) {
         cat("...PList, proj, and Boxes Z-4143 :", "\n")
         cat("idMode      :", idMode, "\n")
         cat("dataPList   :", dataPList, "\n")
      }
          
      data_proj_sel  <- MV$data_proj       # (all boundaries at data's level)
      data_data_sel  <- MV$data_data
      
      #cat("Create data_proj_sel...   dataPList:",dataPList,"\n")
      
      #  dataPList and data_proj
      if (!is.null(dataPList) && !any(is.na(dataPList))) {
         # not null or a NA is in the list
         # valid list
         data_proj_sel  <- data_proj_sel[dataPList,]
         data_data_sel  <- data_data_sel[dataPList,]
         data_box       <- bbox(data_proj_sel)                # primary data box.
         #cat("dataBox Z-4160 : ", data_box, "\n")
      } else {
         # null dataPList or has an NA in list is a major internal error.
         if (!is.null(dataPList)) {
             xmsg    <- paste0("***380 Internal Error - dataPList contains an NA in list:", paste0(dataPList,collapse=", ") )
         } else {
             xmsg    <- paste0("***381 Internal Error - dataPList does not exist.")
         }
         stop(xmsg, call.=FALSE)
      }
      MV$data_proj_sel <- data_proj_sel
      MV$data_data_sel <- data_data_sel
      
      # set up default boxes based on data_proj box (spatial box)

      #cat("Set up default boxes for later...\n")

      data_box        <- bbox(data_proj_sel)                # primary space.
                 # set all other levels to the same - until changed.
      tr_box          <- data_box       # tract outlines
      co_box          <- data_box       # county outline
      hs_box          <- data_box
      seer_box        <- data_box       # seer outline or data
      states_box      <- data_box       # state data
      regions_box     <- data_box       # region outlines
      #
      tr_box_c        <- data_box       # tract clipped to data
      Co_box_c        <- data_box       # county clipped to data
      hs_box_c        <- data_box       # HSA clipped to data
      sa_box_c        <- data_box       # seer clipped to data
      st_box_c        <- data_box       # states clipped to data
      rg_box_c        <- data_box       # regions clipped to data
      #
      ## FZ 10/08/2019 remove # in #if #}
      if (debug) {
         cat("plot size Z-4202 - data_box:", data_box, "\n")
         cat("regionB:",rPM$regionB," stateB:",rPM$stateB," seerB:",rPM$seerB,"  hsaB:",rPM$hsaB," countyB:",rPM$countyB," tractB:",rPM$tractB,"\n")
         cat("regionPList-len  :", length(MV$regionPList), "  list:", MV$regionPList, "\n")
         cat("statePList-len   :", length(MV$statePList),  "  list:", MV$statePList,  "\n")
         cat("seerPList-len    :", length(MV$seerPList),   "  list:", MV$seerPList,   "\n")
         cat("hsaPList-len     :", length(MV$hsaPList),    "  list:", MV$hsaPList,    "\n")
         cat("countyPList-len  :", length(MV$countyPList), "  list:", MV$countyPList, "\n")
         cat("tractPList-len   :", length(MV$tractPList),  "  list:", MV$tractPList,  "\n")
         cat("rg_proj_sel-len  :", length(MV$rg_proj_sel), "\n")
         cat("states_proj-len  :", length(MV$states_proj), "\n")
         cat("SeerRegs_proj-len:", length(MV$SeerRegs_proj), "\n")
         cat("hs99_proj-len    :", length(MV$hs99_proj), "\n")
         cat("co99_proj-len    :", length(MV$co99_proj), "\n")
         cat("tr99_proj-len    :", length(MV$tr99_proj), "\n")
         
      } 

      #  regionPList and regions_Proj
      rg_proj_sel     <- MV$regions_proj
      regionPList     <- MV$regionPList
      rgGO            <- FALSE
      
      #cat("Regional - listData and PList...  regionPList:",regionPList,"\n")
      
      if (!is.null(regionPList)) {
         # if list is present and no NAs included
         if (!any(is.na(regionPList))) {
            # good list
            #  Extra regions boundary layer - does not contain NA
            rg_proj_sel    <- rg_proj_sel[regionPList,]       # get selected regions borders to plot
            regions_box    <- bbox(rg_proj_sel)              # get box space for regions boundaries
            rgGO           <- TRUE
            rg_box_c       <- bbox(rg_proj_sel[unique(dataMapDF$rgID),])
         } else {
            # does contain NA.
            # problem
            xmsg <- paste0("***382 Internal Error - regionPList contains a NA in list:",
                               paste0(regionPList,collapse=", "))
            stop (xmsg,call.=FALSE)
            regionPList    <- NULL
         }
      } else {
         #cat("regionPList ( Z-4237 ) is NULL.\n")
         ErrFnd       <- FALSE
         rg_proj_sel  <- NULL
         # if none - don't change data box used at this level.
      }
      MV$rg_proj_sel  <- rg_proj_sel
      MV$regionPList  <- regionPList
      MV$rgGO         <- rgGO
      
      #

      #  statePList and states_proj
      st_proj_sel     <- MV$states_proj
      statePList      <- MV$statePList
      stGO            <- FALSE
      
      #cat("States - ListData and PList... statePList:",statePList,"\n")
      
      if (!is.null(statePList)) {
         # if list is present and no NAs included
         if (!any(is.na(statePList))) {
            # good list
            #  Extra State boundary layer - does not contain NA
            st_proj_sel    <- st_proj_sel[statePList,]       # get selected states borders to plot
            states_box     <- bbox(st_proj_sel)              # get box space for state boundaries
            stGO           <- TRUE
            #xID            <- unique(dataMapDF$stID)
            #st_box_c       <- bbox(st_proj_sel[unique(dataMapDF$stID),])
         } else {
            # does contain NA.
            # problem
            xmsg <- paste0("***384 Internal Error - statePList contains a NA in list:",paste0(statePList,collapse=", "))
            stop (xmsg,call.=FALSE)
            statePList    <- NULL
         }
      } else {
         #cat("statePList  ( Z-4273 ) is NULL.\n")
         ErrFnd       <- FALSE
         st_proj_sel  <- NULL
         # if none - don't change data box used at this level.
      }
      MV$st_proj_sel  <- st_proj_sel
      MV$statePList   <- statePList
      MV$stGO         <- stGO
      #
      
      #  seerPList  and SeerRegs_Proj
      sa_proj_sel     <- MV$SeerRegs_proj
      seerPList       <- MV$seerPList
      saGO            <- FALSE
      
      #cat("Create SA - ListData and PList...  seerPList:",seerPList,"\n")
      
      if (!is.null(seerPList)) {
         # is not null
         if (!any(is.na(seerPList))) {
            # good list
            #  seerP list present - BBOX should cover all Seer area
            sa_proj_sel    <- sa_proj_sel[seerPList,]      # selected Seer area Boundaries
            #cat("length of sa_proj_sel:",length(sa_proj_sel),"\n")
            
            seer_box       <- bbox(sa_proj_sel)            # get box space for seer boundaries for area ploted.
            saGO           <- TRUE
            #saDList         <- unique(dataMapDF$saID)       # get stIDs referenced by data.
            #cat("saDList Z-4308 :",saDList,"\n")
            
            #xm             <- match(MV$SeerRegs_data$stID,stsaList)
            #xmGood         <- !is.na(xm)
            #saDList        <- MV$SeerRegs_data[xmGood,"saID"]
            #cat("length saDList:",length(saDList),"  saDList:",saDList,"\n")  # we picked up more seer regs 
                       # the data has..  So, not in sa_proj_sel.
            
            #sa_box_c       <- bbox(sa_proj_sel[saDList,])
            #cat("sa_box_c:",sa_box_c,"\n")
            
         } else {
            # problem
            xmsg <- paste0("***386 Internal Error - seerPList contains a NA in list:",paste0(seerPList,collapse=", "))
            stop(xmsg,call.=FALSE)
            seerPList      <- NULL
         }
      } else {
         #cat("seerPList   ( Z-4319 ) is NULL.\n")
         ErrFnd       <- FALSE
         sa_proj_sel  <- NULL
      }
      MV$sa_proj_sel  <- sa_proj_sel
      MV$seerPList    <- seerPList
      MV$saGO         <- saGO
      #
            
      #
      #  hsaPList and hs99_proj  (if no hsProj, the hsaPList should be NULL or NA.
      #
      hs_proj_sel     <- MV$hs99_proj
      hsaPList        <- MV$hsaPList
      hsGO            <- FALSE

      #cat("Creating hsa proj_sel and PList... hsaPList:",hsaPList,"\n")

      if (!is.null(hsaPList)) {
         # not null
         if (!any(is.na(hsaPList))) {
            # good list
            #  hsa P List provided
            hs_proj_sel   <- hs_proj_sel[hsaPList,]  # get county boundries requested
            hs_box        <- bbox(hs_proj_sel)           #  get box space for counties
            hsGO          <- TRUE
            #hs_box_c       <- bbox(hs_proj_sel[unique(dataMapDF$HSAID),])
            #cat("hsBox Z-4353 : ",hs_box,"\n")
         } else {
            # problem
            xmsg          <- paste0("***389 Internal Error - hsaPList contains a NA in list:",
                                        paste0(hsaPList,collapse=", "))
            stop(xmsg,call.=FALSE)
            hsaPList   <- NULL
         }
      } else {
         #cat("hsaPList ( Z-4362 ) is NULL.\n")
         ErrFnd       <- FALSE
         hs_proj_sel  <- NULL
      }
      MV$hs_proj_sel  <- hs_proj_sel
      MV$hsaPList     <- hsaPList
      MV$hsGO         <- hsGO
      #

      #
      #  countyPList and co99_proj  (if no coProj, the countyPList should be NULL or NA.
      #
      co_proj_sel     <- MV$co99_proj
      countyPList     <- MV$countyPList
      coGO            <- FALSE

      #cat("Creating county proj_sel and PList... countyPList:",countyPList,"\n")      

      if (!is.null(countyPList)) {
         # not null
         if (!any(is.na(countyPList))) {
            # good list
            #  county P List provided
            co_proj_sel   <- co_proj_sel[countyPList,]  # get county boundries requested
            co_box        <- bbox(co_proj_sel)           #  get box space for counties
            coGO          <- TRUE
            #cat("coBox Z-4381 : ",co_box,"\n")
         } else {
            # problem
            xmsg          <- paste0("***387 Internal Error - countyPList contains a NA in list:",
                                        paste0(countyPList,collapse=", "))
            stop(xmsg,call.=FALSE)
            countyPList   <- NULL
         }
      } else {
         #cat("countyPList ( Z-4397 ) is NULL.\n")
         ErrFnd       <- FALSE
         co_proj_sel  <- NULL
      }
      MV$co_proj_sel  <- co_proj_sel
      MV$countyPList  <- countyPList
      MV$coGO         <- coGO
      #

      #  tractPList and tr99_proj  (if no trProj, the trPList should be NULL or NA.)
      tr_proj_sel     <- MV$tr99_proj
      tractPList      <- MV$tractPList
      trGO            <- FALSE

      #cat("Creating tract proj_sel and PList... tractPList:",tractPList,"\n")      
     
      if (!is.null(tractPList)) {
         # not null
         if (!any(is.na(tractPList))) {
            # good list
            #  tract P List provided
            tr_proj_sel   <- tr_proj_sel[tractPList,]    # get county boundries requested
            tr_box        <- bbox(tr_proj_sel)           #  get box space for counties
            trGO          <- TRUE
            #cat("trBox Z-4414 : ",tr_box,"\n")
         } else {
            # problem
            xmsg          <- paste0("***388 Internal Error - tractPList contains a NA in list:",
                                        paste0(tractPList,collapse=", "))
            stop(xmsg,call.=FALSE)
            tractPList    <- NULL
         }
      } else {
         #cat("tractPList  ( Z-4423 ) is null.\n")
         ErrFnd       <- FALSE
         tr_proj_sel  <- NULL
      }
      MV$tr_proj_sel  <- tr_proj_sel
      MV$tractPList   <- tractPList
      MV$trGO         <- trGO

      #
      #  clipTo - controls the spatial box to be used for the grpahics.
      #
      xwl    <- c( NA, NA)   # x limits
      ywl    <- c( NA, NA)   # y limits
      
      if (debug) {
           cat(" Z-4445 \n")
           cat("data_box   :",data_box,"\n")
           cat("regions_box:",regions_box,"\n")
           cat("states_box :",states_box,"\n")
           cat("seer_box   :",seer_box,"\n")
           cat("hs_box     :",hs_box,"\n")
           cat("co_box     :",co_box,"\n")
           cat("tr_box     :",tr_box,"\n")
           #cat("rg_box_c   :",rg_box_c,"\n")
           #cat("st_box_c   :",st_box_c,"\n")
           #cat("sa_box_c   :",sa_box_c,"\n")
           #cat("hs_box_c   :",hs_box_c,"\n")
           #cat("co_box_c   :",co_box_c,"\n")
           #cat("tr_box_c   :",tr_box_c,"\n")
      }
      
      rPM$clipRes   <- switch(rPM$clipTo,
                        #          d d r r s s a a h h c c t t     # d=data, r=region, s=state, a=registry, h=hsa, c=county, t=tract
                        NONE   = c(T,T,T,T,T,T,T,T,T,T,T,T,T,T),   # no clipping use all boxes get biggest range
                        DATA   = c(T,T,F,F,F,F,F,F,F,F,F,F,F,F),   # clip to data box
                        COUNTY = c(T,T,F,F,F,F,F,F,F,F,T,T,T,T),   # clip to county and tract box.
                        HSA    = c(T,T,F,F,F,F,F,F,T,T,T,T,T,T),   # clip to hsa, county, tract
                        SEER   = c(T,T,F,F,F,F,T,T,T,T,T,T,T,T),   # clip to Seer, hsa, county, tract
                        STATE  = c(T,T,F,F,T,T,T,T,T,T,T,T,T,T),   # clip to state, seer, hsa, county, tract.
                     NULL
                   )
                   
      #cat("rPM$clipRes Z-4472 :\n")
      #print(rPM$clipRes)
      # SET UP FOR clipping - biggest spatial area.
      xwl       <- c(data_box[1,],regions_box[1,],states_box[1,],seer_box[1,],hs_box[1,],co_box[1,],tr_box[1,])
      ywl       <- c(data_box[2,],regions_box[2,],states_box[2,],seer_box[2,],hs_box[2,],co_box[2,],tr_box[2,])

      
      if (rPM$clipToNum > 0) {
         #  clipping - collect clipped boxes..
      
         #xwl       <- c(data_box[1,],rg_box[1,],st_box[1,],
         #                   sa_box[1,],hs_box[1,],co_box[1,],tr_box[1,])
         #ywl       <- c(data_box[2,],rg_box[2,],st_box[2,],
         #                   sa_box[2,],hs_box[2,],co_box[2,],tr_box[2,])
         #cat("before clip edit\n")
         #cat("xwl:",xwl,"\n")
         #cat("ywl:",ywl,"\n")
     
         xwl       <- xwl[rPM$clipRes]   # select the clip to set
         ywl       <- ywl[rPM$clipRes]
      
         #cat("after clip edit\n")
         #cat("xwl:",xwl,"\n")
         #cat("ywl:",ywl,"\n")
      
      } else {
      
         # no clipping - biggest spatial area.
         xwl       <- c(data_box[1,],regions_box[1,],states_box[1,],seer_box[1,],hs_box[1,],co_box[1,],tr_box[1,])
         ywl       <- c(data_box[2,],regions_box[2,],states_box[2,],seer_box[2,],hs_box[2,],co_box[2,],tr_box[2,])

      }
      
      xlPlot    <- range(xwl)
      ylPlot    <- range(ywl)
      
      xyBox     <- matrix(c(xlPlot,ylPlot), ncol=2, byrow=TRUE)
      colnames(xyBox)   <- c("min","max")
      row.names(xyBox)  <- c("x","y")
      MV$xyBox  <- xyBox
      
      MV$xlPlot <- xlPlot
      MV$ylPlot <- ylPlot
      
      #
      #   xx_proj_sel built
      #   xlPlot and ylPlot of drawning limits known.
      #

      if (debug) {
         cat("limits Z-4515 x:",xlPlot,"  y:",ylPlot,"\n")
      }
     
      ###
      #
      #   Need to find out more about this data
      #
      #   If loadDetails -> subL_proj has all of the df data (subL_data) with information on seer area.
      #   Could be dealing with states_data, SeerRegs_data, or subL_data...
      #
      #  Any stID with saID = "" -> indicates data outside of Seer Area in state.
      #      trigger for fill to state override to fill to seer area.
      #
      ####

      AspRatio <- (xlPlot[2] - xlPlot[1]) / (ylPlot[2] - ylPlot[1])

      #print (paste0("Plot Aspect Ratio is Z-4532 ",AspRatio))

      #windows(width = 7, height = 7 * (1/AspRatio), xpinch=72) # doesn't work yet.
      
      rPM$AspRatio <- AspRatio
      #cat("AspRatio:",AspRatio,"\n")
      
      #  These x and y plot limits are used for all plots.
      #
      ####

      #
      # end of data Location ID validation and row removal.
      #
      #####
      
      #cat("Exiting SM_box_sel-\n")
      
      return(list(rPM=rPM, MV=MV))
      
   }
   #   End of SM_box_sel
   #
   #####
    

   #####
   #
   #  SM_Categ - validate dataCol and set the WrkSPDF@data$Cat and WrkSPDF@data$col 
   #        working columns for the mapping
   #
   #   When done, dataMapDF$cat has the category index and $col has the color to fill the area
   #      categMode = 1 -> calculated breakpoints based on the number of categories 
   #                     and range of data values.
   #      categMode = 2 -> categories assigned based on caller provided breakpoints.
   #      categMode = 3 -> caller provides category indexes (1 to "N"), package assigns colors.
   #      categMode = 4 -> caller provides colors, no category indexes, builds one based 
   #                     on sorted colors.
   #
   #   For categMode = 1 & 2, the breakpoints are calculated and rounded based on brkPtDigits to provide better
   #   presentation in the legend.  One key to this process is to do the rounding and then take
   #   the rounded numbers and use for the breakpoints.
   #
   #   In all cases, NA data should be presented at "white" areas.
   #
   
SM_Categ <- function(rPM) {
   
      ###
      # 
      #  local functions
      #
   
      ###
      #
      #  FindDigits - Find the number of digits (right of decimal point) in number.
      #
      FindDigits <- function (x) {
         dig = 0
         for (ind in c(nchar(x):1)) {
           if (substr(x,ind,ind) != "0") {
              dig = ind
              break
           }
         }
         if (dig > 0) {
            for (jnd in c((ind-1):1)) {
              if (substr(x,jnd,jnd) == ".") {
                dig = dig - jnd
                break
              }
            }
         }
         return(dig)
      }   # end of FindDigits
   
      # Round the set of break point numbers and keep ends neat.
      
      #  end of FindDigits function 
      #
      ##
   
      ###
      #
      #  RateRound - 
      #
      RateRound <- function(AllRateC, wInterv, wIntervDigits) {
   
         #
         #  Floor and Ceiling return whole integers that are not really useful.
         #  However, if the numbers are multipled by the number of digits we want,
         #  then the floor and ceiling functions are applied.  The results can
         #  then be divided by the multipler and become useful numbers.
         #
         #  To ensure the lowest number is below the range of values, the lowest value is floor'd.
         #  To ensuer the highest number is above the range of values, the highest value is ceiling'd.
         #  All number in the middle are rounded to keep their relative value.
         #  Since the low and high values are modified down and up respectfully, none of the
         #  values have to be shifted by any value.
         #
         #  The multiplier is 10^n, where "n" is the number of digits you want to have in
         #  the resulting values.   If n=3, then 0.34534 becomes 0.345, and 124.2345 becomes 124.234 or 124.235
         #  depending on whether it is below or above the mean of the range.
         #  The default number of digits is 2.
         #
         #  The routine must work with -Inf, Inf, positive and negative numbers as the lowest and highest values.
         #
         
         hh            <- length(AllRateC)
         RMul          <- 10^ wIntervDigits
         
         #print(AllRateC)
         
         AllRateC2     <- AllRateC * RMul
         
         #print(AllRateC2)
         
         AllRateC1     <- AllRateC2
         
         AllRateC1[1]  <- floor(AllRateC1[1])        # lowest value -> floor()
         AllRateC1[hh] <- ceiling(AllRateC1[hh])     # highest value -> ceiling()
         cR            <- seq(1,hh)[-c(1,hh)]        # indexes to the middle values
         AllRateC1[cR] <- round(AllRateC1[cR],0)     # round(,0)
         
         #AllRateC1[AllRateC1<ch]  <- floor(AllRateC1[AllRateC1<ch]  ) #   - 0.499999)
         #AllRateC1[AllRateC1>ch]  <- ceiling(AllRateC1[AllRateC1>ch]) #   + 0.499999)
         
         #print(AllRateC1)
         
         AllRateR <- AllRateC1/RMul
         #print(AllRateR)
         
         return(AllRateR)
      }
      #
      #   end of RateRound function
      #
      ##
   
      ###
      #
      #  RateLabel - 
      #
      RateLabel <- function(AllRateC,wIntervDigits) {
         wD <- wIntervDigits
         OpenFrame = "["
         np = length(AllRateC)
         wCat = rep("",np)
         
         for (ind in c(2:np)) {
            wCat[ind] = paste0(OpenFrame,formatC(AllRateC[ind-1],format='f',digits=wD),',',formatC(AllRateC[ind],format='f',digits=wD),']')
            OpenFrame = '('
         }
         # make sure the cut uses "include.lowest"
         return(wCat)
      }
      #
      #  end of RateLabel function
      #
      ###
   
      ###
      #
      #  RateQuan - 
      #
      RateQuan <- function(brkpt, data) {
         #  generate the quantile list for the break points for Rate data.
         wRange <- range(data) #,na.rm=TRUE)     # get range and remove any NA
         wQ     <- quantile(data,probs=brkpt) #,na.rm=T)
         #wQ    <- c(wQ,Inf)    # may not be needed if last break point is the maximum.
         #wQ[1] <- -Inf         # change "0%" value to -Inf
         wQ[1]  <- wRange[1]    # set to minimum
         return(wQ)
      }
      #
      #  end of RateQuan function
      #
      ##
   
      ###
      #
      #  RateCutAdj - adjusts the caller supplied break points to rounded values for better legend labels
      #        and reflect the rounding labels back into the breakpoint list.
      #
      RateCutAdj <- function(brkpt)  {
   
         #
         # review break point list and adjust when duplicate values are found.
         # also create a category label list for use later with "No Values" if needed.
         #
         #cat("RateCutAdj2 Function\n")
         
         bp = brkpt   # get working copy
         
         np   <- length(bp)
         wCat <- rep("",np)
         modp <- FALSE
         #cat("length bp:",np,"  bp:",paste0(bp,collapse=", "),"\n")
         
         # verify the adjustment interval is small enough to not interfer with neighbor values.
         #
         xp0 <- round( diff(bp), digits = 10)    # have to do rounding to handle floating point problem
                                # 0 = 1.77636e-15
         xp <- unique(sort(xp0))                 # sort the difference - intervals
         #cat("sorted (intervals) xp:",paste0(xp,collapse=", "),"\n")
         
         xp <- xp[xp>0]                          #  should not be able to have - internval
         mininterval <- xp[1]/5                  #  1/5 of the smallest difference
         
         Interv <<- 0.01
         #cat("mininterval:",mininterval,"  Interv:",Interv,"\n")
         
         if (Interv > mininterval) Interv <<- mininterval  # needs to be smaller.
         
         #   where is the first non-zero digit in the decimal part of the number?
         wVal = Interv
         for (ind in c(0:9)) {
            if (wVal >= 1) break
            wVal = wVal * 10   # shift left 1 digit
         }
         IntervDigits <<-  ind     # 0=had value => 1 to start,  1=0.x  2=0.0x, etc.
         
         #cat("IntervDigits:",ind,"\n")
         #
         #  Adjusts the cut point to move value and eliminate any duplicates.
         #  The min and max values are preserved.  Any internal duplicates are
         #  adjusted, but the center of the group is maintained.
         #
         #  build literal category list for legend
         #
         
         ##  process lower duplicates and
         NextValue = bp[1]                      # first break point
         jndList   = seq(2,np,by = 1)
         
         for (jnd  in jndList) {
            NextValue = NextValue + Interv      # next point must be "interv" away, if not move it higher.
            if (NextValue > bp[jnd] )  {
               bp[jnd] <- NextValue
               modp <- TRUE
            }
            if (NextValue < bp[jnd] )  break    # if next point > "interv" away, done.
         }
         
         ## This also ensures anything pushed up into another cut point, move it up.
         
         ##  process high end duplicates. adjust down and check for dropping into other cut point.
         
         indList   = seq(np-1, 1, by = -1)
         NextValue = bp[np]   # maximum value   # now check from top value downward.
         
         for (ind in indList) {
            NextValue = NextValue - Interv
            if (NextValue >= bp[ind])  break    # caught up.. high end done.
            if (NextValue <  bp[ind]) {
               bp[ind] = NextValue              # lower breakpoint if not "interv" away from upper number
               modp <- TRUE
            }
         }
         
         ##  high and low cleaned up.  Now look for internal duplications.
         
         numData <- table(bp)[table(bp)>1] - 1
         np2 <- length(numData)
         
         if (np2 > 0) {  # we have more work to do.
            dupData   <- as.numeric(names(numData))
            startData <- match(dupData, bp)   # find starting index of each group
            #catData  <- match(bp, dupData)     # assign group number to each cut.
            for (ind in c(1:np2)) {
               sD    <- startData[ind]
               nD    <- numData[ind]
               vD    <- bp[startData[ind]] # get original duplicate value.
               vFlag <- TRUE
               aD    <- seq((nD/2 * -Interv), (nD/2 * Interv), by=Interv)
               for (jnd in c(0:nD)) {
                  bp[sD+jnd] <- bp[sD+jnd] + aD[jnd+1]
                  modp       <- TRUE
               }
            }
         }
         bpList <- list(before=brkpt, after=bp, Intv = Interv, IntvD = IntervDigits, minIntv = mininterval)
         
         #print(bpList)
         
         #  return data.frame with adjusted break point list and category labels.
         return(bpList)
      }
      #
      #   end of RateCutAdj
      #
      #####

      #####
      #
      #  Main SM_Categ  function 
      #
      
      #  local Variable 
      
      debug       <- rPM$debug
      debugFlag   <- rPM$debugFlag

      numberTestRegExpr <- rPM$numberTestRegExpr
      
      dataMapDF   <- rPM$dataMapDF
      dataListData<- rPM$dataListData
      categMode   <- rPM$categMode
      categ       <- rPM$categ
      wCateg      <- rPM$wCateg
      CatNumb     <- rPM$CatNumb
      brkPtDigits <- rPM$brkPtDigits
      
      #cat("SM_Categ - categ:",categ,"  wCateg:",wCateg,"  categMode:",categMode,"\n")
      
      idMode      <- rPM$idMode
        
      mapData     <- dataMapDF$data
      
      #####  Based on categMode
      #
      #  Step C.1 - validate data in dataCol is numeric or color (categ <> "COLORS") (060-069)
      #
      #  The data can be rates, categories, or colors based on the categ parameter.
      #    categMode = 1 or 2 => rates - real numeric values
      #    categMode = 3      => categories - integers from x to y with a maximum range of "n"
      #    categMode = 4      => colors 
      #

      #  Handle factor if present
      
      if (is.factor(mapData)) {
         #cat("Converting factor to character - mapData.\n")
         mapData    <- as.character(mapData)
      }
      
      #  find NA values in data, notify caller and remove.

      naList           <- is.na(mapData) # find NA values in list (not numeric - no conversion).
    
      #  if any NAs tell caller and remove NA from data table.  They are acceptable.
      #    applicable to any set of data
      if (any(naList)) {
         #  at least one data contains NA value - warning
         ErrFnd        <- TRUE
         BadList       <- dataMapDF[naList,"ID"]
         xmsg          <- paste0("***060 The following data rows in column ",rPM$dataCol,
                                       " of the ",rPM$ndrName,
                                       " data.frame contains missing (NA) values and will not be mapped. Location IDs: ",
                                       paste0(BadList,collapse=", "))
         warning(xmsg, call.=FALSE)
 
         #  Update dataMapDF, mapData, and dataListData  - remove NA locations.
         dataMapDF     <- dataMapDF[!naList,]   # remove rows.
         rPM$dataListData <- dataMapDF$ID       # update dataListData and mapData.
         mapData       <- dataMapDF$data
      }

      #
      #  Validate based on categMode
      #    factor and NAs handled.

      if (categMode == 4) {

         # categMode = 4 = verify and handle colors  (COLORS)
         #cat("Handling categMode=4\n")

         mapData   <- stringr::str_trim(as.character(mapData))
         #cat("mapData:",mapData,"\n")

         iC        <- unique(mapData)
         lenIC     <- length(iC)
         #cat("len-iC:",lenIC," iC:",iC,"\n")
         
         iR <- is.Color(mapData)    # get a vector of the tests of all data values as colors
         if (any(!iR)) {
            # at least one invalid color name or value in column.
            badList        <- unique(mapData[!iR])    # get list of bad values
            ErrFnd         <- TRUE
            xmsg           <- paste0("***065 Data in column ",rPM$dataCol," in ",rPM$ndfName,
                                  " data.frame contains one or more values that are not a color and will be ignored.")
            warning(xmsg, call.=FALSE)
            xmsg           <- paste0("***066   Bad Color value(s):",paste0(badList,collapse=", "))
            warning(xmsg, call.=FALSE)
            mapData[!iR]   <- "white"    # get bad color to "white"
         }

         colorList           <- unique(mapData)
         colorListGood       <- colorList != "white"
         colorList           <- colorList[colorListGood]
         
         NumCList            <- length(colorList)
         if (NumCList > 11) {
            # too many colors being used.
            xmsg         <- paste0("***067 The dataCol (",rPM$dataCol,") contains ",NumCList,
                                     " colors.  It is recommended to limit the number of colors to 11.")
            warning(xmsg, call.=FALSE)
         }
         CatNumb <- NumCList   # save number of categories being used.
         rPM$CB_Rate_Mid2  <- colorList
         #
         #  NOTE: legend will only include colors in symbols - caller needs to provide Labels option to have colors identified.
         #
         
      } else {
      
         # categMode = 1,2,3 - data must be numeric.
      
         #  
         #  VALIDATE dataCol contents as numeric values for categorizing.
         #     categMode = 1 or 2  - Real numbers (- or +)
         #     categMode =  3      - integer values (range =< 10) can be offset + or -
         #
      
         #
         #  add logic to check and enforce limit on the numbre of colors because of the legend.
         #    if legend not being drawn - how cares.
         #
         # factors are converted above.
         #
         # value should be a numeric value - as numeric or character
         
         if (!is.numeric(mapData)) {
    
            # column is not numeric - check for character image of numbers
            if (is.character(mapData)) {
               # it's character - but is it numeric
               
               #cat("processing dataCol - mapData:",paste0(mapData,collapse=", "),"\n")
               #
               #  Initial NA values are OK, just missing data - already handled and removed.  
               #  After this point - any NA  are caused by the conversion to numeric.
               #
               #  Pattern check include integer, real numbers with decimal factions,
               #    and scientific notation.  The number may have commas in the whole number
               #    parts.  These are removed before conversion to numeric.
               #
    
               mapData          <- stringr::str_trim(mapData)       # trim leading and trailing spaces/blanks/tabs/etc.
    
               #goodData        <- unlist(gregexpr(numberTestRegExpr,mapData))      # verify
                     #
                     #  1 = Good (TRUE),   -1 = Bad format (FALSE),  NA = was NA start.
                     #
               
               #cat("goodData:",paste0(goodData,collapse=", "),"\n")
    
               #  make up good number list based on pattern check
               #goodList     <- goodData > 0   # any value > 0 is good value. = TRUE

               # remove possible ","   
               mapData       <- gsub(",","",mapData)  # eliminate commas in number

               # convert to numeric.
               suppressWarnings( mapData  <- as.numeric(mapData) )   # convert to numeric.

               # the values are now numeric or NA if conversion failed.
               mapDataNAs    <- is.na(mapData)
               if (any(mapDataNAs)) {
                  # more problems - some values could not comvert.
                  ErrFnd     <- TRUE
                  BadList    <- dataMapDF$ID[mapDataNAs]
                  xmsg       <- paste0("***061 Some of the data values in the ",rPM$dataCol," column in the ",rPM$ndfName,
                                      " data.frame are not numeric values. Sub-areas will not be mapped. Location IDs:",
                                      paste0(BadList,collapse=", "))
                  warning(xmsg, call.=FALSE)
                  # remove bad data (can't convert to numbers)                  
                  dataMapDF  <- dataMapDF[!mapDataNAs,]   # remove bad data
                  rPM$dataListData <- dataMapDF$ID        # update dataListData and mapData
                  mapData    <- dataMapDF$data
               }
               #cat("mapData after numeric conversion:",paste0(mapData,collapse=", "),"\n")
       
               # end of processing characters
            } else {
               # it is not numeric or character..
               ErrFnd  <- TRUE
               xmsg  <- paste0("***064 The data column ",rPM$dataCol," in ",rPM$ndfName,
                                " data.frame is not numeric or character type numeric data. Processing terminated.")
               stop(xmsg)
            }
         } else {
            # data column is numeric, passes the test..  NAs were handled earlier.
            ErrFnd <- FALSE
         }
         # have numeric values in mapData - or - we stopped.
          
      } #  End of dataCol Format check
        
      #cat("mapData:",mapData,"\n")
      
      #  put data back into the dataMapDF   and save in rPM 
      
      dataMapDF$data   <- mapData      # save the processed data column back into the data.frame.
      rPM$dataMapDF    <- dataMapDF
      rPM$dataListData <- dataListData
      
      #
      #  End of dataCol column ($data) value checking.  We have numbers for categMode = 1,2,3  (4 bypassees this section.)
      #
      #####
      
      #####
      #
      #  Step C.2 - Perform the calculations and apply the categorization/classification (170-179)
      #             (only for categMode == 1 & 2, single value and breakpoint list)
      #

      #cat("User provided categ parameter Z-5047 :\n")
      #cat("   ",categ,"\n")
      ErrFnd    <- FALSE
      CatRange  <- range(mapData) #,na.rm=T)    # data or category numbers
     
      if (debug) {
         cat("Calculating 'categ' and generate values. Z-5044  categ:",categ,"\n")
      }
      #cat("CatNumb:",CatNumb,"  categ:",categ,"\n")
      
      R.Cat           <- categ
      CatBase         <- 1
      catMaxNum       <- rPM$palColorsMaxNum -1 

      #
      #  Step C.2.1 - preparation for breakpoint categories
      #     Get AllRateQ setup.
      #

      #
      #  Setup for data as category value
      #
      if (categMode == 3) {    

         # 3 - categories  (DATA)
         #cat("Categorizing = 3 - category data: ",R.Cat,"\n")

         # dataCol is category value (know they are numeric, but integer?)
         #cat("Using data column as categories...\n")
         CatRange   <- range(mapData)
         CatBase    <- CatRange[1]             # get low end base value to use for offset to colors (base value equivalent to 1
         CatDiff    <- diff(CatRange) + 1

         if (all(as.integer(mapData) == mapData)) {  #  categories must be integers (compare an integer vector to the one provided.
            #cat("Z-5072 Categories - CatRange:",CatRange,"  CatBase:",CatBase," diff:",diff(CatRange),"  catMaxNum:",catMaxNum,"\n")
            
            if (CatDiff > catMaxNum) {
               # error too many categories
               xmsg     <- paste0("***267 The ",rPM$dataCol," column data are integer category values.  ",
                                      "The range of the values is greater than the maximum of ",
                                      catMaxNum,". Reduce number of categories or select different palColors value.")
               stop(xmsg, call.=FALSE)
            }   
         } else {
            xmsg <- paste0("***270 The categ call parameter is 'DATA'. The ",rPM$dataCol,
                              " column does not appear to contain integer values.")
            stop(xmsg, call.=FALSE)
         }

         AllRateCutR     <- NULL
         AllRateCatR     <- formatC(seq(CatRange[1],CatRange[2],by=1),format="f",digits=0)   
                    # only category # is presented, caller must use labels option to provide better labels in legend.
      	 CatNumb         <- CatDiff

         # get colors based on CatNumb
         CB_Rate_Mid     <- rev(RColorBrewer::brewer.pal(CatNumb,rPM$palColors)) 
         rPM$CB_Rate_Mid <- CB_Rate_Mid

         dataMapDF$cat   <- dataMapDF$data - (CatBase - 1)       # get to range 1 to "n"
         dataMapDF$col   <- rPM$CB_Rate_Mid[as.integer(dataMapDF$cat)]

      } # end of categMode=3
     
      #
      #   Setup Breakpoint vector
      #
      if (categMode == 2) {    
      
         # 2 - breakpoint vector 
         # good vector - numeric - setup by the original validation up front.

         ###
         #
         #  Get catagorization parameters
         #
         #  c(-inf, .6, .8, 1, 1.2, 1.4, inf)
         #     >--1--]
         #           >-2-]
         #               >3-]
         #                  >-4-]
         #                      >--5-]
         #                           >--6-]
         #
         #  If vector is provided, either check to see if max above last
         #   value or always set inf and one more category.  5 values, yields
         #   6 groups, since values above last value provided.
         #   If last value >= max value -> only 5 groups.
         #
         #  If category number provided, then top value is the max value
         #   or equal to inf.  So, don't add top category.  Cat = 5, yields
         #   5 groups.  The bottom value is set to the minimum data value.
         #   If the "cut" function is called with "include.lowest", then the
         #   first category is "[x,y]". No adjustment is needed.
         #
         #  -Inf to Inf will only be used with the user provided cut points.
         #
         #  On user provided cut points, but vector must always be 5 values.
         #
         ###

         #cat("Categorizing = 2 - breakpoint vector : ",R.Cat,"\n")
         
         if (any(!is.numeric(R.Cat)))  {
            xmsg      <- paste0("***263 One or more values in the categ breakpoint list is not a number. The default of categ=5 will be used.")
            warning (xmsg, call.=FALSE)
            R.Cat     <- 5
            CatNumb   <- 5
            categMode <- 1   # change mode.
         } else {
            #  It's a number - 
            wRCat           <- sort(unique(R.Cat))
            if (length(categ) != length(wRCat)) {   #  error vector contains duplicates
               xmsg         <- paste0("***265 The categ call parameter contains a break point vector with duplicate values.",
                                  " The duplicate values will be removed.")
               warning(xmsg, call.=FALSE)

               R.Cat        <- wRCat            # get the sorted non-dup list.
            }
            lCat         <- length(wRCat)       # check length
            if (lCat < 3 || lCat > catMaxNum) {
               xmsg      <- paste0("***266 The number of points in the categ call parameter list is out of range. It must be between 3 and ",catMaxNum,".  The default of categ=5 will be used.")
               warning(xmsg, call.=FALSE)
               R.Cat     <- 5
               CatNumb   <- 5
               categMode <- 1       # change mode
            }
            categ <- R.Cat
            lCat  <- length(R.Cat)
            if (lCat > 1) {
               # still have a break point vector of 3 to "n" values.  (already validated)
      
               AllRateQOrig    <- R.Cat
               AllRateQ        <- c(-Inf, R.Cat, Inf)
               CatNumb         <- length(R.Cat) + 1    # number of brk points + 1
               #cat("AllRateQ:",AllRateQ,"\n")
            }
         }  # end of categMode = 2 setup 
         
              # AllRateCutR     <- AllRateQ
              # AllRateCatR     <- RateLabel(AllRateQ, formatC(AllRateCutR,format="f")
              # 
              # # get colors based on CatNumb
              # CB_Rate_Mid     <- rev(RColorBrewer::brewer.pal(CatNumb,rPM$palColors)) 
              # rPM$CB_Rate_Mid <- CB_Rate_Mid
              #
              # RTC             <- cut(mapData,breaks=AllRateCutR,labels=FALSE,include.lowest=TRUE)
              # dataMapDF$cat   <- RTC
              # dataMapDF$col   <- rPM$CB_Rate_Mid[as.integer(dataMapDF$cat)]

            #}
            #   if error or problem, the categMode may have changed to 1, so 1 must follow everything else.
         #}  # end of numeric check
      
      }  # End of categMode == 2            

      #
      #  Single value - calculate the breakpoints and rounding.
      #   categMode = 1
      #
      if (categMode == 1) {

         # at bottom in case any other mode sees an error and sets the default of categ=5
         #cat("Categorizing = 1 - number of categories to calculate : ",R.Cat,"\n")

         # 1 - single value - calculate breakpoints

         CatNumb        <- categ

         # build category tables for rate data
         xq <- seq(0,1,1/categ)   # percentage for each group

         #cat("xq:",paste0(xq,collapse=" "),"\n")

         #          for cat = 4  ==> (0, 25%, 50%, 75%, 100%)
         #                           min, a,  b,   c,   max   
         #          for cat = 5  ==> (0, 20%, 40%, 60%, 80%, 100%)
         #                           min, a,  b,   c,   d,   max  
         #
         #   --> no -inf or inf will be used, since the min and max includes all of the data.
         #   So, categ=5, uses 5 groups/colors.
         #
         #cat("dataMapDF$data:",paste0(dataMapDF$data,collapse=" "),"\n")

         AllRateQ      <- RateQuan(xq,mapData)   # the breakpoint list.
         AllRateQOrig  <- AllRateQ

         #cat("AllRateQ:",AllRateQ,"\n")

         rm(xq)       # clean up.

         #
         # calculate break points for next step
         #

         #  Adjust break point list and build labels
         #cat("Data Min and Max Values Z-5233 :",CatRange,"\n")

         #cat("\n")
         #cat("Rate Table Quantile Adjusted list used for cut break points :\n")
         #cat("  Calculated:",paste0(AllRateQ,collapse=" "),"\n")
       
       }   # done categMode == 1 setup
       #
       #
       if (categMode == 1 || categMode == 2) {
         # complete processing
         
         AllRateCut     <- RateCutAdj(AllRateQ)              # returns df
         Interv         <- as.numeric(AllRateCut$Intv)
         IntervDigits   <- as.integer(AllRateCut$IntvD)
       
         BrkPtLabs      <- sapply(AllRateCut$after, function(u) formatC(u,format="f",digits=8))
         BrkPtDigs      <- sapply(BrkPtLabs,FindDigits)
         BrkPtDigsMax   <- max(BrkPtDigs)
         BrkPtDigsRange <- range(BrkPtDigs)
       
         if (BrkPtDigsMax < IntervDigits)   IntervDigits = BrkPtDigsMax

         ###
         #
         if (debug) {
            cat("  Original Cal. List (AllRateQ):\n  ")
            cat(AllRateCut$before,"\n")
            cat("  Modified List ($after)       :\n  ")
            cat(AllRateCut$after,"\n")
            cat("  Interval Value ($Intv)       :",Interv,"\n")
            cat("  Interval Digits ($IntvD)     :",IntervDigits,"\n")
            cat("  Brk Pt Digits (parameter)    :",brkPtDigits,"\n")
            cat("  dim of dataMapDF             :",dim(dataMapDF),"\n")
            cat("\n")
         }
       
         #  Need test and handling for duplicate break points......

         # get rounded version of cut points
         RndDig          <- IntervDigits
         if (!is.null(brkPtDigits))  RndDig <- rPM$brkPtDigits

         AllRateCutR     <- RateRound(AllRateCut$after, Interv, RndDig)

         #cat("  Rounded Rate Data Break Points List (AllRateCutR) Z-5278 :\n")
         #cat("    ",AllRateCutR,"\n",sep="  ")
       
         # get labels for categories
         AllRateCatR     <- RateLabel(AllRateCutR,RndDig)

         # get colors based on CatNumb
         CB_Rate_Mid     <- rev(RColorBrewer::brewer.pal(CatNumb,rPM$palColors)) 
         rPM$CB_Rate_Mid <- CB_Rate_Mid

         RTC             <- cut(mapData,breaks=AllRateCutR,labels=FALSE,include.lowest=TRUE)
         dataMapDF$cat   <- RTC
         dataMapDF$col   <- rPM$CB_Rate_Mid[as.integer(dataMapDF$cat)]
     
      }  # End of 1 and 2

      #
      #  the data is the color
      #

      if (categMode == 4) {
         # 4 - Colors
         #cat("Categorizing = 4 - colors : ",R.Cat,"\n")
         # colors have been valided, but not counted.
     
         catColL         <- sort(unique(mapData))   # get list of unique colors sorted.  This is now our key.
         #cat("Z-5304 sorted cat color list:",catColL,"\n")

         rPM$CB_Rate_Mid2 <- catColL                # update the ColorB_Rate_Mid to the color set provided by user.
        
         CatRange        <- c(1,length(catColL))    # get index range for the colors.
        
         catColCatM      <- match(mapData,catColL)  # match to color in data to color table position -> index (cat)
        
         dataMapDF$cat   <- catColCatM              # based on match position, use it as the Category.
        
         AllRateCutR     <- NULL
         AllRateCatR     <- formatC(catColL,format="f",digits=0)
         
         #cat("Colors Cats:",AllRateCatR,"\n")
         # copy data to col
         dataMapDF$cat  <- match(mapData,catColL)
         dataMapDF$col  <- mapData                  # assign color from data.

      }
      
      #
      #  Now have AllRateCatR, AllRateCutR, and $cat has category value for categMode = 1, 2, 3 
      #

      # all category modes 1,2,3,4  - work from the $cat and $col values set at this point.

      # build table of number of entries per category
      CatCount        <- tabulate(dataMapDF$cat)

      np              <- length(AllRateCatR)
      AllRateCatRw    <- as.character(AllRateCatR[2:np])
      AllRateCatRwCnt <- paste0(AllRateCatRw," <",CatCount,">")

      AllRateCatRAdj  <- AllRateCatRw

      if (rPM$mLegend$lNoValue) {
         AllRateCatRAdj[CatCount == 0] <- paste0(AllRateCatRAdj[CatCount==0]," NV")
      }

      # categorize report
      if (debug) {
         cat("  Category Labels (AllRateCatR):\n")
         cat("    ",AllRateCatR,"\n")
         cat("  Category Adj Labels (AllRateCatRAdj):\n")
         cat("    ",AllRateCatRAdj,"\n")
         cat("  Category Labels with counts (AllRateCatRwCnt):\n")
         cat("    ",AllRateCatRwCnt,"\n")
         cat("\n")

         cat("Table of number of areas in each category:\n")
         cat(c(1:length(CatCount)),"\n")
         cat(CatCount,"\n")
         cat("\n")
         cat("dataMapDF$col variable- Z-5357 :\n",paste0(dataMapDF$col,collapse=", "),"\n")
         print(str(dataMapDF))
         print(dataMapDF[,c("ID","data","hData","h2Data","col","cat","hRes","h2Res")])     # categorize
      }

      rPM$dataMapDF  <- dataMapDF
      rPM$CatR       <- AllRateCatR
      rPM$CatRAdj    <- AllRateCatRAdj
      rPM$CatRwCnt   <- AllRateCatRwCnt
      #
      #  Classification and Color assignment Done
      #
      ######
     
      return(rPM)
      #
      ###
   }   
   #
   #    End of SM_Categ
   #
   #####
    

   ###
   #
   #  SM_Mapper - Creates the mapping and hatching of the maps
   #    Input required:  rPM - run parameters and variables
   #                     MV  - collection of boundary SPDFs and data level information.
   #                     data_data$col - contains the aub-area olor.
   #
         
   SM_Mapper <- function(rPM, MV) {
        
        debug   <- rPM$debug
        #cat("SM_Mapper Z-5401 startup.\n")
        
        dataMapDF <- rPM$dataMapDF       # get data and hatching controls
        
        #
        #  at initialization, make a few copies to help speed things along.
        #
        
        ####
        #
        #  Mapping Part 1   - Setup defaults
        #
        #
        # Mapping is done using the data_proj_sel and data_data_sel data.frames
        #    set by SM_box_sel.
        #
        #    All of the data areas and colors are contained in the data_data_sel
        #    Other spstial data frame are used for boundaries at 
        #    regional, state, seer, and county levels. 
        #
        ####
        
        ####
        #
        #  Data Mapping - data areas and overlays
        #
        #  Area borders must increase as you go up.
        #
        #                          Type of data
        #                          tracts   county   HSA     seer   state 
        #    Data Level          = 0.1      0.2      0.25    0.3    0.5   
        #    tract Overlay       = NA       NA       NA      NA     NA   
        #    County Overlay      = 0.2      NA       NA      NA     NA   
        #    Health SA   Overlay = 0.3      0.75     NA      NA     NA
        #    Seer Overlay        = 0.1      0.1      0.1     NA     NA   
        #    State Overlay       = 0.2      0.2      0.2     0.75   NA  
        #
        #    idMode    1=State, 2=County, 3=tract, 4=Seer, 5=Health Service Area 6=GROUPs feature
        #
   
        #
        # Plot Data Level for Areas  (State or County or Census Tract or Seer)  (Wrk_proj_df)
        #
   
        # default mapping variables.
   
   
        # line sizes based on the boundary level
        
        dataLwd    <- 0.75     # always the same
        
        # boundary line weight if not data level.
        tractLwd   <- 0.75
        countyLwd  <- 1.0
        hsaLwd     <- 1.33
        seerLwd    <- 1.66
        stateLwd   <- 2.0
        regionLwd  <- 2.0
   
        # default colors for boundaries - set in SM_GlobInit. 
        data_BCol        <- rPM$ColorB_Data
        Tract_BCol       <- rPM$ColorB_O_Tract
        County_BCol      <- rPM$ColorB_O_County
        HSA_BCol         <- rPM$ColorB_O_Hsa
        Seer_BCol        <- rPM$ColorB_O_Seer
        State_BCol       <- rPM$ColorB_O_State
        Region_BCol      <- rPM$ColorB_O_Region
  
        ##  make local some parameters from collected projections
   
        wData_proj       <- MV$data_proj_sel      # sub-areas to map based on Data and Boundary Options
        wData_data       <- MV$data_data_sel
        dataMapDF        <- rPM$dataMapDF
   
        wDataBCol        <- rPM$dataBCol          # data boundary color.
        wDataBCol_caller <- rPM$dataBCol_caller
   
        wIdMode          <- rPM$idMode
        
        wDataID          <- row.names(wData_proj)
        
        #cat("size wData_proj : ",length(wData_proj),"\n")
        #cat("size dataMapDF  : ",dim(dataMapDF),"\n")
        
   
        #cat("loading x and y limits\n")
        
        #   x and y limits for all plots
        vxLim            <- MV$xlPlot
        vyLim            <- MV$ylPlot
   	
        if (debug) {
           cat("Main plot - Z-5494 - Length wData_proj:", length(wData_proj),"  length cols:",length(dataMapDF$col),"\n")
           print("wData_data:")
           #print(wData_data)
           #  Boundary Plot Flags (xxGO) and xxxPList  keys to plot
           cat("rgGO",MV$rgGO," regionPList:",MV$regionPList,"\n")
           cat("stGO",MV$stGO,"  statePList:",MV$statePList, "\n")
           cat("saGO",MV$saGO,"   seerPList:",MV$seerPList,  "\n")
           cat("hsGO",MV$hsGO,"    hsaPList:",MV$hsaPList,   "\n")
           cat("coGO",MV$coGO," countyPList:",MV$countyPList,"\n")
           cat("trGO",MV$trGO,"  tractPList:",MV$tractPList, "\n")
       
           #cat("Class of data_proj    :",class(MV$data_proj),   "\n")
           #cat("Class of wData_proj   :",class(wData_proj),     "\n")
           #cat("Class of rg_proj_sel  :",class(MV$rg_proj_sel), "\n")
           #cat("Class of st_proj_sel  :",class(MV$st_proj_sel), "\n")
           #cat("Class of sa_proj_sel  :",class(MV$sa_proj_sel), "\n")
           #cat("Class of hs_proj_sel  :",class(MV$hs_proj_sel), "\n")
           #cat("Class of co_proj_sel  :",class(MV$co_proj_sel), "\n")
           #cat("Class of tr_proj_sel  :",class(MV$tr_proj_sel), "\n")
        }
   
        #   
        #  Step 2 = data mapping   
        #
        #  The basic map and data areas.  
        #
        #     1) Setup variables
        #     2) data areas - colored - no borders
        #     3) hatching of data areas (if requested) (two hatches)
        #     4) boundaries
        #         tract    if(tract data)
        #        county   if(county or tract data)
        #        hsa
        #        seer
        #        state
        #        region
        #
        
        #
        #  Start Mapping.
        #
        par(mar=c(3.1, 1.1, 3.1, 1.1))   # adjust margins to get more mapping space.
        #         B    L    T    R       #  Don't need space for Axis labels and ticks
         
        plot.new()                       # create new plot.
        #cat("xlim:",vxLim,"  ylim:",vyLim,"\n")
           
        #
        #    plot DATA area and colors layer - NO boundaries.
        #       Boundaries added later due to hatching.
        #
        #cat("Mapping colors layer. No border color.\n")
        #cat("wData_proj\n")
        #print(str(wData_proj))
        #cat("wData_data\n")
        #print(wData_data)
        
        par(new=T)
        plot(wData_proj,
              col    = wData_data$col,   # color of areas
              den    = NA,
              border = NA,
        #     border = rPM$dataBCol,           # color or area borders  (no borders)
        #     lwd    = dataLwd,                # was 0.01  line weight
        #     lty    = 1,                      # 0 to 6  1=solid
              xlim   = vxLim, ylim   = vyLim
            )
   
        IDList <- dataMapDF$ID      # get list of Loc ID from the map data DF.
        
        ##### Hatching Overlay #####  Hatch # 1 #####
   
        if (rPM$HatchFlag) {
           #
           # Plot Hatch#1 for data
           #
           Hk         <- rPM$hatch
           #  Get everything needed into Hk for hatch # 1
           Hk$ID      <- dataMapDF$ID
           Hk$hRes    <- dataMapDF$hRes     # T/F
           HIDList    <- Hk$ID[Hk$hRes]            # get list of areas to be hatched
       
           # Plot hatched overlay based on P_Value <= 0.05 (hatched)(defaults)
   
           if (debug) {
              cat("Hatching #1 requested Z-5565 -  ", rPM$ndfName, "\n")
              cat("hatch:\n")
              print(str(Hk))
           }
    
           #    hCol is the color used for the hatching - default = grey (0.66) a medium grey.
           #    hDen is density control the hatch.  If hDen=NA or 0 for no hatching.
           #    hLwd is line weight for the hatching (hatch and borders).
           #    there for hatching does not draw boundaries, border=NA is needed.
           
           # get list of sub-areas to be hatched - nothing else.  ($hRes==TRUE)
           xm         <- match(wDataID,HIDList)   # find polygons
           #print(xm)
           xmDo       <- !is.na(xm)           # xmDo = polygons to hatch.
           hData_proj <- wData_proj[xmDo,]    # list of sub projection areas to hatch
           
           par(new=TRUE)
           plot(hData_proj,                             # area sp
                    density = Hk$hDen,                  # Density - lines per inche.
                    col     = Hk$hCol,                  # hatch color when den!=NA,
                    lty     = 1,                        # Solid
                    border  = NA,                       # don't do borders.
                    lwd     = Hk$hLwd,                  # was 0.01     (effects border and hatching.)
                    angle   = Hk$hAngle,                # angle of hatch lines.
                    xlim    = vxLim, ylim    = vyLim
                 )
        }
        ##### End of Hatching Overlay # 1
        
        ##### Hatching Overlay #####  Hatch # 2 #####
   
        if (rPM$Hatch2Flag) {
           #
           # Plot Hatch #2 for data
           #
           Hk       <- rPM$hatch2
           #  Get everything needed into Hk
           Hk$ID    <- dataMapDF$ID
           Hk$hRes  <- dataMapDF$h2Res
           HIDList    <- Hk$ID[Hk$hRes]            # get list of areas to be hatched
       
           # Plot hatched overlay based on P_Value <= 0.05 (hatched)(defaults)
   
           if (debug) {
              cat("Hatching requested Z-5609 -  ", rPM$ndfName, "\n")
              cat("hatch2:\n")
              print(str(Hk))
           }
           
           #    hCol is the color used for the hatching - default = grey (0.66) a medium grey.
           #    hDen is density control the hatch.  If hDen=NA or 0 for no hatching.
           #    hLwd is line weight for the hatching (hatch and borders).
           #    there for hatching does not draw boundaries, border=NA is needed.
            
           # get list of sub-areas to be hatched - nothing else.  ($hRes==TRUE)
           xm         <- match(wDataID,HIDList)
           #print(xm)
           xmDo       <- !is.na(xm)
           hData_proj <- wData_proj[xmDo,]    # list of areas to hatch
           #hData_data <- wData_data[xmDo,]
           
           par(new=TRUE)
           plot(hData_proj,                             # area sp
                    density = Hk$hDen,                  # Density - lines per inche.
                    col     = Hk$hCol,                  # hatch color when den!=NA,
                    lty     = 1,                        # Solid
                    border  = NA,                       # don't do borders.
                    lwd     = Hk$hLwd,                  # was 0.01     (effects border and hatching.)
                    angle   = Hk$hAngle,                # angle of hatch lines.
                    xlim    = vxLim, ylim    = vyLim
                 )
        }
        ##### End of Hatching Overlay # 2 #####
 
        ##### Area Boundaries #####
        #
        #  Now do overlaying of higher level boundaries (County and State) as needed.
        #
        ##### Mapping Part 2 #####
        #
        #cat("Layers - trGO:",MV$trGO,"  coGO:",MV$coGO,"  hsGO",MV$hsGO,"  saGO:",MV$saGO,"  stGO:",MV$stGO,"  rgGO:",MV$rgGO,"\n")
        
        #
        #  Plot tract boundary overlay  (if present)
        #
        if (MV$trGO) {
           if (wIdMode == 3) {
              if (wDataBCol_caller)  Tract_BCol <- wDataBCol
              tractLwd <- dataLwd   # set line weight  (if data=tract use dataLwd not overlay weight.
           }
              
           par(new=TRUE)
           plot(MV$tr_proj_sel,
                 border = Tract_BCol,
                 col    = NA,
                 lwd    = tractLwd,
                 xlim   = vxLim, ylim   = vyLim
               )
        }
   
        #
        #  Plot county boundary overlay (if present)
        #
        if (MV$coGO) {
           if (wIdMode == 2) {
              if (wDataBCol_caller)  County_BCol <- wDataBCol
              countyLwd  <- dataLwd
           }
           
           par(new=TRUE)
           plot(MV$co_proj_sel,
                 border = County_BCol,
                 col    = NA,
                 lwd    = countyLwd,
                 xlim   = vxLim, ylim   = vyLim
               )
        }
   
        #
        #  Plot hsa boundary overlay (if present)
        #
        if (MV$hsGO) {
           if (wIdMode == 5) {
              if (wDataBCol_caller)  HSA_BCol <- wDataBCol
              hsaLwd  <- dataLwd
           }
           
           par(new=TRUE)
           plot(MV$hs_proj_sel,
                 border = HSA_BCol,
                 col    = NA,
                 lwd    = hsaLwd,
                 xlim   = vxLim, ylim   = vyLim
               )
        }
   
        #
        # plot Seer Area Overlay
        #
        if (MV$saGO) {
           if (wIdMode == 4) {
              if (wDataBCol_caller)  Seer_BCol <- wDataBCol
              seerLwd  <- dataLwd
           }
           
           par(new=TRUE)
           plot(MV$sa_proj_sel,
                 border = Seer_BCol,
                 col    = NA,
                 lwd    = seerLwd,
                 xlim   = vxLim, ylim   = vyLim
               )
        }
   
        #
        # plot State Area Overlay
        #
        if (MV$stGO) {
           if (wIdMode == 1) {
              if (wDataBCol_caller)  State_BCol <- wDataBCol
              stateLwd  <- dataLwd
           }
           
           par(new=TRUE)
           plot(MV$st_proj_sel,
                 border = State_BCol,
                 col    = NA,
                 lwd    = stateLwd,
                 xlim   = vxLim, ylim   = vyLim
               )
        }
   
        #
        # plot Regions Area Overlay
        #
        if (MV$rgGO) {
           
           par(new=TRUE)
           plot(MV$rg_proj_sel,
                 border = Region_BCol,
                 col    = NA,
                 lwd    = regionLwd,
                 xlim   = vxLim, ylim   = vyLim
               )
        }
   
        ##### category map - done.
        xyBox <- data.frame(min=numeric(),max=numeric())
        xyBox <- rbind(xyBox,vxLim)
        xyBox <- rbind(xyBox,vyLim)
        colnames(xyBox) <- c("min","max")
        row.names(xyBox) <- c("x","y")
        
        #cat("Exiting SM_Mapper - bbox:\n")
      
        ##### End of Mapping Part 2 #####
        invisible(xyBox)   # return plotting box.
   }
   #
   #  End of SM_Mapper
   #
   ###
   
   
   ###
   #
   #  SM_Legend - draws the legends for SeerMapper 
   #    Input required:  rPM-run parameters and variables
   #                     MV -boundary info.
   #
   SM_Legend <- function(rPM, MV) {
      #
      #  Function to add legend to the SeerMapper graphic.
      #
      #  Load up some local variable
      #
      Lg         <- rPM$mLegend    # get all of the legend parameters
      debug      <- rPM$debug
      
      wPin       <- par("pin")
      wMai       <- par("mai")
      
      #cat("par - mai:",wMai,"  mar:",par("mar"),
      #  "\n      din:",par("din"),"  fin:",par("fin"),"  pin:",wPin,"\n")
              
      BotRoom    <- - (wMai[1] / wPin[2]) / 2   # space in bottom margin for titles and axis labels.  
      #  However, we are not doing this on the maps.  so, reduce the bottom and top spaces to 3.1
      #  and allow the legend box is sink below the plot box.
      
      #cat("Bottom Space in Margins (% of plot height):",BotRoom,"\n")
    
      ##### Legend Overlay #####
   
      tempCex <- Lg$lSize * 2
      tempPch <- Lg$lPch 
   
      if (debug) {
         cat("Mapping legend Z-5809 \n","  tempCex :",tempCex,"  tempPch:",tempPch,"  CatNumb:",rPM$CatNumb,"\n")
         print(str(Lg))
         cat("CB_Rate_Mid:",rPM$CB_Rate_Mid,"\n")
      }
      if (Lg$lCounts) {
          # print legend with category counts
          #cat("Lg-Counts CatRwCnt:",rPM$CatRwCnt,"\n")
          
          legend(Lg$lPosv, rPM$CatRwCnt[1:rPM$CatNumb],
                       ncol  = Lg$lNumCols,
                       cex   = Lg$lSize,
                       xpd   = NA,
                       inset = c(0.01,BotRoom),
                   #    inset = c(0.01),
                       pt.bg = rPM$CB_Rate_Mid,
                       pt.cex= tempCex,
                       pch   = tempPch
                    )
   
      } else {
         #cat("Lg-no Counts CatRAdj:",rPM$CatRAdj,"\n")
          
          # print legend without category counts
          legend(Lg$lPosv, rPM$CatRAdj[1:rPM$CatNumb],
                       ncol  = Lg$lNumCols,
                       cex   = Lg$lSize,
                       inset = c(0.01,BotRoom),
                       xpd   = NA,
                    #   inset = c(0.01),
                       pt.bg = rPM$CB_Rate_Mid,
                       pt.cex= tempCex,
                       pch   = tempPch
                    )
      }
    
      ##### End of Legends Overlay #####
      #cat("End of SM_Legend\n")
   }   # end of SM_Legend function

   #
   # End of SM_Legend
   #
   ###   

#
# End of Master Functions - stage routines
#
#####


#####
#
#  Main function definitions
#

SeerMapper2000 <- function(...) {
     SeerMapper(censusYear="2000",...)
}

SeerMapper2010 <- function(...) {
     SeerMapper(censusYear="2010",...)
     
}

SeerMapper.Version <- function() {return("SeerMapper V1.2.2 2019-07-31 03:04pm")}

SeerMapper <- function(ndf,
              censusYear     = NULL,       # default: "2000"  (hidden)
              proj4          = NULL,       # default: "" (or NULL)   # added 18/03/15
              idCol          = NULL,       # default: "FIPS"
              dataCol        = NULL,       # default: "Rate"
              categ          = NULL,       # default: "5"  categories.
              mTitle         = NULL,                             # changed 17/01/08
              mTitle.cex     = NULL,       # default: 1 multiplier # changed 17/01/16
              us48Only       = NULL,       # default: FALSE      # changed 17/01/08
              includePR      = NULL,       # default: FALSE
              regionB        = NULL,       # default: "NONE"
              stateB         = NULL,       # default: depends: NONE or DATA
              seerB          = NULL,       # default: depends: NONE or DATA
              hsaB           = NULL,       # default: depends: NONE or DATA  # added 18/03/15
              countyB        = NULL,       # default: depends: NONE or DATA
              tractB         = NULL,       # default: depenes: NONE or DATA
              dataBCol       = NULL,       # default: default color for boundary level   # added 17/01/15
              fillTo         = NULL,       # default: "SEER"
              clipTo         = NULL,       # default: "NONE"
              hatch          = NULL,       # default: FALSE      
              hatch2         = NULL,       # default: NULL       # added 18/03/15
              mLegend        = NULL,       # build legend -> see options for defaults    # changed 17/01/08
              brkPtDigits    = NULL,       # default: 2
              palColors      = NULL,       # default: "RdYlBu"  - RColorBrewer palette   # New 5/16
              debug          = NULL        # default: FALSE
              )
      {

      ####
      #
      #  Internal Functions - Stage 0 and 1
      #

      ###
      #
      # CheckColnn - Check Column Name or Number function to verify a column name or number.
      #

      CheckColnn <- function(varName, msgNums, varValue, stDat, stDatName) {

         # msgNums:
         #     [1] - column number out of range.
         #     [2] - column name is invalid
         #     [3] - column name/number is invalid data type
         #     [4] - column name is empty
         #
         #  return value - ERROR indicator
         xr        <- list(Err = FALSE, colNum = as.integer(0), colName="")

         wstname   <- names(stDat)   # get list of names of data.frame columns
         wstMax    <- dim(stDat)[2]  # maximum number of columns
         wstname   <- c(wstname,seq(1:wstMax))
         ErrFnd    <- FALSE

         if (is.numeric(varValue)) {
            if (varValue < 1 || varValue > wstMax) {
               ErrFnd  <- TRUE
               xmsg    <- paste0("***",msgNums[1]," ",varName," parameter is out of range. It must be a column number between 1 and ",wstMax,".")
            } else {
               # valid column number
               xr$colNum  <- varValue
               xr$colName <- wstname[varValue]
            }
         } else {
            if (is.character(varValue)) {
               varValue  <- stringr::str_trim(varValue)
               if (nchar(varValue) < 1) {
                  ErrFnd <- TRUE
                  xmsg   <- paste0("***",msgNums[4]," ",varName," parameter is a character string, but is empty.")
               } else {
                  # got a character value
                  xm <- match(varValue, wstname)   # see if value name.
                  if (is.na(xm)) {
                     # not a valid value
                     ErrFnd <- TRUE
                     xmsg  <- paste0("***",msgNums[2]," ",varName," parameter is not a valid column name (",varValue,") in the ",stDatName," data.frame.")
                  } else {
                     if (xm > wstMax) { xm <- ( xm - wstMax) }   # adjust if numeric as character
                     xr$colNum  <- as.integer(xm)
                     xr$colName <- varValue
                  }
               }

            } else {
               ErrFnd <- TRUE
               xmsg  <- paste0("***",msgNums[3]," ",
                                    varName, " parameter is not the correct data type (",
                                    typeof(varValue),"). Must be numeric or character.")
            }
         }
         if (ErrFnd) {
            xr$Err <- TRUE
            warning(xmsg,call.=FALSE)
         }
         return(xr)
      }
      #
      #  end of CheckColnn function 
      #
      ###


        
            ####
            #
            #  State 0 - variable initialization
            #
            #      SM_GlobInit()
            #
            #  Stage 1 - Parameter validation
            #
            #     The validation needs to be done in the order of dependency:
            #
            #     Open parameters - required by others.
            #
            #     Part 1
            #
            #       debug   - control informational output
            #
            #       censusYear - Which census year are we dealing with - must know before dealing with 
            #                    the idCol content.
            #
            #       categ   - type of categorization and usage of dataCol, set categMode
            # 
            #       if categMode <> 4 then
            #
            #          palColors  - validate and determine the max number categories.
            #
            #          brkPtDigits - numeric -> used in category calculation (categMode = 1)
            #
            #       end if 
            #
            #       us48Only and includePR - What states/territories/districts are being used.
            #               - limits geographic areas
            #               - limits data to be used
            #
            #       regionB - Validate for "NONE", "DATA", "ALL"
            #
            #       stateB - Validate for "NONE", "DATA", "REGION", "ALL"
            #
            #       seerB  - Validate for "NONE", "DATA", "STATE", "REGION", "ALL"
            #
            #       hsaB   - Validate for "NONE", "DATA", "SEER", "STATE"
            #
            #       countyB- Validate for "NONE", "DATA", "HSA", "SEER", "STATE"
            #
            #       tractB - Validate for "NONE", "DATA", "COUNTY", "HSA", "SEER", "STATE"
            #
            #       fillTo - Validate for "NONE", "COUNTY", "SEER", "STATE"
            #
            #       clipTo - Validate for "NONE", "DATA", "HSA", "SEER", "STATE", "REGION", or "FALSE", "TRUE"  (def: FALSE/NONE)
            #                 Basic syntax.
            #
            #       dataBCol  a) check for valid color
            #
            #       hatching - all parameter, no content on H:dataCol
            #       hatch     a) angle, lwd, den, col, ops, <value>, rest of parameters.
            #
            #       mLegend   a) categories and colors used for categorization, need categ, 
            #                    data categorization
            #
            #       mTitle    a) number of lines
            #                 b) character
            #
            #       mTitle.cex  a) numeric 
            #
            #       ndf     - a) exists and data.frame
            #                 b) can get column names
            #                 c) can get row.names
            #
            #
            #   Part 2
            #   1st Level dependent parameters  (have rPM and cVL
            #
            #       ndf       d) Handle idCol = "row.names"
            # 
            #       idCol   - a) value (column name/number)
            #                 b) Validate content and set idMode
            #
            #       dataCol   a) value (column name/number)
            #                 b) validate Contect
            #
            #                    - Rate/Data    # categMode = 1 or 2
            #                    - Category     # categMode = 3
            #                    - Color        # categMode = 4
            #
            #       dataBCol  a) reset - color - depended on idMode
            #
            #       clipTo    a) adjust based on data level 
            #
            #       hatching- a) hatch:hDataCol - name/number verification
            #                 b) hatch2:hDataCol
            #                 c) Store data for hatching in dataMapDF$hData and $h2Data
            #
            #   Wrk_Data(dataMapDF)  <-  ID, stID, stcoID, saID, Data, Cat, Col, hData, Col, Den (row.names <- ID)
            #
            #   update rPM
            #   build  rRM$Wrk_Data  (matchs ndf order)??  dataMapDF
            #
            #
            #  Stage 1 Done - Parameters have pass check # 1   - cVL is done (initial pass)
            #
            #  cVL and rPM -> ndf
            #
            #  Stage 2 - Build full and data spatial data structures.  (SM_Build)
            #
            #       Input:   rPM
            #         idCol
            #         stateB, seerB, hdistB, countyB, tractB, regionB, fillTo
            #         RegionListAll, StateListAll, SeerListAll 
            #
            #       idCol   - b) contents - defines the geographic space we are working with.
            #
            #  rPM$idMode set
            #
            #       ## review data loc_id -> set state, seer, region data lists.
            #
            #       ## reduce state, seer and region boundary data.
            #
            #       ## set state, seer, region lists (all independent of data)
            #          read st99, sa99, build regions, xxxxListAll, xxxxListData
            #          
            #          read coXX and trXX - set xxxxListAll, xxxxListData.
            #          
            #          build  xxxxPLists
            #
            #       ## load county and tract (as needed) -> boundaries needed loaded.
            #
            #       regionB, stateB, seerB, hdistB, countyB, tractB, fillTo - boundary controls - needs to know the 
            #                 geographic space to know how to set their defaults.
            #                 a) relates to which geographic areas are active.  (US, all/state, all/seer, and data)
            #                 b) effects the drawing levels within up to ALL, But not lower then DATA.
            #
            #                 # after categ and brkPtDigits known.
            #        Results:
            #
            #      MV$
            #           regions_proj, states_proj, seerRegs_proj, 
            #           county_proj, tract_proj,
            #           data_proj
            #           regions_proj_sel, states_proj_sel, seerRegs_proj_sel,
            #           county_proj_sel, tract_proj_sel,
            #
            #           regions_data, states_data, seers_data, 
            #           county_data, tract_data,
            #           data_data
            #
            #           RegionListAll, StateListAll, SeerRegListAll,
            #           CountyListAll, TractListAll
            #           RegionListData, StateListData, SeerRegListData,
            #           CountyListData, TractListData,
            #           RegionPList, StatePList, SeerRegPList, 
            #           CountyPList, TractPList
            #           
            #       All stored in MV and rPM.
            #     
            #        return (MV and rPM)
            
            #  Stage 2 - Done - MV$ built
            #
            #  Stage 3 - Categorization and Color & hatching    (SM_Categ)
            #
            #        Input:      cVL, rPM
            #            dataCol
            #            hDataCol
            #            categ
            #            HatchingFlag
            #
            #       dataCol - Validate:
            #                 b) rates - content (categMode = 1 or 2)  Rate  
            #                 c) Cats  - content - categories (categMode = 3) (1-"n")
            #                 d) color - content (categMode = 4) (colors() | "#hhhhhh")
            #       rates (1 and 2), breakpoint table
            #                        categorize rates
            #                        set color
            #
            #
            #       check range of Cats (base and number of elements) (3)
            #                Set up conversion to Col
            #
            #       check valid colors for (4)
            #
            #       ## Do Categorization of the data - categMode = 1 or 2
            #
            #       hatching- c) hDataCol - content - OK.
            #
            #        results:
            #          added $cat, $col, $hCol, $hDen
            #          HatchingFlag
            #       
            #        return(rPM)    # cVL should not have changed)
            #
            #  Stage 3 -Done  - $col and $hcol, etc, filled in in MV$
            #
            #
            #
            #  Stage 4 - Graphics
            #
            #          Map w/hatching   (SM_Mapping, rPM, MV)
            #
            #          legend           (SM_Legend, rPM, MV)
            #
            #          title
            #
            #
            ####



      #
      #####

      ############################################################

      #####
      #
      #   Main Code Body
      #
      ##  entry point for inline code debug.
      #cat("Call SM_GlobInit\n")
      
      rPM            <- SM_GlobInit()
    
      rPM$debugFlag  <- FALSE
      
      #cat("Return from SM_GlobInit\n")
      
      #####
      #
      #    execute:
           #rPM$debugFlag <- TRUE # if running testing line code and not the package at this point.
           rPM$debugFlag <- FALSE # run package ##FZ.

      #
      #####

      debugFlag      <- rPM$debugFlag
      
      #####
      #
      #  package variant - variables (2000 vs. 2010 versions)
      #

      censusYear_def <- "2000"
      #
      #####
      
      #####
      #
      #  Get list of call parameters and values
      #
      #####
      #
      #  Save call parameter values for warning and error messages, not content, name of variables.
      #  Can only in live function call.
      #
      #
      #  Can't do this in a function because the environment and frames will change. Setup outside of in-line test.
      #
      
      if (!debugFlag) {
         frml          <- formals()                   # get list of call parameters - the formals - for the function and default values. (as defined).
         frmlNames     <- names(formals())            # get the name of the parameters  (as we validate the parameter, we will back file the defaults.
         if (length(frmlNames) == 1 && frmlNames[[1]] == "fun") {
           #cat("WE are running line by line but debugFLAG is not set to TRUE.\n")
            debugFlag  <- TRUE
         } 
         if (!debugFlag) {
            callVar       <- as.list(match.call())[-1]   # get the names and values used on the current call.
            callVarNames  <- names(callVar)              # get the names of the used call parameters
   
            # merge the formals parameter list with the parameter list used at the time of the micromapST call with user set values.
   
            callVL               <- frml                   # Seed the call variable list with the formals and default values
            callVL[callVarNames] <- callVar[callVarNames]  # copy the values used in the call.
   
            callVarList  <- as.list(callVL)                # convert data.frame to list.
            #printNamedList("callVarList",callVar)
         }
      }
      if (debugFlag) {
         callVL       <- list(ndf=I(ndf), 
                              censusYear = censusYear_def,
                              proj4      = NULL,
                              idCol      = "FIPS",
                              dataCol    = "Rate",
                              categ      = 5,
                              hatch      = FALSE, 
                              hatch2     = NULL,
                              mLegend    = NULL,
                              regionB    = "NONE",
                              stateB     = "DATA",
                              seerB      = "DATA",
                              hsaB       = "NONE",
                              countyB    = "NONE", 
                              tractB     = "NONE", 
                              regions    = FALSE,
                              fillTo     = "NONE", 
                              clipTo     = "NONE",
                              dataBCol   = NULL,
                              us48Only   = FALSE, 
                              includePR  = FALSE,
                              mTitle     = NULL,     
                              mTitle.cex = NULL,
                              brkPtDigits= 2,
                              debug      = NULL
                            )
          callVL$ndfName     <- "ndf"
          #callVarList <- as.list(callVL)     # convert data.frame to list.
          callVarList  <- callVL              # convert data.frame to list.
      }
      ndfName      <- callVarList$ndfName     # get variable name of data.frame with the data.

      #cat("Call arguments Z-6286 - ndf:", ndfName, "\n")
      
      callVarList$ndfName  <- ndfName
      rPM$ndfName          <- ndfName
      
      #
      #####

   ##### Stage 1 - Validate call parameters #####
     
      #################
      ######
      ###
      #
      #  Check parameters.
      #
    
      ####
      #
      #  Step 1 = check for debug request
      #
      debug_def    <- FALSE

      if (is.null(debug)) {
         # no no debug request specified.
         debug     <- debug_def
      } else {
         debug <- debug[[1]][1]
         if (is.na(debug)) {
            debug  <- debug_def
         }
      }
      callVarList$debug <- debug    # add to cVL list.
      rPM$debug         <- debug
      #
      ####
      
      ####
      #
      #  Step 2.1 - censusYear Parameter  (010-012)
      #
      #  Which census year are we mapping?
      #
      if (is.null(censusYear)) {
         # no Census Year specified.  Set default of "2000"
         censusYear    <- censusYear_def
      } else {
         # have census Year parameter, check for validate value "2000" or "2010"
         censusYear    <- censusYear[[1]][1]
         if (is.na(censusYear)) {
            # use default
            censusYear <- censusYear_def
         }
         censusYear    <- stringr::str_trim(toupper(censusYear))
         if (!(censusYear == "2000" || censusYear == "2010")) {
            # invalid value in censusYear
            ErrFnd     <- TRUE
            xmsg       <- paste0("***010 The censusYear parameter is set to ",censusYear," and is invalid. It must be '2000' or '2010'.")
            warning(xmsg, call.=FALSE)
            censusYear <- censusYear_def
         }
      }
      cYear                  <- stringr::str_sub(censusYear,-2,-1)  # get last two digits

      callVarList$censusYear <- censusYear
      rPM$censusYear         <- censusYear
      rPM$cYear              <- cYear
      cY                     <- ""                   # test for file names.
      if (censusYear != "2000") { cY <- censusYear }
      rPM$cY                 <- cY
      
      if (debug) {
         cat("censusYear Z-6358 :",censusYear,"  cYear:",cYear,"  cY:",cY,"\n")
      }
      #
      ####
    
      ####
      #
      #  Step 2.2 - proj4 Parameter  (013-014)
      #
      proj4_def <- NULL
      CRSproj4  <- NULL
      #
      #  Override the default map projection with the user's projection..
      #  The projection string is provided in proj 4 format and must 
      #  be convertable by CRS to a usable projection.  It must also be 
      #  reversable back to the proj 4 string as a validation.
      #
      #  The transformation is done right before printing the maps.
      #  The projection of the maps is returned to the SeerMapper caller.
      #
      if (is.null(proj4)) {
         # no proj4 string specified.  Set default of NULL
         proj4    <- proj4_def
      } else {
         # have proj 4 string parameter, check for proj4 string 
         proj4    <- proj4[[1]][1]
         if (is.na(proj4)) {
            # use default
            proj4 <- proj4_def
         } else {
            proj4 <- stringr::str_trim(proj4)
         }
      }
      if (!is.null(proj4)) {
      
         res <- convertPROJ4(proj4)
         
         if (class(res) == "CRS") {
            # got a conversion to CRS - looks good.
            CRSproj4 <- res
         } else {
            # Error found and reported.
            EffFnd = TRUE
            CRSproj4 <- NULL
            xmsg <- paste0("***909 Error on processing proj4 parameter.  No user specified projection will be done.")
            warning(xmsg,call.=FALSE)
         }
     
      }
     
      callVarList$proj4      <- proj4
      rPM$proj4              <- proj4
      callVarList$CRSproj4   <- CRSproj4
      rPM$CRSproj4           <- CRSproj4
      
      if (debug) {
         cat("proj 4  Z-6420 : proj4:",proj4,"\n")
         if (!is.null(proj4)) print(CRSproj4)
      }
      #
      ####
    
      ####
      #
      #  Step 3 - Validate 'categ' call parameter..  (250-269)
      #
      
      categMode     <- 0     # dataCol is rates - calculate and categorize
                             #  0 - determination not completed
                             #  1 - number of categories  (initially it's numeric)
                             #  2 - breakpoint list
                             #  3 - data = category index
                             #  4 - data = colors
                             
      CatNumb       <- 0     # not determined
      categ_def     <- 5
      ErrFnd        <- FALSE
      #cat("User provided categ parameter Z-6441 :\n")
      #cat("   ",categ,"\n",sep="  ")

      #
      #  We have to validate the categ parameter, but can't check the ranges until the palColors
      #  parameter is processed.  However, the palColors parameter is only processed with categ
      #  is NOT set to "COLORS".
      #

      if (is.null(categ)) {
         # if null, use the default
         categ     <- categ_def
      } else {
         #cat("categ is NOT NULL\n")
         #  With categ possibly being a vector - checking for NA is a little harder.
         if (length(categ) == 0) {
            #cat("categ has a length of 0.\n")
            # could be NON-NULL but have length = 0
            # value provided is empty - length of 0  # should be caught as NULL
            ErrFnd <- TRUE
         } else {
            #cat("categ has length > 0\n")
            # single item, so if any are NA (which should be one.)(could be list with one item.)
            if (any(is.na(categ))) {
               #cat("categ has an NA in it:",categ,"\n")
               # it's an NA some how!
               ErrFnd <- TRUE
            }
         }
         
         if (ErrFnd) {
            # Something was provided, but it's an NA or has no length (empty) - give warning.
            #  No value provided.  Not an error or warning - but notify default will be used.
            xmsg        <- paste0("***250 The categ call parameter is missing, empty or contains NAs.",
                                    " The default value of 5 will be used.")
            warning(xmsg, call.=FALSE)
            # set defaults
            categ       <- 5
         }
      }
      if (!ErrFnd) {
         ErrFnd        <- FALSE
         wCateg        <- categ    # get a copy
      
         #  Got value now check for a number (length=1), a string (length=1), or vector of points (length > 1).

         repeat {
            # loop processing R.Cat until it is resolved or an error occurs.
            
            if (length(wCateg) == 1) {  # a single value was provided by user - number of categories, "data" or "colors".
          
               #  Variation # 1  - single value (number of categs = 3 to "n")
               wCateg  <- wCateg[[1]][1]   # depending on structure - pick up only one value.
               
               if (is.character(wCateg)) {
                  # of character - possibly number, word or series - not a big help.
                  
                  wCateg <- stringr::str_trim(wCateg)   # convert to uppercase
                  
                  if (nchar(wCateg)>0) {
                     # we have a string to work with.
                     wCateg2   <- toupper(wCateg)
                     
                     if (wCateg2 == "DATA") {
                        # the dataCol is category numbers.   # variation # 1 a
                        categMode <- 3          # dataCol is category values.
                        wCateg    <- wCateg2
                        break
                  
                     } else {
                        if (wCateg2 == "COLORS") {
                           # the "colors" option is specified.  # variation # 1 b
                           categMode <- 4      # dataCol is fill colors.
                           wCateg    <- wCateg2
                           break 
                     
                        } else {
                     
                           # check for numeric value     # variation # 2  - numeric
                           #  (Can't check range until later - range of 3 to max. )  (length=1)
                           # convert to numbers.  The parameter many be categ = "c(1,2,3,4,5)" format...  OUCH!
                           
                           suppressWarnings(wCateg2   <- as.numeric(wCateg))       # see if it converts to numeric.
                        
                                # 
                                # A single non-numeric  or "c(1,2,3,5)"  or "(1,2,3,4,5)" or "1,2,3,4,5" 
                                #       will through an error,
                                # Only a single value numeric will pass this test.
                                #
         
                           if (is.na(wCateg2)) {
                             
                              # did not convert to numeric  - try assigning it as wCateg2 <- c(a,b,s,e,d)
                           
                              wCateg2   <- 0
                              xcmd      <- paste0("wCateg2 <- ",wCateg)
                              #print(xcmd)
                              iR   <- try(eval(parse(text=xcmd)),TRUE)  # try the command, if fails, error.
                                 # the following will throw an error:
                                 #    x <- (1,2,3,4,5)
                                 #    x <- 1,2,3,4,5
                                 #    x <- c(1,2,3,)  (empty element)
                                 #    x <- junk       # when it's a bad single value.
                                 #
                                 #  it must be the correct format and syntax:  x <- c(1,2,3,4,5)  or
                                 #    at least x <- c(1,3,4,5,"ag")
                                 #    The c(1,2,3,4,5) was entered as categ="c(1,2,3,4,5)" for us to get to this point.
                                 #
                           
                               if (class(iR) == "try-error") {
                                 # could not execute the x <- value or vector assignment
                                 xmsg     <- paste0("***252 The value entered for categ parameter is not valid : ",wCateg,"  The default of categ=5 is used.")
                                 warning(xmsg,call.=FALSE)
                                 wCateg      <- 5   # set default
                                 categMode   <- 1
                                 break
                              } else {
                                 # the assignment worked, try reprocessing of the value (wCateg) as numeric vector
                                 wCateg      <- wCateg2 
                                 # looks good - reprocess as numbers.
                              }
                           } else {
                              # a valid single numeric (converted OK) loop to get to the numeric validation.
                              wCateg    <- wCateg2
                              categMode <- 1       # set to mode 1 until a numeric list is detected.
                              # looks good - reprocess as numbers.
                           }
                        }
                     }
                  } else {
                     # categ is empty  categ = "".
                     # could not execute the x <- value or vector assignment
                     xmsg     <- paste0("***253 The categ parameter provided does not contain any value. The default of categ=5 is used.")
                     warning(xmsg,call.=FALSE)
                     wCateg      <- 5   # set default
                     categMode   <- 1
                     break
                  }
                        
                  #
                  #  At this point we have a successful conversion of a numeric character string (single number of 
                  #  c(1,2,2,3,4) to numeric.  Otherwise we have thrown an error message and are breaking
                  #  out of the loop.
                  #
                 
               } else {
               
                  # Not character - keep testing, but length still = 1
                  # We get here is the value was a single numeric st start with or on the second repeat 
                  #   it was a character numeric was converted to a single number.
               
                  # We are still dealing with a length=1 value.
               
                  if (is.numeric(wCateg)) {
                     #cat("wCateg:",wCateg," is numeric.\n")
                     categMode <- 1
                   
                     #  Check the numeric value later, after palColors is evaluated.
                   
                     break
                     
                  } else {
                  
                     # length = 1 and it's something else (logical, complex, list, or data.frame)
                     xmsg     <- paste0("***256 The categ call parameter is not 'DATA', 'COLORS', or a valid single value numeric value.",
                                            " The default of categ=5 will be used.")
                     warning(xmsg,call.=FALSE)
                     ErrFnd    <- TRUE
                     wCateg    <- 5
                     categMode <- 1
                     break
                  }
               }
               #
               #  end of processing categ with a length of 1.
               #

            } else {
            
               # length of categ vector is > 1 >> must be a set of breakpoints.
               # length can't be <= 0, so it must be > 1
               #
               # if it was a vector of numbers as characters.  MUST be converted NOW.
               #
               ##########################
               
               #cat("multiple element test:",wCateg,"\n")
               #  categ is a vector - for error messages, need a c() image of categ.
               wCategL <- paste0("c(",paste0(wCateg,collapse=", "),")")
               
               if (is.character(wCateg)) {
                  # character number?  Convert
                  
                  suppressWarnings(wCateg2  <- as.numeric(wCateg))
                  if (any(is.na(wCateg2))) {
                     # Error - something did not convert from character to numeric.
                     xmsg    <- paste0("***262 The categ call parameter contains non-numeric value in",
                                           " the break point vector: ",wCategL,
                                           ". The default of categ=5 is used.")
                     warning(xmsg,call.=FALSE)
                     wCateg      <- 5   # set default
                     categMode   <- 1
                     break
                     
                  } else {   
                     # good vector
                     # is it sorted?
                     if (any(wCateg2 != sort(wCateg2))) {
                        xmsg    <- paste0("***264 The categ call parameter breakpoint vector is not ",
                                              "in order (low to high). Breakpoint vector has been sorted.")
                        warning(xmsg,call.=FALSE)
                     }
                     wCateg    <- sort(wCateg2)     # save sorted version.
                     categMode <- 2    # vector of break points.
                     break
                     
                  }    
               } else {
              
                  if (!is.numeric(wCateg)) {
                     xmsg    <- paste0("***268 The categ call parameter contains non-numeric value(s) in",
                                            " the break point vector: ",wCategL,
                                            ". The default of categ=5 is used.")
                                            
                     warning(xmsg,call.=FALSE)
                     wCateg      <- 5   # set default
                     categMode   <- 1
                     break
                    
                  } else {
                     # good vector - check values and length later
                     if (any(wCateg != sort(wCateg))) {
                        xmsg    <- paste0("***264 The categ call parameter breakpoint vector is not ",
                                              "in order (low to high). Breakpoint vector has been sorted.")  # duplicate message.
                        warning(xmsg,call.=FALSE)
                     }
                     wCateg    <- sort(wCateg)     # save sorted version.
                     categMode <- 2    # vector of break points.
                     break
                 
                  }    
                 
               }  # end of multiple element check
              
            }  # end of leng=1 or leng>1 check
         }  # end of repeat loop.
         #  the repeat loop is to handle the conversion of a set of character values into number.

         #cat("End of loop - wCateg:",wCateg,"\n")
      }
     
      #cat("end of categ processing - wCateg:",wCateg,"  categMode:",categMode,"  CatNumb:",CatNumb,"\n")
   
      if (categMode == 1)  CatNumb <- wCateg
      if (categMode == 2)  CatNumb <- length(wCateg)     
      #  if categMode = 3 or 4, have to wait until dataCol is inspected to set CatNumb  
      #
      #  Do range change for single value and breakpoint list after palColors, idCol, and dataCol has 
      #    been validated.
      #  If categMode = 1 or 2, the wCateg is a numeric single or list with CatNumb is the number of elements.
      #             CabNumb = 1 (number of categories)   CabNumb > 1 (wCateg is a list of breakpoints)
      #  If categMode = 3 or 4, the wCateg should be ignored.
      #
      
      callVarList$categMode <- categMode
      callVarList$categ     <- categ
      callVarList$wCateg    <- wCateg
      callVarList$CatNumb   <- CatNumb
      
      rPM$categMode         <- categMode
      rPM$wCateg            <- wCateg
      rPM$CatNumb           <- CatNumb
      rPM$categ             <- categ
      
      #
      #  We now know we have a good categ parameter.
      #
      ###
       
      ###
      #
      #  Step 4 - palColors  and brkPt
      #
      palColors_def        <- "RdYlBu"
      brkPtDigits_def      <- 2
      
      if (categMode != 4) {
         # only checked if categMode is not 4.
         
         #####
         #
         #  Step 4.1 - palColors call parameter  (015-019)  (Process palColors to get the max number of categories.)
         #
         
         # only check for this parameter and validate it if categ <> "COLORS".
         
         if ( ( is.null(palColors) || any(is.na(palColors)) || length(palColors)==0) ) {
            palColors       <- palColors_def
            # set the default
         }
         #
         wPmatch          <- match(toupper(palColors),rPM$RCBrewerDF$Name)  # check for match ALL CAPS
      
         if (is.na(wPmatch)) {
            # no match
            xmsg          <- paste0("***015 The palColors parameter value of ",palColors,
                                " is not valid in the RColorBrewer package. ",
                                "The default of 'RdYlBu' will be used."
                                )
            warning(xmsg,call.=FALSE)
            palColors    <- palColors_def
         } else {
            palColors     <- rPM$RCBrewerDF[wPmatch,"PName"]
         }
         #
         #####
      
         #####
         #
         #  Step 4.2 - Number of digits on breakpoint values. (102-103)
         #
         #  Processed only if categMode = 1. Only applies when we calculate the breakpointn list.
         #
         
         if (categMode == 1) {
      
            if (is.null(brkPtDigits) || any(is.na(brkPtDigits)) || length(brkPtDigits) == 0) {
               # set to the default
               brkPtDigits <- brkPtDigits_def 
      
            } else {
               # not null, NA or numeric
               
               brkPtDigits <- as.integer(brkPtDigits)
               if (any(is.na(brkPtDigits)) || brkPtDigits < 1 || brkPtDigits > 5) {
                  xmsg <-    paste0("***103 The brkPtDigits call parameter must be greater than 0,",
                                    " no greater than 5 and not NA. Set to a value of 2.")
                  warning(xmsg,call.=FALSE)
                  brkPtDigits  <- brkPtDigits_def           # set to default
               }
            }
         } else {
            # for categMode = 2 and 3
            brkPtDigits  <- brkPtDigits_def           # set to default
         }
         
         #
         #####

      } else {
         # if categMode = 4, ignore and set placeholders for these parameters (they should not be used.)
         palColors       <- palColors_def
         brkPtDigits     <- brkPtDigits_def
      }
      
      wPmatch          <- match(toupper(palColors),rPM$RCBrewerDF$Name)  # check for match
      palColorsMaxNum  <- rPM$RCBrewerDF[wPmatch,"maxcolors"]
    
      callVarList$brkPtDigits     <- brkPtDigits
      callVarList$palColors       <- palColors
      callVarList$palColorsMaxNum <- palColorsMaxNum

      rPM$palColors               <- palColors
      rPM$palColorsMaxNum         <- palColorsMaxNum
      rPM$brkPtDigits             <- brkPtDigits
         
      #
      #####

      #####   Based on categMode
      #
      #  Step 5  - check limits of the categ numerically or Color.
      #
     
      if (categMode == 1) {
         catMaxNum   <- palColorsMaxNum - 1
         # single value categ = "n".  "n" must be between 3 and the limit set by palColors.
         #cat("categ:",wCateg,"  palColorsMaxNum:",palColorsMaxNum,"  catMaxNum:",catMaxNum,"\n")
         if (wCateg < 3) {
            # the value is to small, zero or negative.
            xmsg <- paste0("***258 The categ call parameter has a single value of ",wCateg," and must be => 3 as a minimum.  The default of 5 will be used.")
            warning(xmsg,call.=FALSE)
            ErrFnd   <- TRUE
            wCateg   <- 5
            CatNumb  <- 5
         } else {
            if (wCateg > catMaxNum) {
               # value is to large.
               xmsg <- paste0("***259 The categ call parameter value has a single value of ",wCateg," and > to ",catMaxNum,". The categ will be set to ",catMaxNum,".")
               warning(xmsg,call.=FALSE)
               ErrFnd   <- TRUE
               wCateg   <- catMaxNum 
               CatNumb  <- wCateg
            }
         }
      }
      if (categMode == 2) {
         catMaxNum   <- palColorsMaxNum - 1
         # vector of breakpoints.
         
         lCat  <- length(wCateg)   # get length of the vector
         
         #  remove -Inf or Inf if in the list  (list has been sorted)
         if (wCateg[lCat] == Inf)  wCateg <- wCateg[-lCat]
         if (wCateg[1] == -Inf)    wCateg <- wCateg[-1]
         # end Inf values will be added back later.
         cat("wCateg:",wCateg,"\n")
         
         lCat  <- length(wCateg)
         if (lCat < 3 ) {
            # number of values is less than the minimum of 3.
            xmsg      <- paste0("***260 The categ call parameter break point list has a length < 3 items. The default of \var{categ} = 5 will be used.")
            warning(xmsg,call.=FALSE)
            ErrFnd    <- TRUE
            wCateg    <- 5
            CatNumb   <- 5
            categMode <- 1
         } else {
            if (lCat > catMaxNum) {
               # number of values is greater than catMaxNum allowed.
               xmsg     <- paste0("***261 The categ call parameter break point list has a length > ",catMaxNum," items. Only the first ",catMaxNum," values will be used.")
               warning(xmsg,call.=FALSE)
               ErrFnd   <- TRUE
               wCateg   <- wCateg[1:catMaxNum] 
               CatNumb  <- length(wCateg)
            }
         }
      }

      categ              <- wCateg
      callVarList$categ  <- wCateg
      callVarList$wCateg <- wCateg
      rPM$categ          <- wCateg
      rPM$wCateg         <- wCateg
      rPM$CatNumb        <- CatNumb
      
      #
      ####
      
      #####
      #
      #  Step 6 - Verify us48Only and includePR options  (020-025)
      #
      #   This step must be done before we get into the idCol validate and other checks.
      #   Need to know what is the list of states and areas that we will be allowing.
      #
      
      stateSelDel          <- NULL     # list of states to remove
      
      #
      #  Check us48Only call parameter 
      #
      
      us48Only_def         <- FALSE

      if (is.null(us48Only) || any(is.na(us48Only)) || length(us48Only) == 0)  {
         us48Only          <- us48Only_def
      } else {
         us48Only          <- us48Only[[1]][1]   # get first value
         if (typeof(us48Only) != "logical") {
            xmsg           <- paste0("***020 us48Only parameter is not a logical value of TRUE or FALSE.",
                                         " The default of TRUE will be used.")
            warning(xmsg, call.=FALSE)
            us48Only       <- us48Only_def
         }
      }
      us48OnlyFlag         <- us48Only
      callVarList$us48Only <- us48Only
      
      if (us48OnlyFlag) { stateSelDel <- c(stateSelDel, "02", "15") }   # if us48Only no Alaska, Hawaii 
      
      #
      #  Check includePR parameter 
      #

      includePR_def   <- FALSE
      includePRFlag   <- FALSE
      
      # includePR is only active and verified when us48Only is not TRUE
      if (is.null(includePR) || any(is.na(includePR)) || length(includePR) == 0) {
         # missing
         includePR    <- includePR_def
      } else{
         if (is.logical(includePR)) {
            includePR    <- includePR[[1]][1]   # get first value
         } else {
            xmsg         <- paste0("***022 includePR parameter is not a logical value of TRUE or FALSE.",
                                       " The default of FALSE will be used.")
            warning(xmsg, call.=FALSE)
            includePR     <- includePR_def
         }
      }
      
      includePRFlag         <- includePR
      callVarList$includePR <- includePR
      
      if (!includePRFlag) { stateSelDel <- c(stateSelDel, "72") }

      #  key result - what states should be deleted from the initial state list.

      rPM$stateSelDel   <- stateSelDel
      
      #cat("stateSelDel:",stateSelDel,"\n")      
      #
      ####
      
      ####
      #
      #  Step 7 - regions parameter  (106-107)
      #
      # 
      # regions_def   <- FALSE
      # 
      # if (is.null(regions) || is.na(regions) ) {
      #    regions <- regions_def
      # } else {
      # 
      #   regions  <- regions[[1]][1]   # get first value
      #   
      #   if (typeof(regions) != "logical") {
      #     xmsg         <- paste0("***194 The regions parameter is not a logical value of TRUE or FALSE.",
      #                                " The default of FALSE will be used.")
      #     warning(xmsg, call.=FALSE)
      #     regions <- regions_def
      #   }
      # }
      # 
      # callVarList$regions <- regions
      # rPM$regions         <- regions
      #
      ####
    
      ####
      #
      # Step 8 - Process boundary options - check values against data type.
      #
      # For the following steps validating the regionB, stateB, seerB, hdistB, countyB, and tractB
      # the default values are set based on the type of data (above code).
 
      #####
      #
      #
      #  New boundary options:
      #    xxxxB = "NONE"   -> (all) do not draw boundary
      #    xxxxB = "DATA"   -> (all) draw boundary if it or any sublayers contains data
      #    xxxxB = "COUNTY" -> (tract) draw all xxxx boundaries in county if county contains data.
      #    xxxxB = "HSA"    -> (county/tract) draw all xxxx boundaries in health service area containing data.
      #    xxxxB = "SEER"   -> (all) draw all xxxx boundaries in Seer Reg if Reg contains data.
      #    xxxxB = "STATE"  -> (all) draw all xxxx boundaries in State if State contains data.
      #    stateB or seerB = "REGION"  - draw boundaries up to regional boundary
      #    stateB or seerB = "ALL" -> draw all boundaries.
      #
      #    Contains data is defined as the area or any sub area has data associated with it
      #    in the user provided data.frame.
      #
      #  Index Values:
      #    1 = NONE
      #    2 = DATA
      #    3 = COUNTY
      #    4 = STATE
      #    5 = REGION
      #    7 = HSA
      #    8 = RESERVED   
      #    9 = ALL
      #
      #####
 
      #
      #  Step 8.0 - regionB Parameter  (080-081)
      #
      #  Used with all types of data (state, seer, county, tract)
      #  Default is "NONE".
      #
      #  Validates and then reflects on idCol.   
      #     
      #  COMMON code thoughts - parameters xxxxxB, xxxxxB_def, xxxxxB_caller, xxxxxB_lwd, GoodValues, ErrorNum, ErrFnd 
      #       return:  xxxxxB, xxxxxxB_caller, xxxxxB_lwd, ErrFnd
      #  errornumber would have to be a pair - one for xxxxB value wrong and one for xxxxB_lwd out of range.
      #
      regionB_def     <- "NONE"
      regionB_caller  <- FALSE
  
      if (is.null(regionB) || any(is.na(regionB)) || length(regionB) == 0) {
         # not provided - set default
         regionB     <- regionB_def
  
      } else {
         regionB    <- stringr::str_trim(toupper(regionB[[1]]))
         if (regionB == "") regionB = "NONE"
  
         SMatch    <- match(regionB,c("NONE","DATA", NA, NA, NA, NA, NA, NA, "ALL"))
         #                             1      2     3   4   5     6   7   8    9
         if (is.na(SMatch)) {
            ErrFnd        <- TRUE
            xmsg          <- paste0("***080 The regionB call parameter is ",regionB,
                                      " and must be NONE, DATA or ALL. The default of ",regionB_def," will be used.")
            warning(xmsg, call.=FALSE)
            regionB        <- regionB_def
         } else {
            # good value
            regionB_caller <- TRUE
         }
      }
      callVarList$regionB        <- regionB
      callVarList$regionB_caller <- regionB_caller
      rPM$regionB        <- regionB
      rPM$regionB_caller <- regionB_caller
      #
      ###
      #
      #  Step 8.1 - stateB Parameter  (082-083)
      #
      #  Used with all types of data (state, seer, county, tract)
      #  Default is "ALL for state data, otherwise the default is "NONE"
      #
      #  Validates and then reflects on idCol.
      #
      stateB_def     <- "NONE"
      stateB_caller  <- FALSE
  
      if (is.null(stateB) || any(is.na(stateB)) || length(stateB) == 0) {
         # not provided - set default
         stateB     <- stateB_def
  
      } else {
         stateB    <- stringr::str_trim(toupper(stateB[[1]]))
         if (stateB == "") stateB = "NONE"
  
         SMatch    <- match(stateB,c("NONE","DATA", NA, NA, NA, "REGION", NA, NA, "ALL"))
         #                             1      2     3   4   5     6       7   8    9
         if (is.na(SMatch)) {
            ErrFnd        <- TRUE
            xmsg          <- paste0("***082 The stateB call parameter is ",stateB,
                                      " and must be NONE, DATA, REGION, or ALL. The default of ",stateB_def," will be used.")
            warning(xmsg, call.=FALSE)
            stateB        <- stateB_def
         } else {
            # good value
            stateB_caller <- TRUE
         }
      }
      callVarList$stateB        <- stateB
      callVarList$stateB_caller <- stateB_caller
      rPM$stateB        <- stateB
      rPM$stateB_caller <- stateB_caller
      #
      ###
      #
      #  Step 8.2 - seerB Parameter  (084)
      #
      #  Used with all types of data (state, seer, county, tract)
      #  Default for Registry data is "DATA", otherwise default is "NONE"
      #
      seerB_def     <- "NONE"
      seerB_caller  <- FALSE
  
      if (is.null(seerB) || any(is.na(seerB)) || length(seerB) == 0) {
         # not provided - set default
         seerB           <- seerB_def
  
      } else {
         seerB    <- stringr::str_trim(toupper(seerB[[1]][1]))
         if (seerB == "")  seerB = seerB_def
  
         SMatch   <- match(seerB,c("NONE", "DATA", NA, NA, "STATE", "REGION", NA, NA, "ALL"))
         #                           1       2     3   4     5        6       7   8     9
  
         if (is.na(SMatch)) {
            ErrFnd       <- TRUE
            xmsg         <- paste0("***084 The seerB call parameter is ",seerB,
                                     " and must be NONE, DATA, STATE, REGION, or ALL. The default of ",seerB_def," will be used.")
            warning(xmsg, call.=FALSE)
            seerB        <- seerB_def
         } else {
            # good value
            seerB_caller <- TRUE   # user provided.
         }
      }
      callVarList$seerB        <- seerB
      callVarList$seerB_caller <- seerB_caller
      rPM$seerB        <- seerB
      rPM$seerB_caller <- seerB_caller
      #
      #
      ###
      #
      #  Future step 8.3a  - hsaB Parameter    (085-086) 
      #
      #  Used with HSA, county and tract data  "NONE", "DATA", "SEER", "STATE"
      #  Default for county data is "DATA", otherwise the default is "NONE"
      #
      hsaB_def     <- "NONE"
      hsaB_caller  <- FALSE
  
      if (is.null(hsaB) || any(is.na(hsaB)) || length(hsaB) == 0) {
         # not provided - set default
         hsaB               <- hsaB_def
  
      } else {
         hsaB            <- stringr::str_trim(toupper(hsaB[[1]][1]))
         hsaB_caller     <- TRUE
         
         SMatch             <- match(hsaB,c("NONE", "DATA", NA, "SEER", "STATE", NA, NA, NA, NA))
         #                                      1       2    3    4       5       6   7   8   9
         if (is.na(SMatch)) {
            ErrFnd          <- TRUE
            xmsg            <- paste0("***085 The hsaB call parameter is ",hsaB,
                                         " and must be NONE, DATA, SEER, or STATE. The default of ",hsaB_def," will be used.")
            warning(xmsg, call.=FALSE)
            hsaB            <- hsaB_def
         } else {
            # good value
            hsaB_caller     <- TRUE   # indicate user provided parameters
         }
      }
      callVarList$hsaB        <- hsaB
      callVarList$hsaB_caller <- hsaB_caller
      rPM$hsaB                <- hsaB
      rPM$hsaB_caller         <- hsaB_caller
      #
      ###
      #
      #  Step 8.3b - countyB Parameter  (087-088)
      #
      #  Used with only county and tract type data   "NONE", "DATA", "HSA", "SEER", "STATE"
      #  Default for county data is "DATA", otherwise the default is "NONE"
      #
      countyB_def     <- "NONE"
      countyB_caller  <- FALSE
  
      if (is.null(countyB) || any(is.na(countyB)) || length(countyB) == 0) {
         # not provided - set default
         countyB            <- countyB_def
  
      } else {
         countyB            <- stringr::str_trim(toupper(countyB[[1]][1]))
         countyB_caller     <- TRUE
         
         SMatch             <- match(countyB,c("NONE", "DATA", NA, "SEER", "STATE", NA, "HSA", NA, NA))
         #                                      1       2      3    4       5       6    7     8   9
         if (is.na(SMatch)) {
            ErrFnd          <- TRUE
            xmsg            <- paste0("***087 The countyB call parameter is ",countyB,
                                         " and must be NONE, DATA, HSA, SEER, or STATE. The default of DATA will be used.")
            warning(xmsg, call.=FALSE)
            countyB         <- countyB_def
         } else {
            # good value
            countyB_caller <- TRUE   # indicate user provided parameters
         }
      }
      callVarList$countyB        <- countyB
      callVarList$countyB_caller <- countyB_caller
      rPM$countyB                <- countyB
      rPM$countyB_caller         <- countyB_caller
      #
      ###
      #
      #  Step 8.4 - tractB Parameter  (089-091)
      #
      #  Used with only tract data.   "NONE", "DATA", "HSA", "COUNTY", "SEER", "STATE"
      #  default is "DATA" when there is tract data, otherwise it's "NONE"
      #
      tractB_def          <- "NONE"
      tractB_caller       <- FALSE
  
      if (is.null(tractB) || any(is.na(tractB)) || length(tractB) == 0 ) {
         # not provided - set default
         tractB           <- tractB_def
  
      } else {
         tractB           <- stringr::str_trim(toupper(tractB[[1]][1]))
         if (tractB == "") tractB=tractB_def
  
         SMatch           <- match(tractB,c("NONE", "DATA", "COUNTY", "SEER", "STATE", NA, "HSA", NA, NA))
         #                                    1       2       3         4       5      6    7     8   9
         if (is.na(SMatch)) {
            ErrFnd        <- TRUE
            xmsg          <- paste0("***089 tractB call parameter is ",tractB,
                                        " and must be DATA, COUNTY, HSA, SEER or STATE. The default of DATA will be used.")
            warning(xmsg, call.=FALSE)
            tractB        <- tractB_def
         } else {
            # good value
            tractB_caller <- TRUE   # indicate user provided parameters
         }
      }
      callVarList$tractB        <- tractB
      callVarList$tractB_caller <- tractB_caller
      rPM$tractB                <- tractB
      rPM$tractB_caller         <- tractB_caller
      #
      ####

      ####
      #
      #  Step 9.1 - fillTo Parameter  (092-093)  (defunct - remove.))
      #
      #    Values:  "NONE", "COUNTY", "SEER", "STATE"
      #    Default: "NONE"
      #
      fillTo_def      <- "NONE"
      fillTo_caller   <- FALSE

      if ( is.null(fillTo) || any(is.na(fillTo)) || length(fillTo) == 0 ) {
         # not provided - set default
         fillTo    <- fillTo_def

      } else {
         fillTo           <- stringr::str_trim(toupper(fillTo))
         SMatch           <- match(fillTo,c("NONE", NA, "COUNTY", "SEER", "STATE", NA, NA, NA, NA))
         #                             1     2     3         4       5      6   7   8   9
         if (is.na(SMatch)) {
            ErrFnd        <- TRUE
            xmsg          <- paste0("***092 The fillTo call parameter is ",fillTo,
                                      " and must be 'NONE', 'COUNTY', 'SEER', or 'STATE'.",
                                      " The default of 'NONE' will be used.")
            warning(xmsg, call.=FALSE)
            fillTo        <- fillTo_def
         } else {
            # good value
            fillTo_caller <- TRUE
         }
      }
      callVarList$fillTo         <- fillTo
      callVarList$fillTo_caller  <- fillTo_caller
      rPM$fillTo         <- fillTo
      rPM$fillTo_caller  <- fillTo_caller

      #
      ####

      ####
      #
      #  Step 9.2 - clipTo Parameter  (094-095)
      #
      #    Values:  "NONE", "DATA", "HSA", "SEER", "STATE", "REGION", "TRUE", "FALSE"
      #    Default: "NONE"
      #
      #   No upgraded to work on HSA boundaries.
      #
      clipTo_def      <- "NONE"
      clipTo_caller   <- FALSE
      clipToNum       <- 1
      
      vClipTo         <- clipTo[[1]][1]
      vClipTo         <- stringr::str_trim(toupper(vClipTo))

      if ( is.null(clipTo) || any(is.na(clipTo)) || length(clipTo) == 0 ) {
         # not provided - set default
         clipTo    <- clipTo_def

      } else {
         SList            <- c("NONE", "DATA", NA, "SEER", "STATE", "REGION", "HSA", "TRUE","FALSE")
         #                      1       2      3     4       5       6         7      8      9
         SMatch           <- match(vClipTo,SList)
         if (is.na(SMatch)) {
            ErrFnd        <- TRUE
            xmsg          <- paste0("***094 The clipTo call parameter is ",fillTo,
                                   " and must be 'NONE', 'DATA', 'HSA', 'SEER', 'STATE', 'REGION' or 'TRUE'/'FALSE'.",
                                   " The default of 'NONE' will be used.")
            warning(xmsg, call.=FALSE)
            clipTo        <- clipTo_def
         } else {
            # good value
            clipTo        <- SList[SMatch]
            clipToNum     <- SMatch
            if (SMatch == 8) {
               # "TRUE"   ->  "DATA"
               clipTo     <- "DATA"
               clipToNum  <- 2
            }
            if (SMatch == 9) {
               # "FALSE" turn off  -> "NONE"
               clipTo     <- "NONE"
               clipToNum  <- 1
            }
            # caller did specify this parameter.
            clipTo_caller <- TRUE
         }
      }
      callVarList$clipTo         <- clipTo
      callVarList$clipToNum      <- clipToNum
      callVarList$clipTo_caller  <- clipTo_caller
      rPM$clipTo                 <- clipTo
      rPM$clipToNum              <- clipToNum
      rPM$clipTo_caller          <- clipTo_caller
      #
      #
      #   Later check the value against the data level.
      #
      #
      ####

      ####
      #
      #  Step 10 - dataBCol Parameter  (100-102)
      #
      
      #  Set Default based on the data mode (idMode)
      
      dataBCol_def          <- rPM$ColorB_Data
      dataBCol_caller       <- FALSE
            
      if ( is.null(dataBCol) || any(is.na(dataBCol)) || length(dataBCol) == 0 ) {
         # not provided - set default
         dataBCol           <- dataBCol_def
    
      } else {
         dataBCol           <-  stringr::str_trim(dataBCol)
         iR                 <-  is.Color(dataBCol)   # test to see if it is a color value
         if (!iR) {
            # not a color
            ErrFnd          <- TRUE
            xmsg            <- paste0("***100 The dataBCol call parameter is not a valid color: ",dataBCol,", ",
                                      " The default of 'black' will be used.")
            warning(xmsg, call.=FALSE)
            dataBCol        <- dataBCol_def
         } else {
            # good value
            dataBCol_caller <- TRUE   # caller set value.
         }
      }
      callVarList$dataBCol         <- dataBCol
      callVarList$dataBCol_caller  <- dataBCol_caller
      rPM$dataBCol                 <- dataBCol
      rPM$dataBCol_caller          <- dataBCol_caller

      #
      #####

      #####
      #
      #  Step 11 - verify mTitle parameter  (105-107)
      #
      mTitle_def   <- NA
      
      if ( ( is.null(mTitle) || any(is.na(mTitle)) || length(mTitle) == 0 ) ) {
         mTitle    <- mTitle_def
      } else {
         # we have a title for the graph
         if (is.character(mTitle)) {
            # it's a character string vector
            ilen   <- length(mTitle)   # get number of title lines

            if (ilen == 0) {
              # Title empty (no elements)
              mTitle  <- mTitle_def   # adjust and accept it.

            } else {
               if (ilen > 2) {
                 xmsg      <- paste0("***105 The 'mTitle' option can only contain one or two strings.",
                                         " Only the first two will be used.")
                 warning(xmsg, call.=FALSE)
                 mTitle     <- mTitle[1:2] # keep first two items.
               }
            }

         } else {
            xmsg <- paste0("***106 The 'mTitle' option is not a character vector. 'mTitle' parameter is ignored.")
            warning(xmsg)
            mTitle <- mTitle_def
         }
      }
      callVarList$mTitle <- mTitle
      rPM$mTitle         <- mTitle
      #
      #####

      #####
      #
      #  Step 12 - verify mTitle.cex parameter  (108-109)
      #
      mTitle.cex_def  <- 1
      
      if ( ( is.null(mTitle.cex) || any(is.na(mTitle.cex)) || length(mTitle.cex) == 0 ) ) {
         mTitle.cex <- mTitle.cex_def
      } else {
         # we have a title for the graph
         if (is.numeric(mTitle.cex)) {
            # it's a numeric vector
            mTitle.cex <- mTitle.cex[[1]][1]
            if (mTitle.cex <= 0 ||  mTitle.cex > 4) {
               xmsg <- paste0("***108 The 'mTitle.cex' call parameter is out of range (<=0 or >4). 'mTitle.cex' is set to the default of 1.")
               warning(xmsg)
               mTitle.cex  <- mTitle.cex_def 
            }
         } else {
            xmsg <- paste0("***109 The 'mTitle.cex' option is not a numeric value. The 'mTitle,cex' parameter is set to the default of 1.")
            warning(xmsg)
            mTitle.cex  <- mTitle.cex_def 
         }
      }
      
      callVarList$mTitle.cex <- mTitle.cex
      rPM$mTitle.cex         <- mTitle.cex
      #
      #####
      
      #####
      #
      #  Step 14 - Hatch Parameters
      #
      #  Step 14.1 - Verify user provided hatch parameter (logical or list)  (110-141)
      #    hatch and hatch2
    
      HatchFlag    <- FALSE     # disable hatching.
      Hatch2Flag   <- FALSE
      ErrFnd       <- FALSE
      hatch_caller <- FALSE
 
      # Set default settings
   
      H_SettingList <- c("dataCol", "ops", "value", "col" ,"lwd",  "density", "den", "angle", "range","incAngle","lab",
                         "hDataCol","hOps","hValue","hCol","hLwd", "hDensity","hDen","hAngle","hRange","")
 
      HOpsList      <- c("eq","ne","lt","le","gt","ge")  # char form Used twice UC and LC.
      HOpsList2     <- c("=", "<>","<", "=<",">", "=>")  # odd forms
      HOpsCode      <- c("==","!=","<", "<=",">" ,">=")  # good R forms
      HOpsTest      <- c(HOpsList,HOpsList2,HOpsCode)
      HOpsRCode     <- c(HOpsCode,HOpsCode, HOpsCode)
      
      #  Types of lines and equivalent number values.
      #HLtyTypes     <- c("blank","solid","dashed","dotted","dotdash","longdash","twodash","0","1","2","3","4","5","6")
      #           #        0     , 1     , 2      , 3      , 4       , 5        , 6

      # hatch # 1 specific
      H_dataColName <- "pValue"
      H_dataColNum  <- 0       # indicate it has not been looked up.
      H_dataCol     <- "pValue"   # set to default value - if not present this provides a value
      H_data        <- c()
      H_ops         <- ">"
      H_value       <- 0.05
      H_range       <- NA      # default if not present
      H_range_def   <- c(0,1)  # default if TRUE
      H_lab         <- "hatch#1"

      # hatch generic
      H_col         <- rPM$ColorB_hatching
      H_lwd         <- 0.85
      #H_lty         <- 1       # solid
      H_den         <- 25      # pattern density
      H_angle       <- 45      # pattern angle = 45 degree CCW
      H_incAngle    <- 60      # incremental angle for additional hatchs.

      if (debug) {
         cat("Hatching default setting Z-7480 \n")
         cat("   parameters -- dataCol:",H_dataColName," #:",H_dataColNum,"  ops:",H_ops,"  value:",H_value,"\n")
         cat("  range:",H_range,"  label:",H_lab,"\n")
         cat("  col  :",H_col,"  lwd:",H_lwd,"  den:",H_den,"  angle:",H_angle,
             "  IncAngle:",H_incAngle,"\n")
      }

      # hatch=list()

      if ( !(is.null(hatch) || any(is.na(hatch)) || length(hatch) == 0) ) {

         # Hatch value present - process it (logical or list)
         if (is.list(hatch)) {

            # is a list - process named entries
            HatchColNames  <- names(hatch)            # get list of settings
            HMatch         <- match(HatchColNames, H_SettingList)
            HList          <- hatch                   # get the list of settings.

            #  Check name list
            HMatch1        <- is.na(HMatch)           # Get T/F for bad setting names in list.
            if (any(HMatch1)) {
               # Have entries in list that are not valid
               HBadList    <- HatchColNames[HMatch1]  # get list of bad entries
               ErrFnd      <- TRUE
               xmsg        <- paste0("***115 The following hatch options are not valid and be ignored:",
                                         paste0(HBadList,collapse=", "))
               warning(xmsg, call.=FALSE)
               HList       <- hatch[!HMatch1]         # keep only good entries
               rm(HBadList, xmsg)
            }
            
            rm(HMatch1)

            if (length(HList)>0) {

               # still have values
               hatch_caller <- TRUE
               HatchFlag    <- TRUE                     # enable hatching.
                  # from this point on, if error HatchFlag would be set to FALSE.
                  # If so at the end, tell caller.

               HMatch       <- match(H_SettingList,names(HList))
               HMatch       <- HMatch[!is.na(HMatch)]   # get new order  (H_SettingsList order), an NA means setting is not in list.
               HList        <- HList[HMatch]            # reorder

               numHList     <- length(HList)
               namHList     <- names(HList)

               # step through lists in HList and check values and assign to settings.
               for (ind in c(1:numHList)) {

                  var      <- HList[[ind]]      # get the value of the list
                  nam      <- namHList[ind]     # get name of list
                  
                 #cat("hatch:",nam," <- ",var,"   \n")
                  
                  if (is.factor(var)) var <- as.character(var)   # get rid of factors, but it will create character values.
                  
                  if (debug) {
                     cat("Parsing List - name:",nam,"  value:",var,"\n")
                  }

                  ErrFnd   <- FALSE
                  
                  #
                  #   hatching dataCol optin 
                  #
                  if (nam == "dataCol" || nam == "hDataCol") {
                    #cat("hatch:dataCol  nam:",nam,"  var:",var,"\n")
                     
                     # ignore value on dataCol if NA - let default show through
                     if (!is.na(var)) {
                        #cat("h:var is not an NA.\n")
                        if (class(var) == "character" || class(var) == "numeric") {
                           ##cat("h:var is char or num.\n")
                           # either numeric or character (number or name)
                           H_dataCol    <- var[[1]][1]      # no tests, value checked later.
                           #cat("H_dataCol:",H_dataCol,"\n")
                           
                        } else {
                           # neither a numeric or character (not a number of column name)
                           #cat("not char or numeric\n")
                           
                           xmsg <- paste0("***116 The hatch option dataCol is not a character vector or numeric. The default value of ",H_dataCol," will be used.")
                           warning(xmsg, call.=FALSE)
                           ErrFnd    <- TRUE
                           
                        }  # end of type check
                     }  # end of NA check              
                  }  # end of hatch dataCol verification
                  #  The hatch:dataCol is checked against the ndf data.frame later.
                  #cat("hitch: dataCol - Done - H_dataCol:",H_dataCol,"\n")
                  
                  #
                  #   range option
                  #      values:   NA      no check range to be applied
                  #      c(l,h):   vector of low and high values
                  #
                  if (nam == "range" || nam == "hRange") {    # should be c(l,h) or NA?  (default - NA)
                     InvParm  <- FALSE
                     lenVar   <- length(var)
                     
                     if (lenVar == 1) {
                        # if length = 1, must be an NA.  (An NA is a logical value.)
                        if (!is.na(var)) {
                           # invalid form
                           InvParm <- TRUE
                           lenVar  <- -1
                        }
                        H_Range <- NA       # disable range checking
                     }  # end of len=1 check
                     
                     if (lenVar == 2) {
                        # OK! a vector with two elements - good form.
                        
                        if (class(var) == "numeric" || class(var) == "character") {
                        
                           #  one of the value may be NA, but let it go. Convert to numeric.
                           suppressWarnings(wVal <- as.numeric(var[1:2]))        # only take first two just to make sure.
                           
                           # possible range c(l,h) vector form (can be an NA)
                           # check for possible NA values.
                           if (any(is.na(wVal))) {
                              #  one of the values is an NA. Was an NA to start with or could not be converted to numeric.
                              #    Disable range change.
                              InvParm <- TRUE   # indicate invalid option format.  Tell them at the end.
                           } else {
                              # got length of two with numeric values.
                              if (wVal[1] > wVal[2]) {
                                 # range values are out of order.
                                 # if low and high are reverse - fix it.  Tell caller if debug set.
                                 if (debug) {
                                    xmsg <- paste0("***117 The hatch option range values are out of order.  First value must be",
                                                      " less than second value. Reversed.")
                                    warning(xmsg,call.=FALSE)
                                 }
                                 wVal <- rev(wVal)  # reverse the out of order values.
                              }
                              H_range <- wVal   # save the two range values.
                           }  # done 2 value NA check.       
                        } else {
                           #  Not a character or numeric value (even though it has a length of 2)
                           #  Could be all NAs and logical, but that's not good.
                           InvParm <- TRUE     # set flag.
                        } # end of len=2 type check
                     } # end of len=2 check.
                     
                     if (lenVar > 2) {
                        # invalid length of vector > 2.
                        InvParm  <- TRUE    # set flag
                     }
                     
                     #  check if any errors indicating the option was not good.
                     if (InvParm) {
                        xmsg       <- paste0("***118 The hatch option range is not valid. It must be NA or a",
                                                 " vector containing 2 numeric values (low and high limits) for the range.",
                                                 " Range checking is disabled.")
                        warning(xmsg, call.=FALSE)
                        H_range <- NA
                        rm(xmsg)
                     }
                  }  # end of range verification
                  
                  #
                  #   value option
                  #
                  if (nam == "value") {
                     # can be any type of variable.
                     wVal       <- var[[1]][1]
                  
                     if (is.factor(wVal))  wVal <- as.character(wVal)
                     H_value    <- wVal     # no tests. could be a string or numeric in the "range"
                  
                  }  # end of value verification
                  
                  #
                  #   ops option
                  #
                  if (nam == "ops" || nam == "hOps") {
                    
                    H_ops        <- as.character(var[[1]][1])  # get first element and make character
                    H_ops        <- tolower(H_ops)
                    HMatch       <- match(H_ops,HOpsTest)      # check character "ge", "lt".
                    
                    if (is.na(HMatch)) {
                       # no match with operation list
                       ErrFnd    <- TRUE
                       xmsg      <- paste0("***119 The comparison operator provided in the hatch ops options - ", H_ops,
                                               " - is not valid. Hatching disabled.")
                       warning(xmsg, call.=FALSE)
                       HatchFlag <- FALSE
                       rm(xmsg)
                    } else {
                       H_ops     <- HOpsRCode[HMatch]
                    }
                    rm(HMatch)
                  }  #  end of ops verification
                  
                  #
                  #   col setting
                  #
                  if (nam == "col" || nam == "hCol") {
                     #  hatching color
                     wCol       <- var[[1]][1]
                     if (!is.Color(wCol)) {
                        ErrFnd    <- TRUE
                        xmsg      <- paste0("***125 The hatch col option is not a valid color : ",wCol,
                                                   ". The default of ",H_col," will be used.")
                        warning(xmsg, call.=FALSE)
                        wCol      <- H_col  # get default
                        rm(xmsg)
                     }
                     H_col      <- wCol
                     rm(wCol)
                  }  # end of col options
                  
                  #
                  #  lwd option
                  #
                  if (nam == "lwd" || nam == "hLwd") {
                     vLwd    <- var[[1]][1]            # get first value
                     suppressWarnings(wLwd    <- as.numeric(vLwd))       # is it a number (convert)
                     if (is.na(wLwd)) {
                        # not a numeric or can't be converted to numeric.
                        ErrFnd       <- TRUE
                        xmsg         <- paste0("***126 The hatch lwd option is not numeric - ",vLwd,".",
                                                " The default will be used.")
                        warning(xmsg, call.=FALSE)
                        wLwd         <- H_lwd
                        rm(xmsg)
                     } else {
                        # valid numeric value
                        if ( wLwd < 0 || wLwd > 5 ) {
                           #  Out of range
                           ErrFnd    <- TRUE
                           xmsg      <- paste0("***127 The hatch lwd option is ",wLwd,
                                                   " and is out of the range  ( > 0 to <= 5).",
                                                   " The default value of 0.85 will be used.")
                           warning(xmsg, call.=FALSE)
                           wLwd      <- H_lwd
                           rm(xmsg)
                        }
                     }
                     H_lwd    <- wLwd
                     rm(vLwd, wLwd)
                  }  # end of lwd verification
                  
                  #
                  #  lty - option
                  #
                  #if (nam == "lty" || nam == "hLty") {
                  #   vLty    <- var[[1]][1]          # get first value
                  #   suppressWarnings( wLty    <- as.numeric(vLty) )    # is it a number
                  #   if (is.na(wLty)) {
                  #      # not a number (NA)
                  #      if (is.character(vLty)) {
                  #         # character type - check for name of type
                  #         vLtyMatch    <- match(vLty,HLtyTypes)   # is it a valid string type?
                  #         if (is.na(vLtyMatch)) {
                  #            # bad value in option
                  #            ErrFnd    <- TRUE
                  #            xmsg      <- paste0("***1xx The hatch lty setting is not valid - ",vLty,
                  #                                    " Check the par(lty) variable for acceptable values.",
                  #                                    " The default value of 'solid' will be used.")
                  #            warning(xmsg, call.=FALSE)
                  #            rm(xmsg)
                  #         } else {
                  #            # have a good value - match is the number to use.
                  #            H_lty      <- vLtyMatch - 1   # convert char to num
                  #         }
                  #      } else {
                  #         # not numeric or character
                  #         ErrFnd    <- TRUE
                  #         xmsg      <- paste0("***1xx The hatch lty setting is not numeric.",
                  #                                 " The default will be used.")
                  #         warning(xmsg, call.=FALSE)
                  #         rm(xmsg)
                  #      }
                  #   } else {
                  #      # it's a number
                  #      if ( wLty < 0 || wLty > 6 ) {
                  #         #  Out of range
                  #         ErrFnd    <- TRUE
                  #         xmsg      <- paste0("***1xx The hatch lty setting is ",wLty,
                  #                                 " and is out of the range (0 to 6).",
                  #                                 " The default valve of 'solid' will be used.")
                  #         warning(xmsg, call.=FALSE)
                  #         rm(xmsg)
                  #      } else {
                  #         H_lty  <- as.integer(wLty)
                  #      }
                  #   }
                  #   rm(vLty,wLty)
                  #} # end of lty verification
                  #
                  
                  #
                  #  density (den) - option (lines per inch)
                  #
                  if (nam == "density" || nam == "den" || nam == "hDensity" || nam == "hDen") {
                     vDen    <- var[[1]][1]          # get first value
                     suppressWarnings(wDen    <- as.numeric(vDen))     # is it a number (lines per inch)
                     if (is.na(wDen)) {
                        ErrFnd       <- TRUE
                        xmsg         <- paste0("***128 The hatch density setting is not numeric - ",vDen,
                                                " The default value of 25 will be used.")
                        warning(xmsg, call.=FALSE)
                        wDen         <- H_den
                        rm(xmsg)
                     } else {
                        if ( wDen < 5 || wDen > 64 ) {
                           #  Out of range
                           ErrFnd    <- TRUE
                           xmsg      <- paste0("***129 The hatch density setting is ",wDen,
                                                   " and is out of the range of > 4 to <= 64 lines per inch.",
                                                   " The default value of 25 will be used.")
                           warning(xmsg, call.=FALSE)
                           wDen      <- H_den
                           rm(xmsg)
                        }
                     }
                     H_den    <- wDen
                     rm(vDen, wDen)
                  } # end of den verification
 
                  #
                  #  angle - option (degrees)
                  #
                  if (nam == "angle" || nam == "hAngle") {
                     vAng    <- var[[1]][1]          # get first value
                     suppressWarnings(wAng    <- as.numeric(vAng))     # is it a number (lines per inch)
                     if (is.na(wAng)) {
                        ErrFnd       <- TRUE
                        xmsg         <- paste0("***130 The hatch angle option is not numeric.",
                                                " The default will be used.")
                        warning(xmsg, call.=FALSE)
                        wAng         <- H_angle
                        rm(xmsg)
                     } else {
                        if ( wAng < -360 || wAng > 360 ) {
                           #  Out of range
                           ErrFnd    <- TRUE
                           xmsg      <- paste0("***131 The hatch angle option is out of the range of => -360 to <= 360 degrees.",
                                                   " The default will be used.")
                           warning(xmsg, call.=FALSE)
                           wAng      <- H_angle
                           rm(xmsg)
                        }
                     }
                     H_angle    <- wAng
                     rm(vAng, wAng)
                  } # end of angle verification

                  #
                  #  incAngle - option (degrees)
                  #
                  if (nam == "incAngle" ) {
                     vIncAng    <- var[[1]][1]          # get first value
                     suppressWarnings(wIncAng    <- as.numeric(vIncAng))     # is it a number (lines per inch)
                     if (is.na(wIncAng)) {
                        ErrFnd       <- TRUE
                        xmsg         <- paste0("***132 The hatch incremental angle option (incAngle) is not numeric.",
                                                " The default will be used.")
                        warning(xmsg, call.=FALSE)
                        wIncAng         <- H_incAngle
                        rm(xmsg)
                     } else {
                        if ( wIncAng < -120 || wIncAng > 120 ) {
                           #  Out of range
                           ErrFnd    <- TRUE
                           xmsg      <- paste0("***133 The hatch incremental angle setting is out of the range of => -120 to <= 120 degrees.",
                                                   " The default will be used.")
                           warning(xmsg, call.=FALSE)
                           wIncAng      <- H_angle
                           rm(xmsg)
                        }
                     }
                     H_incAngle    <- wIncAng
                     rm(vIncAng, wIncAng)
                  } # end of angle verification

                  #
                  #  hatching label
                  #
                  hLab_def <- "hatch1"
                  
                  if (nam == "lab" ) {
                     vLab    <- var[[1]][1]          # get first value
                     wLab    <- as.character(vLab)     # make sure it's characters
                     if (is.null || is.na(wLab)) {
                        # no label - ignore parameter
                        H_lab <- hLab_def
                     }
                     H_lab    <- hLab_def
                  } # end of Label verification for what it is.


               }  # end of options "for" loop.
 
            }  # end of hatch option verification

            if (!HatchFlag) {
               xmsg <- paste0("***134 A hatch call parameter error was detected.",
                                  "  The hatch parameter is disabled. See previous messages for details.")
               warning(xmsg,call.=FALSE)
            }
            # end of hatching "list" of options check
         } else {
            # not a list, must be a logical
            if (is.logical(hatch)) {
               # we have a logical value
               HatchFlag    <- hatch        # copy to run flag.  TRUE or FALSE
               hatch_caller <- TRUE         # caller requested
            } else {
               # not a list or logical
               ErrFnd       <- TRUE
               xmsg         <- paste0("***135 The hatch call parameter must be a logical value (T/F)",
                                          " or a list of options. Parameter is ignored.")
               warning(xmsg, call.=FALSE)
               HatchFlag    <- FALSE
               rm(xmsg)
            }
         }  # end of hatch parameter check
      } else {
         # hatch list or T/E  is NULL or NA or empty (length=0)
         HatchFlag <- FALSE   # disable hatching.
      
      } # end of hatch parameter processing.
      
      #
      #  End of validation of user provided parameter hatch
      #
      rPM$HatchFlag    <- HatchFlag
      rPM$hatch_caller <- hatch_caller
      #             Specific
      callVarList$hatch  <- list(hDataCol     = H_dataCol,
                                 hDataColName = H_dataColName,
                                 hDataColNum  = H_dataColNum,
                                 hData        = H_data,
                                 hOps         = H_ops, 
                                 hValue       = H_value,
                                 hRange       = H_range,
                                 hLab         = H_lab,
                          # Generic
                                 hCol         = H_col, 
                                 hLwd         = H_lwd, 
                                 hDen         = H_den,
                                 hAngle       = H_angle,
                                 hIncAngle    = H_incAngle
                                )
      rPM$hatch <- callVarList$hatch
      hatch     <- rPM$hatch
      # print(str(rPM$hatch))
   
      #
      #####
      
      #####
      #
      #   hatch2=list()
      #
      Hatch2Flag   <- FALSE     # disable hatching.
      hatch2_caller<- FALSE

      # set default values.
      H_dataColName <- "pValue"
      H_dataColNum  <- 0       # indicate it has not been looked up.
      H_dataCol     <- "pValue"   # set to default value - if not present this provides a value
      H_data        <- c()
      H_ops         <- ">"
      H_value       <- 0.05
      H_range       <- NA      # default if not present
      H_range_def   <- c(0,1)  # default if TRUE
      H_lab         <- "hatch#2"

      # calculated from harch not in options list.
      H_angle       <- H_angle + H_incAngle   # base off of hatch= options.
 
      H2_SettingList <- c("dataCol", "ops", "value", "range","lab")
  
      if ( !(is.null(hatch2) || any(is.na(hatch2)) || (length(hatch2) == 0)) ) {

         # Hatch value present - process it (logical or list)
         if (is.list(hatch2)) {

            # is a list - process named entries
            HatchColNames  <- names(hatch2)            # get list of settings
            HMatch         <- match(HatchColNames, H2_SettingList)
            HList          <- hatch2                   # get the list of settings.

            #  Check name list
            HMatch1        <- is.na(HMatch)           # Get T/F for bad setting names in list.
            if (any(HMatch1)) {
               # Have entries in list that are not valid
               HBadList    <- HatchColNames[HMatch1]  # get list of bad entries
               ErrFnd      <- TRUE
               xmsg        <- paste0("***120 The following hatch2 options are not valid and be ignored:",
                                         paste0(HBadList,collapse=", "))
               warning(xmsg, call.=FALSE)
               HList       <- hatch[!HMatch1]         # keep only good entries
               rm(HBadList, xmsg)
            }
            
            rm(HMatch1)

            if (length(HList)>0) {

               # still have values
               hatch2_caller <- TRUE
               Hatch2Flag    <- TRUE                     # enable hatching.
                  # from this point on, if error HatchFlag would be set to FALSE.
                  # If so at the end, tell caller.

               HMatch       <- match(H2_SettingList,names(HList))
               HMatch       <- HMatch[!is.na(HMatch)]   # get new order  (H_SettingsList order), an NA means setting is not in list.
               HList        <- HList[HMatch]            # reorder

               numHList     <- length(HList)
               namHList     <- names(HList)

               # step through lists in HList and check values and assign to settings.
               for (ind in c(1:numHList)) {

                  var      <- HList[[ind]]      # get the value of the list
                  nam      <- namHList[ind]     # get name of list
                  
                 #cat("hatch:",nam," <- ",var,"   \n")
                  
                  if (is.factor(var)) var <- as.character(var)   # get rid of factors, but it will create character values.
                  
                  if (debug) {
                     cat("Parsing hatch2 List - name:",nam,"  value:",var,"\n")
                  }

                  ErrFnd   <- FALSE
                  
                  #
                  #   hatching dataCol optin 
                  #
                  if (nam == "dataCol") {
                    #cat("hatch:dataCol  nam:",nam,"  var:",var,"\n")
                     
                     # ignore value on dataCol if NA - let default show through
                     if (!is.na(var)) {
                        #cat("h:var is not an NA.\n")
                        if (class(var) == "character" || class(var) == "numeric") {
                           ##cat("h:var is char or num.\n")
                           # either numeric or character (number or name)
                           H_dataCol    <- var[[1]][1]      # no tests, value checked later.
                           #cat("H_dataCol:",H_dataCol,"\n")
                           
                        } else {
                           # neither a numeric or character (not a number of column name)
                           #cat("not char or numeric\n")
                           
                           xmsg <- paste0("***121 The hatch2 option dataCol is not a character vector or numeric. The default value of ",H_dataCol," will be used.")
                           warning(xmsg, call.=FALSE)
                           ErrFnd    <- TRUE
                           
                        }  # end of type check
                     }  # end of NA check              
                  }  # end of hatch dataCol verification
                  #  The hatch:dataCol is checked against the ndf data.frame later.
                  #cat("hitch: dataCol - Done - H_dataCol:",H_dataCol,"\n")
                  
                  #
                  #   range option
                  #      values:   NA      no check range to be applied
                  #      c(l,h):   vector of low and high values
                  #
                  if (nam == "range") {    # should be c(l,h) or NA?  (default - NA)
                     InvParm  <- FALSE
                     lenVar   <- length(var)
                     
                     if (lenVar == 1) {
                        # if length = 1, must be an NA.  (An NA is a logical value.)
                        if (!is.na(var)) {
                           # invalid form
                           InvParm <- TRUE
                           lenVar  <- -1
                        }
                        H_Range <- NA       # disable range checking
                     }  # end of len=1 check
                     
                     if (lenVar == 2) {
                        # OK! a vector with two elements - good form.
                        
                        if (class(var) == "numeric" || class(var) == "character") {
                        
                           #  one of the value may be NA, but let it go. Convert to numeric.
                           suppressWarnings(wVal <- as.numeric(var[1:2]))        # only take first two just to make sure.
                           
                           # possible range c(l,h) vector form (can be an NA)
                           # check for possible NA values.
                           if (any(is.na(wVal))) {
                              #  one of the values is an NA. Was an NA to start with or could not be converted to numeric.
                              #    Disable range change.
                              InvParm <- TRUE   # indicate invalid option format.  Tell them at the end.
                           } else {
                              # got length of two with numeric values.
                              if (wVal[1] > wVal[2]) {
                                 # range values are out of order.
                                 # if low and high are reverse - fix it.  Tell caller if debug set.
                                 if (debug) {
                                    xmsg <- paste0("***122 The hatch2 range option values are out of order.  First value must be",
                                                      " less than second value. Reversed.")
                                    warning(xmsg,call.=FALSE)
                                 }
                                 wVal <- rev(wVal)  # reverse the out of order values.
                              }
                              H_range <- wVal   # save the two range values.
                           }  # done 2 value NA check.       
                        } else {
                           #  Not a character or numeric value (even though it has a length of 2)
                           #  Could be all NAs and logical, but that's not good.
                           InvParm <- TRUE     # set flag.
                        } # end of len=2 type check
                     } # end of len=2 check.
                     
                     if (lenVar > 2) {
                        # invalid length of vector > 2.
                        InvParm  <- TRUE    # set flag
                     }
                     
                     #  check if any errors indicating the option was not good.
                     if (InvParm) {
                        xmsg       <- paste0("***123 The hatch2 range option is not valid. It must be NA or a",
                                                 " vector containing 2 numeric values (low and high limits) for the range.",
                                                 " Range checking is disabled.")
                        warning(xmsg, call.=FALSE)
                        H_range <- NA
                        rm(xmsg)
                     }
                  }  # end of range verification
                  
                  #
                  #   value option
                  #
                  if (nam == "value") {
                     # can be any type of variable.
                     wVal       <- var[[1]][1]
                  
                     if (is.factor(wVal))  wVal <- as.character(wVal)
                     H_value    <- wVal     # no tests. could be a string or numeric in the "range"
                  
                  }  # end of value verification
                  
                  #
                  #   ops option
                  #
                  if (nam == "ops" ) {
                    
                    H_ops        <- as.character(var[[1]][1])  # get first element and make character
                    H_ops        <- tolower(H_ops)
                    HMatch       <- match(H_ops,HOpsTest)      # check character "ge", "lt".
                    
                    if (is.na(HMatch)) {
                       # no match with operation list
                       ErrFnd    <- TRUE
                       xmsg      <- paste0("***124 The comparison operator provided in the hatch2 ops option - ", H_ops,
                                               " - is not valid. Hatching disabled.")
                       warning(xmsg, call.=FALSE)
                       InvParm   <- TRUE
                       Hatch2Flag <- FALSE
                       rm(xmsg)
                    } else {
                       H_ops     <- HOpsRCode[HMatch]
                    }
                    rm(HMatch)
                  }  #  end of ops verification
                  
                  #
                  #  hatching label
                  #
                  hLab_def <- "hatch1"
                    
                    if (nam == "lab" ) {
                       vLab    <- var[[1]][1]          # get first value
                       wLab    <- as.character(vLab)     # make sure it's characters
                       if (is.null || is.na(wLab)) {
                          # no label - ignore parameter
                          H_lab <- hLab_def
                       }
                       H_lab    <- hLab_def
                    } # end of Label verification for what it is.


               }  # end of options "for" loop for hatch2
 
            }  # end of hatch option verification

            if (!Hatch2Flag) {
               xmsg <- paste0("***136 A hatch2 call parameter error was detected.",
                                  "  The hatch2 parameter is disabled. See previous messages for details.")
               warning(xmsg,call.=FALSE)
            }
            # end of hatching "list" of options check
         } else {
            # not a list, nothing else is value on this one.
            # not a list or logical
            ErrFnd       <- TRUE
            xmsg         <- paste0("***137 The hatch2 call parameter must be a list of options.",
                                       " Parameter is ignored.")
            warning(xmsg, call.=FALSE)
               Hatch2Flag    <- FALSE
               rm(xmsg)
           
         }  # end of hatch parameter check
      } else {
         # hatch list or T/E  is NULL or NA or empty (length=0)
         Hatch2Flag <- FALSE   # disable hatching.
      
      } # end of hatch2 parameter processing.
      
      #
      #  End of validation of user provided parameter hatch2
      #
      rPM$Hatch2Flag      <- Hatch2Flag
      rPM$hatch2_caller   <- hatch2_caller
      #                Specific 
      callVarList$hatch2  <- list(hDataCol    = H_dataCol,
                                 hDataColName = H_dataColName,
                                 hDataColNum  = H_dataColNum,
                                 hData        = H_data,     
                                 hOps         = H_ops, 
                                 hValue       = H_value,
                                 hRange       = H_range,
                                 hLab         = H_lab,
                     # Generic
                                 hCol         = H_col,      # copy from hatch  
                                 hLwd         = H_lwd,      # copy from hatch 
                                 hDen         = H_den,      # copy from hatch
                                 hAngle       = H_angle     # calculated from hatch
                              )
      rPM$hatch2 <- callVarList$hatch2
      hatch2 <- rPM$hatch2
      #print(str(rPM$hatch2))
            
      #
      #####

      #####
      #
      #  Step 15 - mLegend Parameter and Options list (150-179)
      #
      #  Step 15.1 - setup defaults and verify parameters are present.
      #
      #  Set default to be changed if required.
      #cat("Step 15.1\n")

      mLegend_caller <- FALSE
      mLegendFlag    <- TRUE
      mLegendOpt     <- mLegend
      lSize          <- 0.85
      lNoValue       <- FALSE
      lNumCols       <- 3
      lCounts        <- FALSE
      lPos           <- "left"
      lPosv          <- "bottomleft"
      lPch           <- 22
      lLabels        <- ""   # no override labels for categories.  # future implementation 
      
      L_SettingList  <- c("counts",   "size",     "numCols","pos",      "noValue",    "pch",    "labels")

      L_SettingOld   <- c("legendCnt","legendCex","cex",    "legendPos","legendColn", "ncol",   "nCol",   "numColumns")
      L_TestList     <- c(L_SettingList,L_SettingOld)

      L_SettingMap   <- c("counts",   "size",     "size",   "pos"      ,"numCols",    "numCols","numCols","numCols"   )
      L_ResList      <- c(L_SettingList,L_SettingMap)

      if (debug) {
        cat("Legend Def Settings Z-8259 flag:",mLegendFlag,
                " size:",   lSize,  " numCols:",  lNumCols,
                " pos:",    lPos,   " ", lPosv, 
                " pch:",    lPch, 
                " counts:", lCounts," noValue:",  lNoValue, "\n")
      }

      if ( is.null(mLegend) ) {
         # mLegend is NULL - no caller provided value
         mLegend     <- TRUE    #set to default of TRUE and use defaults values for legend.
         mLegendFlag <- TRUE
         mLegendOpt  <- NULL
      } else {
         # test if length=1 and is set to NA.
         if ( length(mLegend) == 1 && any(is.na(mLegend)) ) {
            #xmsg          <- paste0("***150 The 'mLegend' parameter is set to NA.",
            #                            " Set to the default of TRUE.")
            #message(xmsg, call.=FALSE)

            # Not really an error, just an indicator to set the defaults..
            mLegend         <- TRUE
            mLegendOpt      <- NULL
            mLegendFlag     <- TRUE
         }
      }
      # the mLegend parameter is present but not a list.
      if (is.logical(mLegend)) {
         # found logical value
         mLegendFlag      <- mLegend[[1]][1]   # get first value.
         #  Use the default value
         mLegendOpt       <- NULL              # signal no options.
         #cat("mLegend=",mLegendFlag," not a list.\n")
         mLegend_caller   <- TRUE
      } else {
         # not a logical - better be a list
         if (is.list(mLegend)) {
            # save named list information.
            mLegendOpt    <- mLegend
            mLegendFlag   <- TRUE
            #cat("mLegendOpt:\n")
            #print(str(mLegendOpt))
            
         } else {
            # not a list or logical - bad format.
            ErrFnd        <- TRUE
            xmsg          <- paste0("***152 The 'mLegend' call parameter must be a list or logical value.",
                                       " Parameter Ignored.")
            warning(xmsg, call.=FALSE)
            mLegendFlag   <- TRUE    # will be using defaults.
            mLegendOpts   <- NULL
         } # end of test list
      } # end of test logical or list or error test.
      
      ErrFnd    <- FALSE

      #
      #  Step 15.2 - check the options provided.
      #
      #cat("Step 15.2\n")

      #  mLegend defaults that may be needed.  - mLegend default is ON!
      
      if (mLegendFlag) {
         if (!is.null(mLegendOpt)) {  # this test is to see if mLegend=NULL,
            # check for legend list of options  (If T/F  mLegendOpt is set to NULL)
            
            # remove NA and NULL entries in list.
            wNANULLs       <- (is.na(mLegendOpt) | sapply(mLegendOpt, function(x) is.null(x)))      # get list of any option set to NA.
            mLegendOpt     <- mLegendOpt[!wNANULLs]   # remove NAs
            
            if (length(mLegendOpt) >= 1) {   
               #  process options list - its not empty
            
               mLegendOptNames <- names(mLegendOpt)       # get list of names from list
               #cat("mLegendOpt names:",mLegendOptNames,"\n")
                  
               if (is.null(mLegendOptNames)) {
                  # no names used in option list
                  ErrFnd      <- TRUE
                  xmsg        <- paste0("***153 The mLegend options list does not names for each list entry.",
                                          "  The format must be mLegend=list(pos='left',numCols=4). Defaults used."
                                               )
                  warning(xmsg, call.=FALSE)
                  
               } else {
                  #cat("L_TestList:",L_TestList,"\n")
                  #cat("mLegendOptNames:",mLegendOptNames,"\n")
                     
                  lMatch         <- match(mLegendOptNames,L_TestList)
      
                  badMatches     <- is.na(lMatch)  # list (T) of not match.

                  if (any(badMatches)) {
                     # one or more options in the mLegend option list is not valid.
                     BadList     <- mLegendOptNames[badMatches]   # get list
                     ErrFnd      <- TRUE
                     xmsg        <- paste0("***154 The following options in the mLegend parameter",
                                               " are not valid and will be ignored:",
                                               paste0(BadList,collapse=", "),".")
                     warning(xmsg, call.=FALSE)
                     rm(BadList, xmsg)
                  }
                     
                  mLegendOptNames <- mLegendOptNames[!badMatches]   # get list of good options
                              
                  rm(badMatches)
                     
                  if (length(mLegendOptNames) > 0) {
                  
                     mLegend_caller  <- TRUE

                     # 
                     # Process the named lists we have in the mLegend list.
                     #
                        
                     #
                     # counts value - include legend count values
                     #
                     optValue       <- mLegendOpt$counts                        # primary
                     if (is.null(optValue))  optValue <- mLegendOpt$legendCnt   # alternate
                     optValue       <- optValue[[1]][1]
                     
                     lCounts_def    <- FALSE      # set default
                     if (!is.null(optValue)) {
                        if (!is.logical(optValue)) {
                           xmsg        <- paste0("***156 The mLegend parameter 'counts' must a logical variable.",
                                                        " Set to FALSE.")
                           warning (xmsg, call.=FALSE)
                           ErrFnd      <- TRUE
                           lCounts     <- lCounts_def
                           rm(xmsg)
                        } else {
                           if (any(is.na(optValue))) {
                              # NA is a logical value
                              lCounts  <- lCounts_def
                           } else {
                              lCounts     <- optValue
                              if (lCounts && lNoValue) {
                                 # noValue TRUE, turn it off - duplications
                                 lNoValue <- FALSE
                              }
                           }
                        }
                     }
                     
                     #
                     #  legend text size
                     #
                     optValue  <- mLegendOpt$size
                     if (is.null(optValue))  optValue <- mLegendOpt$cex
                     if (is.null(optValue))  optValue <- mLegendOpt$legendCex
                     optValue       <- optValue[[1]][1]
                     
                     lSize    <- 0.85     # default value.
                     if (!is.null(optValue)) {
                     
                        suppressWarnings(optValue <- as.numeric(optValue))   # make numeric
                        if (any(is.na(optValue))) {
                           # not a number can't convert
                           ErrFnd    <- TRUE
                           xmsg      <- paste0("***158 The mLegend parameter 'size' option must",
                                                " be a numeric value The default is used.")
                           warning(xmsg, call.=FALSE)
                           lSize <- 0.85     # set to default
                           rm(xmsg)
                        } else {
                           # we have a good number
                           ####changed####
                           if (optValue <= 0.1 || optValue >= 5) {
                              ErrFnd    <- TRUE
                              xmsg      <- paste0("***160 The mLegend parameter 'size' option must be in the",
                                                      " range from 0.1 to 5. Set to 0.85.")
                              warning(xmsg, call.=FALSE)
                              lSize    <- 0.85  # set to default
                              rm(xmsg)
                           } else {
                              lSize    <- optValue
                           }
                           }
                     }
                                          
                     #
                     #  legend number of columns
                     #
                     optValue <- mLegendOpt$numCols
                     if (is.null(optValue))  optValue <- mLegendOpt$ncol           # check alternative
                     if (is.null(optValue))  optValue <- mLegendOpt$nCol           # check alternative
                     if (is.null(optValue))  optValue <- mLegendOpt$numColumns     # check alternative
                     if (is.null(optValue))  optValue <- mLegendOpt$legendColn     # check alternative
                     optValue          <- optValue[[1]][1]
                     
                     lNumCols          <- 3   # set the default
                     if (!is.null(optValue)) {
                        suppressWarnings(optValue       <- as.numeric(optValue))
                     
                        if (any(is.na(optValue)) || optValue < 1 || optValue > 8) {
                           xmsg        <- paste0("***162 The mLegend parameter option 'numColumns' must be",
                                                    " numeric and between 1 and 8. Set to 3.")
                           warning(xmsg)
                           ErrFnd      <- TRUE
                           lNumCols    <- 3  # set to default
                           rm(xmsg)
                        } else {
                           lNumCols    <-  optValue
                        }
                     } # end of numCols
                     
                     #
                     #   legend position  - enhance later to include other positions.
                     #
                     #  Tables of the values.
                     ValidPos <- c("left", "center", "right")               # parameter values
                     LegPos   <- c("bottomleft", "bottom", "bottomright",
                                   "left",       "center", "right",
                                   "topleft",    "top",    "topright")      # legend options
                     
                     optValue       <- mLegendOpt$pos                        # get option entered (?)
                     if (is.null(optValue)) { optValue <- mLegendOpt$legendPos }   # check alternative
                     
                     optValue       <- optValue[[1]][1]
                     
                     lPos          <- "left"       # set default
                     mPosInx        <- 1            # index into legend equivalent name table
                     
                     if (!is.null(optValue)) {
                        # have a value to evaluate
                     
                        optValue    <- tolower(stringr::str_trim(optValue))  # option now character no matter what it was.
                     
                        mPosInx     <- match(optValue,ValidPos)             # find match - validate
                        lPosHere    <- !is.na(mPosInx)                      # found an answer?
                     
                        #cat("legend pos:",optValue,"  mPosInx:",mPosInx,"  lPosHere:",lPosHere,"\n")
                     
                        if (!lPosHere) {
                           # didn't match name list
                           xmsg      <- paste0('***164 The legendPos parameter is not "left", "center", or "right", Set to "left".')
                           warning(xmsg, call.=FALSE)
                           ErrFnd    <- TRUE
                           lPos      <- "left"   # set to default
                           mPosInx   <- 1
                           rm(xmsg)
                        } else {
                           # valid name
                           lPos      <- optValue
                           # mPosInx already set properly.
                        }
                     } # end of pos=
                     
                     lPosv        <-  LegPos[mPosInx]            # get legend call position value
                     # now the position is "bottomleft", "bottom", or "bottomright"
                     
                     if (debug) {
                        cat("Position:",lPos,"  ",mPosInx," ",lPosv,"\n")
                     }
                     
                     #
                     #   Future - pch value   1 to 25.    (Future symbol option for the legend.)
                     #
                     
                     optValue <- mLegendOpt$pch[[1]][1]
                     
                     lPch     <- 22                  # default value
                     
                     if (!is.null(optValue)) {
                        if (typeof(optValue) != "numeric") {
                           xmsg        <- paste0("***166 The pch parameter must be a numeric value. Set to 19.")
                           warning(xmsg, call.=FALSE)
                           ErrFnd      <- TRUE
                           lPch       <- 22
                        } else {
                           if (optValue < 19 || optValue > 25) {
                              xmsg      <- paste0("***167 The pch parameter must be a value between 19 and 25. Set to 19.")
                              warning(xmsg, call.=FALSE)
                              ErrFnd    <- TRUE
                              lPch     <- 22
                           } else {
                              lPch     <- optValue
                           }
                        }
                     } # end of pch=
                     
                     #
                     #   legend noValue option
                     #
                     optValue <- mLegendOpt$noValue[[1]][1]
                       
                     lNoValue_def  <- FALSE    # default value
                     
                     if (!is.null(optValue)) {
                        if (typeof(optValue) != "logical") {
                           xmsg       <- paste0("***168 The noValue parameter must be a logical (TRUE or FALSE) value. Set to FALSE.")
                           warning(xmsg, call.=FALSE)
                           ErrFnd     <- TRUE
                           lNoValue   <- lNoValue_def
                        } else {
                           if (any(is.na(optValue))) {
                              ErrFnd  <-TRUE
                              lNoValue <- lNoValue_def
                           } else {
                              lNoValue   <- optValue
                              if (lCounts) lNoValue <- FALSE
                           }
                        }
                     } # end of noValue 
                     
                     #
                     #   labels = override legend labels.
                     #     (Move into plot code. Can't do the length check until the categories are set.)
                     #
                     optValue   <- mLegendOpt$labels
                     
                     lLabels   <- ""   # no override
                     #
                     #  Future implementation and Testing.  Not functional
                     #  need code to get first vector of labels.  array, matrix and vector
                     #  should be simple,  List and DataFrame are complex.
                     #
                     
                     if (!is.null(optValue)) {
                        if (typeof(optValue) != "character") {
                           xmsg        <- paste0("***170 The labels parameter must be a vector of character strings.",
                                                     " Set to an empty string.")
                           warning(xmsg, call.=FALSE)
                           ErrFnd      <- TRUE
                           lLabels     <- ""
                        } else {
                           if (any(is.na(lLabels))) {
                               lLabels <- ""
                           } else {
                              # leave length check until categ= is processed.
                              lLabels     <- optValue
                           }
                        }
                     } # end of labels=
                  } # length check
               
               }   # end of check for names 
            } else {
            
               # mLegend options list existed but has a length of zero (empty-NULL).
               xmsg        <- paste0("***172 The mLegends parameter is an empty list. ",
                                          "The legend will be drawn using default values.")
               warning(xmsg, call.=FALSE)
               
            }  # end of check for options.
            # end of mLegend Option Processing.
            
         }  # mLegend is not NULL (end of section)
      }          # end of legendFlag
      
      #
      #  Step 15.3 - list mLegend options.
      #
      #cat("Step 15.3\n")
   
      if (mLegendFlag) {
         # print out legend settings:
         if (debug) {
           cat("mLegend Settings Z-8610 counts:", lCounts, " size:", lSize,
                    " numCols:", lNumCols,
                    " pos:", lPos, " ", lPosv, " pch:", lPch, " noValue:", lNoValue,"\n")
         }
      }

      #
      #  Step 15.4 - save mLegend listing into rPM structure and callVarList.
      #
      #cat("Step 15.4\n")

      rPM$mLegendFlag      <- mLegendFlag
      rPM$mLegend_caller   <- mLegend_caller
      callVarList$mLegend  <- list(lCounts = lCounts,
                                   lSize   = lSize,
                                   lNumCols= lNumCols,
                                   lPos    = lPos,
                                   lPosv   = lPosv,
                                   lNoValue= lNoValue,
                                   lPch    = lPch,
                                   lLabels = lLabels
                                 )

      rPM$mLegend <- callVarList$mLegend
      mLegend     <- rPM$mLegend
      
      #print(callVarList$mLegend)

      #
      #####
      
      #####
      #
      #  Step 16 - Verify user info ndf data.frame is present  (030-039)
      #
      #  Is NDF viable.
      #
      #  a) Make sure data.frame is present
      #  b) is it a data.frame?
      #  c) Get list of column names.
      #
      #cat("Step 16\n")
      
      ErrFnd <- FALSE

      dfGood <- TRUE   #  verify df is present and good

      if (is.null(ndf)) {
         dfGood       <- FALSE
         xmsg         <- paste0("***030 The first parameter should be the statistics data.frame, but is missing or NULL.")
         warning(xmsg, call.=FALSE)
         ErrFnd       <- TRUE
      } else {
         # do we have a data.frame?
         if (typeof(ndf) != "list" || class(ndf) != "data.frame") {
            dfGood    <- FALSE
            xmsg      <- paste0("***031 ",ndfName," parameter is not a correctly formed data.frame.")
            warning(xmsg, call.=FALSE)
            ErrFnd    <- TRUE
         } else {
            # good data.frame
            if (!ncol(ndf) > 2) {
               if (idCol == "row.names") {
                   # handle case where the row.names has the IDs, so only one more column is required.
                   if (ncol(ndf) < 1) {
                     xmsg     <- paste0("***032 ",ndfName," data.frame muat have at least one column for Data.")
                     warning(xmsg, call.=FALSE)
                     ErrFnd   <- TRUE
                  }
               
               } else {
                  # idCol is a column in the data.frame (need at last two columns)
                  if (ncol(ndf) < 2) {
                     xmsg     <- paste0("***033 ",ndfName," data.frame muat have at least two columns for ID and Data.")
                     warning(xmsg, call.=FALSE)
                     ErrFnd   <- TRUE
                  }
                  if (nrow(ndf) < 1) {
                     dfGood = FALSE
                     xmsg     <- paste0("***034 ",ndfName," data.frame does not have any data rows. Must have at least one row of data.")
                     warning(xmsg, call.=FALSE)
                     ErrFnd   <- TRUE
                  }
               }
            }
         }
      }

      if (ErrFnd) {
         stop("***990 ERRORS found in statistic data.frame checking - Run Terminated")
      }
      
      # have a good ndf - get column names

      ndfColNames  <- colnames(ndf)  # get list of column names

      if (debug) {
         cat("ndfColNames Z-8707 ",paste0(ndfColNames,collapse=", "),"\n")
         cat("ndf dim:",dim(ndf),"\n")
         str(ndf)
      }
      
      #str(ndf)

      rPM$ndfColNames  <- ndfColNames      
      callVarList$ndf  <- ndf
      rPM$ndf          <- ndf
      rPM$ndfColMax    <- dim(ndf)[2]   # get number of columns.
      #
      #####

      #####
      #
      #  Step 17 - Validate and process column references (idCol, dataCol, H:dataCol and H2:dataCol
      #
     
      ErrFnd          <- FALSE
      StopFnd         <- FALSE
      
      #
      #  Step 17.1 - idCol parameter and column  (040-045)
      #
      #cat("Step 17  - Z-8732 \n")
     
      #  Changed 16/10/02 - added support for idCol as the column number.
      #   Also correct code to validate idCol as column name and to access
      #   first element of vector, matrix (numeric), list, and data.frame.
      #
      idCol_def        <- "FIPS"         # set default value (character)

      if ( is.null(idCol) ) {
         # idCol name is not present - assign default
         idCol         <- idCol_def
      }
      save_idCol       <- idCol
      idCol            <- idCol[[1]][1]      # get first element.
      
      if ( any(is.na(idCol)) ) {
         # idCol name is an NA
         idCol         <- idCol_def
      }
      if (idCol == "row.names") {
         # pull out the row.names  
         ndf$XXXrnid   <- row.names(ndf)         # <- use column name that will not conflict with user's 
         idCol         <- "XXXrnid"              #  set up 
         ndfColNames   <- names(ndf)
      }
            
      rPM$ndfColNames  <- ndfColNames  # they have changee -update rPM.
      
      ##  Check idCol value against ndf data.frame
   
      xxr            <- CheckColnn("idCol",c("040","041","042","043"),idCol,ndf,ndfName)

      if (xxr$Err) {
         xmsg   <- paste0("***991 The location ID column could not be found. Run Terminated.")
         stop(xmsg, call.=FALSE)
      } 
      idCol       <- xxr$colName
      idColNum    <- xxr$colNum
      
      # Have to have a good name or number by user to get this far.
      #  at this point idCol has "FIPS" or a value provided by user.

      idColName             <- idCol
      callVarList$idCol     <- idCol
      callVarList$idColName <- idCol
      callVarList$idColNum  <- idColNum
      
      rPM$idCol             <- idCol
      rPM$idColName         <- idCol
      rPM$idColNum          <- idColNum
      
      idList                <- ndf[,idCol]    # get list of IDs
      if (is.factor(idList)) idList <- as.character(idList)
          

      #cat("idCol:",idCol,"  idColName:",idColName,"  idColNum:",idColNum, "\n")
      #cat("idList:",idList,"\n")
      #
      ####
      
      ####
      #
      #  Step 17.2 - data column (dataCol) (080-085)
      #
      #  Validate data column name or number
      #
       
      dataCol_def       <- "Rate"

      if (is.null(dataCol)) {
         #xmsg           <- paste0("***055 The dataCol parameter is set to NULL or NA, The default column name of ",dataCol_def," will be used.")
         #message(xmsg, call.=FALSE)
         dataCol        <- dataCol_def
      }

      varValue          <- as.character(dataCol[[1]][1])
      
      ### Check dataCol value against ndf data.frame
      
      xxr               <- CheckColnn("dataCol",c("050","051","052","053"), varValue, ndf, ndfName)
          #  050 - column number out of range
          #  051 - column name is invalid or does not exist
          #  052 - column name/number is invalid data type
          #  053 - column name is empty

      if (xxr$Err) {
         xmsg      <- paste0("***992 The data column could not be found. Run Terminated.")
         stop(xmsg,call.=FALSE)
      } 
      #
      dataCol                  <- xxr$colName
      dataColNum               <- xxr$colNum
      dataColName              <- dataCol
      
      callVarList$dataCol      <- dataCol
      callVarList$dataColName  <- dataCol
      callVarList$dataColNum   <- dataColNum

      rPM$dataCol              <- dataCol
      rPM$dataColName          <- dataCol
      rPM$dataColNum           <- dataColNum
      
      dataList                 <- ndf[,dataCol]   # get data.
      if (is.factor(dataList))  dataList <- as.character(dataList)
      
      #cat("dataColName Z-8837 :",dataColName,"   dataColNum:",dataColNum," len(data):",length(dataList),"  categMode:",categMode,"\n")
      #cat("dataList:",dataList,"\n")
      
      #
      ####
      
      #cat("Step17.3 - Hatching dataCol Z-8851 \n")
      ####
      #
      #  Step 17.3a - hatching #1 dataCol 
      #       If default hatching dataCol name used, validate it exists. (110-114)
      #
      H_data                   <- rep(NA,length(dataList))
      hatch                    <- rPM$hatch
     
      if (HatchFlag) {
         #  Empty hDataList. 

         H_dataCol                <- hatch$hDataCol   # get value from initial checks
         
                           # if needed the default has been set
         #cat("checking hatch dataCol:",H_dataCol,"\n")
         
         #H_dataColName           <- "pValue"
         #H_dataColNum            <- 0
      
         # H:dataCol is a little different, it's not a call parameter,
         #  but named list item under hatch=list().
         # It's variable (H_dataCol) has already been set to the default
         # and changed if user provided alternative.
         # But is it a valid name for this ndf?
         #
         save_H_dataCol <- H_dataCol
      
         #cat("ndf:",dim(ndf),"  ndfName:",ndfName,"\n")
      
         xxr            <- CheckColnn("hatch:dataCol",c("110","111","112","113"),H_dataCol,ndf,ndfName)

         if (xxr$Err) {
            xmsg        <- paste0("***114 The data column for the hatch comparison could not be found. ",
                           " The hatch parameter has been disabled.") 
            warning(xmsg, call.=FALSE)
            HatchFlag      <- FALSE
         } else {
            H_dataCol      <- xxr$colName
            H_dataColNum   <- xxr$colNum
            H_dataColName  <- H_dataCol
           
            H_data         <- ndf[,H_dataCol]
            if (is.factor(H_data)) H_data <- as.character(H_data)   # get rid of factors.
         }
         #cat("H_dataCol:",H_dataCol,"  len-H_data:",length(H_data),"\n")

         hatch$hDataCol     <- H_dataCol
         hatch$hDataColName <- H_dataColName
         hatch$hDataColNum  <- H_dataColNum
      }
      
      hDataList              <- H_data

      #
      #  Re-Save final set of hatching parameters
      # 
      
      callVarList$HatchFlag  <- HatchFlag
      callVarList$hatch      <- hatch
      
      rPM$HatchFlag          <- HatchFlag
      rPM$hatch              <- hatch
       
      #  end Hatching #1 option check
      
      #
      ####
      
      ####
      #
      #
      #  Empty H_data for hatch 2
      #
 
      H_data                   <- rep(NA,length(dataList))
      hatch2                   <- rPM$hatch2
      
      if (Hatch2Flag) {
    
         H_dataCol                <- hatch2$hDataCol   # get value from initial checks
                                              # if needed the default has been set
         #cat("checking hatch2 dataCol:",H_dataCol,"\n")
         
         #H_dataColName            <- "pValue"
         #H_dataColNum             <- 0
      
         # H:dataCol is a little different, it's not a call parameter,
         #  but named list item under hatch=list().
         # It's variable (H_dataCol) has already been set to the default
         # and changed if user provided alternative.
         # But is it a valid name for this ndf?
         #
         save_H2_dataCol <- H_dataCol
      
         #cat("ndf:",dim(ndf),"  ndfName:",ndfName,"\n")
      
         xxr            <- CheckColnn("hatch2:dataCol",c("141","142","143","144"),H_dataCol,ndf,ndfName)

         if (xxr$Err) {
            xmsg        <- paste0("***145 The data column for the hatch2 comparison could not be found. ",
                           " The hatch2 parameter has been disabled.") 
            warning(xmsg, call.=FALSE)
            Hatch2Flag      <- FALSE
         } else {
            H_dataCol      <- xxr$colName
            H_dataColNum   <- xxr$colNum
            H_dataColName  <- H_dataCol
           
            H_data         <- ndf[,H_dataCol]
            if (is.factor(H_data)) H_data <- as.character(H_data)   # get rid of factors.
         }
         #cat("H_dataCol :",H_dataCol,"  len-H_data:",length(H_data),"\n")

         hatch2$hDataCol    <- H_dataCol
         hatch2$hDataColName<- H_dataColName
         hatch2$hDataColNum <- H_dataColNum
      }
      
      h2DataList   <- H_data

      #
      #  Re-Save final set of hatching parameters
      # 
      
      callVarList$Hatch2Flag  <- Hatch2Flag
      rPM$Hatch2Flag          <- Hatch2Flag

      callVarList$hatch2      <- hatch2
      rPM$hatch2              <- hatch2      
      
      #  end Hatching option check
      
      #
      ####

      #cat("Build dataMapDF Z-8979 \n")
      ####
      #
      #  The three/four column names/numbers are validated and good or
      #  adjustments made (stop or hatching disabled.)
      #    idList, dataList, hDataList, h2DataList...
      #
      dataMapDF      <- NULL
      dataMapDF      <- data.frame(ID=idList,data=dataList, hData=hDataList, h2Data=h2DataList,
                     stringsAsFactors=FALSE)

      lenDataMapDF   <- dim(dataMapDF)[1]

      dataMapDF$rSeq     <- seq(1,lenDataMapDF)   # row seq number for error messages.
                                 # if a row is deleted, the seq number will still be 
                                 # reported correctly and reference original DF.
      cNA                <- rep(as.character(NA),lenDataMapDF)
      dataMapDF$good     <- TRUE      # all rows are valid at this time.
      dataMapDF$rgID     <- cNA       # region ID
      dataMapDF$stID     <- cNA       # state  ID
      dataMapDF$saID     <- cNA       # Seer Registry ID
      dataMapDF$HSAID    <- cNA       # health Service Area ID
      dataMapDF$stcoID   <- cNA       # state/county ID
      dataMapDF$stcotrID <- cNA       # state/county/tract ID
      dataMapDF$cat      <- 0         # data category #
      dataMapDF$col      <- "white"   # color
      
      dataMapDF$hRes     <- FALSE
      dataMapDF$h2Res    <- FALSE
 
      rPM$dataMapDF      <- dataMapDF

      #cat("dataMapDF Z-9011 :\n")
      #print(str(dataMapDF))
      #
      ####

      ####
      #
      #  Results:
      #      cVL - all parameters validated
      #      rPM - a few run parameters in addition to the initial SM_GlobIni set.
      #         added variables:
      #      dataMapDF - ID, data, hData.
      #
      ##### End of Stage 1

      #print("At end of parameter checks Z-9019 ")

      #
      #  End of parameter checking.  All column names have been confirmed.
      #
      #   and the state and seer area boundaries are loaded.
      #
      #####
      #printNamedList("callVarList",callVarList)
      #printNamedList("rPM",rPM)
      #
      #print("End of Stage 1 - call parameter analysis.")
      
      ####################################
      
      
      ####################################
      
      ##### Stage 2 - build SPDFs. & Validate ID

      #cat("Call SM_Build\n")
      xRes   <- SM_Build(rPM)
      MV     <- xRes$MV
      rPM    <- xRes$rPM

      #cat("SM_Build Completed.\n")
      #
      #   At this point:
      #     a) All boundary files loaded, 
      #     b) ID is validated, table adjusted, 
      #     c) xxxxB merged between defaults and caller provided, 
      #     d) xxxxB implemented -> xxxxxPList and xxxx_sel, 
      #     e) proj bbox created, xxxx_proj_sel created, and xlim and ylim returned (MV)
      #

      ####
      #
      #  defaults on regionB, stateB, seerB, countyB, and tractB change based on
      #  the level of data provided by the user.
      #  a) if state data   : regionB=NONE, stateB=DATA,  seerB=NONE,  hdaB, countyB and tractB are not used.
      #  b) if seer reg data: regionB=NONE, stateB=NONE,  seerB=DATA,  hsaB, countyB and tractB are not used.
      #  c) if hsa data     : regionB=NONE, stateB=NONE,  seerB=NONE,  hsaB=DATA,  countyB and tractB are not used.
      #  d) if county data  : regionB=NONE, stateB=NONE,  seerB=NONE,  hsaB=NONE,  countyB=DATA  and tractB is not used.
      #  e) if tract data   : regionB=NONE, stateB=NONE,  seerB=NONE,  hsaB=NONE,  countyB=NONE, tractB=DATA.
      #
      ####

      #printNamedList("MV",MV)
      
      #print("End of Stage 2 - boundary data collection and inspection.")
            
      ##### Stage 3 - Inspect Data & Categorization & Colors

      #cat("Call SM_Categ\n")
      rPM  <- SM_Categ(rPM)
      
      #cat("Returned from SM_Categ.\n")
      
      ###
      #
      #   add test for categMode = COLOR for too many colors in the data.  Can't do legend.
      #   Only if legend enabled.
      #
      ###

      #printNamedList("rPM",rPM)
      #print("End of Stage 3 - Data Inspection and Categorization")
      
      ###
      #
      #  Step 5.03 - mLegend - handle labels valiations - had to wait until categ was processed
      #
      if (rPM$mLegend$lLabels != "") {
         if (length(rPM$mLegend$lLabels) != rPM$CatNumb) {
            xmsg      <- paste0("***280 The labels parameter must have one entry for each categories. ",
                                    "Set to an empty string.")
            warning(xmsg, call.=FALSE)
            ErrFnd    <- TRUE
            rPM$mLegend$lLabels  <- ""
         }
      }

      #
      ###
      
      ##### Stage 4 - Set up Hatching.
      
      if (rPM$HatchFlag || rPM$Hatch2Flag ) {
         #cat("Calling SM_Hatching.\n")
         rPM <- SM_Hatching(rPM)
      
         #str(rPM$dataMapDF)
         #str(rPM$hatch)
      }
      
      ##### Stage 5 - Final Prep for Mapping 
 
      HatchFlag   <- rPM$HatchFlag
      Hatch2Flag  <- rPM$Hatch2Flag
      
      #####
      #
      #   Prep for mapping  - table size, colors, 
      #
      
      dataMapDFLen      <- dim(rPM$dataMapDF)[1]

      if (debug) {
         xmsg <- paste0("***295 Number of locations found in the Rate Table with borders: ",dataMapDFLen)
         print(xmsg)
      }
      
      #  Off to work.
      if (dataMapDFLen > 0) {    # We have rate data for this year...

         #  Have data to map. SETUPs
  
         ##### Look at data and set hatching variables in xxxx$hDen

         #
         #  Step 2 - set up data for SPDF - area colors (categories) and hatching from RateTable (dataMapDF)
         #
         #print("Step 2 - set up Colors Z-9150 ")
         #
         # color vector for polygon drawing (one per polygon.) (rate classification.
         #  DF  - Col=area color,  Rel=area reliability (density for hatching.
         #  Order matches the dataProj/Wrk_proj sequence.

         dataMapDF      <- rPM$dataMapDF
         data_data_sel  <- MV$data_data_sel # {save as dataMapDF???) (Get copy of structure)
         
         # set up spaces for data.
         data_data_sel$col  <- "white"
         data_data_sel$cat  <- 0
         data_data_sel$hRes <- FALSE
         data_data_sel$h2Res <- FALSE
         
         # Transfer from RateWork to WrkCol
         #       Fill Color and Hatching information into SPDF element - dataMapDF - transfer info.

         data_data_sel[dataMapDF$ID,"col"]   <- dataMapDF$col
         data_data_sel[dataMapDF$ID,"cat"]   <- dataMapDF$cat
         data_data_sel[dataMapDF$ID,"hRes"]  <- dataMapDF$hRes
         data_data_sel[dataMapDF$ID,"h2Res"] <- dataMapDF$h2Res
         
         if (debug) {
           cat("Number of polygons in areas with data Z-9174 : ", dim(data_data_sel)[1], "\n")
         }
         
         MV$data_data_sel <- data_data_sel
         #
         ####

         ##### structures are set for mapping.  Only thing that may change for SaTScanMapper is the 
         #####   data_proj_seldata$col field for different maps.
     
         if (debug) {
            cat("Color and dataMapDF table Z-9185 :\n")
            cat("data_data_sel:\n")
            print(data_data_sel)
            #print(head(data_data_sel,40))
            cat("dataMapDF:\n")
            #print(head(dataMapDF,40))
            print(dataMapDF)
         
         }

     ##### Data Structures Required #####
     
         #  boundary data and coloring.
    
         #
         #  data_proj_sel   - data sp
         #  data_data_sel   - data sub-area hatching and col.
         #  tr_proj_sel     - tract sp
         #  co_proj_sel     - county sp
         #  hs_proj_sel     - hsa sp
         #  sa_proj_sel     - seer registry sp
         #  st_proj_sel     - state sp
         #  rg_proj_sel     - regional sp 
         #  xxGO            - map control variables for each boundary dataset.
         #  

         #
         #   rPM and MV are major vector contining the required data to do the mapping.
         #   The attributes for teach sub-area are held in the data_data_sel data.frame
         #       data_data_sel$col  = color of sub-area
         #       data_data_sel$cat  = category number (1-n)
         #       data_data_sel$den  = density of any hatching (if required)
         #       data_data_sel$ID   = sub-area ID  (should match row.names of SPDF
         #
         #
         
         #  Map areas with color.
         
         #cat("Calling SM_Mapper\n")
         wbox <- SM_Mapper(rPM, MV)    # do mapping.

         MV$MapBox <- wbox
         
         #cat("SM_Mapper results-plot box:\n")
         #print(wbox)
         
         #cat("mLegendFlag Z-9235 :",mLegendFlag,"\n")

         if (mLegendFlag) {
            # overlay legend
            SM_Legend(rPM, MV)          # draw legend
         }
     
     ##### Title #####
        #cat("Title:",mTitle,"  cex:",mTitle.cex,"\n")

         #  When writing to files, put on separate page for reference.
         if (!(is.null(mTitle) || any(is.na(mTitle)))) {
            title(main=mTitle, cex = mTitle.cex)     # kast item - title
         }
         
         #cat("End of Title.. Z-9242 \n")
         
     ##### End of Title #####    
             
         if (debug) {
             xRes <- list(lim=wbox, proj4=rPM$CRSproj4, rPM=rPM, MV=MV)
         } else {
             xRes <- list(lim=wbox, proj4=rPM$CRSproj4)
         }
         invisible(xRes)
      } else {
         #  no data lines to plot.
         wbox2 <- matrix(c(MV$xlPlot,MV$ylPlot),ncol=2,byrow=TRUE,dimnames=list(c("x","y"), c("min","max")))
         if (debug) {
            xRes <- list(lim=wbox2, proj4=rPM$CRSproj4, rPM=rPM, MV=MV)
         } else {
            xRes <- list(lim=wbox2, proj4=rPM$CRSproj4)
         }
         invisible(xRes)
      }  # end of test for data - Rlen > 0
      
      ##### End of Mapping #####
      
      #
      #  return box and coordinate information same as SM_Mapper
      #
      
   }  # end of function.
  
  ###