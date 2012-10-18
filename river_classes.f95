MODULE river_classes
! Classes for river type things
    USE global_defs
    IMPLICIT NONE
    

    TYPE STATION_DATA_TYPE
        ! Type for river stations (xsections, lateral structures, bridges, etc)
        CHARACTER(len=charlen):: myname

    END TYPE STATION_DATA_TYPE
    
    TYPE, EXTENDS(STATION_DATA_TYPE):: XSECT_DATA_TYPE
        ! Type for xsectional data
        REAL(dp), ALLOCATABLE:: cutline(:,:) ! Cutline coords
        REAL(dp), ALLOCATABLE:: yz(:,:) ! Profile coords
        REAL(dp), ALLOCATABLE:: downstream_dists(:) ! Distances 
        REAL(dp), ALLOCATABLE:: roughness(:) ! Manning?
    END TYPE XSECT_DATA_TYPE

    TYPE REACH_DATA_TYPE
        ! Type to hold reach information
        REAL(dp), ALLOCATABLE:: coordinates(:,:) ! Centreline coordinates
        CHARACTER(len=charlen), ALLOCATABLE:: names(:) ! Hold an array of names associated with the reach
        TYPE(XSECT_DATA_TYPE), ALLOCATABLE:: xsects(:) ! Hold array of xsection information
        INTEGER(dp):: xsect_count ! Number of xsections
        !INTEGER(dp):: xsect_count2 ! Number of xsections
    END TYPE REACH_DATA_TYPE

END MODULE river_classes
