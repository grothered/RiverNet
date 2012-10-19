MODULE river_classes
! Classes for river type things
    USE global_defs
    IMPLICIT NONE
    

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE STATION_DATA_TYPE
        ! Type for river stations (xsections, lateral structures, bridges, etc)
        CHARACTER(len=charlen):: myname

    END TYPE STATION_DATA_TYPE
    
    TYPE, EXTENDS(STATION_DATA_TYPE):: XSECT_DATA_TYPE
        ! Type for xsectional data
        REAL(dp), ALLOCATABLE:: cutline(:,:) ! Cutline coords
        REAL(dp), ALLOCATABLE:: yz(:,:) ! Profile coords
        REAL(dp), ALLOCATABLE:: downstream_dists(:) ! Distances 
        REAL(dp), ALLOCATABLE:: roughness(:,:) ! Manning?
    END TYPE XSECT_DATA_TYPE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE REACH_BOUNDARY
        CHARACTER(len=charlen):: boundary_type ! junction or physical -- do I even need this?
    END TYPE REACH_BOUNDARY

    TYPE, EXTENDS(REACH_BOUNDARY):: JUNCTION_BOUNDARY
        ! Store the flow variables at every cross-section that meets at this boundary
        ! Allow for junctions with 2 or more reaches joining
        CHARACTER(len=charlen):: junction_name
        CHARACTER(len=charlen), ALLOCATABLE:: reach_names ! Names of reaches that join here
        CHARACTER(len=charlen), ALLOCATABLE:: reach_ends ! Upstream or Downstream? for each reach
        REAL(dp), ALLOCATABLE:: distances ! Distance from the junction, for each reach
    END TYPE JUNCTION_BOUNDARY

    TYPE, EXTENDS(REACH_BOUNDARY):: PHYSICAL_BOUNDARY
        ! Store a file with w,Q at the boundary, and a 'compute_method' to use if
        ! we should not enforce both w and Q at the boundary.
        ! [e.g. for subcritical flows, we could either
        ! force the water surface elevation, or the discharge, or some combination of them,
        ! but it is incorrect to force both w and Q]
        CHARACTER(len=charlen):: input_file ! File containing t, w and Q
        CHARACTER(len=charlen):: compute_method ! 'Always_w, Always_Q, Incoming_characteristic,...'
    END TYPE PHYSICAL_BOUNDARY

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE REACH_DATA_TYPE
        ! Type to hold reach information
        CHARACTER(len=charlen), ALLOCATABLE:: names(:) ! Hold an array of names associated with the reach
        
        REAL(dp), ALLOCATABLE:: coordinates(:,:) ! Centreline coordinates
        
        INTEGER(dp):: xsect_count ! Number of xsections
        TYPE(XSECT_DATA_TYPE), ALLOCATABLE:: xsects(:) ! Hold array of xsection information

        ! Need an upstream and a downstream boundary
        ! Now, these probably need to be specialised versions of the class reach_boundary
        ! They might be either junction boundary &/or physical boundary
        CLASS(REACH_BOUNDARY), pointer:: Downstream_boundary, Upstream_boundary

    END TYPE REACH_DATA_TYPE
    
END MODULE river_classes
