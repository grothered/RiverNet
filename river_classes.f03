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

        contains
        PROCEDURE:: print => print_xsect
    END TYPE XSECT_DATA_TYPE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE REACH_BOUNDARY
        CHARACTER(len=charlen):: boundary_type ! junction or physical -- do I even need this?
        contains
        PROCEDURE:: print => print_boundary
    END TYPE REACH_BOUNDARY

    TYPE, EXTENDS(REACH_BOUNDARY):: JUNCTION_BOUNDARY
        ! Store the flow variables at every cross-section that meets at this boundary
        ! Allow for junctions with 2 or more reaches joining
        CHARACTER(len=charlen):: junction_name

        CHARACTER(len=charlen), ALLOCATABLE:: reach_names(:) ! Names of reaches that join here
        CHARACTER(len=charlen), ALLOCATABLE:: reach_ends(:) ! Upstream or Downstream? for each reach
        REAL(dp), ALLOCATABLE:: distances(:) ! Distance from the junction, for each reach
        !CHARACTER(len=charlen):: reach_names(3) ! Names of reaches that join here
        !CHARACTER(len=charlen):: reach_ends(3) ! Upstream or Downstream? for each reach
        !REAL(dp) :: distances(3) ! Distance from the junction, for each reach
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

        ! Variables which will be dynamically allocated the boundary information
        CLASS(REACH_BOUNDARY), ALLOCATABLE:: Downstream_boundary, Upstream_boundary

        contains
        PROCEDURE:: print => print_reach

    END TYPE REACH_DATA_TYPE
 
    CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE print_boundary(generic_boundary)
        CLASS(REACH_BOUNDARY):: generic_boundary

        SELECT TYPE(generic_boundary)
            TYPE IS (PHYSICAL_BOUNDARY)
                print*, trim(generic_boundary%boundary_type),' ', &
                                            trim(generic_boundary%input_file)
            TYPE IS (JUNCTION_BOUNDARY)
                print*, trim(generic_boundary%boundary_type), ' ',&
                                            trim(generic_boundary%junction_name)
        END SELECT

    END SUBROUTINE print_boundary 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE print_reach(reach_data)
        CLASS(REACH_DATA_TYPE), INTENT(IN):: reach_data

        INTEGER:: j
        ! Print name
        print*, trim(reach_data%names(1)), ' ',trim(reach_data%names(2))

        ! Print boundary inforation -- note, polymorphic boundary variables
        print*, 'Downstream_boundary Info:'
        call reach_data%Downstream_boundary%print()
        print*, 'Upstream_boundary Info:'
        call reach_data%Upstream_boundary%print()

        ! Print coordinates
        print*, 'Coordinates count=', size(reach_data%coordinates(:,1))
        DO j=1,size(reach_data%coordinates(:,1))
            print*, reach_data%coordinates(j,1:2)
        END DO

        print*, 'XSECT COUNT =', reach_data%xsect_count

    END SUBROUTINE print_reach
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE print_xsect(xsects)
        CLASS(XSECT_DATA_TYPE), INTENT(IN):: xsects
        INTEGER:: k

        print*, trim(xsects%myname)
        print*, 'Downstream distances are ', xsects%downstream_dists

        print*, 'Cutline size = ', size(xsects%cutline(:,1))
        DO k=1,size(xsects%cutline(:,1))
            print*, xsects%cutline(k,1:2)
        END DO

        print*, 'Xsect size = ', size(xsects%yz(:,1))
        DO k=1,size(xsects%yz(:,1))
            print*, xsects%yz(k,1:2) 
        END DO
        
        print*, 'Xsect roughness change points:'
        DO k=1, size(xsects%roughness(:,1))
            print*, xsects%roughness(k,1:2)
        END DO

    END SUBROUTINE print_xsect

END MODULE river_classes
