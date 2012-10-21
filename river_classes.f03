MODULE river_classes
! Classes for river type things
    USE global_defs
    IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE MONOTONIC_RELATION
        ! Holds two variables with a monotonic relation (e.g. Stage vs Area)
        ! This can be used to make a function which computes one variable given the other
        ! (with interpolation as appropriate). 
        ! e.g. Compute 'Area' given 'Stage', or vice-versa
        !
        ! Also hold the 'last_lower_search_index', which is the (lower) index near where
        ! we last evaluated the relation. The idea is that we will often
        ! evaluate the function near to where we last evaluated it. Storing the 
        ! index can make the look-up fast.
        REAL(dp), ALLOCATABLE:: Stage_Area(:,:)
        INTEGER(dp):: last_search_index
        !contains
        !PROCEDURE:: init=> init_stage_area_relation ! Initialise (and/or update) the stage_area relation
        !PROCEDURE:: eval=> stage_from_area ! eval(Area1) = Stage1, or eval(Stage1, inverse=TRUE)=Area1
    END TYPE    

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

        ! 
        TYPE(MONOTONIC_RELATION), ALLOCATABLE:: stage_area_curve
        
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
        
        ! Variables which will be dynamically allocated the boundary information
        CLASS(REACH_BOUNDARY), ALLOCATABLE:: Downstream_boundary, Upstream_boundary
        
        INTEGER(dp):: xsect_count ! Number of xsections
        TYPE(XSECT_DATA_TYPE), ALLOCATABLE:: xsects(:) ! Hold array of xsection information

        !
        ! 1D flow variables
        !
        REAL(dp), ALLOCATABLE:: Stage(:), Discharge(:), Area(:) 

        ! Array of the downstream distances (DX) -- e.g. for the left & right banks + channel
        REAL(dp), ALLOCATABLE:: downstream_dists(:,:)


        contains
        PROCEDURE:: print => print_reach
        PROCEDURE:: get_downstream_dists_from_xsections => get_downstream_dists_from_xsections

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
    SUBROUTINE print_reach(reach)
        CLASS(REACH_DATA_TYPE), INTENT(IN):: reach

        INTEGER:: j
        ! Print name
        print*, trim(reach%names(1)), ' ',trim(reach%names(2))

        ! Print boundary inforation -- note, polymorphic boundary variables
        print*, 'Downstream_boundary Info:'
        call reach%Downstream_boundary%print()
        print*, 'Upstream_boundary Info:'
        call reach%Upstream_boundary%print()

        print*, 'Downstream distances:'
        DO j=1,size(reach%downstream_dists(:,1))
            print*, '   ', reach%downstream_dists(j,:)
        END DO

        ! Print coordinates
        print*, 'Coordinates count=', size(reach%coordinates(:,1))
        DO j=1,size(reach%coordinates(:,1))
            print*, reach%coordinates(j,1:2)
        END DO

        print*, 'XSECT COUNT =', reach%xsect_count

    END SUBROUTINE print_reach
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE print_xsect(xsect)
        CLASS(XSECT_DATA_TYPE), INTENT(IN):: xsect
        INTEGER:: k

        print*, trim(xsect%myname)
        print*, 'Downstream distances are ', xsect%downstream_dists

        print*, 'Cutline size = ', size(xsect%cutline(:,1))
        DO k=1,size(xsect%cutline(:,1))
            print*, xsect%cutline(k,1:2)
        END DO

        print*, 'Xsect size = ', size(xsect%yz(:,1))
        DO k=1,size(xsect%yz(:,1))
            print*, xsect%yz(k,1:2) 
        END DO
        
        print*, 'Xsect roughness change points:'
        DO k=1, size(xsect%roughness(:,1))
            print*, xsect%roughness(k,1:2)
        END DO

    END SUBROUTINE print_xsect
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE get_downstream_dists_from_xsections(reach)
        ! Copy the 'downstream_dists' from each xsection into a single array held at the reach level
        ! Why?? Cleaner access
        CLASS(REACH_DATA_TYPE), INTENT(INOUT):: reach

        INTEGER(dp)::i

        ! Make array of dim (number of xsections, number of downstream distances per xsection)
        ! There can be more than 1 downstream distance per xsection (e.g. hecras uses the left bank, channel and right bank)
        ALLOCATE(reach%downstream_dists( reach%xsect_count, size(reach%xsects(1)%downstream_dists) ) )
        DO i=1,reach%xsect_count
            reach%downstream_dists(i,:) = reach%xsects(i)%downstream_dists
        END DO

    END SUBROUTINE get_downstream_dists_from_xsections

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !SUBROUTINE init_stage_area_relation(xsect)
    !   
    !END SUBROUTINE init_stage_area_relation 
END MODULE river_classes
