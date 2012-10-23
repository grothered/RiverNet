MODULE river_classes
! Classes for river type things
    USE global_defs
    USE xsect_classes
    IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE REACH_BOUNDARY
        CHARACTER(len=charlen):: boundary_type ! junction or physical -- do I even need this?
        REAL(dp):: boundary_location(2) ! x-y data associated with the boundary
        contains
        PROCEDURE:: print => print_boundary
        PROCEDURE:: delete => deallocate_reach_boundary
    END TYPE REACH_BOUNDARY

    TYPE, EXTENDS(REACH_BOUNDARY):: JUNCTION_BOUNDARY
        ! Store the flow variables at every cross-section that meets at this boundary
        ! Allow for junctions with 2 or more reaches joining
        CHARACTER(len=charlen):: junction_name
        CHARACTER(len=charlen):: junction_description

        CHARACTER(len=charlen), ALLOCATABLE:: reach_names(:, :) ! Names of reaches that join here
        CHARACTER(len=charlen), ALLOCATABLE:: reach_ends(:) ! Upstream or Downstream? for each reach
        REAL(dp), ALLOCATABLE:: distances(:) ! Distance from the junction, for each reach
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
        CHARACTER(len=charlen):: names(2) ! Hold an array of names associated with the reach. Hecras has 2
        
        REAL(dp), ALLOCATABLE:: coordinates(:,:) ! Centreline coordinates
        
        ! Variables which will be dynamically allocated the boundary information
        CLASS(REACH_BOUNDARY), ALLOCATABLE:: Downstream_boundary, Upstream_boundary
        
        INTEGER(ip):: xsect_count ! Number of xsections
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

        INTEGER:: i

        SELECT TYPE(generic_boundary)
            TYPE IS (PHYSICAL_BOUNDARY)
                print*, trim(generic_boundary%boundary_type),' ', &
                                            trim(generic_boundary%input_file)
                print*, trim(generic_boundary%compute_method)
            TYPE IS (JUNCTION_BOUNDARY)
                print*, '###############'
                print*, 
                print*, 'FIXME: Need to check that distances are set correctly'
                print*, trim(generic_boundary%boundary_type), ' ',&
                                            trim(generic_boundary%junction_name)
                print*, trim(generic_boundary%junction_description)
                DO i=1, size(generic_boundary%reach_names(:,1))
                    print*, trim(generic_boundary%reach_names(i,1)), ' ',& 
                            trim(generic_boundary%reach_names(i,2)), ' ',&
                            trim(generic_boundary%reach_ends(i)), ' ',&
                            generic_boundary%distances(i)
                END DO
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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE get_downstream_dists_from_xsections(reach)
        ! Copy the 'downstream_dists' from each xsection into a single array held at the reach level
        ! Why?? Cleaner access compared with having them all within their cross-sections
        CLASS(REACH_DATA_TYPE), INTENT(INOUT):: reach

        INTEGER(ip)::i

        ! Make array of dim (number of xsections, number of downstream distances per xsection)
        ! There can be more than 1 downstream distance per xsection (e.g. hecras uses the left bank, channel and right bank)
        ALLOCATE(reach%downstream_dists( reach%xsect_count, size(reach%xsects(1)%downstream_dists) ) )
        DO i=1,reach%xsect_count
            reach%downstream_dists(i,:) = reach%xsects(i)%downstream_dists
        END DO

    END SUBROUTINE get_downstream_dists_from_xsections

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE deallocate_reach_boundary(jb)
            CLASS(reach_boundary):: jb

            SELECT TYPE(jb)
                TYPE IS(JUNCTION_BOUNDARY)
                    DEALLOCATE(jb%reach_names)
                    DEALLOCATE(jb%reach_ends)
                    DEALLOCATE(jb%distances)
            END SELECT
    END SUBROUTINE deallocate_reach_boundary

END MODULE river_classes
