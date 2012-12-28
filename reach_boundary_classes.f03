MODULE reach_boundary_classes
! Classes for reach boundary conditions / junctions
    use global_defs
    use one_d_relation_class

    IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! BOUNDARY CONDITIONS
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE REACH_BOUNDARY
        ! Generic type for boundaries of reaches. Specialisations of this are
        ! junction boundary and physical boundary
        CHARACTER(len=charlen):: boundary_type ! junction or physical -- do I even need this?
        REAL(dp):: boundary_location(2) ! x-y data associated with the boundary

        ! Set flag to determine computation method for sub-critical boundary 
        CHARACTER(len=charlen):: compute_method ! 'stage', 'discharge', Incoming_characteristic,...'

        contains
        PROCEDURE:: print => print_boundary
        PROCEDURE:: delete => deallocate_reach_boundary_data
        PROCEDURE:: eval => get_boundary_values
    END TYPE REACH_BOUNDARY

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE, EXTENDS(REACH_BOUNDARY):: JUNCTION_BOUNDARY
        ! Store the flow variables at every cross-section that meets at this boundary
        ! Allow for junctions with 2 or more reaches joining
        CHARACTER(len=charlen):: junction_name
        CHARACTER(len=charlen):: junction_description

        CHARACTER(len=charlen), ALLOCATABLE:: reach_names(:, :) ! Names of reaches that join here
        CHARACTER(len=charlen), ALLOCATABLE:: reach_ends(:) ! Upstream or Downstream? for each reach
        REAL(dp), ALLOCATABLE:: distances(:) ! Distance from the junction, for each reach

        ! Hydrodynamic variables
        REAL(dp):: Stage
        REAL(dp):: Volume
        REAL(dp):: Discharge_x, Discharge_y

        TYPE(ONE_D_RELATION):: Stage_volume_curve

    END TYPE JUNCTION_BOUNDARY

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE, EXTENDS(REACH_BOUNDARY):: PHYSICAL_BOUNDARY
        ! Store a file with w,Q at the boundary, and a 'compute_method' to use if
        ! we should not enforce both w and Q at the boundary.
        ! [e.g. for subcritical flows, we could either
        ! force the water surface elevation, or the discharge, or some combination of them,
        ! but it is incorrect to force both w and Q]
        CHARACTER(len=charlen):: input_file ! File containing t, w and Q


        TYPE(ONE_D_RELATION):: Boundary_t_w_Q ! One-d-relation which will hold t, w, and Q timeseries for the boundary
    END TYPE PHYSICAL_BOUNDARY


    contains

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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE deallocate_reach_boundary_data(jb)
        ! Deallocates sub-components of the boundary. Used to avoid memory leaks
        CLASS(reach_boundary):: jb

        SELECT TYPE(jb)
            TYPE IS(JUNCTION_BOUNDARY)
                DEALLOCATE(jb%reach_names)
                DEALLOCATE(jb%reach_ends)
                DEALLOCATE(jb%distances)
            TYPE IS(PHYSICAL_BOUNDARY)
                call delete_one_d_relation(jb%Boundary_t_w_Q)
                !DEALLOCATE(jb)
        END SELECT
    END SUBROUTINE deallocate_reach_boundary_data

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    FUNCTION get_boundary_values(rb, time, vartype)
        ! Function to get variable 'vartype' at time='time' from boundary rb, which may be a 
        ! reach or a junction boundary
        class(reach_boundary):: rb
        REAL(dp):: time, get_boundary_values
        CHARACTER(*):: vartype

        SELECT TYPE(rb)
            TYPE IS(JUNCTION_BOUNDARY)

            TYPE IS(PHYSICAL_BOUNDARY)
                get_boundary_values=rb%Boundary_t_w_Q%eval(time, 'time', vartype)
        END SELECT

    END FUNCTION get_boundary_values

END MODULE reach_boundary_classes

