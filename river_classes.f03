MODULE river_classes
! Classes for river type things
    USE global_defs
    USE one_d_relation_class
    USE xsect_classes
    USE reach_boundary_classes
    IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    !
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE REACH_DATA_TYPE
        ! Type to hold reach information
        CHARACTER(len=charlen):: names(2) ! Hold an array of names associated with the reach. Hecras has 2
        
        REAL(dp), ALLOCATABLE:: coordinates(:,:) ! Centreline coordinates
        
        ! Boundary information
        CLASS(REACH_BOUNDARY), ALLOCATABLE:: Downstream_boundary, Upstream_boundary
       
        ! XSECTIONAL INFORMATION 
        INTEGER(ip):: xsect_count ! Number of xsections
        TYPE(XSECT_DATA_TYPE), ALLOCATABLE:: xsects(:) ! Hold array of xsection information

        !
        ! 1D flow variables
        ! Length of these can be equal (number of cross-sections+2)
        ! So we can store the boundary conditions here as well, and implement them smoothly
        !
        REAL(dp), ALLOCATABLE:: Stage(:), Discharge(:), & 
                                Area(:), Width(:), &
                                Drag_1D(:), Discharge_con(:)
        ! size(Volume) = size(Stage)-1
 
        ! Array of the downstream distances (DX) for the cross-sections -- e.g.
        ! for the left & right banks + channel
        REAL(dp), ALLOCATABLE:: downstream_dists(:,:)

        ! Depth at which we set velocity/fluxes to zero to prevent bad solver behaviour
        REAL(dp):: wet_dry_depth=wet_dry_depth


        contains
        PROCEDURE:: print => print_reach
        PROCEDURE:: get_downstream_dists_from_xsections => get_downstream_dists_from_xsections
        PROCEDURE:: allocate_1d_vars => allocate_1d_vars
        PROCEDURE:: reverse_reach_order => reverse_reach_order

    END TYPE REACH_DATA_TYPE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    !
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    TYPE NETWORK_DATA_TYPE
        ! River network, containing reaches and junctions, and ...
        INTEGER(ip):: num_reaches
        INTEGER(ip):: num_junctions
        TYPE(REACH_DATA_TYPE), ALLOCATABLE:: reach_data(:)
        TYPE(JUNCTION_BOUNDARY), ALLOCATABLE:: reach_junctions(:)

        REAL(dp):: time=start_time ! Time (s) from arbitrary start time
        REAL(dp):: dT=maximum_allowed_timestep ! Hydrodynamic time-step
        REAL(dp):: CFL=cfl_1d_solver ! CFL number
   
        INTEGER(ip):: output_file_unit, time_file_unit 

        contains
        PROCEDURE:: print_status => print_network_status
        PROCEDURE:: create_outfiles => create_network_outfiles
        PROCEDURE:: close_outfiles => close_network_outfiles
        PROCEDURE:: write_data => write_network_data

    END TYPE NETWORK_DATA_TYPE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
    CONTAINS

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
            IF(minval(reach%downstream_dists(j,:)) < 0._dp) THEN
                print*, 'ERROR: Negative downstream distance'
                stop
            END IF
        END DO

        ! Print coordinates
        print*, 'Coordinates count=', size(reach%coordinates(:,1))
        DO j=1,size(reach%coordinates(:,1))
            print*, reach%coordinates(j,1:2)
        END DO

        print*, 'XSECT COUNT =', reach%xsect_count

    END SUBROUTINE print_reach

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE print_network_status(network, counter)
        ! Print some statistics to the console
        CLASS(network_data_type), INTENT(IN):: network
        INTEGER(ip), INTENT(IN):: counter

        !Local vars
        INTEGER(ip):: M


        ! IO BLOCK
        IF(mod(counter-1,writfreq).eq.0) THEN
            M=network%reach_data(1)%xsect_count

            print*, '## Step ', counter, '; Time (hr) ', network%time/3600._dp, '; dT:', network%dT
            print*, '   Q(1) = ', network%reach_data(1)%Discharge(1), ' Q(M) = ', network%reach_data(1)%Discharge(M)
            print*, ' Drag_1D(1) * d_bar ', &
                     network%reach_data(1)%Drag_1D(1)*network%reach_data(1)%Area(1)/network%reach_data(1)%Width(1)
            print*, 'd_bar(1) = ', network%reach_data(1)%Area(1)/network%reach_data(1)%Width(1)
       END IF 
    END SUBROUTINE print_network_status

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE create_network_outfiles(network)
        ! Open files for writing output
        CLASS(network_data_type), INTENT(INOUT):: network

        open(newunit=network%output_file_unit, file='output.txt')
        open(newunit=network%time_file_unit, file='time.txt')
    END SUBROUTINE create_network_outfiles
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE close_network_outfiles(network)
        ! Close files for writing output
        CLASS(network_data_type), INTENT(IN):: network

        close(network%output_file_unit)
        close(network%time_file_unit)
    END SUBROUTINE close_network_outfiles

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE write_network_data(network, counter) 
        ! Write the reach information to the output file
        CLASS(network_data_type), INTENT(IN):: network
        INTEGER(ip), INTENT(IN):: counter

        INTEGER(ip):: output_file_unit, time_file_unit, M

        output_file_unit=network%output_file_unit
        time_file_unit=network%time_file_unit
        M=network%reach_data(1)%xsect_count

        IF(mod(counter-1,writfreq).eq.0) THEN
            write(output_file_unit,*) network%reach_data(1)%Stage
            write(output_file_unit,*) network%reach_data(1)%Area
            write(output_file_unit,*) network%reach_data(1)%Area/network%reach_data(1)%Width
            write(output_file_unit,*) network%reach_data(1)%Discharge
            write(output_file_unit,*) network%reach_data(1)%Discharge_con(1:M)
            write(time_file_unit,*) network%time
        END IF
    END SUBROUTINE write_network_data
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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE allocate_1d_vars(reach)
        ! Routine to allocate 1D variables in the reach (e.g Stage / Discharge/ Area / Width ..)
        CLASS(reach_data_type), INTENT(INOUT)::reach

        INTEGER(ip):: N

        N=reach%xsect_count

        ALLOCATE(reach%Stage(N), reach%Discharge(N), reach%Area(N), reach%Width(N), reach%Drag_1D(N), &
                 reach%Discharge_con(N+1))

    END SUBROUTINE allocate_1d_vars

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE set_initial_conditions(reach, depth_start, Q_start)
        ! Quick routine to set initial depth = depth_start (above min bed elevation),
        ! and initial_Q = Q_start
        CLASS(reach_data_type), INTENT(INOUT):: reach
        REAL(dp), INTENT(IN):: depth_start, Q_start

        INTEGER(ip):: N, i
        
        N=size(reach%Stage)
        DO i=1,N
            ! Depth of 1m
            reach%Stage(i)= minval(reach%xsects(i)%yz(:,2)) + depth_start
            reach%Area(i) = reach%xsects(i)%stage_etc_curve%eval( &
                                                  reach%Stage(i), 'stage', 'area')
            reach%Width(i) = reach%xsects(i)%stage_etc_curve%eval( &
                                                  reach%Stage(i), 'stage', 'width')
            reach%Discharge(i) = Q_start
            reach%Drag_1D(i) = reach%xsects(i)%stage_etc_curve%eval( &
                                                  reach%Stage(i), 'stage', 'drag_1D')
        END DO

    END SUBROUTINE set_initial_conditions

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE reverse_reach_order(reach)
        CLASS(reach_data_type), INTENT(INOUT)::reach

        INTEGER(ip):: i
        CLASS(reach_boundary), ALLOCATABLE:: temp_reach_boundary


        ! Reverse boundaries
        allocate(temp_reach_boundary, source=reach%Upstream_boundary)
        call reach%Upstream_boundary%delete()
        deallocate(reach%Upstream_boundary)
        
        allocate(reach%Upstream_boundary, source=reach%Downstream_boundary)
        call reach%Downstream_boundary%delete()
        deallocate(reach%Downstream_boundary)
        allocate(reach%Downstream_boundary,source=temp_reach_boundary)

        call temp_reach_boundary%delete()
        deallocate(temp_reach_boundary)
             
        ! Make sure 'Q' at the boundaries has the right sign
        SELECT TYPE(x=>reach%Upstream_boundary)
        TYPE IS(PHYSICAL_BOUNDARY)
            x%Boundary_t_w_Q%x_y(:,3) = - x%Boundary_t_w_Q%x_y(:,3)
        END SELECT 
        
        SELECT TYPE(x=>reach%Downstream_boundary)
        TYPE IS(PHYSICAL_BOUNDARY)
            x%Boundary_t_w_Q%x_y(:,3) = - x%Boundary_t_w_Q%x_y(:,3)
        END SELECT 
 
        ! Reverse xsections / stage information
        ! Discharge has to be reversed since positive x direction has changed
        reach%xsects=reach%xsects( reach%xsect_count:1:-1 )
        reach%Stage=reach%Stage( reach%xsect_count:1:-1 )
        reach%Discharge=-reach%Discharge( reach%xsect_count:1:-1 )
        reach%Area=reach%Area( reach%xsect_count:1:-1 )
        reach%Width=reach%Width( reach%xsect_count:1:-1 )
        reach%Drag_1D=reach%Drag_1D( reach%xsect_count:1:-1 )
        reach%Discharge_con=-reach%Discharge_con( (reach%xsect_count+1):1:-1 )
     
        ! Reverse the 'delX' term 
        DO i=1,size(reach%downstream_dists(1,:)) 
            reach%downstream_dists(1:(reach%xsect_count-1),i) = reach%downstream_dists( (reach%xsect_count-1):1:-1, i)
            reach%downstream_dists(reach%xsect_count,i) = 0._dp
        END DO

    END SUBROUTINE reverse_reach_order

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE update_junction_values(network)
        TYPE(network_data_type), INTENT(INOUT):: network

    END SUBROUTINE update_junction_values
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE river_classes
