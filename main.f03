PROGRAM main
    !! Fortran program: Read hecras file into an array of reach_data
    USE global_defs
    USE hecras_IO
    USE river_classes
    USE network_solver
    IMPLICIT NONE

    !TYPE(REACH_DATA_TYPE), ALLOCATABLE:: reach_data(:) ! Array of reaches
    TYPE(NETWORK_DATA_TYPE):: network
    !CHARACTER(len=charlen):: input_geometry_file='hectest.g05'
    !CHARACTER(len=charlen):: input_geometry_file='./eg/test.g01'
    !CHARACTER(len=charlen):: input_boundary_file='./eg/test.u03'
    CHARACTER(len=charlen):: input_geometry_file='./test_3rivers/marikina.g02'
    CHARACTER(len=charlen):: input_boundary_file='./test_3rivers/marikina.u03'

    INTEGER(ip):: i, N_flow, N_time, M
    REAL(dp):: xx

    !print*, index('stage_discharge', 'discharge')
    !stop

    ! Initiate the geometry by reading the data
    print*, 'Reading geometry ...'
    call read_hecras_file(input_geometry_file, network, print_output=.FALSE.)
    
    ! Open output files
    call network%create_outfiles()

    ! Set the boundary conditions
    print*, 'Reading boundary conditions ...'
    call read_hecras_boundary_conditions(input_boundary_file, network) 

    !! Hacky checks
    !DO i=1,network%num_reaches
    !    IF(trim(network%reach_data(i)%names(2)) == 'Upper_marikina') THEN
    !        !print*, trim(network%reach_data(i)%names(1)), trim(network%reach_data(i)%names(2))
    !        !call network%reach_data(i)%print()
    !        !print*, ' ---'
    !        !call network%reach_data(i)%Upstream_boundary%print()
    !    END IF
    !END DO

    ! Set the initial conditions
    print*, 'Seting initial conditions ...'
    call set_initial_conditions(network, 1.0e-01_dp, 0._dp)


    ! Run the simulation
    print*, 'Running the simulation...'
    DO i=1,max_its
        call network%print_status(i)
        call network%write_data(i)
        call evolve_hydraulics(network)
    END DO

    ! Close the output files
    call network%close_outfiles()

END PROGRAM

!! CHECK IF THE BOUNDARY CONDITION STARTTIMES FIT WITH THE MODEL STARTTIME
!! THIS COULD BE A GOOD TEST TO INCLUDE GENERALLY
!print*, 'Time = ', network%time
!print*, 'Boundary condition starttimes are: '
!DO i=1, network%num_physical_boundaries
!    xx=network%physical_boundaries(i)%Boundary_t_w_Q%x_y(1,1)
!    print*, i, xx, xx.LE.network%time, network%physical_boundaries(i)%Boundary_t_w_Q%last_search_index, &
!            network%physical_boundaries(i)%physical_boundaries_index
!END DO
!print*, 'Finished boundary condition starttimes'

!! LOOK AT THE BOUNDARIES
!DO i=1,network%num_reaches
!    call network%reach_data(i)%Upstream_boundary%print()
!    call network%reach_data(i)%Downstream_boundary%print()
!END DO

!xx=datetime_string_to_seconds('Date/Time=24SEP2009,00:00')
!print*, xx
!stop

!stop
!print*, 'reversing reach data for sport'
!call reverse_reach_order(network%reach_data(1), network)
