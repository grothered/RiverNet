PROGRAM main
    ! Fortran program: Read hecras file into an array of reach_data
    USE global_defs
    USE hecras_IO
    USE river_classes
    USE network_solver

    !TYPE(REACH_DATA_TYPE), ALLOCATABLE:: reach_data(:) ! Array of reaches
    TYPE(NETWORK_DATA_TYPE):: network
    !CHARACTER(len=charlen):: input_geometry_file='hectest.g05'
    CHARACTER(len=charlen):: input_geometry_file='./eg/test.g01'
    CHARACTER(len=charlen):: input_boundary_file='./eg/test.u03'

    INTEGER(ip):: i, N_flow, N_time, M

    ! Initiate the geometry by reading the data
    call read_hecras_file(input_geometry_file, network, print_output=.FALSE.)

    !DO i=1,network%num_junctions
    !   call network%reach_junctions(i)%print()
    !END DO    
    !stop

    ! Set the boundary conditions
    call read_hecras_boundary_conditions(input_boundary_file, network) 
    
    ! Set the initial conditions
    call set_initial_conditions(network%reach_data(1), 1.0e-04_dp, 0._dp)
    print*, 'Have set initial conditions'

    !print*, 'reversing reach data for sport'
    !call reverse_reach_order(network%reach_data(1), network)

    ! Open output files
    call network%create_outfiles()

    ! Run the simulation
    DO i=1,max_its
        call network%print_status(i)
        call network%write_data(i)
        
        call evolve_hydraulics(network)
        !call network%reach_data(1)%reverse_reach_order()
        !print*, 'reversing reach data for sport'
        !call reverse_reach_order(network%reach_data(1), network)
    END DO

    ! Close the output files
    call network%close_outfiles()

END PROGRAM
