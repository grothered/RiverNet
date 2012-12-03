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

    INTEGER:: i, N

    ! Initiate the geometry by reading the data
    !call read_hecras_file(input_geometry_file, network, .TRUE.)
    call read_hecras_file(input_geometry_file, network, print_output=.FALSE.)

    ! Set the boundary conditions
    call read_hecras_boundary_conditions(input_boundary_file, network) 
    
    ! Set the initial conditions
    print*, 'Warning -- setting initial conditions in a crazy way -- FIXME:'
    N=size(network%reach_data(1)%Stage)
    DO i=1,N
        ! Depth of 1m
        network%reach_data(1)%Stage(i)= minval(network%reach_data(1)%xsects(i)%yz(:,2)) + 1.0_dp
        network%reach_data(1)%Area(i) = network%reach_data(1)%xsects(i)%stage_etc_curve%eval( &
                                              network%reach_data(1)%Stage(i), 'stage', 'area')
        network%reach_data(1)%Width(i) = network%reach_data(1)%xsects(i)%stage_etc_curve%eval( &
                                              network%reach_data(1)%Stage(i), 'stage', 'width')
        network%reach_data(1)%Discharge(i) = 0._dp 
        network%reach_data(1)%Width(i) = network%reach_data(1)%xsects(i)%stage_etc_curve%eval( &
                                              network%reach_data(1)%Stage(i), 'stage', 'drag_1D')
    END DO
    print*, 'Have set initial conditions'

    open(newunit=N, file='output.txt')

    ! Run the simulation
    DO i=1,1000
        print*, '## Step ', i, '; Time ', network%time, '; dT:', network%dT
        call evolve_hydraulics(network)
        write(N,*) network%reach_data(1)%Stage
        write(N,*) network%reach_data(1)%Area
        write(N,*) network%reach_data(1)%Area/network%reach_data(1)%Width
        write(N,*) network%reach_data(1)%Discharge
        write(N,*) network%reach_data(1)%Discharge/network%reach_data(1)%Area
    END DO

END PROGRAM
