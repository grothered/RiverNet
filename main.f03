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

    INTEGER:: i, N, M

    ! Initiate the geometry by reading the data
    !call read_hecras_file(input_geometry_file, network, .TRUE.)
    call read_hecras_file(input_geometry_file, network, print_output=.FALSE.)

    ! Set the boundary conditions
    call read_hecras_boundary_conditions(input_boundary_file, network) 
    
    ! Set the initial conditions
    call set_initial_conditions(network%reach_data(1), 1.0_dp, 0._dp)
    print*, 'Have set initial conditions'

    ! Make an output file
    open(newunit=N, file='output.txt')

    M=network%reach_data(1)%xsect_count

    ! Run the simulation
    DO i=1,1000
        print*, '## Step ', i, '; Time ', network%time, '; dT:', network%dT
        print*, '   Q(1) = ', network%reach_data(1)%Discharge(1), ' Q(M) = ', network%reach_data(1)%Discharge(M)
        print*, ' Drag_1D(1) * d_bar ', &
                 network%reach_data(1)%Drag_1D(1)*network%reach_data(1)%Area(1)/network%reach_data(1)%Width(1)
        print*, 'd_bar(1) = ', network%reach_data(1)%Area(1)/network%reach_data(1)%Width(1)


        write(N,*) network%reach_data(1)%Stage
        write(N,*) network%reach_data(1)%Area
        write(N,*) network%reach_data(1)%Area/network%reach_data(1)%Width
        write(N,*) network%reach_data(1)%Discharge
        write(N,*) network%reach_data(1)%Discharge/network%reach_data(1)%Area
        
        call evolve_hydraulics(network)
    END DO

END PROGRAM
