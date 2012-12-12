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

    INTEGER:: i, N_flow, N_time, M

    ! Initiate the geometry by reading the data
    !call read_hecras_file(input_geometry_file, network, .TRUE.)
    call read_hecras_file(input_geometry_file, network, print_output=.FALSE.)

    ! Set the boundary conditions
    call read_hecras_boundary_conditions(input_boundary_file, network) 
    
    ! Set the initial conditions
    call set_initial_conditions(network%reach_data(1), 1.0_dp, 0._dp)
    print*, 'Have set initial conditions'

    !print*, 'Reversing the reach data for sport'
    !call reverse_reach_order(network%reach_data(1))
    
    ! Make an output file
    open(newunit=N_flow, file='output.txt')
    open(newunit=N_time, file='time.txt')

    M=network%reach_data(1)%xsect_count

    ! Run the simulation
    DO i=1,40000

        ! IO BLOCK
        IF(mod(i-1,10).eq.0) THEN
            print*, '## Step ', i, '; Time (hr) ', network%time/3600._dp, '; dT:', network%dT
            print*, '   Q(1) = ', network%reach_data(1)%Discharge(1), ' Q(M) = ', network%reach_data(1)%Discharge(M)
            print*, ' Drag_1D(1) * d_bar ', &
                     network%reach_data(1)%Drag_1D(1)*network%reach_data(1)%Area(1)/network%reach_data(1)%Width(1)
            print*, 'd_bar(1) = ', network%reach_data(1)%Area(1)/network%reach_data(1)%Width(1)
    
    
            write(N_flow,*) network%reach_data(1)%Stage
            write(N_flow,*) network%reach_data(1)%Area
            write(N_flow,*) network%reach_data(1)%Area/network%reach_data(1)%Width
            write(N_flow,*) network%reach_data(1)%Discharge
            write(N_flow,*) network%reach_data(1)%Discharge_con
            write(N_time,*) network%time
            !write(N,*) network%reach_data(1)%Discharge/network%reach_data(1)%Area
        END IF
        
        call evolve_hydraulics(network)
    END DO

    close(N_flow)
    close(N_time)

END PROGRAM
