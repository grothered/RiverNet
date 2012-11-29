PROGRAM main
    ! Fortran program: Read hecras file into an array of reach_data
    USE global_defs
    USE hecras_IO
    USE river_classes

    !TYPE(REACH_DATA_TYPE), ALLOCATABLE:: reach_data(:) ! Array of reaches
    TYPE(NETWORK_DATA_TYPE):: network
    !CHARACTER(len=charlen):: input_file='hectest.g05'
    CHARACTER(len=charlen):: input_geometry_file='./eg/test.g01'
    CHARACTER(len=charlen):: input_boundary_file='./eg/test.u03'

    ! Initiate the geometry by reading the data
    !call read_hecras_file(input_geometry_file, network, .TRUE.)
    call read_hecras_file(input_geometry_file, network, print_output=.FALSE.)

    ! Set the initial conditions

    ! Set the boundary conditions
    call read_hecras_boundary_conditions(input_boundary_file, network, print_output=.TRUE.) 

    ! Run the simulation
    !call evolve_hydraulics(network)
END PROGRAM
