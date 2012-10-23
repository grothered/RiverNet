PROGRAM main
    ! Fortran program: Read hecras file into an array of reach_data
    USE global_defs
    USE hecras_IO
    USE river_classes

    TYPE(REACH_DATA_TYPE), ALLOCATABLE:: reach_data(:) ! Array of reaches
    CHARACTER(len=charlen):: input_file='hectest.g05'

    ! Initiate the geometry by reading the data
    call read_hecras_file(input_file, reach_data, .TRUE.)

    ! Set the initial conditions

    ! Set the boundary conditions 

    ! Run the simulation

END PROGRAM
