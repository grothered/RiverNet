PROGRAM main
    ! Fortran program: Read hecras file into an array of reach_data
    USE global_defs
    USE hecras_IO
    USE river_classes

    TYPE(REACH_DATA_TYPE), ALLOCATABLE:: reach_data(:) ! Array of reaches
    CHARACTER(len=charlen):: input_file='hectest.g05'

    call read_hecras_file(input_file, 11_dp, reach_data, .TRUE.) 

END PROGRAM
