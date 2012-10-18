! Test code to try to read formatted text

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Test program to extract info from hecras geometry file
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM read_text
    USE global_defs
    USE hecras_IO
    IMPLICIT NONE

    character(len=charlen):: input_file='hectest.g05'!, temp_char(100)
    character(len=charlen):: temp_char(veclen), format_string
    integer(dp):: input_file_unit_no=11, temp_int(veclen), num_reaches
    real(dp):: temp_real(veclen)

    ! Variables to hold data
    type(reach_data_type), allocatable:: reach_data(:)
    character(len=charlen), allocatable:: file_lines(:)
    integer(dp), allocatable:: indices(:)
    
    integer:: i,j,k
    ! Open the input file
    OPEN(unit=input_file_unit_no, file=input_file)
  
    ! Read input file 
    format_string="(A1024)" 
    call read_character_file(input_file_unit_no, file_lines, format_string)
    print*, 'Length of file_lines = ', size(file_lines)

    temp_char(1)='River Reach='
    call find_line_matches(file_lines, temp_char(1), indices)
    print*, indices
    stop

    ! Count the number of reaches
    call count_reaches(input_file_unit_no, num_reaches)
    print*, 'There are ', num_reaches, ' reaches'

    ! Set up storage for reach information
    ALLOCATE( reach_data(num_reaches) )

    call count_xsections(input_file_unit_no, reach_data, num_reaches)

    call read_reaches(input_file_unit_no, reach_data, num_reaches)
    CLOSE(input_file_unit_no)

    ! Print output
    IF(.FALSE.) THEN
        DO i=1,num_reaches
            print*, trim(reach_data(i)%names(1)), ' ',trim(reach_data(i)%names(2))
            print*, 'XSECT COUNT =', reach_data(i)%xsect_count
            !print*, 'XSECT COUNT2 =', reach_data(i)%xsect_count2
            DO j=1,size(reach_data(i)%coordinates(:,1))
                print*, reach_data(i)%coordinates(j,1:2)
            END DO
        
            DO j=1,size(reach_data(i)%xsects)
                print*, trim(reach_data(i)%xsects(j)%myname)


                ! FIXME: Cutline reading needs fixing
                print*, 'Cutline size = ', size(reach_data(i)%xsects(j)%cutline(:,1))
                DO k=1,size(reach_data(i)%xsects(j)%cutline(:,1))
                    print*, reach_data(i)%xsects(j)%cutline(k,1:2)
                END DO
            END DO
        END DO
    END IF
END PROGRAM

