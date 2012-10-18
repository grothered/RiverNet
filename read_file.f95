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
    character(len=charlen):: temp_char(veclen)
    integer(dp):: input_file_unit_no=11, temp_int(veclen), num_reaches
    real(dp):: temp_real(veclen)

    type(reach_data_type), allocatable:: reach_data(:)

    integer:: i,j,k
    ! Open the input file
    OPEN(unit=input_file_unit_no, file=input_file)

    ! Count the number of reaches
    CALL COUNT_REACHES(input_file_unit_no, num_reaches)
    print*, 'There are ', num_reaches, ' reaches'

    ! Set up storage for reach information
    ALLOCATE( reach_data(num_reaches) )

    CALL COUNT_XSECTIONS(input_file_unit_no, reach_data, num_reaches)

    CALL READ_REACHES(input_file_unit_no, reach_data, num_reaches)
    CLOSE(input_file_unit_no)

    ! Print output
    DO i=1,num_reaches
        print*, trim(reach_data(i)%names(1)), ' ',trim(reach_data(i)%names(2))
        print*, 'XSECT COUNT =', reach_data(i)%xsect_count
        !print*, 'XSECT COUNT2 =', reach_data(i)%xsect_count2
        DO j=1,size(reach_data(i)%coordinates(:,1))
            print*, reach_data(i)%coordinates(j,1:2)
        END DO
    
        DO j=1,size(reach_data(i)%xsects)
            print*, trim(reach_data(i)%xsects(j)%myname)
            DO k=1,size(reach_data(i)%xsects(j)%cutline(:,1))
                print*, reach_data(i)%xsects(j)%cutline(k,1:2)
            END DO
        END DO
    END DO
END PROGRAM
