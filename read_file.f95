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
    character(len=charlen), pointer:: tmp_char_pointer

    ! Variables to hold data
    type(reach_data_type), allocatable, target:: reach_data(:)
    character(len=charlen), allocatable:: file_lines(:)
    integer(dp), allocatable:: reach_indices(:), xsect_indices(:)
    
    integer:: i,j,k
    ! Open the input file
    OPEN(unit=input_file_unit_no, file=input_file)
  
    ! Read input file (note -- some earlier functions don't need the file read
    ! in, they just read from it as needed). But reading is probably easier
    format_string="(A)" ! Read all lines 
    call read_character_file(input_file_unit_no, file_lines, "(A)")
    print*, 'Length of file_lines = ', size(file_lines)


    ! Count the number of reaches
    !call count_reaches(input_file_unit_no, num_reaches) -- this operates on the file
    call find_line_matches(file_lines, 'River Reach=', reach_indices)
    print*, reach_indices
    num_reaches=size(reach_indices)
    print*, 'There are ', num_reaches, ' reaches'
    !stop

    ! Set up storage for reach information
    ALLOCATE( reach_data(num_reaches) )

    ! Count the number of xsections
    !call count_xsections(input_file_unit_no, reach_data, num_reaches) -- this operates on the file
    call find_line_matches(file_lines, 'Type RM Length L Ch R = 1', xsect_indices)
    reach_data(num_reaches)%xsect_count=count(xsect_indices>reach_indices(num_reaches))
    IF(num_reaches>1) THEN
        DO i = num_reaches-1, 1, -1
            reach_data(i)%xsect_count=count((xsect_indices>reach_indices(i)).AND.(xsect_indices<reach_indices(i+1)))
        END DO
    END IF

    call read_reaches(input_file_unit_no, reach_data, num_reaches) !-- this operates on the file
    CLOSE(input_file_unit_no)

    ! Print output
    IF(.TRUE.) THEN
        ! FIXME: Could use this to make a 'print_reach' routine
        DO i=1,num_reaches
            print*, trim(reach_data(i)%names(1)), ' ',trim(reach_data(i)%names(2))
            !print*, 'XSECT COUNT2 =', reach_data(i)%xsect_count2
            print*, 'Coordinates count=', size(reach_data(i)%coordinates(:,1))
            DO j=1,size(reach_data(i)%coordinates(:,1))
                print*, reach_data(i)%coordinates(j,1:2)
            END DO

            print*, 'XSECT COUNT =', reach_data(i)%xsect_count
        
            DO j=1,size(reach_data(i)%xsects)
                print*, trim(reach_data(i)%xsects(j)%myname)
                !tmp_char_pointer=>reach_data(i)%xsects(j)%myname
                !print*, tmp_char_pointer
                !write(tmp_char_pointer,*) temp_char(1:2)
                !DO k=1,20
                !    print*, temp_char(k)
                !END DO
                !stop
                print*, 'Cutline size = ', size(reach_data(i)%xsects(j)%cutline(:,1))
                DO k=1,size(reach_data(i)%xsects(j)%cutline(:,1))
                    print*, reach_data(i)%xsects(j)%cutline(k,1:2)
                END DO

                print*, 'Xsect size = ', size(reach_data(i)%xsects(j)%yz(:,1))
                DO k=1,size(reach_data(i)%xsects(j)%yz(:,1))
                    print*, reach_data(i)%xsects(j)%yz(k,1:2) 
                END DO
                
                print*, 'Xsect roughness change points:'
                DO k=1, size(reach_data(i)%xsects(j)%roughness(:,1))
                    print*, reach_data(i)%xsects(j)%roughness(k,1:2)
                END DO
            END DO
        END DO
    END IF
END PROGRAM

