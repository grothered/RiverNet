!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
module hecras_IO
    !! Routines to read hec-ras .g* geometry files
    USE global_defs
    USE river_classes
    USE IO_util
    IMPLICIT NONE
   
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE COUNT_REACHES(input_file_unit_no, num_reaches)
        ! DEPRECATED -- Replaced by the generic 'find_line_matches' + size()
        ! Subroutine to count the number of reaches in the input file,
        ! which are identified with the lines 'River Reach='

        INTEGER(dp), INTENT(IN):: input_file_unit_no
        INTEGER(dp), INTENT(OUT):: num_reaches

        INTEGER(dp):: io_test=0, counter=0
        CHARACTER(len=charlen):: pattern_char, format_char

        DO WHILE(io_test>=0)
            pattern_char='River Reach='
            format_char="(A12)"
            CALL next_match(input_file_unit_no, pattern_char, io_test, format_char)

            IF(io_test>=0) THEN
                counter=counter+1
                !print*, 'INCREMENTING, Counter=', counter
            END IF
            !END IF
        END DO

        num_reaches=counter

        ! Back to start of file
        rewind(input_file_unit_no)
    END SUBROUTINE COUNT_REACHES

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE COUNT_XSECTIONS(input_file_unit_no, reach_data, num_reaches)
        ! Count the xsections on each reach, and append the number to the reach_data
        INTEGER(dp), INTENT(IN):: input_file_unit_no, num_reaches
        TYPE(reach_data_type), INTENT(IN OUT):: reach_data(num_reaches)

        CHARACTER(len=charlen):: temp_char
        INTEGER:: io_test=0, reach_counter=0, xsect_counter=0
        
        rewind(input_file_unit_no)
        ! Method: Read the file, incrementing xsect_counter for each new
        ! xsection, and storing/resetting it to zero each time we hit a new river reach 
        DO WHILE(io_test>=0)
            read(input_file_unit_no, "(A25)", iostat=io_test) temp_char

            ! If we hit a new river reach, store the xsect_counter, and reset it to zero
            IF((io_test>=0).AND.(temp_char(1:12)=='River Reach=') )THEN
                !print*, 'NEW REACH', reach_counter
                IF(reach_counter>0) reach_data(reach_counter)%xsect_count=xsect_counter
                reach_counter=reach_counter+1
                xsect_counter=0
            END IF

            ! Count the xsections
            IF((io_test>=0).AND.(temp_char=='Type RM Length L Ch R = 1'))THEN
                xsect_counter=xsect_counter+1
            END IF
        END DO

        ! Add the last one
        reach_data(reach_counter)%xsect_count=xsect_counter

        rewind(input_file_unit_no)

    END SUBROUTINE COUNT_XSECTIONS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    SUBROUTINE READ_REACHES(input_file_unit_no, reach_data, num_reaches)
        ! Subroutine to read reach names/coordinates/xsections etc from hecras geometry file
        INTEGER(dp), INTENT(IN):: input_file_unit_no, num_reaches
        TYPE(reach_data_type), INTENT(IN OUT), target:: reach_data(num_reaches)

        ! Local vars
        CHARACTER(len=charlen):: temp_char, temp_chars(veclen), pattern_char, format_char
        INTEGER(dp):: io_test=0, loop_count, i, j,reach_count, xsect_count
        INTEGER(dp):: cutline_len, yz_len, coordinates_len, mann_change_len
        CHARACTER(len=charlen):: row_chars(2)
        LOGICAL:: NEXT_REACH
        ! Boundary conditions
        TYPE(PHYSICAL_BOUNDARY):: db_pb
        TYPE(JUNCTION_BOUNDARY), ALLOCATABLE:: ub_jb
        ! Shorthand for xsectional data
        TYPE(XSECT_DATA_TYPE), POINTER:: xs

        ! Read every line of the file
        reach_count=0 ! Count which reach we are on
        DO WHILE (io_test>=0) 
            ! Read the enture file
            read(input_file_unit_no, "(A12)",iostat=io_test) temp_char

            IF(io_test<0) THEN
                IF(reach_count==0) THEN
                    PRINT*, "WARNING: No reaches were found in the input file"
                    STOP
                END IF
                RETURN
            END IF

            ! If we are at a 'River Definition', extract name + coordinates
            IF(temp_char=='River Reach=') THEN
                reach_count=reach_count+1
                ! Read the reach name
                BACKSPACE(input_file_unit_no)
                read(input_file_unit_no, "(12X, A16, 1X, A16)", iostat=io_test) temp_chars(1:2)

                PRINT*, "###############################"
                print*, 'REACH NAME:: ', trim(temp_chars(1)),' ', trim(temp_chars(2)) 
                print*, 'REACH COUNT:: ', reach_count
              
                ! Get 'reach_names' 
                ALLOCATE(reach_data(reach_count)%names(2)) 
                reach_data(reach_count)%names(1:2)=temp_chars(1:2)
               
                !!!HYPOTHETICAL BOUNDARY CONDITION ALLOCATION
                ! FIXME: THIS BELONGS ELSEWHERE
                ! Fake physical boundary
                db_pb%boundary_type='Physical_boundary'
                db_pb%input_file='myfile.txt'
                db_pb%compute_method='Prefer_w'

                ! Fake junction boundary
                ALLOCATE(ub_jb)
                ub_jb%boundary_type='Junction_boundary'
                ub_jb%junction_name='myjunc'
                !allocate(jb(reach_count)%reach_names(3))
                ALLOCATE(ub_jb%reach_names(3))
                ALLOCATE(ub_jb%reach_ends(3))
                ub_jb%reach_names(1)="asf"
                ub_jb%reach_names(2)= "asa"
                ub_jb%reach_names(3)= "asdfadsf"
                !allocate(jb(reach_count)%reach_ends(3))
                ub_jb%reach_ends(1:3)=(/ 'Up  ', 'Up  ', 'Down' /)
             
                ! **Randomly** assign boundary types, to test that we can use polymorphism
                IF(mod(reach_count,2_dp)==0) THEN 
                    allocate(reach_data(reach_count)%Downstream_boundary, source=db_pb) 
                    allocate(reach_data(reach_count)%Upstream_boundary, source=ub_jb) 
                ELSE
                    allocate(reach_data(reach_count)%Downstream_boundary, source=ub_jb) 
                    allocate(reach_data(reach_count)%Upstream_boundary, source=db_pb) 
                END IF 
                DEALLOCATE(ub_jb)
                !! END TEST OF BOUNDARY CONDITION 
 
                ! Read the coordinates -- this code pattern is repeated for reading cross-sectional info
                CALL next_match(input_file_unit_no, "Reach XY=", io_test, "(A9)")
                IF(io_test<0) THEN
                    print*, 'ERROR: Failed to find reach coordinates for this reach'
                    stop
                END IF
                backspace(input_file_unit_no)
                READ(input_file_unit_no, "(9X, I8)", iostat=io_test) coordinates_len
                allocate(reach_data(reach_count)%coordinates(coordinates_len,2))
                loop_count=0
                DO i=1,ceiling(coordinates_len/2.0_dp) ! Loop over every line
                    ! We have at most 4 numbers on this line
                    read(input_file_unit_no, "(4A16)", iostat=io_test) temp_chars(1:4)
                    DO j=1,(4/2)
                        loop_count=loop_count+1
                        row_chars=temp_chars((2*j-1):(2*j))
                        ! Check for missing data which can occur on the last line
                        IF(len_trim(row_chars(1))>0) THEN
                            ! Pack into array
                            reach_data(reach_count)%coordinates(loop_count,1:2) = char_2_real(row_chars(1:2))
                        END IF
                    END DO
                END DO

                ! Read the xsectional information on this reach
                ALLOCATE(reach_data(reach_count)%xsects( reach_data(reach_count)%xsect_count ) )
                NEXT_REACH=.FALSE. ! Flag to check if we move onto the next reach
                xsect_count=0 ! Keep track of which xsection we are on

                DO WHILE (NEXT_REACH.eqv..FALSE.)
                    READ(input_file_unit_no, "(A25)", iostat=io_test) temp_char
                    IF(io_test<0) THEN
                        ! End of file
                        RETURN
                    ELSE if(temp_char(1:11)=='River Reach') THEN
                        ! New reach, go back to loop
                        backspace(input_file_unit_no)
                        NEXT_REACH=.TRUE.
                    ELSE if(temp_char=='Type RM Length L Ch R = 1') THEN

                        xsect_count=xsect_count+1 ! Index of cross-section
                        
                        ! Get the cross-sectional header information
                        backspace(input_file_unit_no)
                        READ(input_file_unit_no, "(A)", iostat=io_test) temp_char

                        
                        ! Make pointer to the xsection for shorthand notation
                        xs=> reach_data(reach_count)%xsects(xsect_count)
                        xs%myname=temp_char
                        !reach_data(reach_count)%xsects(xsect_count)%myname=temp_char

                        ! Get the downstream distances, by re-reading the header information
                        ALLOCATE(xs%downstream_dists(3))
                        READ(temp_char(37:charlen),*) xs%downstream_dists(1), &
                                                      xs%downstream_dists(2), &
                                                      xs%downstream_dists(3)

                        ! Get the Cutline information -- follows a similar pattern to reading coordinates
                        CALL next_match(input_file_unit_no, "XS GIS Cut Line", io_test, "(A15)")
                        backspace(input_file_unit_no)
                        READ(input_file_unit_no, "(A16,I8)", iostat=io_test) temp_char, cutline_len
                        !DO WHILE (trim(temp_char) /= "")
                        !print*, 'Cutline length is ', cutline_len
                        allocate(xs%cutline(cutline_len,2)) 
                        loop_count=0 ! Track the row number in cutline
                        DO i=1,ceiling(cutline_len/2.0_dp)
                            ! Pack either 4 or 2 numbers into the cutline array
                            IF(2*i <= cutline_len) THEN
                                ! We have 4 numbers on this line
                                read(input_file_unit_no, "(4A16)", iostat=io_test) temp_chars(1:4)
                                loop_count=loop_count+1
                                xs%cutline(loop_count,1:2) = char_2_real(temp_chars(1:2) )
                                loop_count=loop_count+1
                                xs%cutline(loop_count,1:2) = char_2_real(temp_chars(3:4) )
                            ELSE
                                ! We have 2 numbers on this line
                                read(input_file_unit_no, "(2A16)", iostat=io_test) temp_chars(1:2)
                                loop_count=loop_count+1
                                xs%cutline(loop_count,1:2) = char_2_real(temp_chars(1:2) )
                            END IF
                        END DO                        

                        ! Get the yz information -- this follows a similar pattern to reading the cutline / reach coordinates
                        ! Step 1: Advance file to the right location
                        CALL next_match(input_file_unit_no, '#Sta/Elev=', io_test, "(A10)")
                        backspace(input_file_unit_no)
                        ! Step 2: Read the number of points on the xsection, and allocate an array
                        read(input_file_unit_no, "(11X, I8)", iostat=io_test) yz_len
                        allocate(xs%yz(yz_len,2)) 
                        ! Step3: Pack the numbers into the array
                        loop_count=0 ! Track the row number in the array
                        DO i=1,ceiling(yz_len/5.0_dp) ! Loop over every line
                            ! We have at most 10 numbers on this line
                            read(input_file_unit_no, "(10A8)", iostat=io_test) temp_chars(1:10)
                            ! Pack into array in pairs
                            DO j=1,(10/2)
                                loop_count=loop_count+1
                                row_chars=temp_chars((2*j-1):(2*j))
                                ! Check for 'missing data' which can occur on the last line
                                IF(len_trim(row_chars(1))>0) THEN
                                    ! Pack into array
                                    xs%yz(loop_count,1:2) = char_2_real(row_chars(1:2))
                                END IF
                            END DO
                        END DO                        

                        ! Get the manning's n information -- note that we will already be at the correct line
                        ! FIXME: Careful, this format may not be correct with > 9 manning change points (??)
                        read(input_file_unit_no, "(6X, I3)", iostat=io_test) mann_change_len
                        allocate(xs%roughness(mann_change_len,2))
                        loop_count=0
                        ! Loop over every line
                        DO i=1,ceiling(mann_change_len/3.0_dp)
                            read(input_file_unit_no, "(9A8)", iostat=io_test) temp_chars(1:9)
                            DO j=1,3
                                loop_count=loop_count+1
                                row_chars=temp_chars( (3*j-2):(3*j-1) ) ! y value, manning change value (ignore the zero)
                                IF(len_trim(row_chars(1))>0) THEN
                                   xs%roughness(loop_count, 1:2) = char_2_real(row_chars(1:2)) 
                                END IF 
                            END DO
                        END DO 

                    END IF ! Type RM ...
                END DO 

            END IF ! temp_char=='River Reach'


        END DO ! while io_test/=0

        rewind(input_file_unit_no)
    END SUBROUTINE READ_REACHES



END MODULE hecras_IO

