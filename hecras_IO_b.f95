!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
module hecras_IO
    !! Routines to read hec-ras .g* geometry files
    USE global_defs
    USE river_classes
    IMPLICIT NONE
   
    contains
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    PURE ELEMENTAL FUNCTION char_2_real(x)
        ! Function to convert a character to a real, by 'reading' it
        CHARACTER(len=charlen), INTENT(IN):: x
        REAL(dp):: char_2_real

        IF(len_trim(x)>0) THEN
            read(x,*) char_2_real
        ELSE
            ! 'MISSING VALUE'
            char_2_real=missing_value
        END IF 
    END FUNCTION

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    FUNCTION count_file_lines(input_file_unit_no)
        ! Count the number of lines in input_file_unit_no
        INTEGER(dp):: input_file_unit_no
        INTEGER(dp):: count_file_lines

        INTEGER(dp)::io_test=0

        rewind(input_file_unit_no) ! Start of file

        count_file_lines=0
        DO WHILE (io_test>=0)
            READ(input_file_unit_no,*, iostat=io_test)
            IF(io_test>=0) count_file_lines=count_file_lines+1 
        END DO

        rewind(input_file_unit_no) ! Start of file
    END FUNCTION

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE read_character_file(input_file_unit_no, output_lines, format_string)
        ! Read the entire contents of input_file_unit_no into the allocatable
        ! character array 'output_lines'. Use format_string in the read
        INTEGER(dp), INTENT(IN):: input_file_unit_no
        CHARACTER(*), INTENT(IN):: format_string
        CHARACTER(len=charlen), ALLOCATABLE, INTENT(INOUT):: output_lines(:)

        INTEGER(dp):: num_lines, i, io_test=0

        ! Clean out 'output_lines' in case it is already allocated
        if(allocated(output_lines)) deallocate(output_lines)

        ! Count how many lines we will need
        num_lines=count_file_lines(input_file_unit_no)
        allocate(output_lines(num_lines))

        print*, 'reading file ...' 
        !stop
        DO i=1,num_lines
            read(input_file_unit_no, format_string, iostat=io_test) output_lines(i) 
            !print*, trim(output_lines(i))
        END DO
        rewind(input_file_unit_no)

    END SUBROUTINE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE next_match(input_file_unit_no, pattern, io_test, format_string)
        ! If input_file_unit_no is open for reading, advance to the next line
        ! matching 'pattern'.
        ! If no match, then return to the start of the file [io_test will be
        ! returned negative in this case]
        ! Assumes file is read with format=format_string, and io_test is used
        ! to track IO errors
        INTEGER(dp), INTENT(IN):: input_file_unit_no
        INTEGER(dp), INTENT(OUT)::  io_test
        CHARACTER(*), INTENT(IN):: pattern, format_string

        CHARACTER(len=charlen) temp_char

        temp_char='NOT'//pattern ! A string which will not = pattern

        DO WHILE ( (trim(temp_char) .NE. pattern).AND.(io_test >=0) )
            READ(input_file_unit_no, trim(format_string), iostat=io_test) temp_char
        END DO
       
        IF(io_test<0) rewind(input_file_unit_no) 

    END SUBROUTINE next_match

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE find_line_matches(lines, pattern, indices)
        ! Find the indices in 'lines' (character vector of length 'n') which match with 'pattern'
        ! FIXME: Presently, pattern must be at the start of the string
        CHARACTER(len=charlen), INTENT(IN):: lines(:)
        CHARACTER(*),INTENT(IN):: pattern
        INTEGER(dp), allocatable, INTENT(OUT):: indices(:)

        INTEGER(dp):: pos, i, n, counter
        CHARACTER(len=charlen):: mychar

        ! Remove indices if already exists
        if(allocated(indices)) deallocate(indices)

        n=len_trim(pattern) ! Length of pattern

        ! Count occurrences of pattern (so we know size to allocate)
        counter=0
        DO i=1,size(lines)
            mychar=lines(i)
            IF(mychar(1:n)==pattern) THEN
                counter=counter+1
            END IF
        END DO

        IF(counter>0) THEN
            ALLOCATE(indices(counter))
            counter=0
            DO i=1,size(lines)
                mychar=lines(i)
                IF(mychar(1:n)==pattern) THEN
                    counter=counter+1
                    indices(counter)=i
                END IF
            END DO
        END IF
            
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
        ! Subroutine to read reach names/coordinates from hecras geometry file
        INTEGER(dp), INTENT(IN):: input_file_unit_no, num_reaches
        TYPE(reach_data_type), INTENT(IN OUT):: reach_data(num_reaches)

        ! Local vars
        CHARACTER(len=charlen):: temp_char, temp_chars(veclen), pattern_char, format_char
        INTEGER(dp):: io_test=0, loop_count, i, j,reach_count, xsect_count, cutline_len, yz_len
        REAL(dp):: xs(large_array_len), ys(large_array_len)
        CHARACTER(len=charlen):: xs_c(large_array_len), ys_c(large_array_len), row_chars(2)
        LOGICAL:: NEXT_REACH

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
                
                ! 
                read(input_file_unit_no, *, iostat=io_test) ! Skip a line
                IF(io_test<0) THEN
                    PRINT*, "ERROR: UNEXPECTED END OF FILE_a"
                    STOP  
                END IF

                ! Read the coordinates
                loop_count=0
                DO WHILE (trim(temp_char)/='Rch Text')
                    !print*, '.'
                    read(input_file_unit_no, "(A8)",iostat=io_test) temp_char ! Check for 'Rch'
                    !print*, temp_char
                    IF(io_test<0) THEN
                        PRINT*, "ERROR: UNEXPECTED END OF FILE_b"
                        STOP  
                    END IF
                    !print*, temp_char, temp_char=='Rch Text'

                    IF(trim(temp_char)=='Rch Text') THEN
                        continue
                    ELSE
                        backspace(input_file_unit_no)
                        ! Read coordinates as characters
                        read(input_file_unit_no, "(4A16)",iostat=io_test) xs_c(2*loop_count+1), ys_c(2*loop_count+1),&
                                                       xs_c(2*loop_count+2), ys_c(2*loop_count+2)
                        ! Coerce to numeric vectors
                        !print*, xs_c(2*loop_count+1), ys_c(2*loop_count+1)
                        !print*, xs_c(2*loop_count+2), ys_c(2*loop_count+2)
                        !print*, xs_c(2*loop_count+2), ys_c(2*loop_count+2)
                        loop_count=loop_count+1
                        IF(io_test<0) THEN
                            PRINT*, "ERROR: UNEXPECTED END OF FILE_c"
                            STOP  
                        END IF
                    END IF
                END DO
                ! Coerce character data to reals
                xs(1:(2*loop_count))=char_2_real(xs_c(1:(2*loop_count)))
                ys(1:(2*loop_count))=char_2_real(ys_c(1:(2*loop_count)))

                allocate(reach_data(reach_count)%coordinates(2*loop_count,2))
                reach_data(reach_count)%coordinates(1:2*loop_count,1) = xs(1:(2*loop_count))
                reach_data(reach_count)%coordinates(1:2*loop_count,2) = ys(1:(2*loop_count))
                
                !print*, 'DEBUG: Allocated reach coords'
                !DO i=1,2*loop_count
                !    print*, xs(i), ys(i)
                !END DO

                ! Read the xsectional information
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

                        ! Get the cross-sectional header information
                        backspace(input_file_unit_no)
                        READ(input_file_unit_no, "(A100)", iostat=io_test) temp_char

                        xsect_count=xsect_count+1
                        reach_data(reach_count)%xsects(xsect_count)%myname=temp_char

                        ! Get the Cutline information
                        ! FIXME: Problem is that not all things matching 'Type RM ...'
                        ! have cutlines / yz sections etc. Need some type extension
                        CALL next_match(input_file_unit_no, "XS GIS Cut Line", io_test, "(A15)")
                        backspace(input_file_unit_no)
                        READ(input_file_unit_no, "(A16,I8)", iostat=io_test) temp_char, cutline_len
                        !DO WHILE (trim(temp_char) /= "")
                        !print*, 'Cutline length is ', cutline_len
                        allocate(reach_data(reach_count)%xsects(xsect_count)%cutline(cutline_len,2)) 
                        loop_count=0 ! Track the row number in cutline
                        DO i=1,ceiling(cutline_len/2.0_dp)
                            ! Pack either 4 or 2 numbers into the cutline array
                            IF(2*i <= cutline_len) THEN
                                ! We have 4 numbers on this line
                                read(input_file_unit_no, "(4A16)", iostat=io_test) temp_chars(1:4)
                                loop_count=loop_count+1
                                reach_data(reach_count)%xsects(xsect_count)%cutline(loop_count,1:2) = char_2_real(temp_chars(1:2) )
                                loop_count=loop_count+1
                                reach_data(reach_count)%xsects(xsect_count)%cutline(loop_count,1:2) = char_2_real(temp_chars(3:4) )
                            ELSE
                                ! We have 2 numbers on this line
                                read(input_file_unit_no, "(2A16)", iostat=io_test) temp_chars(1:2)
                                loop_count=loop_count+1
                                reach_data(reach_count)%xsects(xsect_count)%cutline(loop_count,1:2) = char_2_real(temp_chars(1:2) )
                            END IF
                        END DO                        

                        ! Get the yz information
                        ! Step 1: Advance file to the right location
                        CALL next_match(input_file_unit_no, '#Sta/Elev=', io_test, "(A10)")
                        backspace(input_file_unit_no)
                        ! Step 2: Read the number of points on the xsection, and allocate an array
                        read(input_file_unit_no, "(11X, I8)", iostat=io_test) yz_len
                        allocate(reach_data(reach_count)%xsects(xsect_count)%yz(yz_len,2)) 
                        ! Step3: Pack the numbers into the array
                        loop_count=0 ! Track the row number in the array
                        DO i=1,ceiling(yz_len/10.0_dp) ! Loop over every line
                            ! We have at most 10 numbers on this line
                            read(input_file_unit_no, "(10A8)", iostat=io_test) temp_chars(1:10)
                            ! Pack into array in pairs
                            DO j=1,(10/2)
                                loop_count=loop_count+1
                                row_chars=temp_chars((2*j-1):(2*j))
                                ! Check for 'missing data' which can occur on the last line
                                IF(len_trim(row_chars(1))>0) THEN
                                    ! Pack into array
                                    reach_data(reach_count)%xsects(xsect_count)%yz(loop_count,1:2) = char_2_real(row_chars(1:2))
                                END IF
                            END DO
                        END DO                        
                        !pattern_char='#Sta/Elev='
                        !format_char="(A9)"
                        !CALL next_match(input_file_unit_no, pattern_char, io_test, format_char)
                        !backspace(input_file_unit_no)
                        !READ(input_file_unit_no, "(10X,I8)", iostat=io_test) yz_len
                        !loop_count=0
                        !DO i=1,ceiling(yz_len/10.0_dp)
                        !    IF(10*i<=yz_len) THEN

                        !    ELSE

                        !    END IF

                        !END DO

                        ! Get the manning's n information
                        !END DO
                    END IF
                END DO
                !reach_data%xsect_count2=xsect_count
            END IF 


        END DO 

        rewind(input_file_unit_no)
    END SUBROUTINE READ_REACHES



END MODULE hecras_IO

