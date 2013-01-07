MODULE IO_util
    USE global_defs
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
        INTEGER(ip):: input_file_unit_no
        INTEGER(ip):: count_file_lines

        INTEGER(ip)::io_test=0

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
        INTEGER(ip), INTENT(IN):: input_file_unit_no
        CHARACTER(*), INTENT(IN):: format_string
        CHARACTER(len=charlen), ALLOCATABLE, INTENT(INOUT):: output_lines(:)

        INTEGER(ip):: num_lines, i, io_test=0

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
        INTEGER(ip), INTENT(IN):: input_file_unit_no
        INTEGER(ip), INTENT(OUT)::  io_test
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
        INTEGER(ip), allocatable, INTENT(OUT):: indices(:)

        INTEGER(ip):: pos, i, n, counter
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
    FUNCTION count_line_matches(input_file_unit_no, pattern, format_string)
        ! Count the number of lines in a file with unit number 'input_file_unit_no',
        ! matching 'pattern'. File is read with 'format_string'
        INTEGER(ip), INTENT(IN):: input_file_unit_no
        CHARACTER(*), INTENT(IN):: pattern, format_string
        INTEGER(ip):: count_line_matches
        ! Local variable
        INTEGER(ip):: io_test

        rewind(input_file_unit_no)
       
        io_test=0
        count_line_matches=0
        DO WHILE (io_test>=0)
           call next_match(input_file_unit_no, pattern, io_test,format_string)

           IF(io_test>=0) count_line_matches=count_line_matches+1  
        END DO
        !print*, count_line_matches
        !print*, io_test
        !stop
        rewind(input_file_unit_no)

    END FUNCTION count_line_matches
END MODULE IO_util
