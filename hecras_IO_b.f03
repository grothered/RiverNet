!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
module hecras_IO
    !! Routines to read hec-ras .g* geometry files
    USE global_defs
    USE river_classes
    USE reach_boundary_classes
    USE xsect_classes
    USE IO_util
    IMPLICIT NONE
   
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE COUNT_REACHES(input_file_unit_no, num_reaches)
        ! Subroutine to count the number of reaches in the input file,
        ! which are identified with the lines 'River Reach='

        ! NOTE: CAN ALTERNATIVELY USE the generic 'find_line_matches' + size()
        ! But that requires reading the entire input file, whereas this doesn't

        INTEGER(ip), INTENT(IN):: input_file_unit_no
        INTEGER(ip), INTENT(OUT):: num_reaches

        INTEGER(ip):: io_test=0, counter=0
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
        INTEGER(ip), INTENT(IN):: input_file_unit_no, num_reaches
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
        INTEGER(ip), INTENT(IN):: input_file_unit_no, num_reaches
        TYPE(reach_data_type), INTENT(IN OUT), target:: reach_data(num_reaches)

        ! Local vars
        CHARACTER(len=charlen):: temp_char, temp_chars(veclen), pattern_char, format_char
        INTEGER(ip):: io_test=0, loop_count, i, j,reach_count, xsect_count
        INTEGER(ip):: cutline_len, yz_len, coordinates_len, mann_change_len, temp_int
        REAL(dp):: temp_reals(veclen,2)
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
                reach_data(reach_count)%names(1:2)=temp_chars(1:2)
               
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
                        !allocate(xs%roughness(mann_change_len,2))
                        allocate(xs%roughness(yz_len,1)) ! 1 manning value for every bed point
                        loop_count=0
                        ! Loop over every line
                        DO i=1,ceiling(mann_change_len/3.0_dp)
                            read(input_file_unit_no, "(9A8)", iostat=io_test) temp_chars(1:9)
                            ! Loop over every manning value on this line
                            DO j=1,3
                                loop_count=loop_count+1
                                row_chars=temp_chars( (3*j-2):(3*j-1) ) ! y value, manning change value (ignore the zero)
                                IF(len_trim(row_chars(1))>0) THEN
                                    !Store manning_change_point, roughness in temporary array
                                    temp_reals(loop_count, 1:2) = char_2_real(row_chars(1:2)) 
                                ELSE
                                    ! We have read all the manning values
                                    EXIT
                                END IF 
                            END DO
                        END DO
                        ! Now fill out xs%roughness(1:yz_len,1)
                        DO i=1,yz_len
                            temp_int=count(xs%yz(i,1) >= temp_reals(1:mann_change_len,1)) ! Index of desired manning value
                            xs%roughness(i,1) = temp_reals(temp_int,2) 
                            ! Sanity check
                            IF(xs%roughness(i,1) < 0.01) THEN
                                print*, 'ERROR: Low roughness values -- is this an IO error? '
                                print*, xs%roughness(1:i,1)
                                stop
                            END IF
                        END DO

                    END IF ! Type RM ...
                END DO 

            END IF ! temp_char=='River Reach'


        END DO ! while io_test/=0
        xs=>NULL()

        rewind(input_file_unit_no)
    END SUBROUTINE READ_REACHES
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE read_junctions(input_file_unit_no, network)
        ! Read information from hecras file about junctions into network structure
        INTEGER(ip), INTENT(IN):: input_file_unit_no
        TYPE(network_data_type), TARGET, INTENT(INOUT)::network

        INTEGER(ip):: io_test=0, i, join_count, j, junction_count=0, tmp
        TYPE(JUNCTION_BOUNDARY), POINTER:: jb=>NULL()
        CHARACTER(len=charlen):: temp_char, temp_chars(veclen)
        REAL(dp):: temp_reals(veclen)
        

        ! Count the number of junctions, and allocate space for them
        network%num_junctions= count_line_matches(input_file_unit_no, "Junct Name=", "(A11)")
        !ALLOCATE(network%reach_junctions(network%num_junctions))
        
        ! Go to the start of the file
        rewind(input_file_unit_no)

        ! Loop over all the junctions
        !jb%boundary_type='junction_boundary'
        DO WHILE (io_test>=0)
            ! Go to next line matching 'Junct Name='
            !print*, 'Finding match'
            call next_match(input_file_unit_no, "Junct Name=", io_test, "(A11)")
            !print*, 'io_test=', io_test
            IF(io_test<0) EXIT
            junction_count=junction_count+1

            jb=>network%reach_junctions(junction_count)
            jb%boundary_type='junction_boundary'    
            jb%reach_junctions_index=junction_count
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! Read the junction information           
            ! Looks like this:
            !
            !Junct Name=tapayan_j2      
            !Junct Desc=, 0 , 0 ,-1 ,0
            !Junct X Y & Text X Y=510057.59375,1611264.5,510057.59375,1611264.5
            !Up River,Reach=Tapayan_network ,Creek7          
            !Dn River,Reach=Tapayan_network ,Creek4          
            !Dn River,Reach=Tapayan_network ,Creek6          
            !Dn River,Reach=Tapayan_network ,Creek5          
            !Junc L&A=0,0
            !Junc L&A=0,0
            !Junc L&A=0,0
            !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            backspace(input_file_unit_no)
            ! Get junction name and description
            read(input_file_unit_no, "(11X, A30)", iostat=io_test) jb%junction_name
            read(input_file_unit_no, "(11X, A30)", iostat=io_test) jb%junction_description
            
            ! Get coordinates
            read(input_file_unit_no, "(21X, A64)", iostat=io_test) temp_char
            read(temp_char, *) temp_reals(1:4)
            jb%boundary_location=temp_reals(1:2)

            ! Get inflowing reaches, with up/dn information
            ! Lines look like 'Dn River,Reach=Tapayan_network ,Creek5'          
            join_count=0
            DO WHILE (.TRUE.)
                ! Read next line containing names of reaches meeting here
                read(input_file_unit_no, "(A)", iostat=io_test) temp_char
                IF(temp_char(4:8)/='River') THEN
                    backspace(input_file_unit_no)
                    EXIT
                END IF
                !Read all  relevant data into temporary array
                read(temp_char, "(A8, 7X, A16, 1X, A16)", iostat=io_test) temp_chars((3*join_count+1):(3*join_count+3))

                join_count=join_count+1
            END DO

            ! Pack into jb object
            ALLOCATE(jb%reach_names(join_count,2))
            ALLOCATE(jb%reach_ends(join_count))
            ALLOCATE(jb%distances(join_count))
            ALLOCATE(jb%reach_index(join_count))

            DO i=1,join_count
                jb%reach_names(i,1) = temp_chars(3*(i-1) +2)
                jb%reach_names(i,2) = temp_chars(3*(i-1) +3)

                IF(temp_chars(3*(i-1)+1)(1:2) =='Up') THEN
                    jb%reach_ends(i) = 'Up'
                ELSE
                    jb%reach_ends(i) = 'Dn'
                END IF
                
                ! Find index of network%reach_data associated with this connection to jb
                ! by looping over every reach and checking for a match.
                DO j=1,network%num_reaches
                    IF( (jb%reach_names(i,1) == network%reach_data(j)%names(1)).AND. &
                        (jb%reach_names(i,2) == network%reach_data(j)%names(2)) ) THEN
                        jb%reach_index(i) = j
                        print*, 'MATCHED junction ', junction_count, ' with reach ', j
                    END IF
                END DO
            END DO

            ! Read the downstream distances
            ! Lines look like: 'Junc L&A=0,0'
            DO i=1, join_count-1
                read(input_file_unit_no, "(9X, A)", iostat=io_test) temp_char
                read(temp_char, *) temp_reals(1:2)
                jb%distances(i) = temp_reals(1)
            END DO  
            jb%distances(join_count)=0._dp

            ! Stick it into the reach_junctions
            !network%reach_junctions(junction_count)=jb

            ! Point the reach boundaries to the junction information
            DO i=1,join_count
                tmp=jb%reach_index(i)                    
                IF(jb%reach_ends(i)=='Up') THEN
                    !print*, 'NEED TO FIGURE OUT THE FINAL SET-UP OF JUNCTION BOUNDARIES'
                    !stop
                    network%reach_data(tmp)%Downstream_boundary => network%reach_junctions(junction_count) 
                ELSEIF(jb%reach_ends(i)=='Dn') THEN
                    !print*, 'NEED TO FIGURE OUT THE FINAL SET-UP OF JUNCTION BOUNDARIES'
                    !stop
                    network%reach_data(tmp)%Upstream_boundary => network%reach_junctions(junction_count) 
                ELSE
                    print*, 'ERROR: junction reach_ends(', i,') not recognised ', jb%reach_ends(i)
                    stop
                END IF

            END DO

            ! Clean up
            !call jb%delete()
            jb=> NULL()
        END DO
        !jb=> NULL()

        ! Finally, set the stage volume curves for all junctions
        call init_junction_stage_volume_curves(network)
        !print*, 'Finished junction routine'
        ! Back to start of file
        rewind(input_file_unit_no) 

    END SUBROUTINE   

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE read_hecras_file(input_file,network, print_output)
        ! Read hecras data  from 'input_file' into the allocatable reach_data vector
        ! Optionally, print the data out
        CHARACTER(len=*), INTENT(IN):: input_file !, temp_char(100)
        !TYPE(reach_data_type), ALLOCATABLE, INTENT(INOUT):: reach_data(:)
        TYPE(network_data_type), INTENT(INOUT):: network
        LOGICAL, INTENT(IN):: print_output

        ! Local variables
        integer(ip):: input_file_unit_no
        integer:: i,j

        ! Open the input file
        OPEN(newunit=input_file_unit_no, file=input_file)
      
        ! Count the number of reaches
        call count_reaches(input_file_unit_no, network%num_reaches) !-- this operates on the file
        ! Set up storage for reach information
        ALLOCATE( network%reach_data(network%num_reaches) )

        ! Count the number of xsections
        call count_xsections(input_file_unit_no, network%reach_data, network%num_reaches) !-- this operates on the file

        ! Read the data into reach_data
        call read_reaches(input_file_unit_no, network%reach_data, network%num_reaches) !-- this operates on the file

        ! Read the junction data into the network
        call read_junctions(input_file_unit_no, network)
        CLOSE(input_file_unit_no)

        ! Set downstream distances in reach_data, and initiate the stage-area curves
        DO i=1,network%num_reaches
            call network%reach_data(i)%get_downstream_dists_from_xsections()
            ! Loop over every cross-section
            DO j=1, size(network%reach_data(i)%xsects)
                call network%reach_data(i)%xsects(j)%init_stage_etc_curve()
            END DO
        END DO


        ! ALLOCATE 1D variables
        DO i=1,network%num_reaches
            call network%reach_data(i)%allocate_1d_vars()
        END DO

        ! Print output
        IF(print_output) THEN
            ! FIXME: Could use this to make a 'print_reach' routine
            DO i=1,network%num_reaches
                call network%reach_data(i)%print()
            
                DO j=1,size(network%reach_data(i)%xsects)
                    call network%reach_data(i)%xsects(j)%print()
                END DO
            END DO
        END IF


    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE read_hecras_boundary_conditions(input_boundary_file, network)
        ! Read a hecras .uXX boundary conditions file into the network object

        CHARACTER(len=charlen), INTENT(IN):: input_boundary_file
        TYPE(network_data_type), INTENT(INOUT), TARGET:: network
        !LOGICAL, INTENT(IN):: print_output

        ! Local vars       
        INTEGER(ip):: input_file_unit_no , i,j, io_test=0, bnd_data_length
        INTEGER(ip):: lb, ub, num_physical_bounds, boundary_counter=0, myind
        REAL(dp):: Qmin, dt_boundary, local_starttime
        CHARACTER(len=16):: bnd_river_name, bnd_reach_name, bnd_station
        CHARACTER(len=charlen):: interval
        CHARACTER(len=charlen):: hec_bnd_type, datetimestring
        CHARACTER(len=charlen):: river_name, reach_name, station1,stationN
        CHARACTER(len=charlen):: temp_char_vec(veclen), temp_char
        !TYPE(PHYSICAL_BOUNDARY), ALLOCATABLE:: this_boundary
        TYPE(PHYSICAL_BOUNDARY), POINTER:: this_boundary=>NULL()
        LOGICAL:: matched_with_river

        ! Open the input file
        OPEN(newunit=input_file_unit_no, file=input_boundary_file)

        ! Make space for storing the physical boundaries
        !num_physical_bounds=count_line_matches(input_file_unit_no, 'Boundary Location=', '(A18)')
        !print*, num_physical_bounds

        !allocate(network%physical_boundaries(num_physical_bounds))

        ! Loop over every 'Boundary Location' line, and possibly suck the data into a reach boundary
        DO WHILE(io_test==0)

            ! Find lines matching 'Boundary Location=' in the input file
            CALL next_match(input_file_unit_no, 'Boundary Location=', io_test,'(A18)')
            matched_with_river=.FALSE.
            
            IF(io_test==0) THEN 
                ! Not at the end of the file

                BACKSPACE(input_file_unit_no)
                ! read the names associated with this boundary
                read(input_file_unit_no,"(18X, A16, 1X, A16, 1X, A8)"), bnd_river_name, bnd_reach_name, bnd_station

                ! Check if these match any river on the network
                DO i=1,network%num_reaches
                    river_name=network%reach_data(i)%names(1)
                    reach_name=network%reach_data(i)%names(2)

                    IF( (trim(river_name)==trim(bnd_river_name)).AND.(trim(reach_name)==trim(bnd_reach_name))) THEN
                        
                        ! Check the first/last xsections and see if they match the station name
                        ! Otherwise, we are looking at an internal boundary 
                        ! and we need to skip it
                        read(network%reach_data(i)%xsects(1)%myname, "(27X, A8)" ) station1
                        read(network%reach_data(i)%xsects(network%reach_data(i)%xsect_count )%myname, "(27X, A8)"), stationN
                        IF((bnd_station.NE.station1).AND.(bnd_station.NE.stationN)) THEN
                            print*, 'Skipping boundary at ', trim(river_name),' ', trim(reach_name)
                            print*, ' at station ', bnd_station
                            print*, ' which is not at the start/end of the reach'
                            CYCLE
                        END IF

                        matched_with_river=.TRUE.
                        print*, 'Found boundary at ', trim(river_name),' ', trim(reach_name)
                        boundary_counter=boundary_counter+1

                        ! Read data time increment
                        read(input_file_unit_no, *) interval
                        SELECT CASE(trim(interval))
                        CASE('Interval=1MIN')
                            dt_boundary=60._dp
                        CASE('Interval=1HOUR')
                            dt_boundary=60*60._dp
                        CASE('Interval=1DAY')
                            dt_boundary=24*60*60._dp
                        CASE('Interval=1WEEK')
                            dt_boundary=7*24*60*60._dp
                        CASE('Interval=1MON')
                            print*, 'Warning: Incorrect implementation of 1MON boundary'
                            dt_boundary=31*24*60*60._dp
                        CASE('Interval=1YEAR')
                            print*, 'Warning: Incorrect implementation of 1YEAR boundary'
                            dt_boundary=365*24*60*60._dp
                        CASE DEFAULT
                            print*, 'ERROR: Have not implemented time boundaries with spacing ', interval
                            stop
                        END SELECT

                        ! Read type of boundary (hec_bnd_type) and size of boundary data (bnd_data_length)
                        read(input_file_unit_no, "(A50)") temp_char
                        myind=index(temp_char, '=')
                        hec_bnd_type=temp_char(1:(myind))
                        read(temp_char( (myind+1):(myind+8) ), *) bnd_data_length
                    
                        ! Allocate data
                        this_boundary=>network%physical_boundaries(boundary_counter)
                        this_boundary%boundary_type='physical_boundary'
                        this_boundary%input_file='hecras_file'
                        this_boundary%physical_boundaries_index=boundary_counter
                        ALLOCATE(this_boundary%Boundary_t_w_Q%varnames(3))
                        ALLOCATE(this_boundary%Boundary_t_w_Q%x_y(bnd_data_length, 3))

                        ! Set boundary variable names
                        this_boundary%Boundary_t_w_Q%varnames(1)='time'
                        this_boundary%Boundary_t_w_Q%varnames(2)='stage'
                        this_boundary%Boundary_t_w_Q%varnames(3)='discharge'
                        ! Initialise last_search_index 
                        this_boundary%Boundary_t_w_Q%last_search_index=1

                        SELECT CASE(hec_bnd_type)
                        CASE('Flow Hydrograph= ')
                            ! Set compute method
                            this_boundary%compute_method='discharge'

                            ! Make up time / water surface
                            DO j=1,bnd_data_length
                                this_boundary%Boundary_t_w_Q%x_y(j,1) = dt_boundary*(j-1)
                                this_boundary%Boundary_t_w_Q%x_y(j,2) = missing_value ! ARTIFICIAL
                            END DO

                            ! Read discharge data into file
                            DO j=1, ceiling( bnd_data_length*1._dp/(10._dp)) 
                                read(input_file_unit_no, "(10A8)") temp_char_vec(1:10)
                                lb = (j-1)*10 + 1
                                ub = min(j*10, bnd_data_length)
                                this_boundary%Boundary_t_w_Q%x_y(lb:ub,3) = char_2_real(temp_char_vec(1:(ub-lb+1)))
                            END DO

                            ! Now, read the 'Qmin variable', and enforce the minimum discharge, if it exists
                            read(input_file_unit_no, "(21X, A8)") temp_char_vec(1:2)
                            IF(temp_char_vec(1)=='Flow Hydrograph QMin=') THEN
                                Qmin=char_2_real(temp_char_vec(2))
                                this_boundary%Boundary_t_w_Q%x_y(:,3) = merge(this_boundary%Boundary_t_w_Q%x_y(:,3), & 
                                                                      Qmin, &
                                                                      this_boundary%Boundary_t_w_Q%x_y(:,3)> Qmin)
                            END IF
                            !print*, ' ... PRINTING BOUNDARY DATA ...'
                            !DO j=1, bnd_data_length
                            !    print*,'---> ', j, this_boundary%Boundary_t_w_Q%x_y(j,:)
                            !END DO

                        CASE('Stage Hydrograph=')
                            ! Set compute method
                            this_boundary%compute_method='stage'

                            ! Make up time / water surface
                            DO j=1,bnd_data_length
                                this_boundary%Boundary_t_w_Q%x_y(j,1) = dt_boundary*(j-1)
                                this_boundary%Boundary_t_w_Q%x_y(j,3) = missing_value ! ARTIFICIAL
                            END DO

                            ! Read discharge data into file
                            DO j=1, ceiling( bnd_data_length*1._dp/(10._dp)) 
                                read(input_file_unit_no, "(10A8)") temp_char_vec(1:10)
                                lb = (j-1)*10 + 1
                                ub = min(j*10, bnd_data_length)
                                this_boundary%Boundary_t_w_Q%x_y(lb:ub,2) = char_2_real(temp_char_vec(1:(ub-lb+1)))
                            END DO

                        CASE DEFAULT
                            print*, 'ERROR: Have not implemented boundaries of type ', trim(hec_bnd_type)
                            stop
                        END SELECT

                        ! Now, fix up the start-time in the boundary. 
                        CALL next_match(input_file_unit_no, 'Use Fixed Start Time=', io_test,'(A21)')
                        IF(io_test.NE.0) THEN
                            print*, 'ERORR: Missing "Use Fixed Start Time=" string in boundary data for'
                            print*, trim(river_name), ' ', trim(reach_name) 
                            stop
                        ELSE
                            ! Check if we use a boundary specific starting date/time
                            BACKSPACE(input_file_unit_no)
                            read(input_file_unit_no, "(A21,A6)") temp_char_vec(1), temp_char_vec(2)
                            IF(trim(temp_char_vec(2)).EQ.'True') THEN
                                ! Read the date_time_string
                                read(input_file_unit_no, '(A12, A25)') temp_char_vec(1), datetimestring
                                local_starttime=datetime_string_to_seconds(datetimestring)
                            ELSE
                                ! Use globally defined zero time as starttime
                                local_starttime=datetime_string_to_seconds(model_zero_datetime)
                            END IF
                            ! Correct the time
                            this_boundary%Boundary_t_w_Q%x_y(:,1) = local_starttime + & 
                                                    this_boundary%Boundary_t_w_Q%x_y(:,1)
                        END IF
                        
                        ! Now stick the boundary onto the reach 
                        IF(bnd_station==station1) THEN
                            print*, 'Upstream boundary'
                            network%reach_data(i)%Upstream_boundary=> network%physical_boundaries(boundary_counter)
                        ELSEIF(bnd_station==stationN) THEN
                            print*, 'Downstream boundary'
                            network%reach_data(i)%Downstream_boundary=> network%physical_boundaries(boundary_counter)
                        ELSE
                            IF((trim(hec_bnd_type).EQ. 'Flow Hydrograph=').OR. &
                               (trim(hec_bnd_type).EQ. 'Stage Hydrograph=')) THEN
                                print*, 'ERROR -- didnt find the right station ', bnd_station, station1, stationN
                                stop
                            END IF
                        END IF
                        
                        this_boundary=>NULL()
                    END IF
                END DO

                !Report on boundaries which are not matched to a river
                IF(matched_with_river.EQV..FALSE.) THEN
                        print*,'Boundary conditions specified at ', bnd_river_name, bnd_reach_name, bnd_station, &
                                ', but these were not found in network geometry'
                END IF

            END IF
        END DO

        network%num_physical_boundaries=boundary_counter
        !this_boundary=> NULL()

    END SUBROUTINE

END MODULE hecras_IO

