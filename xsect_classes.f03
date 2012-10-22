MODULE xsect_classes
    ! Classes for cross-sectional data / operations
    USE global_defs
    IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE MONOTONIC_RELATION
        ! Holds two variables with a monotonic relation (e.g. Stage vs Area)
        ! This can be used to make a function which computes one variable given the other
        ! (with interpolation as appropriate). 
        ! e.g. Compute 'Area' given 'Stage', or vice-versa
        !
        ! Also hold the 'last_lower_search_index', which is the (lower) index near where
        ! we last evaluated the relation. The idea is that we will often
        ! evaluate the function near to where we last evaluated it. Storing the 
        ! index can make the look-up fast.
        REAL(dp), ALLOCATABLE:: Stage_Area(:,:)
        INTEGER(dp):: last_search_index
        contains
        !PROCEDURE:: init=> init_stage_area_relation ! Initialise (and/or update) the stage_area relation
        PROCEDURE:: eval=> stage_from_area ! eval(Area1) = Stage1, or eval(Stage1, inverse=TRUE)=Area1
    END TYPE    

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE STATION_DATA_TYPE
        ! Type for river stations (xsections, lateral structures, bridges, etc)
        CHARACTER(len=charlen):: myname

    END TYPE STATION_DATA_TYPE

    !
    
    TYPE, EXTENDS(STATION_DATA_TYPE):: XSECT_DATA_TYPE
        ! Type for xsectional data
        REAL(dp), ALLOCATABLE:: cutline(:,:) ! Cutline coords
        REAL(dp), ALLOCATABLE:: yz(:,:) ! Profile coords
        REAL(dp), ALLOCATABLE:: downstream_dists(:) ! Distances 
        REAL(dp), ALLOCATABLE:: roughness(:,:) ! Manning?

        ! 
        TYPE(MONOTONIC_RELATION):: stage_area_curve
        
        contains
        PROCEDURE:: print => print_xsect
        PROCEDURE:: init_stage_area_curve=>init_stage_area_curve
    END TYPE XSECT_DATA_TYPE

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE init_stage_area_curve(xsect)
        !
        ! Compute data that will be used to interpolate stage from Area, and vice-versa
        !

        CLASS(XSECT_DATA_TYPE), INTENT(INOUT):: xsect

        ! NOTE: INTEGERS ARE NOT dp, since this might trouble slatec
        INTEGER:: IPERM(size(xsect%yz(:,1))), ierr, i, j,unique_stage_count

        REAL(dp):: stage_area_protection=1000._dp ! FIXME: MAGIC NUMBER
        REAL(dp):: incremental_area, stg, min_bed,max_bed, stg_lower, stg_higher 

        ! NOTE: PRESENTLY WE DO AN AREA COMPUTATION WHICH WORKS FOR XSECTIONS WITH
        ! INCREASING HORIZONTAL COORDINATE.
        ! THIS IS SIMPLE, BUT DOES NOT WORK e.g. FOR CIRCULAR PIPES, with pressiman slot
        !
        ! FIXME: IN FUTURE, compute all unique bed elevations, and sort from small to large. 
        ! For each stage (taking on the unique values of the bed elevation)
        ! , compute the area in the 'horizontal slice'
        ! [stage_j - 0.5(bed_i-bed_i-1), stage_j + 0.5(bed_i+1-bed_i-1)]
        ! over the entire cross-section.
        ! Then, the cumulative sum will give the stage/area relation
        ! For protection, we add another high stage (with value = max(bed) + stage_area_protection)
        ! to the stage_area_curve
        

        ! Step 1: Compute the indices of the sorted bed elevations.
        ! yz(IPERM, 2) is non-decreasing
        call DPSORT(xsect%yz(:,2), size(xsect%yz(:,2)), IPERM, 1, ierr) ! SLATEC ROUTINE
     
        IF(ierr/=0) THEN
            PRINT*, "ERROR IN SORT when setting stage_area relation"
            PRINT*, 'Xsect info is '
            call xsect%print()
            STOP
        END IF 
        !print*, 'SORTED BED ELEVATIONS' 
        !print*, xsect%yz(IPERM,2) 
        !stop

        ! Step 2: Count the number of unique stage values, and allocate the stage-area relation
        unique_stage_count=1
        DO i=2,size(IPERM)
            IF(xsect%yz(IPERM(i),2)/= xsect%yz(IPERM(i-1), 2)) THEN
                ! Bed value is different to the previous one
                unique_stage_count=unique_stage_count+1
            END IF
        END DO

        ! Allocate stage_area_relation -- include space for one large stage at the end
        ALLOCATE(xsect%stage_area_curve%Stage_Area(unique_stage_count+1,2))
        xsect%stage_area_curve%last_search_index=1 ! Initialise this index

        ! Step 3: Assign values

        ! Set lowest Stage_area pair
        xsect%stage_area_curve%Stage_Area(1,1:2) =(/ minval(xsect%yz(:,2)), 0._dp /) 

        ! Assign stage values
        unique_stage_count=1
        DO i=2, size(IPERM)
            IF(xsect%yz(IPERM(i),2)/= xsect%yz(IPERM(i-1), 2)) THEN
                ! Bed value is different to the previous one
                unique_stage_count=unique_stage_count+1
                xsect%stage_area_curve%Stage_Area(unique_stage_count,1) = xsect%yz(IPERM(i),2)
            END IF
        END DO

        ! Add an upper bound to the stage-area curve, so that extrapolation does not automatically fail
        xsect%stage_area_curve%Stage_Area(unique_stage_count+1, 1) = & 
                xsect%stage_area_curve%Stage_Area(unique_stage_count,1) + stage_area_protection

        ! Find incremental area
        DO i=2,unique_stage_count
            incremental_area=0._dp

            stg=xsect%stage_area_curve%Stage_Area(i,1) ! shorthand

            ! Loop over the width of the xsection
            DO j=1,size(xsect%yz(:,1))-1
                ! Add the area beneath stage_i=Stage_Area(i,2)
                min_bed=min(xsect%yz(j,2), xsect%yz(j+1,2))
                max_bed=max(xsect%yz(j,2), xsect%yz(j+1,2))
                IF(stg> min_bed) THEN
                    ! There is some wetness
                    IF(stg>max_bed) THEN
                        ! Full wetness: incremental_area += mean_depth*width
                        incremental_area=incremental_area + & 
                            (stg - 0.5*(max_bed+min_bed))*(xsect%yz(j+1,1) - xsect%yz(j,1))
                    ELSE 
                        ! Partial wetness: Compute triangular wetted area as 0.5 * height *base
                        incremental_area = incremental_area + &
                             0.5_dp*(stg - min_bed)* & 
                             (xsect%yz(j+1,1) - xsect%yz(j,1))*(stg- min_bed)/(max_bed-min_bed)
                    END IF
                END IF
            END DO
            xsect%stage_area_curve%Stage_Area(i,2) = incremental_area 
        END DO

        ! Add upper bound value so that we can continue extrapolating area, as
        ! though there were vertical walls bounding the xsection
        j=size(xsect%yz(:,1))
        xsect%stage_area_curve%Stage_Area(unique_stage_count+1,2) = &
                    (xsect%yz(j,1)-xsect%yz(1,1))*stage_area_protection

    END SUBROUTINE init_stage_area_curve 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    FUNCTION stage_from_area(stage_area_curve, area)
        ! Function to compute stage from area, using the stage-area curve for the xsection
        CLASS(MONOTONIC_RELATION), INTENT(IN), target:: stage_area_curve
        REAL(dp), INTENT(IN):: area
        REAL(dp):: stage_from_area

        INTEGER(dp):: i, lower_ind, l
        REAL(dp):: weight
        INTEGER(dp), pointer:: last_search_index
        REAL(dp), pointer:: S_A(:,:)

        IF(area<0._dp) THEN
            print*, "ERROR: Area < 0._dp cannot be used to interpolate stage from stage_area_curve"
            stop
        ELSE IF(area==0._dp) THEN
            stage_from_area=stage_area_curve%Stage_Area(1,1)
            RETURN
        ELSE
            S_A=> stage_area_curve%Stage_Area(:,1:2)
            l=size(S_A(:,1))
            IF(area> S_A(l,1)) THEN
                print*, 'ERROR: Area is too large to interpolate stage from stage area curve:'
                print*, 'This is almost definitely an error'
                stop
            END IF

            ! Shorthand notation
            last_search_index=>stage_area_curve%last_search_index

            ! Find index just below 
            IF(area>=S_A(last_search_index,2)) THEN
                DO i= last_search_index+1, l
                    IF (area<S_A(i,2)) THEN
                        lower_ind=i-1
                        GOTO 111 ! Break out of loop
                    END IF
                END DO
                111 CONTINUE
            ELSE
                DO i= last_search_index-1, 1,-1
                    IF (area>=S_A(i,2)) THEN
                        lower_ind=i
                        GOTO 112 ! Break out of loop
                    END IF
                END DO
                112 CONTINUE

            END IF

            last_search_index=lower_ind ! Update last_search_index
            ! Weighted average
            weight=(S_A(lower_ind+1,2)-area)/(S_A(lower_ind+1,2) - S_A(lower_ind,2))
            stage_from_area = S_A(lower_ind,1)*weight + S_A(lower_ind+1,1)*(1.0_dp-weight)

        END IF 
    END FUNCTION stage_from_area

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE print_xsect(xsect)
        CLASS(XSECT_DATA_TYPE), INTENT(IN):: xsect
        INTEGER:: k
        REAL(dp):: tmp

        print*, trim(xsect%myname)
        print*, 'Downstream distances are ', xsect%downstream_dists

        print*, 'Cutline size = ', size(xsect%cutline(:,1))
        DO k=1,size(xsect%cutline(:,1))
            print*, xsect%cutline(k,1:2)
        END DO

        print*, 'Xsect size = ', size(xsect%yz(:,1))
        DO k=1,size(xsect%yz(:,1))
            print*, xsect%yz(k,1:2) 
        END DO
        
        print*, 'Xsect roughness change points:'
        DO k=1, size(xsect%roughness(:,1))
            print*, xsect%roughness(k,1:2)
        END DO

        print*, 'Xsect Stage-Area curve'
        DO k=1,size(xsect%stage_area_curve%Stage_Area(:,1))
            print*, xsect%stage_area_curve%Stage_Area(k,:)
        END DO

        print*, 'Some numbers on stage-area curve'
        tmp=minval(xsect%stage_area_curve%Stage_Area(:,2))
        DO k=1, 10
            tmp=tmp+(k-1)*1.0_dp
            print*, xsect%stage_area_curve%eval(tmp), tmp, xsect%stage_area_curve%last_search_index
        END DO
    

    END SUBROUTINE print_xsect
    
END MODULE xsect_classes
