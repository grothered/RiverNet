MODULE xsect_classes
    ! Classes for cross-sectional data / operations
    USE global_defs
    IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE ONE_D_RELATION
        ! Holds several variables with a one-dimensional relation (e.g. Stage & Area & Width & 1D roughness)
        ! These can be stored in an array, with each row of the array holding the value of
        ! Stage/Area/Width/1D roughness etc which occur simultaneously in a given cross-section
        !
        ! This can be used to make a function which computes one variable given the other
        ! (with interpolation as appropriate). 
        ! e.g. Compute 'Area' given 'Stage', or vice-versa
        !
        ! Also hold the 'last_lower_search_index', which is the (lower) index near where
        ! we last evaluated the relation. The idea is that we will often
        ! evaluate the function near to where we last evaluated it. Storing the 
        ! index can make the look-up fast.
        REAL(dp), ALLOCATABLE:: x_y(:,:)
        INTEGER(ip):: last_search_index
        CHARACTER(len=charlen), ALLOCATABLE:: varnames(:)

        contains
        PROCEDURE:: eval=> eval_one_D_relation 
    END TYPE    

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TYPE STATION_DATA_TYPE
        ! Type for river stations (xsections, lateral structures, bridges, etc)
        CHARACTER(len=charlen):: myname

    END TYPE STATION_DATA_TYPE

    ! Specialization for 'typical' cross-sections
    
    TYPE, EXTENDS(STATION_DATA_TYPE):: XSECT_DATA_TYPE
        ! Type for xsectional data
        REAL(dp), ALLOCATABLE:: cutline(:,:) ! Cutline coords
        REAL(dp), ALLOCATABLE:: yz(:,:) ! Profile coords
        REAL(dp), ALLOCATABLE:: downstream_dists(:) ! Distances 
        REAL(dp), ALLOCATABLE:: roughness(:,:) ! Manning?

        !  Add in relations to compute width/area/roughness from stage, and sometimes vice versa.
        TYPE(ONE_D_RELATION):: stage_etc_curve   
        
        contains
        PROCEDURE:: print => print_xsect
        PROCEDURE:: init_stage_etc_curve=>init_stage_etc_curve
    END TYPE XSECT_DATA_TYPE

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE init_stage_etc_curve(xsect)
        !
        ! Compute data that will be used to interpolate stage from Area, and vice-versa
        !

        CLASS(XSECT_DATA_TYPE), INTENT(INOUT):: xsect

        ! NOTE: INTEGERS ARE NOT ip, since this might trouble slatec
        INTEGER:: IPERM(size(xsect%yz(:,1))), ierr, i, j,unique_stage_count
        INTEGER:: num_vars=3 ! Number of variables in the relation. Stage, Area, Width

        REAL(dp):: stage_protection=1000._dp ! FIXME: MAGIC NUMBER
        REAL(dp):: incremental_area, stg, min_bed,max_bed, stg_lower, stg_higher, incremental_width 

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
        ! For protection, we add another high stage (with value = max(bed) + stage_protection)
        ! to the stage_etc_curve
       
        ALLOCATE(xsect%stage_etc_curve%varnames(num_vars))
        xsect%stage_etc_curve%varnames(1)='stage'
        xsect%stage_etc_curve%varnames(2)='area'
        xsect%stage_etc_curve%varnames(3)='width'

        ! Step 1: Compute the indices of the sorted bed elevations.
        ! yz(IPERM, 2) is non-decreasing
        call DPSORT(xsect%yz(:,2), size(xsect%yz(:,2)), IPERM, 1, ierr) ! SLATEC ROUTINE
     
        IF(ierr/=0) THEN
            PRINT*, "ERROR IN SORT when setting monotonic relation"
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

        ! Allocate 1D_relation -- include space for one large stage at the end
        ALLOCATE(xsect%stage_etc_curve%x_y(unique_stage_count+1,num_vars))
        xsect%stage_etc_curve%last_search_index=1 ! Initialise this index

        ! Step 3: Assign values

        ! Set lowest x_y pair
        xsect%stage_etc_curve%x_y(1,:) =(/ minval(xsect%yz(:,2)), 0._dp, 0._dp /) 

        ! Assign stage values
        unique_stage_count=1
        DO i=2, size(IPERM)
            IF(xsect%yz(IPERM(i),2)/= xsect%yz(IPERM(i-1), 2)) THEN
                ! Bed value is different to the previous one
                unique_stage_count=unique_stage_count+1
                xsect%stage_etc_curve%x_y(unique_stage_count,1) = xsect%yz(IPERM(i),2)
            END IF
        END DO

        ! Add an upper bound to the stage-area curve, so that extrapolation does not automatically fail
        xsect%stage_etc_curve%x_y(unique_stage_count+1, 1) = & 
                xsect%stage_etc_curve%x_y(unique_stage_count,1) + stage_protection

        ! Find incremental area / width/ other variable
        DO i=2,unique_stage_count
            incremental_area=0._dp
            incremental_width=0._dp

            stg=xsect%stage_etc_curve%x_y(i,1) ! shorthand

            ! Loop over the width of the xsection
            DO j=1,size(xsect%yz(:,1))-1
                ! Add the area beneath stage_i=x_y(i,2)
                min_bed=min(xsect%yz(j,2), xsect%yz(j+1,2))
                max_bed=max(xsect%yz(j,2), xsect%yz(j+1,2))
                IF(stg> min_bed) THEN
                    ! There is some wetness
                    IF(stg>max_bed) THEN
                        ! Full wetness: incremental_area += mean_depth*width
                        incremental_area=incremental_area + & 
                            (stg - 0.5*(max_bed+min_bed))*(xsect%yz(j+1,1) - xsect%yz(j,1))
                        incremental_width=incremental_width + & 
                            (xsect%yz(j+1,1) - xsect%yz(j,1))
                    ELSE 
                        ! Partial wetness: Compute triangular wetted area as 0.5 * height *base
                        incremental_area = incremental_area + &
                             0.5_dp*(stg - min_bed)* & 
                             (xsect%yz(j+1,1) - xsect%yz(j,1))*(stg- min_bed)/(max_bed-min_bed)
                        incremental_width = incremental_width + &
                             (xsect%yz(j+1,1) - xsect%yz(j,1))*(stg- min_bed)/(max_bed-min_bed)
                    END IF
                END IF
            END DO
            xsect%stage_etc_curve%x_y(i,2) = incremental_area 
            xsect%stage_etc_curve%x_y(i,3) = incremental_width 
            
        END DO

        ! Add upper bound value so that we can continue extrapolating area, as
        ! though there were vertical walls bounding the xsection
        j=size(xsect%yz(:,1))
        xsect%stage_etc_curve%x_y(unique_stage_count+1,2) = &
                    (xsect%yz(j,1)-xsect%yz(1,1))*stage_protection
        xsect%stage_etc_curve%x_y(unique_stage_count+1,3) = &
                    max( (xsect%yz(j,1)-xsect%yz(1,1)), xsect%stage_etc_curve%x_y(unique_stage_count,3))

    END SUBROUTINE init_stage_etc_curve

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    FUNCTION eval_one_D_relation(stage_etc_curve, predictor, predictor_varname, output_varname)
        ! Interpolation function for one_D_relation 
        !
        ! Compute the value of 'output_varname' associated with the
        ! 'predictor' value of 'predictor_varname' in a ONE_D_RELATION 
        ! 
        ! E.G. stage_from_area(stage_etc_curve, 1.0, 'stage', 'area') =
        ! [ the value of 'area' associated with a 'stage' of 1.0]

        CLASS(ONE_D_RELATION), INTENT(IN), target:: stage_etc_curve
        REAL(dp), INTENT(IN):: predictor
        CHARACTER(*), INTENT(IN):: predictor_varname, output_varname
        REAL(dp):: eval_one_d_relation

        ! Local variables
        INTEGER(ip):: i, lower_ind, l, predictor_index, output_index
        REAL(dp):: weight
        INTEGER(ip), pointer:: last_search_index
        REAL(dp):: S_A(size(stage_etc_curve%x_y(:,1)),2)
        LOGICAL:: inverse_b

        ! Firstly, set up stage_area relation, depending on whether or not we
        ! are interpolating stage from area (default, inverse=false), or area from stage
        ! (inverse=true)
        predictor_index=-1
        output_index=-1
        DO i=1,size(stage_etc_curve%varnames)
            IF(stage_etc_curve%varnames(i).EQ. predictor_varname) predictor_index=i
            IF(stage_etc_curve%varnames(i).EQ. output_varname) output_index=i
        END DO

        IF( (predictor_index<1).OR.(output_index<1)) THEN
            print*, 'ERROR in eval_one_d_relation:'
            print*, 'Cannot find one or both of predictor varname ', predictor_varname
            print*, ' and output varname ', output_varname
            print*, ' in stage_etc_curve%varnames: ', stage_etc_curve%varnames
            stop
        END IF

        ! Store relevant variables in a 2 column array: (output, predictor)
        !S_A=> stage_etc_curve%x_y(:,(/ output_index, predictor_index/) ) 
        S_A= stage_etc_curve%x_y(:,(/ output_index, predictor_index/) ) 
        last_search_index=>stage_etc_curve%last_search_index

        ! Logical checks / quick exit
        IF(predictor<S_A(1,2)) THEN
            print*, "ERROR: Trying to interpolate from stage_etc_curve: Used a"
            print*, "predictor which is < min(values of this predictor) on this cross-section"
            stop
        END IF


        l=size(S_A(:,1))
        IF(predictor > S_A(l,2)) THEN
            print*, "ERROR: Trying to interpolate from stage_etc_curve: Used a"
            print*, "predictor which is > max(values of this predictor) on this cross-section"
            stop
            stop
        END IF

        ! Main routine

        ! Find index just below our desired interpolation region
        lower_ind=-1
        IF(predictor>=S_A(last_search_index,2)) THEN
            DO i= last_search_index+1, l
                IF (predictor<S_A(i,2)) THEN
                    lower_ind=i-1
                    EXIT
                END IF
            END DO
        ELSE
            ! predictor < predictor at last search index
            DO i= last_search_index-1, 1,-1
                IF (predictor>=S_A(i,2)) THEN
                    lower_ind=i
                    EXIT
                END IF
            END DO
        END IF

        last_search_index=lower_ind ! Update last_search_index
        ! Weighted average
        weight=(S_A(lower_ind+1,2)-predictor)/(S_A(lower_ind+1,2) - S_A(lower_ind,2))
        eval_one_d_relation = S_A(lower_ind,1)*weight + S_A(lower_ind+1,1)*(1.0_dp-weight)

    END FUNCTION eval_one_D_relation

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE print_xsect(xsect)
        CLASS(XSECT_DATA_TYPE), INTENT(IN):: xsect
        INTEGER:: k
        REAL(dp):: tmp, tmp2, tmp3, tmp4

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
        
        print*, 'Xsect roughness values:'
        DO k=1, size(xsect%roughness(:,1))
            print*, xsect%yz(k,1) , xsect%roughness(k,1)
        END DO

        print*, 'Xsect Stage-Area-Width curve'
        DO k=1,size(xsect%stage_etc_curve%x_y(:,1))
            print*, xsect%stage_etc_curve%x_y(k,:)
        END DO

        ! Test of stage-area relation
        print*, 'Checking that stage-area curve interpolates okay...'
        tmp=minval(xsect%stage_etc_curve%x_y(:,1))
        DO k=1, 10
            tmp=tmp+(k-1)*1.0_dp ! Hypothetical stage
            tmp3=xsect%stage_etc_curve%eval(tmp, 'stage', 'area') ! area when stage = tmp
            tmp2=xsect%stage_etc_curve%eval(tmp3, 'area', 'stage') ! Should = tmp
            tmp4=xsect%stage_etc_curve%eval(tmp3, 'area', 'width')
            print*, 'When stage = ', tmp, ' area = ', tmp3, ' width = ', tmp4

            ! TEST of Stage-area relation
            IF(abs(tmp2 - tmp) > 1.0e-8_dp) THEN
                print*, 'ERROR: Seems there is a problem on this stage-area curve'
                print*, 'Stage= ', tmp, ' Area= ', tmp3, ' Inverted stage ', tmp2, ' Difference ',tmp-tmp2
                stop
            END IF
            
        END DO
        print*, 'PASS'
    

    END SUBROUTINE print_xsect
    
END MODULE xsect_classes
