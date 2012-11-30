MODULE one_d_relation_class
    ! Fortran class to hold a relation between 1 or more named variables, and
    ! allow interpolation of any variable given the value of any other

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
        INTEGER(ip):: last_search_index=-1
        CHARACTER(len=charlen), ALLOCATABLE:: varnames(:)

        contains
        PROCEDURE:: eval=> eval_one_D_relation 
    END TYPE    

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    FUNCTION eval_one_D_relation(stage_etc_curve, predictor, predictor_varname, output_varname)
        ! Interpolation function for one_D_relation 
        !
        ! Compute the value of 'output_varname' associated with the
        ! 'predictor' value of 'predictor_varname' in a ONE_D_RELATION 
        ! 
        ! E.G. 
        ! eval_one_D_relation(stage_etc_curve, 1.0, 'stage', 'area') =
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

        ! Check that we have initialised the one-d-relation
        last_search_index=>stage_etc_curve%last_search_index
        IF(last_search_index==-1) THEN
            print*, 'ERROR: Have not initialised last_search_index in one_d_relation'
            stop
        END IF

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

        ! Logical checks / quick exit
        IF(predictor<S_A(1,2)) THEN
            print*, "ERROR: Trying to interpolate from stage_etc_curve: Used a"
            print*, "predictor which is < min(values of this predictor) on this cross-section"
            print*, predictor,  predictor_varname, output_varname
            stop
        END IF


        l=size(S_A(:,1))
        IF(predictor > S_A(l,2)) THEN
            print*, "ERROR: Trying to interpolate from stage_etc_curve: Used a"
            print*, "predictor which is > max(values of this predictor) on this cross-section"
            print*, predictor,  predictor_varname, output_varname
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

END MODULE one_d_relation_class
