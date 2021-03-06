MODULE network_solver
    ! Routines to solve the St-Venant Equations with Junctions in a river network
    USE global_defs
    USE river_classes
   
    IMPLICIT NONE 
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE evolve_hydraulics(network)
        ! Evolve the hydraulics in the network
        TYPE(network_data_type), INTENT(IN OUT):: network
        INTEGER:: i

        ! Update the timestep dT
        CALL update_timestep(network)
        !print*, 'NETWORK DT: ', network%dT
        ! New boundary values for time t and t+delT -- the boundaries should store both 
        !CALL update_boundaries(network)
        
        ! Update the flow Stage, Area + Discharge in reaches + junctions

        DO i=1,network%num_reaches
            call one_mccormack_step(network%reach_data(i), network%time, network%dT)
        END DO

        IF(network%num_junctions>0) THEN
            !DO i=1,network%num_junctions
            !    call update_junction_values(network, network%reach_junctions(i))
            !END DO
            call update_junction_values(network)
        END IF

        ! Advance time
        network%time=network%time+network%dT
        !print*, 'new_time', network%time
        
    END SUBROUTINE evolve_hydraulics

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE update_timestep(network)
        ! Loop over every xsection, and compute the new timestep from the CFL condition
        TYPE(network_data_type), INTENT(IN OUT):: network

        ! Local vars
        INTEGER(ip):: i, j
        REAL(dp):: dT, local_wavespeed, local_dt, vel, grav_wavespeed

        ! Predefine timestep
        !dT=maximum_allowed_timestep
        dT = min(maximum_allowed_timestep , network%dT*max_timestep_increase)
        ! Note that this relies on dT being initialised to max_allowed_timestep

        ! Loop over all xsections on all reaches and get the new timestep
        DO i=1,network%num_reaches
            DO j=1, network%reach_data(i)%xsect_count
                ! 1D Velocity
                vel=network%reach_data(i)%Discharge(j)/(network%reach_data(i)%Area(j)+ small_positive_real)
                ! Gravity wavespeed = sqrt( g * mean_depth)
                grav_wavespeed=(gravity*network%reach_data(i)%Area(j)/(network%reach_data(i)%Width(j)+small_positive_real))**0.5_dp
                local_wavespeed = abs(vel) + grav_wavespeed

                ! FIXME: Check interpretation of downstream distances -- perhaps incorrect
                !local_dt = network%reach_data(i)%downstream_dists(j,2)/(local_wavespeed + small_positive_real)
                local_dt = network%reach_data(i)%delX_v(j)/(local_wavespeed + small_positive_real)
                local_dt = local_dt*network%CFL

                IF(local_dt<minimum_allowed_timestep) THEN
                    print*, 'ERROR: local_dt is < minimum_allowed_timestep on reach ', trim(network%reach_data(i)%names(1)), &
                             trim(network%reach_data(i)%names(2)), ' xsect ', j
                    print*,  vel, grav_wavespeed, local_wavespeed, local_dt
                    stop
                END IF

                dT = min(dT, local_dt)
            END DO
        END DO

        network%dT=dT
        !print*, '.', dT

    END SUBROUTINE update_timestep

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE apply_boundary_conditions(reach_data, time, dT, n, location_flags)
        ! Routine to apply the boundary conditions to the reach.
        ! This is called near the end of one_mccormack_step
        !
        ! NOTE: reach_data%Area(1:n), reach_data%discharge(2:n-1) have been set to updated values.
        ! Everything else still takes the last time step value

        TYPE(reach_data_type), INTENT(inout):: reach_data
        REAL(dp), INTENT(IN):: time, dT
        LOGICAL, INTENT(IN):: location_flags
        INTEGER(ip), INTENT(IN)::n


        !IF(location_flags) print*, '  Width computation '
        !! Need width to compute mean depth for characteristic speeds
        !reach_data%Width(2) = reach_data%xsects(2)%stage_etc_curve%eval(reach_data%Area(2), 'area', 'width')
        !reach_data%Width(n-1) = reach_data%xsects(n-1)%stage_etc_curve%eval(reach_data%Area(n-1), 'area', 'width')

        IF(location_flags) print*, '  Downstream Boundary'

        ! Downstream. Check if flow is sub or super critical
        IF(gravity*reach_data%Area(n)/reach_data%Width(n) > (reach_data%Discharge(n)/reach_data%Area(n))**2 ) THEN
            ! Subcritical boundary
            !IF(index(reach_data%Downstream_boundary%compute_method,'stage')>0) THEN
            !    ! Impose stage, extrapolate discharge
            !    reach_data%Stage(n) = max(reach_data%Downstream_boundary%eval(time+dT, 'stage'), &
            !                              reach_data%minbed(n)+wet_dry_depth-small_positive_real)
            !    reach_data%Area(n) = reach_data%xsects(n)%stage_etc_curve%eval(reach_data%Stage(n), 'stage', 'area')

            !    !reach_data%Discharge(n) = reach_data%Discharge(n-1)
            !END IF
            IF(index(reach_data%Downstream_boundary%compute_method,'discharge')>0) THEN
                ! Impose discharge, area is already good
                reach_data%Discharge(n) = reach_data%Downstream_boundary%eval(time+dT, 'discharge')
            END IF
    
        ELSE
            ! Supercritical boundary -- impose everything or nothing
            IF((reach_data%Discharge(n) < 0._dp).OR.(reach_data%Downstream_boundary%compute_method=='stage_discharge')) THEN
                !IF(index(reach_data%Downstream_boundary%compute_method,'stage')>0) THEN
                !    ! Force stage, set discharge to critical flow value
                !    reach_data%Stage(n) = max(reach_data%Downstream_boundary%eval(time+dT, 'stage'), &
                !                              reach_data%minbed(n)+wet_dry_depth-small_positive_real)
                !    reach_data%Area(n) = reach_data%xsects(n)%stage_etc_curve%eval(reach_data%Stage(n), 'stage', 'area')
                !    reach_data%Width(n) = reach_data%xsects(n)%stage_etc_curve%eval(reach_data%Stage(n), 'stage', 'width') 
            
                !    reach_data%Discharge(n) = -(reach_data%Area(n)/reach_data%Width(n)*gravity)**0.5_dp*reach_data%Area(n)
                !END IF
                
                IF(index(reach_data%Downstream_boundary%compute_method,'discharge')>0) THEN
                    ! Force discharge, set area to 'critical value' at n-1? Could compute critical area at n with some coding
                    reach_data%Discharge(n) = reach_data%Downstream_boundary%eval(time+dT, 'discharge')
                    
                    ! Note: Here we are lazy, in that we evaluate width at time tlast.      
                    ! (Q/A)**2 = (g*A/B); A**3=Q**2*(B/g)
                    reach_data%Area(n) = (abs(reach_data%Discharge(n))*(reach_data%Width(n)/gravity)**0.5_dp)**(2.0_dp/3.0_dp)
                END IF
                !    print*, 'ERROR: compute_method not set for a boundary'
                !    stop
                !END IF
            END IF
        END IF


        ! Upstream. Check if flow is sub or super critical
        IF(location_flags) print*, '  Upstream Boundary'

        IF(gravity*reach_data%Area(1)/reach_data%Width(1) > (reach_data%Discharge(1)/reach_data%Area(1))**2 ) THEN
            ! Subcritical boundary
            !IF(index(reach_data%Upstream_boundary%compute_method,'stage')>0) THEN
            !    ! Impose stage, extrapolate discharge
            !    reach_data%Stage(1) = max(reach_data%Upstream_boundary%eval(time+dT, 'stage'), &
            !                              reach_data%minbed(1)+wet_dry_depth-small_positive_real)
            !    reach_data%Area(1) = reach_data%xsects(1)%stage_etc_curve%eval(reach_data%Stage(1), 'stage', 'area')
            !    !reach_data%Discharge(1) = reach_data%Discharge(2)
            !END IF
            IF(index(reach_data%Upstream_boundary%compute_method,'discharge')>0) THEN
                ! Impose discharge, area is already good
                reach_data%Discharge(1) = reach_data%Upstream_boundary%eval(time+dT, 'discharge')
            !ELSE
            !    print*, 'ERROR: compute_method not set for a boundary'
            !    stop
            END IF
    
        ELSE

            ! Supercritical boundary -- impose everything or nothing
            IF((reach_data%Discharge(1) > 0._dp).OR.(reach_data%Upstream_boundary%compute_method=='stage_discharge')) THEN
                !IF(index(reach_data%Upstream_boundary%compute_method,'stage')>0) THEN
                !    ! Force stage, set discharge to critical flow value
                !    reach_data%Stage(1) = max(reach_data%Upstream_boundary%eval(time+dT, 'stage'), &
                !                              reach_data%minbed(1)+wet_dry_depth-small_positive_real)
                !    reach_data%Area(1) = reach_data%xsects(1)%stage_etc_curve%eval(reach_data%Stage(1), 'stage', 'area')
                !    reach_data%Width(1) = reach_data%xsects(1)%stage_etc_curve%eval(reach_data%Stage(1), 'stage', 'width') 
                !
                !    reach_data%Discharge(1) = (reach_data%Area(1)/reach_data%Width(1)*gravity)**0.5_dp*reach_data%Area(1)
                !END IF
                
                IF(index(reach_data%Upstream_boundary%compute_method,'discharge')>0) THEN
                    ! Force discharge, set area to 'critical value'
                    reach_data%Discharge(1) = reach_data%Upstream_boundary%eval(time+dT, 'discharge')
                   
                    ! Note: Here we are lazy, in that we evaluate width at time tlast.      
                    reach_data%Area(1) = (abs(reach_data%Discharge(1))*(reach_data%Width(1)/gravity)**0.5_dp)**(2.0_dp/3.0_dp)
                END IF
            END IF
        END IF

    END SUBROUTINE apply_boundary_conditions

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE one_mccormack_step(reach_data, time, dT)
        !
        ! Mc-Cormack type flow solver with tweaks
        !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Thoughts on imposing different types of boundary conditions
        !###
        !! EXACT DISCHARGE BOUNDARIES
        !
        ! Area(t+1,i) = Area_pred+Area_cor =Area(t) -dT*[ (Q(t,i+1) -Q(t,i))/(dX) + (Qpred(*,i) - Qpred(*,i-1) )/(dX) ]
        !
        ! So, we can define the 'conservative' variant of the discharge as:
        !
        ! Qcon(t+1/2,i+1/2) := 0.5*(Q(t,i+1) + Q_pred(*,i))
        !
        ! which is 'conservative in the sense that it exactly satisfies:
        !
        ! dX*(A(t+1, i)-A(t, i)) = -dT * ( Qcon(t+1/2,i+1/2) - Qcon(t+1/2,i-1/2) )
        !
        ! We can impose exact discharge boundary conditions by ensuring that
        ! the values of Qcon at the discharge boundaries (n+1/2 & 1/2) attain the values that we want
        !
        !
        ! Subcritical Upstream -- set Apred, Qpred to ensure conservation?
        ! Qpred(*,n) = Desired boundary flows at (t+1) 
        ! Apred(*,n): 0.5*(Apred(*,n) + Acor(*,n)) = -dT/dX*(Qboundary(t+1/2, n+1/2) - Qcon(t+1/2, n-1/2))
        !
        ! Problem: We don't know Acor when evaluating Apred -- but we can
        !          calculate it anyway ahead of time, straightforward
        ! 
        ! Subcritical downstream -- set Acor(*,1) and Qcor(*,1) to ensure conservation? 
        ! Acor1: 0.5*(Acor(*,1) +Apred(*,1)) = -dT/dX*(Qcon(t+1/2, 1+1/2) -Qboundary(t+1,1/2))
        ! Qcor1: Qcor(*,1) = Qboundary(t+1,1/2) 
        !
        !###
        !! AREA/STAGE BOUNDARIES
        ! Can compute Area from stage given the stage_etc_curve
        !
        ! Subcritical upstream
        ! A_pred(*,n) = Desired Area
        ! Q_pred(*,n) = Whatever -- first order velocity extrapolation / flux extrapolation / zero gradient flux / ...
        !
        ! Subcritical downstream
        ! A_cor(*,1) = Desired area
        ! Qcor(*, 1) = Whatever -- first order velocity extrapolation / flux extrapolation / zero gradient flux / ...
        !
        !###
        !! SUPER CRITICAL BOUNDARIES
        !   Here, we might just want to force the values at the boundaries.
        !   Inflow: A(t+1) = Desired Area at t+1?
        !           Q(t+1) = Desired discharge at t+1?
        !
        !   Outflow: First order / zero order extrapolation ??
        ! 
        ! Will that do it?
        !

        TYPE(reach_data_type), INTENT(INOUT):: reach_data
        REAL(dp), INTENT(IN):: time, dT

        ! Local vars
        INTEGER(ip)::i, n
        REAL(dp):: delX(reach_data%xsect_count), dry_flag(reach_data%xsect_count), delX_v(reach_data%xsect_count)
        REAL(dp):: Area_pred(reach_data%xsect_count), Stage_pred(reach_data%xsect_count), Q_pred(reach_data%xsect_count)
        REAL(dp):: Area_cor(reach_data%xsect_count), Stage_cor(reach_data%xsect_count), Q_cor(reach_data%xsect_count)
        !REAL(dp):: Vol_pred(reach_data%xsect_count-1), Vol_cor(reach_data%xsect_count-1)
        REAL(dp):: mean_depth, convective_flux(reach_data%xsect_count), slope(reach_data%xsect_count)
        REAL(dp):: drag_factor(reach_data%xsect_count), Af(reach_data%xsect_count), Ab(reach_data%xsect_count)
        REAL(dp):: Width_pred(reach_data%xsect_count), Width_cor(reach_data%xsect_count), Drag1D_pred(reach_data%xsect_count)
        REAL(dp):: Qcon, Discharge_old(reach_data%xsect_count), Qpred_zero, timestep_increase_buffer, Qdiff, Qdown, Qup
        REAL(dp):: Qtmp(reach_data%xsect_count), Area_old(reach_data%xsect_count), safety
        LOGICAL:: implicit_friction=.TRUE., convective_terms=.TRUE., location_flags=.FALSE.

        ! Predefine some useful vars
        n=reach_data%xsect_count
        Discharge_old=reach_data%Discharge
        Area_old=reach_data%Area

        ! Use channel delX as temporary delX here

        ! delX gives the distance between cross-sections
        ! Warning: delX(n) might be zero (since the downstream distance is not really defined
        delX=reach_data%delX
        ! delX_v denotes the lengths of each 'volume', centred around each
        ! cross-section, with boundaries at the mid-point between cross-sections
        delX_v=reach_data%delX_v

        IF(minval(delX_v)<=0._dp) THEN
            print*, 'ERROR: min delX_v <= 0'
            print*, delX_v
            stop
        END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! PREDICTOR STEP
        !
        IF(location_flags) THEN
            print*, ' # Reach --> ', trim(reach_data%names(1)),' ', trim(reach_data%names(2))
            print*, 'Start Pred'
        END IF

        ! Compute Area predictor Apred_i = Alast_i + dT/dX_v*[Qlast_(i+1/2) - Qlast_(i-1/2)],
        ! where Qlast_(i+1/2) ~= Qlast_(i+1)
        Area_pred(1:n-1) = reach_data%Area(1:n-1) -dT/delX_v(1:n-1)*&
                          (reach_data%Discharge(2:n) - reach_data%Discharge(1:n-1))
        ! Boundary condition
        Area_pred(n) = reach_data%Area(n)

        ! Check it
        DO i=1,n-1
            IF((Area_pred(i)<0._dp).OR.(Area_pred(i).NE.Area_pred(i))) THEN
                print*, 'Area_pred(', i,') is negative or nan, ', Area_pred(i), &
                         reach_data%Discharge(i+1), reach_data%Discharge(i), &
                         reach_data%Area(i), (dT/delX_v(i))*(reach_data%Discharge(i+1)-reach_data%Discharge(i))
                stop
            END IF
        END DO

        ! Wet-dry flag
        IF(wet_dry_hacks) THEN
            dry_flag=merge(1.0_dp, 0.0_dp, reach_data%Area/max(reach_data%Width,1.0_dp) > reach_data%wet_dry_depth)
        ELSE
            dry_flag=1.0_dp + 0._dp*reach_data%Area
        END IF

        IF(convective_terms) THEN
            convective_flux= reach_data%Discharge**2 / max(reach_data%Area, small_positive_real) * dry_flag

            ! Experimental limiting, doesn't work
            !call minmod_limit(convective_flux, n)
        ELSE
            convective_flux=dry_flag*0._dp
        END IF

        !call minmod_limit(reach_data%Stage,n)
        ! Compute slope, and area, with extrapolation at n
        slope(1:n-1) = (reach_data%Stage(2:n) - reach_data%Stage(1:n-1))/delX(1:n-1)*dry_flag(1:n-1)
        ! If we have stage in the boundary, use it in setting the slope
        IF(index(reach_data%Downstream_boundary%compute_method,'stage')>0) THEN
            slope(n) = (reach_data%Downstream_boundary%eval(time, 'stage') - reach_data%Stage(n))/delX(n-1)*dry_flag(n)
        ELSE
            slope(n)=(reach_data%Stage(n)-reach_data%Stage(n-1))/delX(n-1)*dry_flag(n)
        END IF

        Af(1:n-1)=0.5_dp*(reach_data%Area(1:n-1)+reach_data%Area(2:n)) ! 'Forward' area estimate
        Af(n)=Af(n-1)

        ! Compute Q predictor with implicit friction
        ! Convective + gravity terms
        !Q_pred(1:n-1) = reach_data%Discharge(1:n-1) -  &
        !               dT/delX_v(1:n-1)*(convective_flux(2:n) - convective_flux(1:n-1)) &
        !               -dT*gravity*Af(1:n-1)*slope(1:n-1) 
        Q_pred= reach_data%Discharge -  &
                       !dT/delX_v*(/ (convective_flux(2:n) - convective_flux(1:n-1)), 0._dp /) &
                       dT/delX_v*(/ (convective_flux(2:n) - convective_flux(1:n-1)), 0._dp-convective_flux(n) /) &
                       -dT*gravity*Af*slope 

        !print*, 'Qp1, No Frict: ', Q_pred(1)
        ! IMPLICIT FRICTION: g*Af*Sf = drag_factor*Q_pred*abs(Q_pred)
        IF(implicit_friction) THEN
            !drag_factor(1:n-1)=1.0_dp*(gravity*Af(1:n-1)*(-sign(1._dp, Q_pred(1:n-1))/(Area_pred(1:n-1)**2))*&
            !                          reach_data%Drag_1D(1:n-1) )
            drag_factor=1.0_dp*(gravity*Af*(-sign(1._dp, Q_pred)/(max(Area_pred**2,small_positive_real)))*&
                                      reach_data%Drag_1D )
            !print*, 'df1: ', drag_factor(1), dT*drag_factor(1)*Q_pred(1)
            !DO i=1,n-1
            DO i=1,n
                 IF(abs(drag_factor(i)) > 0._dp+small_positive_real) THEN
                    Q_pred(i)= (1._dp - sqrt(1._dp- 4._dp*dT*drag_factor(i)*Q_pred(i) ))/(2._dp*dT*drag_factor(i))
                    !IF(i==1) THEN
                    !    print*, Q_pred(i), 1.0_dp/(dT*drag_factor(i)), dT
                    !END IF
                 ELSE
                    ! Friction is negligible
                 END IF
            END DO
        ELSE
            !Q_pred(1:n-1) = Q_pred(1:n-1) -dT*gravity*Af(1:n-1)*Discharge_old(1:n-1)*abs(Discharge_old(1:n-1))/&
            !                                (reach_data%Area(1:n-1)**2+small_positive_real)*reach_data%Drag_1D(1:n-1)
            Q_pred = Q_pred -dT*gravity*Af*Discharge_old*abs(Discharge_old)/&
                                            (reach_data%Area**2+small_positive_real)*reach_data%Drag_1D

        END IF
        
        ! BOUNDARY CONDITIONS: NOMINAL ONLY -- we fix at the end. They only affect A_cor(n), Q_cor(n)
        !Area_pred(n) = reach_data%Area(n)
        !Q_pred(n) = reach_data%Discharge(n)

        IF(location_flags) print*, 'Pred boundaries'

        ! Try to enforce 'conservative' discharge boundaries
        ! These should ensure that inflows have the desired values
        ! Idea: 0.5*(Qpred_zero + Qlast(1)) = Desired discharge at time + dT/2, at 1/2
        IF(index(reach_data%Downstream_boundary%compute_method,'discharge')>0) THEN
            ! This will enforce the discharge
            Q_pred(n) = ( reach_data%Downstream_boundary%eval(time+dT, 'discharge') ) !+ &
        END IF

        IF(index(reach_data%Upstream_boundary%compute_method,'discharge')>0) THEN
            ! This enforces the discharge for physical boundaries, but might
            ! not work for junctions
            Qpred_zero=( reach_data%Upstream_boundary%eval(time+dT, 'discharge') + &
                        reach_data%Upstream_boundary%eval(time, 'discharge')  ) &
                        - reach_data%Discharge(1)
        ELSE
        !    ! Need to give this a value
            Qpred_zero = 2.0_dp*Q_pred(1)-Q_pred(2)! !reach_data%Discharge(1) !min(2.0_dp*reach_data%Discharge(1)-reach_data%Discharge(2))
                        ! 2.0_dp*Q_pred(1) -Q_pred(2)!2.0_dp*reach_data%Discharge(1)-reach_data%Discharge(2)
        END IF

        !print*, 'Qp1, A: ', Q_pred(1)

        IF(wet_dry_hacks) THEN

            ! NOW, enforce a no-drying limit on Area_cor
            ! Idea: Don't allow Q_pred(i) > volume in cell i-1 (if Q_pred is positive)
            ! or < - volume in cell i (if Q_pred is negative)
            DO i=1,n-1
                Qcon = Q_pred(i)
                IF(Qcon>0._dp) THEN
                    IF(Qcon*dT> reach_data%Area(i)*delX_v(i)-small_positive_real) THEN
                        Q_pred(i) = max(reach_data%Area(i)*delX_v(i)/dT -small_positive_real, 0._dp)
                    END IF
                ELSE
                    IF(abs(Qcon)*dT> reach_data%Area(i+1)*delX_v(i+1)-small_positive_real) THEN
                        Q_pred(i) = -max(reach_data%Area(i+1)*delX_v(i+1)/dT-small_positive_real,0._dp)
                    END IF
                END IF
            END DO
            
            ! As above for Qpred_zero
            Qcon=Qpred_zero
            IF(Qcon<0._dp) THEN
               IF(abs(Qcon)*dT> reach_data%Area(1)*delX_v(1)-small_positive_real) THEN
                   Qpred_zero = -max(reach_data%Area(1)*delX_v(1)/dT-small_positive_real, 0._dp)
               END IF
            ELSE
                ! If we have a junction boundary, make sure that the inflow is not > volume in junction
                SELECT TYPE(x=>reach_data%Upstream_boundary)
                    TYPE IS(JUNCTION_BOUNDARY)
                        Qpred_zero=min(Qpred_zero, (2.0_dp*x%Volume/dT-Discharge_old(1)) )
                END SELECT

            END IF
            ! As above for Qpred(n)
            Qcon=Q_pred(n)
            IF(Qcon>0._dp) THEN
               IF(Qcon*dT> reach_data%Area(n)*delX_v(n)-small_positive_real) THEN
                   Q_pred(n) = max(reach_data%Area(n)*delX_v(n)/dT-small_positive_real, 0._dp)
               END IF
            ELSE
                ! If we have a junction boundary, make sure that the inflow is not > volume in junction
                SELECT TYPE(x=>reach_data%Downstream_boundary)
                    TYPE IS(JUNCTION_BOUNDARY)
                        Q_pred(n)=max(Q_pred(n), (-2.0_dp*x%Volume/dT-Discharge_old(n)) )
                END SELECT
            END IF

            ! Here, we check for changes in the sign of Q_pred. This could
            ! voilate the no-dry logic above. Can fix by setting 1 value to zero
            DO i=1,n-1
                IF(sign(1.0_dp, Q_pred(i)) == -1._dp*sign(1.0_dp, Q_pred(i+1)) ) THEN

                    IF( (Q_pred(i+1)-Q_pred(i)) > reach_data%Area(i+1)*delX_v(i+1)/dT-small_positive_real  ) THEN
                        IF(abs(Q_pred(i))<abs(Q_pred(i+1))) THEN
                            Q_pred(i)=0._dp
                        ELSE
                            Q_pred(i+1)=0._dp
                        END IF
                    END IF
                END IF
            END DO


        END IF

        !print*, 'Qp1, B: ', Q_pred(1)

        ! Back-calculate stage/width/drag
        !print*, 'pred_interp'
        DO i=1,n
            Stage_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'stage')
            Width_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'width')
            Drag1D_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'drag_1D')
        END DO

        ! Wet dry hack
        IF(wet_dry_hacks) THEN
            Q_pred(1:n-1)=merge(Q_pred(1:n-1), 0._dp*Q_pred(1:n-1),&
                           Area_pred(1:n-1)/max(Width_pred(1:n-1),1.0_dp) > reach_data%wet_dry_depth)
        END IF

        !print*, 'Qp1, C: ', Q_pred(1)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! CORRECTOR STEP
        IF(location_flags) print*, 'Start Cor'

        ! Compute Area corrector
        Area_cor(2:n) = reach_data%Area(2:n) -dT/delX_v(2:n)*&
                          (Q_pred(2:n) - Q_pred(1:n-1))
        ! Discharge Conservative boundary treatment.
        Area_cor(1) = reach_data%Area(1) -dT/delX_v(1)*(Q_pred(1)-Qpred_zero)
        
        ! ERROR CHECK
        DO i=1,n
            IF((Area_cor(i)<0._dp).OR.(Area_cor(i).NE.Area_cor(i))) THEN
                IF(i.EQ.1) THEN
                    print*, 'Area_cor(', i,') is negative, ',Area_cor(i), Q_pred(i), Qpred_zero, reach_data%Area(i), &
                                                    (dT/delX_v(i))*(Q_pred(i)-Qpred_zero), i, n, drag_factor(i), & 
                                                    reach_data%Drag_1D(i), reach_data%Discharge(i), slope(i), convective_flux(i)
                    stop
                ELSE
                    print*, 'Area_cor(', i,') is negative, ',Area_cor(i), Q_pred(i), Q_pred(i-1), reach_data%Area(i), &
                                                    (dT/delX_v(i))*(Q_pred(i)-Q_pred(i-1)), i, n, drag_factor(i), &
                                                    reach_data%Drag_1D(i), reach_data%Discharge(i), slope(i), convective_flux(i)
                    stop
               
                END IF 
            END IF
        END DO

        ! Wet-dry flag
        IF(wet_dry_hacks) THEN
            dry_flag=merge(1.0_dp, 0.0_dp, Area_pred/max(Width_pred,1.0_dp) > reach_data%wet_dry_depth)
        ELSE
            dry_flag=1.0_dp + 0._dp*Area_pred
        END IF
        
        IF(convective_terms) THEN
            convective_flux = Q_pred**2 / max(Area_pred, small_positive_real) * dry_flag  

            ! Experimental limiting, doesn't work
            !call minmod_limit(convective_flux, n)

        ELSE
            convective_flux=dry_flag*0._dp
        END IF

        ! Experimental limiting
        !call minmod_limit(Stage_pred,n)
        ! Extrapolate Slope and Ab at 1
        slope(2:n) = (Stage_pred(2:n) - Stage_pred(1:n-1))/delX(1:n-1)*dry_flag(2:n)
        !slope(1) = (Stage_pred(2)-Stage_pred(1))/delX(1)*dry_flag(1)
        IF(index(reach_data%Upstream_boundary%compute_method,'stage')>0) THEN
            slope(1) = (Stage_pred(1) - reach_data%Downstream_boundary%eval(time, 'stage') )/delX(1)*dry_flag(1)
        ELSE
            slope(1) = (Stage_pred(2)-Stage_pred(1))/delX(1)*dry_flag(1)
        END IF
        
     
        Ab(2:n)=0.5_dp*(Area_pred(1:n-1)+Area_pred(2:n)) ! 'backward' area estimate
        Ab(1) = Ab(2)
        ! Convective + gravity terms
        !Q_cor(2:n) = reach_data%Discharge(2:n) -  &
        !               dT/delX_v(2:n)*(convective_flux(2:n) - convective_flux(1:n-1)) &
        !               -dT*gravity*Ab(2:n)*slope(2:n) 
        Q_cor = reach_data%Discharge -  &
                       !dT/delX_v*(/ 0._dp ,(convective_flux(2:n) - convective_flux(1:n-1)) /) &
                       dT/delX_v*(/ convective_flux(1) -0._dp,(convective_flux(2:n) - convective_flux(1:n-1)) /) &
                       -dT*gravity*Ab*slope 
        
        ! IMPLICIT FRICTION: g*Ab*Sf = drag_factor*Q_cor*abs(Q_cor)
        IF(implicit_friction) THEN
            !drag_factor(2:n)=1.0_dp*(gravity*Ab(2:n)*&
            !                 (-sign(1._dp, Q_cor(2:n))/(Area_cor(2:n)**2))*Drag1D_pred(2:n) )
            drag_factor=1.0_dp*(gravity*Ab*&
                             (-sign(1._dp, Q_cor)/(max(Area_cor**2, small_positive_real)))*Drag1D_pred )
            !DO i=2,n
            DO i=1,n
                 IF(abs(drag_factor(i)) > 0._dp+small_positive_real) THEN
                    Q_cor(i)= (1._dp - sqrt(1._dp- 4._dp*dT*drag_factor(i)*Q_cor(i) ))/(2._dp*dT*drag_factor(i))
                 ELSE
                    ! Friction is negligible
                 END IF
            END DO
        ELSE
            !Q_cor(2:n) = Q_cor(2:n) -dT*gravity*Ab(2:n)*Q_pred(2:n)*abs(Q_pred(2:n))/&
            !                                (Area_pred(2:n)**2+small_positive_real)*Drag1D_pred(2:n)
            Q_cor(1:n) = Q_cor(1:n) -dT*gravity*Ab(1:n)*Q_pred(1:n)*abs(Q_pred(1:n))/&
                                            (Area_pred(1:n)**2+small_positive_real)*Drag1D_pred(1:n)

            !Q_cor(2:n) = Q_cor(2:n) -dT*gravity*Ab(2:n)*reach_data%Discharge_con(2:n)*abs(reach_data%Discharge_con(2:n))/&
            !                                (Area_pred(2:n)**2+small_positive_real)*Drag1D_pred(2:n)
        END IF

        IF(wet_dry_hacks) THEN
            ! Put discharge to zero in nearly dry cells
            DO i=2,n
                Width_cor(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_cor(i), 'area', 'width')
            END DO
            Q_cor(2:n)=merge(Q_cor(2:n), 0._dp*Q_cor(2:n), Area_cor(2:n)/max(width_cor(2:n),1.0_dp)>reach_data%wet_dry_depth)
        END IF


        

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! COMPUTE 'FINAL' UPDATE

        !reach_data%Discharge(2:n-1)= 0.5_dp*(Q_pred(2:n-1) + Q_cor(2:n-1))
        reach_data%Discharge(1:n)= 0.5_dp*(Q_pred(1:n) + Q_cor(1:n))
        reach_data%Area(1:n) = 0.5_dp*(Area_pred(1:n) + Area_cor(1:n)) 

        !print*, 'before boundary', reach_data%Area(1), reach_data%Area(n)
        IF(location_flags) print*, 'Boundary conditions'
        call apply_boundary_conditions(reach_data, time, dT, n, location_flags)

       
        ! Back-calculate Stage, width, 1D drag
        !print*, 'final_interp', reach_data%Area(1), reach_data%Area(n)
        DO i=1,n
            !print*, 'bc', i
            reach_data%Stage(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'stage')
            reach_data%Width(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'width')
            reach_data%Drag_1D(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'drag_1D')
        END DO
        

        ! Wet dry hack
        IF(wet_dry_hacks) THEN
            reach_data%Discharge(2:n-1)=merge(reach_data%Discharge(2:n-1), 0._dp*reach_data%Discharge(2:n-1), &
                                   reach_data%Area(2:n-1)/max(reach_data%Width(2:n-1),1.0_dp) > reach_data%wet_dry_depth)
            ! NOW, enforce a no-drying limit on Area
            ! outgoing_flux < cell volume
            ! Try to prevent negative depths on the next A_pred, by ensuring that 
            ! 'Outflow volume <= volume in cell'. 
            ! FIXME: Note: this assumes no change in dT, which might not be realistic
            ! However, if dT is constant, then it can perhaps be justified with a
            ! CFL condition type constraint
            ! FIXME: CHECK THAT THIS DOESNT SCREW OTHER RESULTS
            ! Test 1 -- commented out this section, and re-ran nangka. Needed a
            ! small piolet discharge, but I did not see any effect on the results otherwise
            timestep_increase_buffer=1.0_dp/max_timestep_increase 
            !timestep_increase_buffer=1.0_dp
            DO i=1,n-1
                Qup = reach_data%Discharge(i+1) 
                Qdown = reach_data%Discharge(i) 
                Qdiff=Qup-Qdown!max(abs(Qup), abs(Qdown), abs(Qup-Qdown))!Qup-Qdown
                IF((Qup>=0._dp).AND.(Qdown>=0._dp)) THEN
                    IF(Qdiff*dT> max(reach_data%Area(i)*delX_v(i)-small_positive_real,0._dp)*timestep_increase_buffer) THEN
                        ! On the next timestep, the cell would go dry. Stop it by limiting Qup
                        reach_data%Discharge(i+1) = &
                              max(reach_data%Area(i)*delX_v(i)/dT -small_positive_real,0._dp)*timestep_increase_buffer
                    END IF
                ELSEIF( (Qup<=0._dp).AND.(Qdown<=0._dp)) THEN
                    IF(Qdiff*dT> max(reach_data%Area(i)*delX_v(i)-small_positive_real, 0._dp)*timestep_increase_buffer) THEN
                        ! On the next timestep, the cell would go dry. Stop it by limiting Qdown
                        reach_data%Discharge(i) = &
                              -max(reach_data%Area(i)*delX_v(i)/dT-small_positive_real,0._dp)*timestep_increase_buffer
                    END IF
                ELSE IF( Qup*Qdown<0._dp) THEN
                    IF(Qdiff*dT> (reach_data%Area(i)*delX_v(i)-small_positive_real)*timestep_increase_buffer) THEN
                        ! On the next timestep, the cell would go dry. Stop it
                        reach_data%Discharge(i+1) = 0.5_dp*sign(1.0_dp, reach_data%Discharge(i+1))*&
                            max(reach_data%Area(i)*delX_v(i)/dT -small_positive_real,0._dp)*timestep_increase_buffer
                        reach_data%Discharge(i) = 0.5_dp*sign(1.0_dp, reach_data%Discharge(i))*&
                            max(reach_data%Area(i)*delX_v(i)/dT -small_positive_real,0._dp)*timestep_increase_buffer
                    END IF
                END IF

            END DO


        END IF
        ! Compute 'conservative' discharge, which is much better behaved than
        ! pointwise discharge
        reach_data%Discharge_con=(/0.5_dp*(Qpred_zero+Discharge_old(1)),&
                                   0.5_dp*(Q_pred(1:n-1) + Discharge_old(2:n)) , &
                                   0.5_dp*(Q_pred(n)+Discharge_old(n))/)
        
        !print*, 'Discharge_con 1: ', reach_data%Discharge_con(1)
        !print*, 'Discharge_con n+1: ', reach_data%Discharge_con(n+1)
        ! Try to fix up conservation at the ends
        reach_data%Discharge_con(1) = delX_v(1)*(reach_data%Area(1) - Area_old(1))/dT + reach_data%Discharge_con(2)
        reach_data%Discharge_con(n+1) =-delX_v(n)*(reach_data%Area(n) - Area_old(n))/dT +reach_data%Discharge_con(n)

        ! If we have a junction boundary, make sure that the inflow is not > volume in junction
        safety=1.0_dp
        SELECT TYPE(x=>reach_data%Downstream_boundary)
            TYPE IS(JUNCTION_BOUNDARY)
                IF(reach_data%Discharge_con(n+1) < -2.0_dp*x%Volume/dT*safety) THEN
                    ! Volume is too large, let's clip it and adjust the area accordingly (and hope that doesn't go dry)
                    reach_data%Discharge_con(n+1) = min(-2.0_dp*x%Volume/dT*safety +small_positive_real, 0._dp)
                    reach_data%Area(n) = (reach_data%Discharge_con(n) - reach_data%Discharge_con(n+1))/delX_v(n)*dT + Area_old(n)

                    IF(reach_data%Area(n)<0._dp) THEN
                        print*, 'Making xsect area n < 0. Im gonna cheat for now! CONSERVATION ERROR'
                        reach_data%Area(n)=0._dp
                        !stop
                    END IF
                END IF
        END SELECT
        SELECT TYPE(x=>reach_data%Upstream_boundary)
            TYPE IS(JUNCTION_BOUNDARY)
                IF(reach_data%Discharge_con(1) > 2.0_dp*x%Volume/dT*safety) THEN
                    ! Volume is too large, let's clip it and adjust the area accordingly (and hope that doesn't go dry)
                    reach_data%Discharge_con(1) = max(2.0_dp*x%Volume/dT*safety -small_positive_real, 0._dp)
                    reach_data%Area(1) = -(reach_data%Discharge_con(2) - reach_data%Discharge_con(1))/delX_v(1)*dT + Area_old(1)

                    IF(reach_data%Area(1)<0._dp) THEN
                        print*, 'Making xsect area 1 < 0. Im gonna cheat for now! CONSERVATION ERROR'
                        reach_data%Area(1)=0._dp
                        !stop
                    END IF
                END IF
        END SELECT
        
        !print*, 'Discharge_con 1: ', reach_data%Discharge_con(1)
        !print*, 'Discharge_con n+1: ', reach_data%Discharge_con(n+1)

        IF(location_flags) print*, 'done'

    END SUBROUTINE one_mccormack_step

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE update_junction_values(network)
        ! Update the volume / momentum in each junction
        TYPE(network_data_type), INTENT(INOUT):: network
        
        INTEGER(ip):: i, j, r, N, M
        REAL(dp):: Q_update, Qx_update, Qy_update, V, volume_old

        IF(network%num_junctions>0) THEN
            ! Loop over every junction
            DO i=1,network%num_junctions
                N=size(network%reach_junctions(i)%reach_ends)

                ! Re-set junction momentum to zero
                network%reach_junctions(i)%Discharge_x=0._dp
                network%reach_junctions(i)%Discharge_y=0._dp

                volume_old=network%reach_junctions(i)%Volume

                ! Loop over every reach connecting to the junction
                DO j=1,N
                    ! Identify the associated reach index
                    r=network%reach_junctions(i)%reach_index(j)

                    ! Update the volume of water in the junction
                    IF(network%reach_junctions(i)%reach_ends(j) == 'Up') THEN
                        M = network%reach_data(r)%xsect_count
                        Q_update=network%reach_data(r)%Discharge_con(M+1)
                        ! Momentum update = u*Q
                        Qx_update= Q_update*network%reach_data(r)%Discharge(M)/(network%reach_data(r)%Area(M)+small_positive_real)
                    ELSEIF(network%reach_junctions(i)%reach_ends(j) == 'Dn') THEN
                        Q_update=-network%reach_data(r)%Discharge_con(1)
                        ! Momentum update = u*Q
                        Qx_update= Q_update*network%reach_data(r)%Discharge(1)/(network%reach_data(r)%Area(1)+small_positive_real)
                    ELSE
                        print*, 'ERROR: reach end is neither Up or Dn'
                        stop
                    END IF
                    network%reach_junctions(i)%Volume = network%reach_junctions(i)%Volume + network%dT*Q_update
                    print*, 'Q_update ', j, Q_update, trim(network%reach_data(r)%names(2)), &
                             trim(network%reach_junctions(i)%reach_ends(j))
                    ! Update the momenta
                    ! FIXME -- use a crude average at present
                    network%reach_junctions(i)%Discharge_x=network%reach_junctions(i)%Discharge_x + network%dT*Qx_update
                    network%reach_junctions(i)%Discharge_y=0._dp
                END DO

                ! Update the stage
                V = network%reach_junctions(i)%Volume
            
                IF(V>0._dp) THEN
                    network%reach_junctions(i)%Stage = network%reach_junctions(i)%Stage_volume_curve%eval( V, 'volume', 'stage')
                ELSE
                    print*, 'ERROR: V<0: WARNING: CONSERVATION ERROR'
                    print*, 'junction ', i, ' s= ', network%reach_junctions(i)%Stage, network%reach_junctions(i)%Volume, &
                    network%reach_junctions(i)%Volume - volume_old, network%reach_junctions(i)%Discharge_x,&
                    trim(network%reach_junctions(i)%reach_names(1,2)) ,'-',trim(network%reach_junctions(i)%reach_ends(1)),' '
                    stop
                    !network%reach_junctions(i)%Volume=0._dp
                    !V=0._dp
                    !network%reach_junctions(i)%Stage = network%reach_junctions(i)%Stage_volume_curve%eval( V, 'volume', 'stage')
                END IF

                print*, 'junction ', i, ' s= ', network%reach_junctions(i)%Stage, network%reach_junctions(i)%Volume, &
                        network%reach_junctions(i)%Volume - volume_old, network%reach_junctions(i)%Discharge_x,&
                        trim(network%reach_junctions(i)%reach_names(1,2)) ,'-',trim(network%reach_junctions(i)%reach_ends(1)),' ',&
                        trim(network%reach_junctions(i)%reach_names(2,2)), '-',trim(network%reach_junctions(i)%reach_ends(2)),' ', &
                        trim(network%reach_junctions(i)%reach_names(3,2)), '-',trim(network%reach_junctions(i)%reach_ends(3))
            END DO
        END IF

    END SUBROUTINE update_junction_values
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    FUNCTION minmod(a,b)
        REAL(dp), INTENT(IN):: a, b
        REAL(dp):: minmod

        IF(sign(1.0_dp, a) /=sign(1.0_dp, b)) THEN
            minmod=0._dp
        ELSE
            minmod=min(abs(a), abs(b))*sign(1.0_dp, a)
        END IF

    END FUNCTION minmod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE minmod_limit(X, n)
        INTEGER(ip), INTENT(IN)::n
        REAL(dp), INTENT(INOUT):: X(n)

        INTEGER:: i
        REAL(dp):: Xtmp(n)

        ! Experimental limiting
        Xtmp=X
        DO i=2,n-1
            IF(sign(1.0_dp, X(i)-X(i-1))== &
               sign(1.0_dp, X(i+1)-X(i))*(-1)) THEN
                Xtmp(i) = minmod(X(i+1), X(i-1))
            END IF
        END DO
        X=Xtmp
    
    END SUBROUTINE



END MODULE network_solver
