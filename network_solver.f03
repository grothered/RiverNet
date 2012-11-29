MODULE network_solver
    ! Routines to solve the St-Venant Equations with Junctions in a river network
    USE global_defs
    USE river_classes
   
    IMPLICIT NONE 
    contains

    SUBROUTINE evolve_hydraulics(network)
        ! Evolve the hydraulics in the network
        TYPE(network_data_type), INTENT(IN OUT):: network
        INTEGER:: i

        ! Update the timestep dT
        CALL update_timestep(network)

        ! New boundary values for time t and t+delT -- the boundaries should store both 
        CALL update_boundaries(network)
        
        ! Update the flow Stage, Area + Discharge in reaches + junctions

        DO i=1,network%num_reaches
            call one_mccormack_step(network%reach_data(i), network%time, network%dT)
        END DO

        DO i=1,network%num_junctions
            call update_junction_values(network%reach_junctions(i))
        END DO

        ! Advance time
        network%time=network%time+network%dT
        
        

    END SUBROUTINE evolve_hydraulics

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE update_timestep(network)
        ! Loop over every xsection, and compute the new timestep from the CFL condition
        TYPE(network_data_type), INTENT(IN OUT):: network

        ! Local vars
        INTEGER(ip):: i, j
        REAL(dp):: dT, local_wavespeed, local_dt, vel, grav_wavespeed

        ! Predefine timestep
        dT = maximum_allowed_timestep 

        ! Loop over all xsections on all reaches and get the new timestep
        DO i=1,network%num_reaches
            DO j=1, network%reach_data(i)%xsect_count
                ! 1D Velocity
                vel=network%reach_data(i)%Discharge(j)/max(network%reach_data(i)%Area(j), small_positive_real)
                ! Gravity wavespeed = sqrt( g * mean_depth)
                grav_wavespeed=(gravity*network%reach_data(i)%Area(j)/network%reach_data(i)%Width(j))**0.5_dp
                local_wavespeed = abs(vel) + grav_wavespeed

                ! FIXME: Check interpretation of downstream distances -- perhaps incorrect
                local_dt = network%reach_data(i)%downstream_dists(j,2)/max(local_wavespeed, small_positive_real)
                local_dt = local_dt*network%CFL
                dT = min(dT, local_dt)
            END DO
        END DO

        network%dT=dT

    END SUBROUTINE update_timestep

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE one_mccormack_step(reach_data, time, dT)
        !
        ! Mc-Cormack type flow solver with tweaks
        !
        ! FIXME: Consider re-schematising: Volumes occur between x-sections,
        ! 'conservative' discharge occurs at cross-sections. This might make it
        ! easier to impose boundary conditions, and to avoid conceptually 'extending' the
        ! reach beyond the bounds of the first and last xsection
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

        n=reach_data%xsect_count

        ! Use channel delX as temporary delX here

        ! delX gives the distance between cross-sections
        delX = reach_data%downstream_dists(:,2)
        ! delX_v denotes the lengths of each 'volume', centred around each
        ! cross-section, with boundaries at the mid-point between cross-sections

        ! Assume the upstream volume has upstream half-length equal to downstream half-length?
        !delX_v = (/ 0.5_dp*(delX(1:n-1) + delX(2:n)  ), delX(n) /) 

        ! Assume the upstream/downstream volumes are 'half as long' as other
        ! volumes, so that the reach still ends at the bounding x-sections
        delX_v = (/0.5*delX(2),  0.5_dp*(delX(2:n-1) + delX(3:n)  ), 0.5_dp*delX(n) /) 

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! PREDICTOR STEP
        !

        ! Compute Area predictor Apred_i = Alast_i + dT/dX_v*[Qlast_(i+1/2) - Qlast_(i-1/2)],
        ! where Qlast_(i+1/2) ~= Qlast_(i+1)
        Area_pred(1:n-1) = reach_data%Area(1:n-1) -dT/delX_v(1:n-1)*&
                          (reach_data%Discharge(2:n) - reach_data%Discharge(1:n-1))
        !Vol_pred(1:n-1) = reach_data%Volume(1:n-1) -dT*&
        !                  (reach_data%Discharge(2:n) - reach_data%Discharge(1:n-1))

        ! Wet-dry flag
        dry_flag=merge(1.0_dp, 0.0_dp, reach_data%Area/reach_data%Width > reach_data%wet_dry_depth)

        convective_flux= reach_data%Discharge**2 / max(reach_data%Area, small_positive_real) * dry_flag  
        slope(1:n-1) = (reach_data%Stage(2:n) - reach_data%Stage(1:n-1))/delX(2:n)*dry_flag(1:n-1)

        Af(1:n-1)=0.5_dp*(reach_data%Area(1:n-1)+reach_data%Area(2:n)) ! 'Forward' area estimate
        ! Compute Q predictor with implicit friction
        ! Convective + gravity terms
        Q_pred(1:n-1) = reach_data%Discharge(1:n-1) -  &
                       dT/delX_v(1:n-1)*(convective_flux(2:n) - convective_flux(1:n-1)) &
                       -dT*gravity*Af(1:n-1)*slope(1:n-1) 

        ! IMPLICIT FRICTION: g*Af*Sf = drag_factor*Q_pred*abs(Q_pred)
        drag_factor(1:n-1)=(gravity*Af(1:n-1)*(-sign(1._dp, Q_pred(1:n-1))/(Area_pred(1:n-1)**2._dp))*reach_data%Drag_1D(1:n-1) )
        DO i=1,n-1
             IF(drag_factor(i) > 0._dp) THEN
                Q_pred(i)= (1._dp - sqrt(1._dp- 4._dp*dT*drag_factor(i)*Q_pred(i) ))/(2._dp*dT*drag_factor(i))
             ELSE
                ! Friction is negligible
             END IF
        END DO
        
        ! BOUNDARY CONDITIONS: NOMINAL ONLY -- we fix at the end. They only affect A_cor(n), Q_cor(n)
        Area_pred(n) = reach_data%Area(n)
        Q_pred(n) = reach_data%Discharge(n)

        ! Back-calculate stage/width/drag
        DO i=1,n
            Stage_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'stage')
            Width_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'width')
            Drag1D_pred(i) = reach_data%xsects(i)%stage_etc_curve%eval(Area_pred(i), 'area', 'drag1D')
        END DO

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! CORRECTOR STEP

        ! Compute Area corrector
        Area_cor(2:n) = reach_data%Area(2:n) -dT/delX_v(2:n)*&
                          (Q_pred(2:n) - Q_pred(1:n-1))

        ! Wet-dry flag
        dry_flag=merge(1.0_dp, 0.0_dp, Area_pred/Width_pred > reach_data%wet_dry_depth)

        convective_flux = Q_pred**2 / max(Area_pred, small_positive_real) * dry_flag  
        slope(2:n) = (Stage_pred(2:n) - Stage_pred(1:n-1))/delX(2:n)*dry_flag(2:n)
        
        ! Compute Q corrector with implicit friction
        Ab(2:n)=0.5_dp*(Area_pred(1:n-1)+Area_pred(2:n)) ! 'backward' area estimate
        ! Convective + gravity terms
        Q_cor(2:n) = reach_data%Discharge(2:n) -  &
                       dT/delX_v(2:n)*(convective_flux(2:n) - convective_flux(1:n-1)) &
                       -dT*gravity*Ab(2:n)*slope(2:n) 
        
        ! IMPLICIT FRICTION: g*Ab*Sf = drag_factor*Q_cor*abs(Q_cor)
        drag_factor(2:n)=(gravity*Ab(2:n)*(-sign(1._dp, Q_cor(2:n))/(Area_cor(2:n)**2._dp))*Drag1D_pred(2:n) )
        DO i=2,n
             IF(drag_factor(i) > 0._dp) THEN
                Q_cor(i)= (1._dp - sqrt(1._dp- 4._dp*dT*drag_factor(i)*Q_cor(i) ))/(2._dp*dT*drag_factor(i))
             ELSE
                ! Friction is negligible
             END IF
        END DO

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! COMPUTE 'FINAL' UPDATE

        reach_data%Area(2:n-1)= 0.5_dp*(Area_pred(2:n-1) + Area_cor(2:n-1))
        reach_data%Discharge(2:n-1)= 0.5_dp*(Q_pred(2:n-1) + Q_cor(2:n-1))

        ! COULD POSSIBLY APPLY BOUNDARY CONDITIONS HERE??
        ! FIXME: Overspecified, not exactly conservative, etc
        reach_data%Area(n) = reach_data%Upstream_boundary%eval(t+delT, 'area')
        reach_data%Discharge(n) = reach_data%Upstream_boundary%eval(t+delT, 'discharge')
        reach_data%Area(1) = reach_data%Downstream_boundary%eval(t+delT, 'area')
        reach_data%Discharge(1) = reach_data%Downstream_boundary%eval(t+delT, 'discharge')
        

        ! Back-calculate Stage, width, 1D drag
        DO i=1,n
            reach_data%Stage(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'stage')
            reach_data%Width(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'width')
            reach_data%Drag_1D(i) = reach_data%xsects(i)%stage_etc_curve%eval(reach_data%Area(i), 'area', 'drag1D')
        END DO
        
        ! Compute 'conservative' discharge??


    END SUBROUTINE one_mccormack_step
END MODULE network_solver
