MODULE mod_clock
   !!------------------------------------------------------------------------------
   !!
   !!       MODULE: mod_clock
   !!
   !!          Updates the TRACMASS clock (time steps )
   !!
   !!          Subroutines included:
   !!               - update_time
   !!
   !!------------------------------------------------------------------------------

   USE mod_time
   USE mod_loopvars, only   : ds, dsc, dsmin, dts
   use mod_grid, only       : dxyz
   USE mod_param, only      : iter

   IMPLICIT NONE

   CONTAINS

     SUBROUTINE update_time
     ! --------------------------------------------------
     !
     ! Purpose:
     ! updates the value of tt, ts, and intrpg  and intrpr
     !
     ! --------------------------------------------------

       ! Transform ds to dt in seconds
       IF (ds /= dsmin) dt = ds * dxyz

       IF (dt.LT.0.d0) THEN
          PRINT *,"Error! dt is less than zero."
          PRINT *,'dt=',dt,"ds=",ds,"dxyz=",dxyz,"dsmin=",dsmin
          STOP
       END IF

       ! If time step makes the integration
       ! exceed the time when fields change
       IF (tss + dt/tseas*DBLE(iter) .GE. DBLE(iter)) THEN

          dts = ts

          dt  = DBLE(INT(ts,8)+1)*tseas-tt
          tt  = DBLE(INT(ts,8)+1)*tseas
          ts  = DBLE(INT(ts,8)+1)
          tss = DBLE(iter)
          ds  = dt/dxyz
          dsc = ds

          dts = ts - dts

       ELSE
          ! Update the real time
          tt = tt + dt

          ! If the time step is equal to the minimum time step
          IF (dt == dtmin) THEN
              ts  = ts  + dsubstep
              tss = tss + 1.d0

              dts = dsubstep

          ! If the time step is equal to the time step to the next subcycle
          ELSE IF (dt == dtreg) THEN

              dts = ts

              ts  = NINT((ts + dtreg/tseas)*DBLE(iter),8)/DBLE(iter)
              tss = DBLE(NINT(tss + dt/dtmin))

              dts = ts - dts

          ! If the time step is equal to any other time step e.g. crossing wall
          ELSE
              ts  = ts + dt/tseas
              tss = tss + dt/dtmin

              dts = dt/tseas

          END IF

       END IF

       ! Update the values of intrpg and intrpr
       !
       !  t1            ts          t2
       !  |--------------x-----------|
       !   <---intrpg---> <-intrpr-->
       !

       intrpg = MOD(ts,1.d0)
       intrpr = 1.d0-intrpg

     END SUBROUTINE update_time

END MODULE mod_clock
