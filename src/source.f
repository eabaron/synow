
       SUBROUTINE SOURCE (elam,thisline,blueline,ctheta,black)
!   THIS SUBROUTINE CALCULATES THE MEAN INTENSITY ariving at a point
!    in the atmosphere.
!   IT THEN CALCULATES THE SOURCE FUNCTION AND OPTICAL DEPTH OF THIS LINE AT
!   THAT POINT.
       USE Global
       IMPLICIT none
       INTEGER, PARAMETER :: nradstep=10000,nwavestep=1024
       REAL (wp) elam(nwavestep)
       REAL (wp) t(nwavestep,nradstep),s(nwavestep,nradstep)
       REAL (wp) td(nwavestep,nradstep),sd(nwavestep,nradstep)
       INTEGER thisline,blueline,rad,line,lline
       REAL (wp) ctheta(nradstep,21),sourcep,sourceo
       REAL (wp) dr,rm
       REAL (wp) elamx(92,0:5),gfx(92,0:5),chix(92,0:5),taux(92,0:5,nradstep)
       REAL (wp) vphot,vmax
       REAL (wp) tbb
       REAL (wp) ea,eb
       REAL (wp) taumin,zeta,black(10000),wk,wk2,u
       LOGICAL flambda
       INTEGER grid,jrlim,nlam,krm,klam,iwk,step
       REAL (wp) in,drm,factor,tav,sav,stspec,delta_v
       INTEGER an(50),ai(50),numref

       include "param.inc"
       include "radial.inc"
       IF (thisline.lt.blueline) THEN
          IWK=thisline+nwavestep
       ELSE
          IWK=thisline
       ENDIF
       factor=(exp(1./tbb/ea)-1.)*(ea/elam(thisline))*3
!  CALCULATE J FOR FIRST GRID LOCATION.
!  ASSUME THAT THE PHOTOSPHERE CONTRIBUTES .5 of the blackbody
!  SO WE ONLY HAVE TO WORRY ABOUT THE OUTSIDE STUFF.
       rad=grid+1
       IF ((t(thisline,rad)+t(thisline,rad+1)+t(thisline,rad-1))&
             .gt.taumin) THEN  
          klam=INT(log(elam(thisline)/ea)/log(eb/ea)*(nlam-1))+1
          sourcep=.5*black(klam)*zeta*zeta
          sourceo= 0.0
          wk=REAL(rad-1)/REAL(grid) 
          IF (blueline.lt.IWK)  THEN
            DO  step=11,20
!  
!  Modify the following line for toplighting.
!  
             in=0.0 
             DO line=blueline,IWK-1
              lline=MOD(line-1,nwavestep)+1
              dr=(elam(thisline)-elam(lline))/elam(thisline)*3.0e5/vphot
              rm=SQRT(wk*wk+dr*dr-2.*dr*wk*ctheta(rad,step))
              IF (rm.lt.vmax/vphot) THEN
                krm=INT(rm*REAL(grid))+1
                drm=rm*REAL(grid)-REAL(krm-1)
                tav=t(lline,krm)+drm*td(lline,krm)
                sav=s(lline,krm)+drm*sd(lline,krm)
                in=in*exp(-tav)+sav*(1.-exp(-tav))&
                   *(elam(lline)/elam(thisline))**3
              ENDIF
             ENDDO
             sourceo=sourceo+in
            ENDDO
            sourceo=sourceo/10.*.5
          ENDIF
 
!  NOW CALCULATE S,T FOR FIRST GRID LOCATION
          s(thisline,rad)=sourceo+sourcep
          t(thisline,rad)=t(thisline,rad)&
                          *(1.-1./(factor/s(thisline,rad)+1.))
        ELSE
          t(thisline,rad)=0.0
          s(thisline,rad)=0.0
        ENDIF

!  NOW CALCULATE J FOR THE REST OF THE GRID LOCATIONS
       DO rad=grid+2,jrlim
         IF (t(thisline,rad)+t(thisline,rad+1).gt.taumin) THEN
           sourcep= 0.0
           sourceo= 0.0
           in= 0.0
           DO step=1,10
             wk=REAL(rad-1)/REAL(grid)
!  wk is the radius we are at
!  u is cos theta for this ray
             u=ctheta(rad,step)
             wk2=wk*u-SQRT(wk*wk*(u*u-1.)+1.)
             klam=INT(log(elam(thisline)*(1.-vphot*wk2/3.0e5)/ea)/&
                  log(eb/ea)*REAL(nlam-1))+1
             klam=MAX(klam,1)
             in=black(klam)*(1.-vphot*wk2/3.0e5)**3*zeta*zeta
             IF (blueline.lt.IWK) THEN
               DO line=blueline,IWK-1
                lline=MOD(line-1,nwavestep)+1
                dr=(elam(thisline)-elam(lline))/elam(thisline)&
                   *3.0e5/vphot
                rm=SQRT(wk*wk+dr*dr-2.*dr*wk*u)
                IF (rm.gt.1.and.u*dr.lt.wk) THEN
                  krm=INT(rm*REAL(grid))+1
                  if(krm .gt. nradstep) then
                   write(0,*)krm,nradstep
                   stop 'nradstep too small in source.f'
                  endif
                  drm=rm*REAL(grid)-REAL(krm-1)
                  tav=t(lline,krm)+drm*td(lline,krm)
                  sav=s(lline,krm)+drm*sd(lline,krm)
                  in=in*exp(-tav)+sav*(1.-exp(-tav))&
                     *(elam(lline)/elam(thisline))**3
                ENDIF
               ENDDO
             ENDIF
             sourcep=sourcep+in
           ENDDO
! NOW WE TAKE THE AVERAGE RAY AND MULTIPLY IT BY THE FRACTION OF SOLID ANGLE
! WHICH THE PHOTOSPHERE SUBTENDS
           sourcep=sourcep/10.*ctheta(rad,21)
! NOW WE MUST DO THE INTERACTION TERM FOR LINES AWAY FROM THE PHOTOSPHERE
           IF (blueline.lt.IWK) THEN  
             DO step=11,20
!  
!  Modify the following line for toplighting.
!  
               in=0.0
               DO line=blueline,IWK-1
                lline=MOD(line-1,nwavestep)+1
                dr=(elam(thisline)-elam(lline))/elam(thisline)&
                   *3.0e5/vphot
                rm=SQRT(wk*wk+dr*dr-2.*dr*wk*ctheta(rad,step))
                IF (rm.lt.vmax/vphot) THEN
                   krm=INT(rm*REAL(grid))+1
                   drm=rm*REAL(grid)-REAL(krm-1)
                   tav=t(lline,krm)+drm*td(lline,krm)
                   sav=s(lline,krm)+drm*sd(lline,krm)
                   in=in*exp(-tav)+sav*(1.-exp(-tav))&
                      *(elam(lline)/elam(thisline))**3
                ENDIF
               ENDDO
               sourceo=sourceo+in
             ENDDO
             sourceo=sourceo/10.*(1.-ctheta(rad,21))
           ENDIF
           s(thisline,rad)=sourceo+sourcep
           t(thisline,rad)=t(thisline,rad)&
                         *(1.-1./(factor/s(thisline,rad)+1.))
         ELSE
           s(thisline,rad)=0.0
           t(thisline,rad)=0.0
         ENDIF
           sd(thisline,rad-1)=s(thisline,rad)-s(thisline,rad-1)
           td(thisline,rad-1)=t(thisline,rad)-t(thisline,rad-1)

        ENDDO
!
! PRINT OUT J and TAU
!     
!       do rad = grid + 1, jrlim
!        wk = real( rad - 1 ) / real( grid ) * vphot
!        write( 100000 + thisline, * )
!    &         wk,s(thisline,rad),t(thisline,rad)
!       end do
!
        RETURN
      
        END
