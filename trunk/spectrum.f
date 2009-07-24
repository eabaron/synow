      SUBROUTINE SPECTRUM(elam,thisline,blueline,xplot,black,&
       jrlim,grid,last,vmax,vphot,flambda,nlam,zeta,ea,eb)
      USE Global
      IMPLICIT none
      INTEGER, PARAMETER ::nradstep=350,nwavestep=1024&
         ,iacc=2
      REAL (wp), PARAMETER :: acc=2.0d0
      REAL (wp) elam(nwavestep),xplot(10000),black(10000)
      INTEGER thisline,blueline,jrlim,grid,IWK,last,nlam,jp,klam
      REAL (wp) s(nwavestep,nradstep),sd(nwavestep,nradstep),tot,in,p,xp
      REAL (wp) t(nwavestep,nradstep),td(nwavestep,nradstep),vmax,vphot
      REAL (wp) ea,eb,zeta,z,wk,rm,tav,sav,drm
      INTEGER lline,krm,ii
      LOGICAL flambda
      include "radial.inc"
      
      
      IWK=thisline
  1   IF (elam(thisline)/xplot(last+1).gt.1.+vmax/3.0e5) THEN
!       NOW WE CAN CALCULATE THE SPECTRUM A LITTLE 
        last=last+1
        tot=0.
        DO jp=iacc,jrlim*iacc
!
!  Modify the following line for toplighting.
!
          in=0.0
          p=REAL(jp-acc)/REAL(grid)/acc
          IF (p.lt.1.) THEN
            xp=(1.-vphot/3.0e5*SQRT(1.-p*p))
            klam=INT(LOG(xplot(last)/xp/ea)/LOG(eb/ea)&
        *(REAL(nlam)-1.))+1
            klam=MIN(klam,nlam)
            in=black(klam)*zeta*zeta/xp/xp/xp
          ENDIF
          IF (blueline.lt.IWK) THEN
            DO lline=blueline,IWK
!              lline=MOD(line-1,nwavestep)+1
!     FOR EACH LINE LOCATE IT IN Z
              z=(elam(lline)-xplot(last))/xplot(last)*3.0e5/vphot
              IF (z.gt.0.0.or.p.gt.1.) THEN 
                  rm=SQRT(z*z+p*p)
                  IF (rm.gt.1.and.rm.lt.vmax/vphot) THEN
                    krm=INT(rm*REAL(grid))+1
                    drm=rm*REAL(grid)-REAL(krm-1)
                    tav=t(lline,krm)+drm*td(lline,krm)
                    sav=s(lline,krm)+drm*sd(lline,krm)
                    in=in*exp(-tav)+sav*(1.-exp(-tav))&
                       *(elam(lline)/xplot(last))**3
                  ENDIF
               ENDIF
             ENDDO
           ENDIF
!
!  Modify the following line for toplighting.  Add term from the front.
!
           tot=tot+in*p
           in=0.
         ENDDO
        IF (flambda) tot=tot*(ea/xplot(last))**2

        WRITE (11, *) xplot(last),tot*2./acc/REAL(grid),black(last)

!        CALL FLUSH(11)
        wk=2.0*SQRT(vmax*vmax-vphot*vphot)/3.0e5
        wk=xplot(last)*(1.-wk)/(1.+wk)
        DO ii=blueline,IWK-1
          IF (elam(ii).lt.wk) THEN
            blueline=ii+1
          ENDIF
        ENDDO

        IF (last.eq.nlam) STOP           
        GOTO 1
        ENDIF
        RETURN
        END
