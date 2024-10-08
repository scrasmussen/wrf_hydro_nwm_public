
!2345678

       subroutine hrldas_drv_HYDRO(STC_io,SMC_io,SH2OX_io,infxsrt,sfcheadrt,soldrain,ii,jj,kk)
          use module_hrldas_HYDRO, only: hrldas_cpl_HYDRO
          implicit none
          integer:: ii,jj,kk, k

          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,kk,jj) :: STC_io,SMC_io,SH2OX_io
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain

          do k = 1, kk
              STC(:,:,k) = STC_io(:,k,:)
              SMC(:,:,k) = SMC_io(:,k,:)
              SH2OX(:,:,k) = SH2OX_io(:,k,:)
          end do

          call hrldas_cpl_HYDRO(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk)

          do k = 1, kk
               STC_io(:,k,:) = STC(:,:,k)
               SMC_io(:,k,:) = SMC(:,:,k)
               SH2OX_io(:,k,:) = SH2OX(:,:,k)
          end do

       end subroutine hrldas_drv_HYDRO

       subroutine hrldas_drv_HYDRO_ini(STC_io,SMC_io,SH2OX_io,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt,dt,olddate,zsoil)
          use module_hrldas_HYDRO, only: hrldas_cpl_HYDRO_ini, open_print_mpp
          implicit none
          integer:: ii,jj,kk, kt, k
          character(len=*) :: olddate
          real :: dt
          real,dimension(ii,jj,kk) :: STC,SMC,SH2OX
          real,dimension(ii,kk,jj) :: STC_io,SMC_io,SH2OX_io
          real,dimension(ii,jj) ::infxsrt,sfcheadrt, soldrain
          real, dimension(kk) :: zsoil
          do k = 1, kk
              STC(:,:,k) = STC_io(:,k,:)
              SMC(:,:,k) = SMC_io(:,k,:)
              SH2OX(:,:,k) = SH2OX_io(:,k,:)
          end do
             call hrldas_cpl_HYDRO_ini(STC,SMC,SH2OX,infxsrt,sfcheadrt,soldrain,ii,jj,kk,kt,dt,olddate,zsoil)

           do k = 1, kk
               STC_io(:,k,:) = STC(:,:,k)
               SMC_io(:,k,:) = SMC(:,:,k)
               SH2OX_io(:,k,:) = SH2OX(:,:,k)
           end do

             call open_print_mpp(6)
       end subroutine hrldas_drv_HYDRO_ini

       subroutine HYDRO_frocing_drv (indir,forc_typ, snow_assim,olddate,     &
          ixs, ixe,jxs,jxe,                       &
          forcing_name_T,forcing_name_Q,forcing_name_U,forcing_name_V,forcing_name_P, &
          forcing_name_LW,forcing_name_SW,forcing_name_PR,forcing_name_SN, forcing_name_LF,&
          T2,Q2X,U,V,PRES,XLONG,SHORT,PRCP1,lai,SNOWBL,fpar,snodep, kt, FORCING_TIMESTEP,pcp_old)

         use module_lsm_forcing, only: read_hydro_forcing
         use config_base, only: nlst
         implicit none
         integer did, ixs,ixe,jxs,jxe
         integer ix,jx, kt
         character(len=19) :: olddate
         character(len=*) :: indir
         character(len=256), intent(in)  :: forcing_name_T
         character(len=256), intent(in)  :: forcing_name_Q
         character(len=256), intent(in)  :: forcing_name_U
         character(len=256), intent(in)  :: forcing_name_V
         character(len=256), intent(in)  :: forcing_name_P
         character(len=256), intent(in)  :: forcing_name_LW
         character(len=256), intent(in)  :: forcing_name_SW
         character(len=256), intent(in)  :: forcing_name_PR
         character(len=256), intent(in)  :: forcing_name_SN
         character(len=256), intent(in)  :: forcing_name_LF
         real, dimension(ixs:ixe,jxs:jxe):: T2,Q2X,U,V,PRES,XLONG,SHORT,PRCP1, &
                 lai, snowbl, fpar, snodep, pcp_old
         integer :: forc_typ, snow_assim, FORCING_TIMESTEP

         ix = ixe-ixs+1
         jx = jxe-jxs+1
         did = 1

         call read_hydro_forcing( &
            indir, olddate, &
            nlst(did)%hgrid,&
            ix,jx,forc_typ,snow_assim,  &
            forcing_name_T,forcing_name_Q,forcing_name_U,forcing_name_V,forcing_name_P, &
            forcing_name_LW,forcing_name_SW,forcing_name_PR,forcing_name_SN, forcing_name_LF,&
            T2,q2x,u,v,pres,xlong,short,prcp1,&
            lai,snowbl,fpar,snodep,FORCING_TIMESTEP*1.0,kt, pcp_old)
       end subroutine HYDRO_frocing_drv

       subroutine get_greenfrac(inFile,greenfrac, idim, jdim, olddate,SHDMAX)
         use config_base, only: nlst
          implicit none
          character(len=*) :: olddate, inFile
          integer :: idim, jdim
          real, dimension(idim,jdim),intent(out):: greenfrac
          real, dimension(idim,jdim),intent(out):: SHDMAX
          integer:: mm, dd, did
          integer :: months
          months = 12
          did = 1
          read(olddate(6:7),*) mm
          read(olddate(9:10),*) dd
	  !call get_greenfrac_netcdf(inFile,greenfrac,idim, jdim, months,mm,dd)
	  !SHDMAX = maxval(greenfrac)
	  !AD_CHANGE: Old get_greenfrac routine was grabbing max across spatial domain.
	  !	      Updated to new routine that grabs maximum for each cell across time series.
	  call get_maxgreenfrac_netcdf(inFile,greenfrac,idim, jdim, months)
          SHDMAX = greenfrac
       end subroutine get_greenfrac

#ifdef MPP_LAND
       subroutine get_greenfrac_mpp(inFile,greenfrac, idim, jdim, olddate,SHDMAX)
          use module_mpp_land, only:global_nx, global_ny, my_id, io_id, decompose_data_real
          implicit none
          character(len=*) :: olddate, inFile
          integer, intent(in) :: idim, jdim
          real, dimension(idim,jdim), intent(out):: greenfrac,SHDMAX
          real, dimension(global_nx,global_ny):: ggreenfrac,gSHDMAX
          if(my_id .eq. io_id) then
                call get_greenfrac(inFile,ggreenfrac, global_nx, global_ny, olddate,gSHDMAX)
          endif
          call decompose_data_real( ggreenfrac, greenfrac)
          call decompose_data_real( gSHDMAX, SHDMAX)
       end subroutine get_greenfrac_mpp
#endif

      subroutine get_greenfrac_netcdf(fileName,array3, idim, jdim, ldim,mm,dd)
          use netcdf
          use module_hydro_stop, only:HYDRO_stop
          implicit none
          character(len=*) :: fileName
          integer, intent(in) :: mm,dd
          integer, intent(in) :: idim, jdim, ldim
          real, dimension(idim,jdim) :: array
          real, dimension(idim,jdim) :: array2
          real, dimension(idim,jdim) :: diff
          real, dimension(idim,jdim), intent(out) :: array3
          integer :: iret, varid, ncid
          real, dimension(idim,jdim,ldim) :: xtmp
          integer, dimension(1) :: mp
          integer :: i, j, mm2,daytot
          real :: ddfrac
          character(len=24), parameter :: name = "GREENFRAC"

!          units = "fraction"

          iret = nf90_open(trim(fileName), NF90_NOWRITE, ncid)
          if(iret /= 0) then
              write(6,*) "FATAL ERROR: failed to open file in get_greenfrac: ", trim(fileName)
              call hydro_stop("In hrldas_drv_HYDRO.F get_greenfrac_netcdf() - "// &
                              "Failed to open file.")
          endif

          iret = nf90_inq_varid(ncid,  trim(name),  varid)
          if (iret /= 0) then
             print*, 'name = "', trim(name)//'"'
             print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf90_inq_varid"
             call hydro_stop("In hrldas_drv_HYDRO.F get_greenfrac_netcdf() - "// &
                             "Failed to call nf90_inq_varid.")
          endif

          iret = nf90_get_var(ncid, varid, xtmp)
          if (iret /= 0) then

             print*, 'name = "', trim(name)//'"'
             print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_greenfrac_netcdf:  nf90_get_var"
             call hydro_stop("In hrldas_drv_HYDRO.F get_greenfrac_netcdf() - "// &
                             "Faile to call nf90_get_var")
          endif


          if (mm.lt.12) then
            mm2 = mm+1
          else
            mm2 = 1
                end if

          !DJG_DES Set up dates for daily interpolation...
                if (mm.eq.1.OR.mm.eq.3.OR.mm.eq.5.OR.mm.eq.7.OR.mm.eq.8.OR.mm.eq.10.OR.mm.eq.12) then
                   daytot = 31
                else if (mm.eq.4.OR.mm.eq.6.OR.mm.eq.9.OR.mm.eq.11) then
                   daytot = 30
                else if (mm.eq.2) then
                   daytot = 28
                end if
                ddfrac = float(dd)/float(daytot)
                if (ddfrac.gt.1.0) ddfrac = 1.0   ! Assumes Feb. 29th change is same as Feb 28th

#ifdef HYDRO_D
               print *,"DJG_DES Made it past netcdf read...month = ",mm,mm2,dd,daytot,ddfrac
#endif

          do i = 1, idim
             do j = 1, jdim
                array(i,j) = xtmp(i,j,mm)   !GREENFRAC in geogrid in units of fraction from month 1
                array2(i,j) = xtmp(i,j,mm2)   !GREENFRAC in geogrid in units of fraction from month 1
                diff(i,j) = array2(i,j) - array(i,j)
                array3(i,j) = array(i,j) + ddfrac * diff(i,j)
             enddo
          enddo
          iret=nf90_close(ncid)
      end subroutine get_greenfrac_netcdf

        subroutine get_maxgreenfrac_netcdf(fileName, array3, idim, jdim, ldim)
          use netcdf
          use module_hydro_stop, only:HYDRO_stop
          implicit none
          character(len=*) :: fileName
          integer, intent(in) :: idim, jdim, ldim
          real, dimension(idim,jdim), intent(out) :: array3
          integer :: iret, varid, ncid
          real, dimension(idim,jdim,ldim) :: xtmp
          character(len=24), parameter :: name = "GREENFRAC"

!          units = "fraction"

          iret = nf90_open(trim(fileName), NF90_NOWRITE, ncid)
          if(iret /= 0) then
              write(6,*) "FATAL ERROR: failed to open file in get_maxgreenfrac: ", trim(fileName)
              call hydro_stop("In hrldas_drv_HYDRO.F get_maxgreenfrac_netcdf() - "// &
                              "Failed to open file.")
          endif

          iret = nf90_inq_varid(ncid,  trim(name),  varid)
          if (iret /= 0) then
             print*, 'name = "', trim(name)//'"'
             print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_maxgreenfrac_netcdf:  nf90_inq_varid"
             call hydro_stop("In hrldas_drv_HYDRO.F get_maxgreenfrac_netcdf() - "// &
                             "Faile to call nf90_inq_varid.")
          endif

          iret = nf90_get_var(ncid, varid, xtmp)
          if (iret /= 0) then

             print*, 'name = "', trim(name)//'"'
             print*, "MODULE_NOAHLSM_HRLDAS_INPUT:  get_maxgreenfrac_netcdf:  nf90_get_var"
             call hydro_stop("In hrldas_drv_HYDRO.F get_maxgreenfrac_netcdf() - "// &
                             "Failed to call lnf90_get_var.")
          endif

          array3 = maxval(xtmp,3)
          iret=nf90_close(ncid)
      end subroutine get_maxgreenfrac_netcdf

      subroutine HYDRO_HRLDAS_ini(fileName,ix,jx, nsoil,smc_io,stc_io,sh2ox_io, cmc, t1, weasd, &
            snodep,lai, fpar, vegtyp,FNDSNOWH)
      ! read the field from wrfinput or output file
          use netcdf
          use module_hydro_stop, only:HYDRO_stop
          implicit none
          character(len=*) fileName
          integer :: ix,jx , nsoil, i, j, k
          integer :: iret, ncid, ierr, varid
          real,dimension(ix,jx,nsoil):: smc,stc,sh2ox
          real,dimension(ix,nsoil,jx):: smc_io,stc_io,sh2ox_io
          real,dimension(ix,jx):: cmc, t1, weasd, snodep, lai, fpar
          integer, dimension(ix,jx) ::    vegtyp
          logical :: FNDSNOWH
!yw          real, dimension(50) :: shdtbl
!yw          data shdtbl /0.1,0.8,0.8,0.8,0.8,0.8,0.8,0.7,0.7,0.5,  &
!yw              0.8,0.7,0.95,0.7,0.8,0,0.6,0.6,0.1, &
!yw              0.6,0.6,0.6,0.3,0,0.5,0,0,           &
!yw              0,0,0,0,0,0,0,0,0,0,0,0,0,          &
!yw              0,0,0,0,0,0,0,0,0,0/


          iret = nf90_open(trim(fileName), NF90_NOWRITE, ncid)
          if(iret /= 0) then
              write(6,*) "FATAL ERROR: failed to open file in HYDRO_HRLDAS_ini: ", trim(fileName)
              call hydro_stop("In hrldas_drv_HYDRO.F HYDRO_HRLDAS_ini() - "// &
                              "Failed to open wrf input/output file.")
          else
#ifdef HYDRO_D
              write(6,*) "initialization from the file  : ", trim(fileName)

#endif
          endif

           iret = nf90_inq_varid(ncid,"VEGFRA",  varid)
           if (iret == 0) then
              iret = nf90_get_var(ncid, varid, fpar)
              if(maxval(fpar) .gt. 10 .and. (maxval(fpar) .lt. 1000) ) fpar = fpar/100.
           endif


          iret = nf90_inq_varid(ncid,"LAI",  varid)
          if (iret == 0) iret = nf90_get_var(ncid, varid, lai)

          iret = nf90_inq_varid(ncid,"SNOWH",  varid)
          if (iret == 0) then
#ifdef HYDRO_D
             print*, "read snowh for initialization ...."
#endif
             iret = nf90_get_var(ncid, varid, snodep)
             FNDSNOWH = .true.
          else
             FNDSNOWH   = .false.
          endif

          iret = nf90_inq_varid(ncid,"SNOW",  varid)
          if (iret == 0) then
#ifdef HYDRO_D
             print*, "read snow for initialization ...."
#endif
             iret = nf90_get_var(ncid, varid, weasd)
          endif


          iret = nf90_inq_varid(ncid,"TSK",  varid)
          if (iret == 0) iret = nf90_get_var(ncid, varid, t1)

          iret = nf90_inq_varid(ncid,"CANWAT",  varid)
          if (iret == 0) iret = nf90_get_var(ncid, varid, cmc)

          iret = nf90_inq_varid(ncid,"SMOIS",  varid)
          if (iret == 0) iret = nf90_get_var(ncid, varid, smc)

          iret = nf90_inq_varid(ncid,"TSLB",  varid)
          if (iret == 0) iret = nf90_get_var(ncid, varid, stc)

          iret = nf90_inq_varid(ncid,"SH2O",  varid)
          if (iret == 0) iret = nf90_get_var(ncid, varid, sh2ox)

          iret=nf90_close(ncid)

!yw    added for
!yw          where(sh2ox == 0) sh2ox = smc

          do k = 1, nsoil
              STC_io(:,k,:) = STC(:,:,k)
              SMC_io(:,k,:) = SMC(:,:,k)
              SH2OX_io(:,k,:) = SH2OX(:,:,k)
          end do
       end subroutine HYDRO_HRLDAS_ini

#ifdef MPP_LAND
      subroutine HYDRO_HRLDAS_ini_mpp(fileName,ix,jx, nsoil,smc_io,stc_io,sh2ox_io, cmc, t1, weasd, &
                                snodep,lai, fpar, vegtyp,FNDSNOWH)
          use module_mpp_land, only:global_nx, global_ny, my_id, io_id, decompose_data_real, &
                     decompose_data_int, mpp_land_bcast, write_io_real,write_io_int
          implicit none
          character(len=*) fileName
          integer :: ix,jx , nsoil, i, j, k
          integer :: iret, ncid, ierr, varid
          real,dimension(ix,nsoil,jx):: smc_io,stc_io,sh2ox_io
          real,dimension(ix,jx):: cmc, t1, weasd, snodep, lai, fpar
          integer, dimension(ix,jx) ::    vegtyp
          logical :: FNDSNOWH

          real,dimension(global_nx,nsoil,global_ny):: gsmc_io,gstc_io,gsh2ox_io
          real,dimension(global_nx,global_ny):: gcmc, gt1, gweasd, gsnodep, glai, gfpar
          integer, dimension(global_nx,global_ny) ::    gvegtyp

          call write_io_real(snodep,gsnodep)
          call write_io_real(lai,   glai   )
          call write_io_real(fpar, gfpar  )
          call write_io_int(vegtyp, gvegtyp)
          call write_io_real(cmc, gcmc  )
          call write_io_real(t1, gt1  )
          call write_io_real(weasd, gweasd  )

          if(my_id .eq. io_id) then
          call HYDRO_HRLDAS_ini(fileName,global_nx,global_ny, nsoil,gsmc_io,gstc_io,gsh2ox_io, gcmc, gt1, gweasd, &
               gsnodep,glai, gfpar, gvegtyp,FNDSNOWH)
          endif


          call mpp_land_bcast(FNDSNOWH)
          do k = 1, nsoil
              call decompose_data_real(gsmc_io(:,k,:),smc_io(:,k,:))
              call decompose_data_real(gstc_io(:,k,:),stc_io(:,k,:))
              call decompose_data_real(gsh2ox_io(:,k,:),sh2ox_io(:,k,:))
          end do

          call decompose_data_real(gcmc, cmc)
          call decompose_data_real(gt1, t1)
          call decompose_data_real(gweasd, weasd)
          call decompose_data_real(gsnodep, snodep)
          call decompose_data_real(glai, lai)
          call decompose_data_real(gfpar, fpar)
          call decompose_data_int(gvegtyp,vegtyp)

      end subroutine HYDRO_HRLDAS_ini_mpp
#endif
      subroutine get_iocflag(did, iocflag)
        use config_base, only: nlst
         implicit none
         integer :: did, iocflag
         iocflag = nlst(did)%io_config_outputs
      end subroutine get_iocflag

      subroutine get_rstrt_swc(did, restart_flag)
         use config_base, only: nlst
         implicit none
         integer, intent(in)  :: did
         integer, intent(out) :: restart_flag
         restart_flag = nlst(did)%RSTRT_SWC
      end subroutine

      subroutine get_t0OutputFlag(did, t0OutputFlag)
        use config_base, only: nlst
         implicit none
         integer :: did, t0OutputFlag
         t0OutputFlag = nlst(did)%t0OutputFlag
      end subroutine get_t0OutputFlag


      subroutine getNameList(varName,pVar)
        use config_base, only: nlst
          implicit none
          integer :: pVar
          integer :: did
          character(len=*) :: varName
          did = 1
          if(varName .eq. "GWSPINCYCLES") then
               pVar = nlst(did)%gwSpinCycles
          elseif(varName .eq. "GWBASESWCRT") then
               pVar = nlst(did)%GWBASESWCRT
          elseif(varName .eq. "GWSOILCPL") then
               pVar = nlst(did)%gwsoilcpl
          elseif(varName .eq. "channel_only") then
               pVar = nlst(did)%channel_only
          elseif(varName .eq. "channelBucket_only") then
               pVar = nlst(did)%channelBucket_only
          elseif(varName .eq. "igrid") then
               pVar = nlst(did)%igrid
          else
               pVar = 0
          endif
     end subroutine getNameList

      subroutine updateNameList(varName,pVarIn)
        use config_base, only: nlst
#ifdef MPP_LAND
          use module_mpp_land, only: mpp_land_bcast_int1
#endif
          implicit none
          integer :: did
          character (len=*) :: varName
          integer :: pVar, pVarIn
          pVar = pVarIn
          did = 1
          if(varName .eq. "channel_only") then
#ifdef MPP_LAND
             call mpp_land_bcast_int1(pVar)
#endif
             nlst(did)%channel_only = pVar
          endif
          if(varName .eq. "channelBucket_only") then
#ifdef MPP_LAND
             call mpp_land_bcast_int1(pVar)
#endif
             nlst(did)%channelBucket_only = pVar
          endif

     end subroutine updateNameList
