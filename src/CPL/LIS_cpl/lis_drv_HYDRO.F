
!2345678
       subroutine lis_drv_HYDRO(n)
          use module_lis_HYDRO, only: lis_cpl_HYDRO
          implicit none
          integer n
#ifdef HYDRO_D
          write(6,*) "calling lis_cpl_HYDRO "
#endif
!         stop 888
          call lis_cpl_HYDRO(n)
       end subroutine lis_drv_HYDRO
