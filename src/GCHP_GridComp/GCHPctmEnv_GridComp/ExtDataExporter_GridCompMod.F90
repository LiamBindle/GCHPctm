#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!         NASA/GSFC, Software Systems Support Office, Code 610.3         !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: ExtDataExporter_GridComp -- Prepares derived variables for GEOSctm
!
! !INTERFACE:
!
      module ExtDataExporter_GridComp
!
! !USES:
      use ESMF
      use MAPL_Mod

      implicit none
      private

! !PUBLIC MEMBER FUNCTIONS:

      public SetServices
!
! !DESCRIPTION:
!  This GC is used to produce Mock exports.
! 
!EOP
!-------------------------------------------------------------------------
      integer,  parameter :: r8     = 8
      integer,  parameter :: r4     = 4

      INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,30)
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(14,300)
      INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(18,400)

      real(r8), parameter :: RADIUS = MAPL_RADIUS
      real(r8), parameter :: PI     = MAPL_PI_R8

!-------------------------------------------------------------------------
      CONTAINS
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices -- Sets ESMF services for this component
!
! !INTERFACE:
!
      subroutine SetServices ( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
!
! !OUTPUT PARAMETERS:
      integer, intent(OUT)               :: RC  ! return code
!
! !LOCAL VARIABLES:
      type (ESMF_State)                  :: INTERNAL
!
! !DESCRIPTION:  
!   The SetServices for the CTM Der GC needs to register its
!   Initialize and Run.  It uses the MAPL\_Generic construct for defining 
!   state specs. 
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      integer                    :: STATUS
      integer                    :: I
      type (ESMF_Config)         :: CF
      character(len=ESMF_MAXSTR) :: COMP_NAME
      character(len=ESMF_MAXSTR) :: IAm = 'SetServices'

     ! Get my name and set-up traceback handle
     ! ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
      _VERIFY(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

     ! Register services for this component
     ! ------------------------------------
      call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, __RC__ )
      call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run,        __RC__ )

! !IMPORT STATE:

      call MAPL_AddImportSpec ( gc,                                  &
           SHORT_NAME = 'PS1',                                       &
           LONG_NAME  = 'pressure_at_surface_before_advection',      &
           UNITS      = 'hPa',                                       &
           DIMS       = MAPL_DimsHorzOnly,                           &
           VLOCATION  = MAPL_VLocationEdge,             RC=STATUS  )
      _VERIFY(STATUS)

! Export State
      call MAPL_AddExportSpec(GC,                            &
        SHORT_NAME         = 'PS1_copy',                      &
        LONG_NAME          = 'pressure_at_surface_before_advection',                  &
        UNITS              = 'hPa',                       &
        DIMS               = MAPL_DimsHorzOnly,              &
        VLOCATION          = MAPL_VLocationEdge,  RC=STATUS)
      _VERIFY(STATUS)

      ! Set the Profiling timers
      !-------------------------
      call MAPL_TimerAdd(GC,    name="INITIALIZE"  ,RC=STATUS)
      _VERIFY(STATUS)
      call MAPL_TimerAdd(GC,    name="RUN"         ,RC=STATUS)
      _VERIFY(STATUS)

      ! Create children's gridded components and invoke their SetServices
      ! -----------------------------------------------------------------
      call MAPL_GenericSetServices    ( GC, RC=STATUS )
      _VERIFY(STATUS)

      _RETURN(ESMF_SUCCESS)
  
      end subroutine SetServices
!
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize -- Initialized method for composite the CTMder
!
! !INTERFACE:
!
      subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
      type(ESMF_State),    intent(inout) :: IMPORT ! Import state
      type(ESMF_State),    intent(inout) :: EXPORT ! Export state
      type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
!
! !OUTPUT VARIABLES:
      integer, optional,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: 
!  The Initialize method of the CTM Derived Gridded Component.
!
!EOP
!-------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
      __Iam__('Initialize')
      character(len=ESMF_MAXSTR)    :: COMP_NAME
      REAL, POINTER, DIMENSION(:,:) :: cellArea
      type(ESMF_Grid)               :: esmfGrid
      type (ESMF_VM)                :: VM
      integer                       :: im, jm, km, i
      type(MAPL_MetaComp), pointer  :: ggState      ! GEOS Generic State
      type (ESMF_Config)            :: CF
      integer                       :: dims(3)
      integer :: comm

      !  Get my name and set-up traceback handle
      !  ---------------------------------------
      call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, VM=VM, RC=STATUS )
      _VERIFY(STATUS)
      Iam = TRIM(COMP_NAME)//"::Initialize"

      !  Initialize GEOS Generic
      !  ------------------------
      call MAPL_GenericInitialize ( gc, IMPORT, EXPORT, clock,  RC=STATUS )
      _VERIFY(STATUS)

      !  Get my internal MAPL_Generic state
      !  -----------------------------------
      call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS)
      _VERIFY(STATUS)

      call MAPL_TimerOn(ggSTATE,"TOTAL")
      call MAPL_TimerOn(ggSTATE,"INITIALIZE")

      ! Get the grid related information
      !---------------------------------
      call ESMF_GridCompGet ( GC, GRID=esmfGrid, rc=STATUS)
      _VERIFY(STATUS)

      call MAPL_GridGet ( esmfGrid, globalCellCountPerDim=dims, RC=STATUS)
      _VERIFY(STATUS)

      im = dims(1)
      jm = dims(2)
      km = dims(3)
    
      ! Get the time-step
      ! -----------------------
      !call MAPL_GetResource( ggState, ndt, 'RUN_DT:', default=0, RC=STATUS )
      !_VERIFY(STATUS)
      !dt = ndt

      call MAPL_TimerOff(ggSTATE,"INITIALIZE")
      call MAPL_TimerOff(ggSTATE,"TOTAL")

      _RETURN(ESMF_SUCCESS)

      end subroutine Initialize
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run -- Run method
!
! !INTERFACE:
!
      subroutine Run ( GC, IMPORT, EXPORT, CLOCK, RC )
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
      type(ESMF_State),    intent(inout) :: IMPORT ! Import state
      type(ESMF_State),    intent(inout) :: EXPORT ! Export state
      type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
!
! !OUTPUT PARAMETERS:
      integer, optional,   intent(  out) :: RC     ! Error code
!
! !DESCRIPTION: 
! The Run method of the derived variables CTM Gridded Component.
!
!EOP
!-------------------------------------------------------------------------
!BOC 
!
! !LOCAL VARIABLES:
      character(len=ESMF_MAXSTR)      :: IAm = "Run"
      integer                         :: STATUS
      character(len=ESMF_MAXSTR)      :: COMP_NAME
      type (MAPL_MetaComp), pointer   :: ggState
      type (ESMF_Grid)                :: esmfGrid
      type (ESMF_State)               :: INTERNAL

      ! Imports
      !--------
      real, pointer, dimension(:,:)   ::       PS1_in => null()
      real, pointer, dimension(:,:)   ::       PS1_out => null()

      ! Get the target components name and set-up traceback handle.
      ! -----------------------------------------------------------
      call ESMF_GridCompGet ( GC, name=COMP_NAME, Grid=esmfGrid, RC=STATUS )
      _VERIFY(STATUS)
      Iam = trim(COMP_NAME) // TRIM(Iam)

      ! Get my internal MAPL_Generic state
      !-----------------------------------
      call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS )
      _VERIFY(STATUS)

      call MAPL_TimerOn(ggState,"TOTAL")
      call MAPL_TimerOn(ggState,"RUN")

      ! Get to the imports...
      ! ---------------------
      call MAPL_GetPointer ( IMPORT, PS1_in, 'PS1', RC=STATUS )
      _VERIFY(STATUS)

      ! Get to the exports...
      ! ---------------------
      call MAPL_GetPointer ( EXPORT, PS1_out, 'PS1_copy', RC=STATUS )
      _VERIFY(STATUS)

      PS1_out(:,:) = PS1_in(:,:)

      call MAPL_TimerOff(ggState,"RUN")
      call MAPL_TimerOff(ggState,"TOTAL")

      ! All Done
      ! --------
      _RETURN(ESMF_SUCCESS)

      end subroutine Run
!EOC
!------------------------------------------------------------------------------
!BOP

!EOC
!-----------------------------------------------------------------------
      end module ExtDataExporter_GridComp
