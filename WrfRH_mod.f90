!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  WrfRH_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Contains code used to calculating relative humidity w.r.t. liquid based on
! approach used in WRF.
!
!
!
!------------------------------------------------------------------------------

module WrfRH_mod

   implicit none
   private

   public :: calcSatSpecificHumidity
   public :: calcMixingRatio

   ! Private constants used in Teten's formula
   real, parameter :: SVP1        = 0.6112
   real, parameter :: SVP2        = 17.67
   real, parameter :: SVP3        = 29.65
   real, parameter :: SVPT0       = 273.15

   ! Molecular weights of water vapor and dry air
   real, parameter :: MW_VAP = 18.0152
   real, parameter :: MW_AIR = 28.966

contains

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calcSatSpecificHumidity
   !
   ! DESCRIPTION:  Public function for calculating saturated specific
   ! humidity w.r.t. liquid.  Based on formula in WRF REAL program.
   !
   !---------------------------------------------------------------------------

   function calcSatSpecificHumidity(temperatureK, pressurePa) &
        result (satSpecificHumidity)

      ! Arguments
      real,intent(in) :: temperatureK
      real,intent(in) :: pressurePa
      real :: satSpecificHumidity

      ! Local variables
      real :: satVaporPressurePa

      ! Calculate saturation vapor pressure (Pa)
      satVaporPressurePa = 100.*SVP1*10.* &
           exp(SVP2*(temperatureK-SVPT0)/(temperatureK-SVP3))

      ! Calculate saturation specific humidity
      satSpecificHumidity = MW_VAP * satVaporPressurePa
      satSpecificHumidity = satSpecificHumidity / &
           (satSpecificHumidity + MW_AIR * (pressurePa - satVaporPressurePa))

      return
   end function calcSatSpecificHumidity

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calcMixingRatio
   !
   ! DESCRIPTION:  Public function for calculating water vapor mixing ratio
   ! from specific humidity.
   !
   !---------------------------------------------------------------------------

   function calcMixingRatio(specificHumidity) result (mixingRatio)

      ! Arguments
      real,intent(in) :: specificHumidity

      ! Return variable
      real :: mixingRatio

      ! Conversion formula
      mixingRatio = specificHumidity / (1. - specificHumidity)

      return
   end function calcMixingRatio
end module WrfRH_mod
