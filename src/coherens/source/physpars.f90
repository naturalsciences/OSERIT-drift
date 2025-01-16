MODULE physpars
!************************************************************************
!
! *physpars* Physical and numerical model parameters
!
! Author - Patrick Luyten
!
! Version - @(COHERENS)physpars.f90  V2.0
!
! $Date: 2011-03-18 14:35:25 +0100 (Fri, 18 Mar 2011) $
!
! $Revision: 188 $
!
! Description - 
!
!************************************************************************
!
USE syspars

IMPLICIT NONE


!---general
REAL :: Rearth = 6371000.0, rho_air = 1.2, specheat = 3987.5

!---reference and minimum values
REAL :: atmpres_ref = 101325.0, beta_sal_ref, beta_temp_ref, density_ref, &
      & dlat_ref = 0.0, dlon_ref = 0.0, dlon_ref_anal = 0.0, &
      & dlon_ref_obc = 0.0, gacc_mean, gacc_ref = real_undef, sal_ref = 33.0, &
      & sst_ref = 12.0, temp_min = 0.0, temp_ref = 12.0

!---diffusion coefficients
REAL :: hdifmom_cst = 0.0, hdifscal_cst = 0.0, smag_coef_mom = 0.1, &
      & smag_coef_scal = 0.1, vdifmom_cst = 1.0E-06, vdifscal_cst = 1.0E-06

!---water depths
REAL :: depmean_cst = 0.0, dcrit_fld = 0.1, dmin_fld = 0.02

!---bottom/surface fluxes
REAL :: bdragcoef_cst = 0.0, bdraglin = 0.0, ccharno = 0.014, &
      & cds_cst = 0.0013, ces_cst = 0.00113, chs_cst = 0.00113, ckar = 0.4, &
      & zref_atm = 10.0, zrough_cst = 0.0

!---open boundary conditions
REAL :: cgravratio = 0.03

!---optical parameters
REAL :: optattcoef1_cst = 10.0, optattcoef2_cst = 0.067, opt_frac = 0.54

!---parameters for exchange coefficients in tabular form
INTEGER :: nrelhum, ntemp, nwind
REAL :: drelhum = 0.05, dtempdif = 1.0, dtempmax = 5.0, dtempmin = -5.0, &
      & dwind = 0.25, relhummax = 1.0, relhummin = 0.5, uwindmax = 50.0, &
      & uwindmin = 3.0 

!---numerical
REAL :: theta_cor = 0.5, theta_vadv = 0.501, theta_vdif = 1.0
REAL, PARAMETER :: eps_adv = 1.0E-12

SAVE

!
! Name            Type    Purpose
!------------------------------------------------------------------------------
!*Rearth*         REAL    Mean radius of the Earth                          [m]
!*rho_air*        REAL    Air density                                  [kg/m^3]
!*specheat*       REAL    Specific heat of seawater at constant pressure
!                                                                  [J/kg/deg C]
!*atmpres_ref*    REAL    Reference atmospheric pressure                [N/m^2]
!*beta_sal_ref*   REAL    Reference value for salinity expansion coefficient
!                                                                      [PSU^-1]
!*beta_temp_ref*  REAL    Reference value for temperature expansion coefficient
!                                                                     [1/deg C]
!*density_ref*    REAL    Reference density                            [kg/m^3]
!*dlat_ref*       REAL    Reference latitude in case of Cartesian grid (decimal
!                         degrees, positive North)
!*dlon_ref*       REAL    Reference longitude in case of Cartesian grid
!                         (decimal degrees, positive East)
!*dlon_ref_anal*  REAL    If iopt_astro_pars>0, harmonically analysed phases
!                         are taken with respect to astronomical argument for
!                         this reference longitude at the central time (decimal
!                         degrees, positive East).
!*dlon_ref_obc*  REAL     If iopt_astro_pars>0, phases at open boundaries are
!                         assumed to be taken with respect to astronomical
!                         argument at this reference value (decimal degrees,
!                         positive East).
!*gacc_mean*      REAL    Domain-averaged acceleration of gravity [m/s^2] 
!*gacc_ref*       REAL    Uniform reference acceleration of gravity (if
!                         flagged, gravity acceleration is computed as
!                         function of latitude)                         [m/s^2]
!*sal_ref*        REAL    Reference salinity                              [PSU]
!*sst_ref*        REAL    Reference sea surface temperature             [deg C]
!*temp_min*       REAL    Minimum allowed value for temperature (if flagged,
!                         minimum temperature is set to freezing point
!                         temperature)                                  [deg C]
!*temp_ref*       REAL    Reference water temperature                   [deg C]
!*hdifmom_cst*    REAL    Uniform horizontal diffusion coefficient for momentum
!                                                                       [m^2/s]
!*hdifscal_cst*   REAL    Uniform horizontal diffusion coefficient for scalars
!                                                                       [m^2/s]
!*smag_coef_mom*  REAL    Smagorinsky coefficient for momentum              [-]
!*smag_coef_scal* REAL    Smagorinsky coefficient for scalars        
!*vdifmom_cst*    REAL    Uniform/background vertical diffusion coefficient
!                         for momentum                                  [m^2/s]
!*vdifscal_cst*   REAL    Uniform/background vertical diffusion coefficient
!                         for scalars                                   [m^2/s]
!*depmean_cst*    REAL    Uniform mean water depth for 1-D applications     [m]
!*dcrit_fld*      REAL    Critical water depth for flooding/drying scheme
!*dmin_fld*       REAL    Minimum water depth for flooding/drying scheme
!*bdragcoef_cst*  REAL    Uniform bottom drag coefficient (iopt_bstres_drag=0)
!*bdraglin*       REAL    Linear bottom drag coefficient (iopt_bstres_form=1)
!                                                                         [m/s]
!*ccharno*        REAL    Charnock's constant
!*cds_cst*        REAL    Uniform surface drag coefficient if iopt_sflux_cds=0
!*ces_cst*        REAL    Uniform exchange coefficient for latent heat if
!                         iopt_sflux_cehs=0
!*chs_cst*        REAL    Uniform exchange coefficient for sensible heat if
!                         iopt_sflux_cehs=0
!*ckar*           REAL    Von Karman's constant
!*zref_atm*       REAL    Reference height for meteorological variables     [m]
!*zrough_cst*     REAL    Uniform bottom roughness length (iopt_bstres_drag=2)
!*cgravratio*     REAL    Ratio of internal to external wave speed (at o.b.)
!*optattcoef1_cst*REAL    Attenuation coefficient for infrared radiation  [1/m]
!*optattcoef2_cst*REAL    Attenuation coefficient for short-wave radiation and
!                         pure sea water                                  [1/m]
!*opt_frac*       REAL    Infrared fraction of irradiance absorbed at sea
!                         surface
!*nrelhum*        INTEGER Number of relative humidities in exchange tables
!*ntemp*          INTEGER Number of temperature differences in exchange tables
!*nwind*          INTEGER Number of wind speeds in exchange tables
!*drelhum*        REAL    Resolution for relative humidity in exchange tables
!*dtempdif*       REAL    Resolution for air minus sea surface temperature in
!                         exchange tables                               [deg C]
!*dtempmax*       REAL    Maximum air minus sea surface temperature difference
!                         in exchange tables                            [deg C]
!*dtempmin*       REAL    Maximum air minus sea surface temperature difference
!                         in exchange tables                            [deg C]
!*dwind*          REAL    Wind resolution in exchange tables              [m/s]
!*relhummax*      REAL    Maximum relative humidity in exchange tables
!*relhummin*      REAL    Minimum relative humidity in exchange tables 
!*uwindmax*       REAL    Maximum wind speed in exchange tables           [m/s]
!*uwindmin*       REAL    Minimum wind speed in exchange tables           [m/s]
!*theta_cor*      REAL    Implicity factor for Coriolis terms
!*theta_vadv*     REAL    Implicity factor for vertical advection
!*theta_vdif*     REAL    Implicity factor for vertical diffusion
!*eps_adv*        REAL    Tolerance factor for calculation of flux ratios
!                         (TVD scheme)
!
!************************************************************************
!

END MODULE physpars
