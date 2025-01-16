from netCDF4 import Dataset
import math
import numpy as np
import netcdf_cf_helper as nc_h
import os
import datetime
import pytz

class forcings:
    """Contains all the data to generate forcing"""

    def __init__(self):
        """Put data to default"""
        self.CENTER_LAT = 51.2
        self.CENTER_LON = 2
        self.DOM_L_LAT_M = 2000000 #size of the domain in m (for the latitude)
        self.DOM_L_LON_M = 2000000 #size of the domain in m (for the longitude)

        self.NBR_CELL_LAT = 32
        self.NBR_CELL_LON = 32
        self.NBR_DAY = 5#times :
        self.init_date = 20230101
        self.TMSTPDAY = 12

        self.uo = 0
        self.vo = 0
        self.wo = 0
        self.water_temp = 5
        self.salinity = 34
        #http://www.csgnetwork.com/h2odenscalc.html
        #5-34
        self.density = 1026.908

        self.bath_depth = None
        self.uniform_depth = True
        self.tidal_cycle = []

        self.air_temp = 5+273.15
        self.u_wind = 15
        self.v_wind = 0

        self.dirw = 0
        self.hstt = 1
        self.Tm1 = 5

        self.u_stockes = 0.1
        self.v_stockes = 0

        self.depths = None

    def create_lon_lat_depth_time(self):
        [self.longitudes, self.latitudes] = create_lon_lat_array(self.CENTER_LON, self.CENTER_LAT, self.DOM_L_LON_M,
                                                                    self.DOM_L_LAT_M, self.NBR_CELL_LON, self.NBR_CELL_LAT)
        utc_timezone = pytz.timezone('UTC')
        dt = datetime.datetime.strptime(str(self.init_date), "%Y%m%d")
        dt_utc = utc_timezone.localize(dt)
        self.times = create_time_array(dt_utc.timestamp(), self.NBR_DAY, self.TMSTPDAY)
        if self.depths is None:
            #value of the depths, [m]
            self.depths = [0, 3, 5, 10, 15, 20, 25, 30, 40, 50, 60,75, 100, 125, 150, 175, 200,
                        225, 250, 300, 350, 400, 450, 500, 550, 600, 750, 1000, 2000,
                        3000]
        if self.bath_depth is None:
            self.bath_depth = self.depths[-1]

    def save_forcing(self, path, save_wind = False, save_wave = False):
        self.create_lon_lat_depth_time()
        create_bathymetry(path, self.bath_depth, self.longitudes, self.latitudes)
        create_current(path, self.init_date, self.uo, self.vo, self.wo, self.water_temp, self.salinity, self.density, self.longitudes, 
                        self.latitudes, self.depths, self.times, self.uniform_depth, self.tidal_cycle)
        if save_wind :
            create_wind_forcing(path, self.init_date, self.u_wind, self.v_wind, self.air_temp, self.longitudes, self.latitudes, self.times)
        if save_wave :
            create_wave_forcing(path, self.init_date, self.dirw, self.Tm1, self.hstt, self.u_stockes, self.v_stockes, self.longitudes, self.latitudes, self.times)

def create_lon_lat_array(CENTER_LON, CENTER_LAT, DOM_L_LON_M, DOM_L_LAT_M, NBR_CELL_LON, NBR_CELL_LAT):
    """Create array for longitude and latitude from center of domain"""
    EARTH_RADIUS = 6371000 #m

    deg_lat_m = 360/(EARTH_RADIUS * math.pi * 2) #deg/m
    dom_l_lat_deg = deg_lat_m * DOM_L_LAT_M
    latitudes = np.linspace(CENTER_LAT - dom_l_lat_deg / 2,
                                CENTER_LAT + dom_l_lat_deg / 2, NBR_CELL_LAT)

    deg_lon_m = 360 / (EARTH_RADIUS * math.cos(latitudes[-1] / 180 * math.pi) * math.pi * 2)
    dom_l_lon_deg = deg_lon_m * DOM_L_LON_M
    longitudes = np.linspace(CENTER_LON - dom_l_lon_deg / 2,
                                CENTER_LON + dom_l_lon_deg / 2, NBR_CELL_LON)
    return [longitudes, latitudes]

def create_time_array(INIT_TIME, NBR_DAY, TMSTPDAY):
    """Create array of time"""
    times = []
    for i in range(NBR_DAY*TMSTPDAY):
        times.append(INIT_TIME + i * (3600*24/TMSTPDAY))
    return times

def create_bathymetry(path, bath_depth, longitudes, latitudes):
    """Create a bathymetry of uniform depth"""
    nc_file_h = nc_h.netcdf_file( f'{path}/bathymetry.nc',
                                    "bathymetry",
                                    longitudes,
                                    latitudes)
    nc_file_h.add_variable_lon_lat('deptho',np.full((len(longitudes), len(latitudes)), bath_depth))
    nc_file_h.add_info_var('deptho',
                         "Bathymetry",
                         "sea_floor_depth_below_geoid",
                         "m")
    nc_file_h.close()

def create_current(path, date, uo, vo, wo, water_temp, salinity, density, longitudes, latitudes, depths, times, uniform_depth, tidal_cycle):
    """
    Create an hydrodynamic forcing, if uniform_depth is not True, then create it for sigma coordinate
    current can be 
    -scalar
    -array for each depth
    -array for each time in an array for each depth

    tidal_cycle is the depth (will bbe mutliplied by the max depth), only for non uniform depth

    """

    if tidal_cycle == [] and not uniform_depth:
        tidal_cycle = [1] * len(times)

    nc_file_h = nc_h.netcdf_file( f'{path}/hydro_{date}.nc',
                                    "hydro",
                                    longitudes,
                                    latitudes)
    nc_file_h.add_time(times)
    max_depth = depths[-1]
    if uniform_depth:
        nc_file_h.add_depth(depths)
    else:
        sigma = []
        for depth in depths:
            sigma.append(depth/max_depth)
        nc_file_h.add_depth(sigma)

    array_uo = np.zeros((len(depths),len(longitudes),len(latitudes), len(times)))

    if isinstance(uo, list):
        if isinstance(uo[0], list):#time and
            for k in range(len(times)):
                uo[k] = uo[k] + [uo[k][-1]] * (len(depths) - len(uo[k]))
                for i in range(len(longitudes)):
                    for j in range(len(latitudes)):
                        array_uo[:,i,j,k] = uo[k]
        else:#constant in time
            uo = uo + [uo[-1]] * (len(depths) - len(uo))
            array_uo[:] = np.array(uo)[:, np.newaxis, np.newaxis, np.newaxis]
    else:
        array_uo[:,:,:,:] = uo

    array_vo = np.zeros((len(depths),len(longitudes),len(latitudes), len(times)))
    if isinstance(vo, list):
        if isinstance(vo[0], list):#time and
            for k in range(len(times)):
                vo[k] = vo[k] + [vo[k][-1]] * (len(depths) - len(vo[k]))
                for i in range(len(longitudes)):
                    for j in range(len(latitudes)):
                        array_vo[:,i,j,k] = vo[k]
        else:#constant in time
            vo = vo + [vo[-1]] * (len(depths) - len(vo))
            array_vo[:] = np.array(vo)[:, np.newaxis, np.newaxis, np.newaxis]
    else:
        array_vo[:,:,:,:] = vo
    
    array_wo = np.zeros((len(depths),len(longitudes),len(latitudes), len(times)))
    if isinstance(wo, list):
        if isinstance(wo[0], list):#time and
            for k in range(len(times)):
                wo[k] = wo[k] + [wo[k][-1]] * (len(depths) - len(wo[k]))
                for i in range(len(longitudes)):
                    for j in range(len(latitudes)):
                        array_wo[:,i,j,k] = wo[k]
        else:#constant in time
            wo = wo + [wo[-1]] * (len(depths) - len(wo))
            array_wo[:] = np.array(wo)[:, np.newaxis, np.newaxis, np.newaxis]
    else:
        array_wo[:,:,:,:] = wo

    nc_file_h.add_variable_depth_lon_lat_time('uo',array_uo)
    nc_file_h.add_info_var('uo',
                         "eastward_sea_water_velocity",
                         "Eastward Current Velocity in the Water Column",
                         "m s-1")

    nc_file_h.add_variable_depth_lon_lat_time('vo',array_vo)
    nc_file_h.add_info_var('vo',
                         "northward_sea_water_velocity",
                         "Northward Current Velocity in the Water Column",
                         "m s-1")

    nc_file_h.add_variable_depth_lon_lat_time('wo',array_wo)
    nc_file_h.add_info_var('wo',
                         "vertical_sea_water_velocity",
                         "Vertical Current Velocity in the Water Column",
                         "m s-1")

    nc_file_h.add_variable_depth_lon_lat_time('thetao',np.full((len(depths),len(longitudes),
                                                    len(latitudes), len(times)), water_temp))
    nc_file_h.add_info_var('thetao',
                         "sea_water_potential_temperature",
                         "Sea Water Potential Temperature",
                         "degrees_C")

    nc_file_h.add_variable_depth_lon_lat_time('so',np.full((len(depths),len(longitudes),
                                                    len(latitudes), len(times)), salinity))
    nc_file_h.add_info_var('so',
                         "sea_water_salinity",
                         "Sea Water Salinity",
                         "1e-3")

    nc_file_h.add_variable_depth_lon_lat_time('density',np.full((len(depths),len(longitudes),
                                                    len(latitudes), len(times)), density))
    nc_file_h.add_info_var('density',
                         "sea_water_density",
                         "sea water density",
                         "kg m-3")
    if not uniform_depth:
            depth_tot = np.zeros((len(longitudes),len(latitudes),len(times)))
            for i in range(len(times)):
                depth_tot[:, :, i] = tidal_cycle[i] * max_depth
            nc_file_h.add_variable_lon_lat_time('depth_tot',depth_tot)
            nc_file_h.add_info_var('depth_tot',
                         " ",
                         "depth of the water column",
                         "m")
    nc_file_h.close()

def create_wind_forcing(path,date, u_wind, v_wind, air_temp, longitudes, latitudes, times):
    """Create wind forcing"""
    nc_file_h = nc_h.netcdf_file( f'{path}/wind_{date}.nc',
                                    "wind",
                                    longitudes,
                                    latitudes)
    nc_file_h.add_time(times)
    nc_file_h.add_variable_lon_lat_time('10u',np.full((len(longitudes),len(latitudes),
                                                            len(times)), u_wind))
    nc_file_h.add_info_var('10u',
                         "eastward_wind_velocity_10_meters",
                         "eastward wind velocity at 10 meters",
                         "m s-1")
    nc_file_h.add_variable_lon_lat_time('10v',np.full((len(longitudes),len(latitudes),
                                                            len(times)), v_wind))
    nc_file_h.add_info_var('10v',
                         "northward_wind_velocity_10_meters",
                         "northward wind velocity at 10 meters",
                         "m s-1")

    nc_file_h.add_variable_lon_lat_time('2t',np.full((len(longitudes),len(latitudes),
                                                            len(times)), air_temp))
    nc_file_h.add_info_var('2t',
                         "2_meters_air_temperature",
                         "2 meters air temperature",
                         "K")

    nc_file_h.close()


def create_wave_forcing(path,date, dirw, Tm1, hstt, u_stockes, v_stockes, longitudes, latitudes, times):
    nc_file_h = nc_h.netcdf_file( f'{path}/waves_{date}.nc',
                                "wave",
                                longitudes,
                                latitudes)
    nc_file_h.add_time(times)

    nc_file_h.add_variable_lon_lat_time('hstt',np.full((len(longitudes),len(latitudes),
                                                            len(times)), hstt))
    nc_file_h.add_info_var('hstt',
                         "sea_surface_wave_significant_height",
                         "Spectral significant wave height (Hm0)",
                         "m")

    nc_file_h.add_variable_lon_lat_time('Tm1',np.full((len(longitudes),len(latitudes),
                                                            len(times)), Tm1))
    nc_file_h.add_info_var('Tm1',
                         "sea_surface_wave_mean_period_from_variance_spectral_density_inverse_frequency_moment",
                         "Spectral moments(-1,0)wave period(Tm-10)",
                         "s")
    nc_file_h.add_variable_lon_lat_time('dirw',np.full((len(longitudes),len(latitudes),
                                                            len(times)), dirw))
    nc_file_h.add_info_var('dirw',
                         "sea_surface_wave_from_direction",
                         "Mean wave direction from(Mdir)",
                         "degree")
    
    nc_file_h.add_variable_lon_lat_time('u_stockes',np.full((len(longitudes),len(latitudes),
                                                            len(times)), u_stockes))
    nc_file_h.add_info_var('u_stockes',
                         "sea_surface_wave_stokes_drift_x_velocity",
                         "Stokes drift U",
                         "m s-1")
    nc_file_h.add_variable_lon_lat_time('v_stockes',np.full((len(longitudes),len(latitudes),
                                                            len(times)), v_stockes))
    nc_file_h.add_info_var('v_stockes',
                         "sea_surface_wave_stokes_drift_y_velocity",
                         "Stokes drift V",
                         "m s-1")


    nc_file_h.close()