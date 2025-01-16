from netCDF4 import Dataset
import numpy as np

class netcdf_file:

    def __init__(self, path, title, lons, lats):
        """
        Object for creating a standard netcdf cf file.

        params
        -----
        path : the path of the file
        title : the title of the file
        lons : an array with all the longitude for the data
        lats : an array with all the latitude for the data
        """
        self.nc_file = Dataset(path, 'w', format='NETCDF4_CLASSIC')
        self.nc_file.Conventions = "CF-1.8"
        self.nc_file.title = title
        self.nc_file.source = "OSERIT"
        self.nc_file.institution = 'RBINS'
        self.create_dimension('lon', "longitude", "longitude", "degrees_east", lons)
        self.create_dimension('lat', "latitude", "latitude", "degrees_north", lats)


    def add_time(self, times):
        """
        Add the time dimension to the file, times is an array with
        all the times since 1970-1-1 00:00:00
        """
        self.create_dimension('time', 'time', 'time', "seconds since 1970-01-01 00:00:00", times)


    def add_depth(self, depth):
        """
        Add the depth dimension to the file, times is an array with
        the depth in meters
        """
        self.create_dimension('depth', 'depth', 'depth', "m", depth)

    def create_dimension(self, name, std_name, lg_name, units, data):
        """
        Create a dimension and the variable linked to it

        params
        -----
        name is the dimension/variable name
        std_name is the standard_name of the variable
        lg_name is the long_name of the variable
        units is the units of the variable
        data is the array to be put as dimension
        """
        self.nc_file.createDimension(name,len(data))
        self.nc_file.createVariable(name,np.float64, (name,))
        var = self.nc_file.variables[name]
        var.standard_name = std_name
        var.long_name = lg_name
        var.units = units
        self.nc_file.variables[name][:] = data


    def add_info_var(self,name, std_name, lg_name, units, valid_range = [0,1000000000000]):
        """
        Add the attribute to a variable

        params
        -----
        name : the variable name
        std_name : the standard_name of the variable
        lg_name : the long_name of the variable
        units : the units of the variable
        valid_range : the valid_range, by default [0,1000000000000]
        """
        var = self.nc_file.variables[name]
        var.standard_name = std_name
        var.long_name = lg_name
        var.units = units
        var.valid_range = valid_range

    def add_variable_lon_lat(self, name, data, fill_value = 0):
        """
        Add a variable 2d to the file
        name is the variable name
        data is the array(lon,lat) to put in the netcdf file
        fill_value is the fill value, by default 0
        """
        self.nc_file.createVariable(name,np.float64, ('lat','lon'), fill_value = fill_value)
        self.nc_file.variables[name][:,:] = np.transpose(data, (1,0))


    def add_variable_lon_lat_time(self, name, data, fill_value = 0):
        """
        Add a variable 3d with the the time to the file (lon, lat, time)

        params
        -----
        name : the variable name
        data : the array(lon,lat) to put in the netcdf file
        fill_value : the fill value, by default 0
        """
        self.nc_file.createVariable(name,np.float64, ('time','lat','lon'), fill_value = fill_value)
        self.nc_file.variables[name][:,:,:] = np.transpose(data, (2,1,0))

    def add_variable_depth_lon_lat_time(self, name, data, fill_value = 0):
        """
        Add a variable 3d with the the time to the file (depth, lon, lat, time)

        params
        -----
        name : the variable name
        data : the array(lon,lat) to put in the netcdf file
        fill_value : the fill value, by default 0
        """
        self.nc_file.createVariable(name,np.float64, ('time','depth','lat','lon'), fill_value = fill_value)
        self.nc_file.variables[name][:,:,:] = np.transpose(data, (3,0,2,1))

    def add_variable_lon_lat_x(self, name, data, dim, fill_value = 0):
        """
        Add a variable 3d with the a arbitrary dimension to the file (lon, lat dim)

        params
        -----
        name : the variable name
        data : the array(lon,lat) to put in the netcdf file
        dim : the arbitrary dimension
        fill_value : the fill value, by default 0
        """
        self.nc_file.createVariable(name,np.float64, (dim,'lat','lon'), fill_value = fill_value)
        self.nc_file.variables[name][:,:,:] = np.transpose(data, (2,1,0))


    def close(self):
        self.nc_file.close()
