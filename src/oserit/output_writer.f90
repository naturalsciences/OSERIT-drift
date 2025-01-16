module output_writer

use modstate
use parampars
use partpars
use netcdf
use syspars
use datatypes_ose

implicit none

contains

function create_output_file(filename, params) result(dataf)
    ! create the output file and returns all the var_id for it as an output_file_data
    !filename is a character with the path to the file of output
    !params are the parameters of OSERIT

# ifdef OHNS
    use ohns
# endif

    character(*), intent(in) :: filename
    type(oserit_parameters), intent(in) :: params
    integer :: ncid
    type(output_file_data) :: dataf

    dataf%filename = filename

    call handle_err(nf90_create(dataf%filename,NF90_CLOBBER,ncid))
    
    call handle_err(NF90_def_dim(ncid,"time", NF90_UNLIMITED, dataf%time_dimid))
    call handle_err(NF90_def_dim(ncid,"particles", nopart, dataf%part_dimid))
    call handle_err(NF90_def_dim(ncid,"state", nbr_state, dataf%state_dimid))
    call handle_err(NF90_def_dim(ncid,"string_length", LENGTH_STR, dataf%string_length_dimid))

    !define variable time
    call handle_err(NF90_def_var(ncid, "time", NF90_INT, [dataf%time_dimid], dataf%time_varid))
    call handle_err(NF90_put_att(ncid, dataf%time_varid, 'name',"Time"))
    call handle_err(NF90_put_att(ncid, dataf%time_varid, 'units', "seconds since 1970-01-01 00:00:00"))

    !define variable timestep
    call handle_err(NF90_def_var(ncid, "timestep", NF90_INT, [dataf%time_dimid], dataf%timestep_varid))
    call handle_err(NF90_put_att(ncid, dataf%timestep_varid, 'name',"timestep"))

    !define particle lon
    call handle_err(NF90_def_var(ncid, "lon", NF90_FLOAT, [dataf%part_dimid, dataf%time_dimid], dataf%lon_varid))
    call handle_err(NF90_put_att(ncid, dataf%lon_varid, 'standard_name',"longitude"))
    call handle_err(NF90_put_att(ncid, dataf%lon_varid, 'units', "degree_east"))
    
    !define particle lat
    call handle_err(NF90_def_var(ncid, "lat", NF90_FLOAT, [dataf%part_dimid, dataf%time_dimid], dataf%lat_varid))
    call handle_err(NF90_put_att(ncid, dataf%lat_varid, 'standard_name',"latitude"))
    call handle_err(NF90_put_att(ncid, dataf%lat_varid, 'units', "degree_north"))

    !define particle depth
    call handle_err(NF90_def_var(ncid, "depth", NF90_FLOAT, [dataf%part_dimid, dataf%time_dimid], dataf%depth_varid))
    call handle_err(NF90_put_att(ncid, dataf%depth_varid, 'long_name',"depth_above_sea_surface"))
    call handle_err(NF90_put_att(ncid, dataf%depth_varid, 'units', "m"))

    !define particle state
    call handle_err(NF90_def_var(ncid, "state", NF90_CHAR, [dataf%string_length_dimid, dataf%state_dimid], dataf%state_varid))
    call handle_err(NF90_put_att(ncid, dataf%state_varid, 'long_name',"state"))

    call handle_err(NF90_def_var(ncid, "part_state", NF90_INT, [dataf%part_dimid, dataf%time_dimid], dataf%part_state_varid))
    call handle_err(NF90_put_att(ncid, dataf%part_state_varid, 'long_name',"part_state"))

    !define particle drift_state
    call handle_err(NF90_def_var(ncid, "drift_state", NF90_INT, [dataf%part_dimid, dataf%time_dimid], dataf%drift_state_varid))
    call handle_err(NF90_put_att(ncid, dataf%drift_state_varid, 'long_name',"drift_state"))

    !define particle depth_tot
    call handle_err(NF90_def_var(ncid, "depth_tot", NF90_FLOAT, [dataf%part_dimid, dataf%time_dimid], dataf%depth_tot_varid))
    call handle_err(NF90_put_att(ncid, dataf%depth_tot_varid, 'long_name',"seafloor_depth_below_sea_surface"))
    call handle_err(NF90_put_att(ncid, dataf%depth_tot_varid, 'units', "m"))

    !define particle dist state
    call handle_err(NF90_def_var(ncid, "dist_state", NF90_FLOAT, [dataf%part_dimid, dataf%state_dimid, dataf%time_dimid], &
                            & dataf%dist_state_varid))
    call handle_err(NF90_put_att(ncid, dataf%dist_state_varid, 'long_name',"distance_traveled"))
    call handle_err(NF90_put_att(ncid, dataf%dist_state_varid, 'units', "m"))


#   ifdef OHNS
        call create_output_file_ohns(dataf, ncid)
#   endif
    call handle_err(NF90_enddef(ncid))
    call handle_err(NF90_close(ncid))

end function


subroutine create_name_states(dataf, params)
    ! create the name strings to the ouput file
    !dataf is the dat of the output file
    !components is the array of component 
    type(output_file_data), intent(in) :: dataf
    type(oserit_parameters), intent(in) :: params
    integer :: ncid, i, j
    character, dimension(:,:), allocatable :: state_name
    character, dimension(:,:), allocatable :: state_name_string

    call handle_err(NF90_open(dataf%filename,NF90_WRITE,ncid))
    
    !state_name
    allocate(state_name_string(20, nbr_state))
    allocate(state_name(LENGTH_STR, nbr_state))
    state_name_string = reshape(["s","u","r","f","a","c","e","_","_","_","_","_","_","_","_","_","_","_","_","_",&
                                &"c","o","l","u","m","n","_","_","_","_","_","_","_","_","_","_","_","_","_","_",&
                                &"c","h","e","m","i","c","a","l","l","y","_","d","i","s","p","e","r","s","e","d",&
                                &"s","t","r","a","n","d","e","d","_","_","_","_","_","_","_","_","_","_","_","_",&
                                &"s","e","a","b","e","d","_","_","_","_","_","_","_","_","_","_","_","_","_","_",&
                                &"o","u","t","_","o","f","_","d","o","m","a","i","n","_","_","_","_","_","_","_",&
                                &"a","t","m","o","s","p","h","e","r","e","_","_","_","_","_","_","_","_","_","_",&
                                &"d","i","s","s","o","l","v","e","d","_","_","_","_","_","_","_","_","_","_","_",&
                                &"d","e","g","r","a","d","e","d","_","_","_","_","_","_","_","_","_","_","_","_"],&
                                &  (/20,nbr_state/))

    DO i = 1, nbr_state
      DO j = 1, LENGTH_STR
        IF(j<=20)THEN
          state_name(j,i) = state_name_string(j,i)
        ELSE
          state_name(j,i) = "_"
        ENDIF
      END DO
    END DO 

    call handle_err(NF90_put_var(ncid, dataf%state_varid, state_name,start=[1,1], count=[LENGTH_STR, nbr_state]))

    call handle_err(NF90_close(ncid))

end subroutine

subroutine save_sim_state(dataf, params, time, part_drift, part)
    !save the current state of the simulaiton
    !dataf is the dat of the output file
    ! time is time since epoch
    ! params is the simulation parameters
    !the arrays of particle should be provided

    use timepars

    type(output_file_data), intent(in) :: dataf
    type(oserit_parameters), intent(in) :: params
    integer, intent(in) :: time
    type(PartDrift), allocatable, dimension(:), intent(in) :: part_drift
    type (PartMain), allocatable, dimension(:), intent(in) :: part

    real,dimension(:, :), allocatable :: output_array_dist_state
    integer :: ncid, status, time_id, nopart, i, j, k

    integer, dimension(:), allocatable :: times, times_shape

    call handle_err(NF90_open(dataf%filename,NF90_WRITE,ncid))

    !update time
    call handle_err(nf90_inquire_dimension(ncid, dataf%time_dimid, len=time_id))
    time_id = time_id + 1!to write to the next timestep
    
    call handle_err(NF90_put_var(ncid, dataf%time_dimid, time, start=[time_id]))

    call handle_err(NF90_put_var(ncid, dataf%timestep_varid, nt, start=[time_id]))

    call handle_err(nf90_put_var(ncid, dataf%lon_varid, part%x_lon, start =[1, time_id]))
    call handle_err(nf90_put_var(ncid, dataf%lat_varid, part%y_lat, start =[1, time_id]))
    call handle_err(nf90_put_var(ncid, dataf%depth_varid, part%z_depth, start =[1, time_id]))
    call handle_err(nf90_put_var(ncid, dataf%depth_tot_varid, part%depth_tot, start =[1, time_id]))

    call handle_err(nf90_put_var(ncid, dataf%part_state_varid, part%state, start =[1, time_id]))
    call handle_err(nf90_put_var(ncid, dataf%drift_state_varid, part%drift_state, start =[1, time_id]))

    allocate(output_array_dist_state(size(part_drift), nbr_state))
    do j = 1, nbr_state
        do i = 1, size(part_drift)
            output_array_dist_state(i, j) = part_drift(i)%dist_state(j)
        end do
    end do
    call handle_err(nf90_put_var(ncid, dataf%dist_state_varid, output_array_dist_state, start =[1, 1, time_id]))   
    

    call handle_err(NF90_close(ncid))

end subroutine

subroutine handle_err(status)
    !if the status is not at "no error", print the error and make the model crash
    integer, intent(in) :: status

    if(status /= NF90_NOERR) then
        print*, NF90_strerror(status), "error occured when working on the output file"
        stop NF90_strerror(status)
    end if

end subroutine

subroutine save_outputs(dataf, params, time, &
                            & part_drift, part)
    ! save all the simulation outputs
    use timepars
    use timepars_ose
#   ifdef OHNS
        USE ohns
#   endif

    type(output_file_data), intent(in) :: dataf
    type(oserit_parameters), intent(in) :: params
    integer, intent(in) :: time
    type(PartDrift), allocatable, dimension(:), intent(in) :: part_drift
    type (PartMain), allocatable, dimension(:), intent(in) :: part

    character(len=9) :: tmstp

    REAL :: T1, T2

    call cpu_time(T1)
    write(*,*) 'saving sim state'
    call save_sim_state(dataf, params, time, part_drift, part)
    
#   ifdef OHNS
        call save_state_ohns(dataf)
#   endif

    call cpu_time(T2)

    time_w_out = time_w_out + T2-T1 

end subroutine

end module
