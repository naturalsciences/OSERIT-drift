PROGRAM main

!************************************************************************
!
! *coherens_main* Coherens main program
!
! Author - Patrick Luyten
!
! Version - 4 Nov 2008  @(COHERENS)Coherens_Program.f90  V2.0
!
! $Date: 2011-03-18 14:35:25 +0100 (Fri, 18 Mar 2011) $
!
! $Revision: 188 $
!
! Description -
!
! Reference -
!
!
!
!
!************************************************************************
!
USE iopars
USE timepars
USE timepars_ose
USE rng_library
USE time_routines, ONLY: initialise_time
USE utility_routines, ONLY: loop_index, mult_index
USE output_writer
USE datatypes_ose
USE oseswitches
USE partpars
USE timepars_ose
USE time_routines_ose
USE check_oserit
USE particle_cloud
USE Oserit_Run_Reader

#ifdef OHNS
   use ohns
#endif

IMPLICIT NONE
type(output_file_data) :: data_output
character(len = 1000) :: output_filename, quadtree_folder

logical :: output_tmtsp ! if the data should be produced at this timestep

pglev = 1
procname(1) = 'coherens_main'

!
!0. Runtitle
!--------------
!

write(*,*) 'starting oserit...'
#ifdef OHNS
   write(*,*) '...with OHNS'
#endif
write(*,*) ''

runtitle = ''
input_dir= ''
output_dir = ''
CALL get_command_argument(1,runtitle)
CALL get_command_argument(2,input_dir)
CALL get_command_argument(3,output_dir)
IF (LEN_TRIM(runtitle).EQ.0) THEN
   next_simul = .FALSE.
   write(*,*) 'no runtitle provided, please provide'
   write(*,*) '-runtitle '
   write(*,*) '-input directory '
   write(*,*) '-output directory'
   RETURN
ENDIF
write(*,*) 'runtitle :"',runtitle
write(*,*) 'input directory : "',input_dir
write(*,*) 'output directory : "',output_dir


CALL parameters_reset(oserit_param)
CALL read_input_json(oserit_param,TRIM(input_dir)//'/'//TRIM(runtitle))

#ifdef OHNS
   call read_input_OHNS(TRIM(input_dir)//'/'//TRIM(runtitle))
#endif

CALL rng_init
CALL initialise_time

!---
CALL check_ose_params
#ifdef OHNS
   call check_OHNS_parameters
#endif
CALL oserit_start



output_filename = TRIM(output_dir)//'/'//TRIM(runtitle)//"_PAout.nc"

data_output = create_output_file(output_filename, oserit_param)
call create_name_states(data_output, oserit_param)
#ifdef OHNS
   call create_name_ohns(data_output)
#endif

call save_outputs(data_output, oserit_param, time_since_epoch, part_drift, part)

!
!4. Time loop
!------------
!
print*,""
print*,"Starting time loop for ",nstep, "iterations"
nt_400: DO nt=1,nstep

!
!4.2 Date/time
!-------------
!
   IF (oserit_param%iopt_backtrack.EQ.0) THEN
      CALL update_time
   ELSE
      CALL update_time_back
   ENDIF


!
! Oserit
!---------------------------
!
   write(*,*) 'tmstp start: ',nt, "  time: ", CDateTime

   part_old = part

   time_since_epoch = date_to_epoch(IDateTime)

   IF(oserit_param%iopt_backtrack == 0)THEN
      time_since_epoch_prev = time_since_epoch - INT(delt2d)
   ELSE
      time_since_epoch_prev = time_since_epoch + INT(delt2d)
   END IF

   if (mod(nt, oserit_param%tmstp_per_outtmstp) == 0 .OR. (nt == nstep .AND. oserit_param%iopt_force_last_output == 1))then
      output_tmtsp = .true.
   else
      output_tmtsp = .false.
   end if


   !
   ! 2. Check for new particle release
   !-----------------------------------
   !
   
   CALL new_release


   !
   ! 4. Lagrangian particle tracking module
   !-----------------------------------------
   !

   !---Particle drift
   CALL lagr_drift

# ifdef OHNS
   call weathering_ohns
# endif
   !4.7 Model output
   !----------------

   if (output_tmtsp) call save_outputs(data_output, oserit_param, time_since_epoch, &
                                    & part_drift, part)


   write(*,*) 'tmstp end'

!This will stop simulation if all particles have stopped to move  
!   if(continue_drifting() .eqv. .false.) exit nt_400

ENDDO nt_400

!
!5. Synchronise processes for new simulation
!-------------------------------------------
!

!3000 IF (parallel_set) CALL comms_barrier(comm_world_MPI)

!
!6. Finalise simulation
!----------------------
!

write(*,*) 'simulation_end'
CALL deallocate_ose_arrays
CALL rng_finalize

CALL print_ose_timer


!CALL simulation_end

!GOTO 1000

!
!7. Finalise program
!-------------------
!

!2000 CALL coherens_end


STOP 'Main program terminated'


END PROGRAM main
