Main modules
============
.. _md_program:

mod_calendar.F90
----------------

The module **mod_calendar** contains all the subroutines that initialise and update the calendar of the simulation. This module contains four subroutines: **init_calendar**, **update_calendar**, **end_calendar** and **tt_calendar**.

* The subroutine **init_calendar** defines the starting date of the calendar as well as the time step of the simulation **tseas**.

* **update_calendar** updated the calendar using the given time step and the initial date. The calendar is computed for both forward and backward simulations in time. Let us consider a simulation forward in time, where the start year (**StartYear**) is set to 2000 and the current year (**currYear**) is 2004. The variable **iyear** describes the number of simulation years. If **loopYears** is activated, after reaching the year set as **loopEndYear** (2010 in this case) the calendar is set back to the **loopStartYear** (2000). **iyear** is not altered by this correction.

  .. image:: figs/fig_calendar.png
    :width: 700px
    :align: center
    :height: 600px
    :alt: Example of a forward and backward calendar

  .. warning::  Note than in backward simulations in time, **loopStartYear>loopEndYear**. The start year (**StartYear**) is always larger or equal to the current year (**currYear**) - unless **loopYears** is activated.  If **loopYears** is activated, the calendar is corrected in similar way to the forward simulations (see Figure above).

* **end_calendar** computes the final date of simulation defined by the time step **intrun**

* **tt_calendar** uses the starting date as a reference and translates the time step **tt** into a date (year, month, day) and time (hour, minute, second).

This module contains four subroutines:

.. f:autosubroutine:: init_calendar

.. f:autosubroutine:: update_calendar

.. f:autosubroutine:: end_calendar

.. f:autofunction:: tt_calendar

mod_clock.F90
-------------

The module **mod_clock** calculates the new time step referenced to the initial time step. This module contains one subroutine **update_time**.

.. image:: figs/fig_time.png
    :width: 500px
    :align: center
    :height: 250px
    :alt: Description of mod_time

The subroutine updates **tt** and **ts** based on the value of **ds**. This is transform to a time step in seconds **dt** by multiplying **ds** with the volume **dxyz**. The subroutine chooses between the smallest of three different time steps:

1 - **dtmin** which is the time step between two time subcycles :math:`t_{min} = \frac{\Delta t}{iter}` where **iter** is the number of subcycles.

2 - **dtreg** which is the time step to the next time subcycle.

3 - And the time step corresponding to the smallest wall crossing time computed with **cross_time**.

After updating the values of **tt** and **ts**, the new values of **intrpb** and **intrpr** are computed.

.. f:autosubroutine:: update_time

mod_error.F90
-------------

The module **mod_error** check for possible errors in the simulation. If any error is found a diagnostic file with a summary of the error is created. This module contains two subroutines and a private function: **errorCheck**, **write_error** and **errorType**.

* **errorCheck** check for a possible error defined by **teststr**. The possible errors are listed below:

    +-------------------+---------------+--------------------------------------------------------+
    | **teststr**       |  **errCode**  |  Description                                           |
    +===================+===============+========================================================+
    |  *infLoopError*   |        1      |  Trajectory trapped in an **infinite loop**            |
    +-------------------+---------------+--------------------------------------------------------+
    |  *dxyzError*      |        2      |  The volume of the gridbox is **zero** or **negative** |
    +-------------------+---------------+--------------------------------------------------------+
    |  *boundError*     |        3      |  Trajectory leaving the **domain**                     |
    +-------------------+---------------+--------------------------------------------------------+
    |  *landError*      |        4      |  Trajectory hits a **land** point                      |
    +-------------------+---------------+--------------------------------------------------------+
    |  *coordboxError*  |      5/6/7    |  Trajectory placed in the **wrong** box                |
    +-------------------+---------------+--------------------------------------------------------+
    |  *dsCrossError*   |        8      |  No **available pathways** for the trajectory          |
    +-------------------+---------------+--------------------------------------------------------+

    .. note: A **infinite loop** is defined when a trajectory is iterated more than 30000 times since last time it crossed a wall or started a time subcycle.

* If an error is found in a trajectory, the last position and time step will be stored in a *_err.csv* file. The module **write_error** besides writing the number of the trajectory **ntrac**, the last position **x1, y1, z1**, the volume/mass transport **subvol** and the time step; it also gives a short description of the error.

* **errorType** is a private function that gives a short description of the error given by **errorCode**. This output is used by **write_error**.


This module contains two subroutines:

.. f:autosubroutine:: errorCheck

.. f:autosubroutine:: write_error

and a private function:

.. f:autosubroutine:: errorType

mod_init.F90
------------

The module **mod_init** consists on two subroutines: **init_namelist** that reads the namelist, and **init_alloc** that allocates all the allocatable arrays. More information about the namelist can be found in the *Namelist* chapter.

.. f:autosubroutine:: init_namelist

.. f:autosubroutine:: init_alloc

mod_loop.F90
------------

The module **mod_loop** is the core module of TRACMASS. This module contains the big loop that updates the calendar, the clock and the position of the trajectories.

.. image:: figs/fig_loop.png
    :width: 600px
    :align: center
    :height: 750px
    :alt: Description of mod_pos

This is how the module works:

1 - First the fields are updated according to the value of **ints**.

2 - Then, if the time step corresponds to a seeding time, the subrotuine **seed** is called.

3 - Then the loop checks all the possible trajectories given by **ntrac**. If the trajectory is not activated the module while skip it.

4 - If a trajectory is activated it will be iterated several times where its position will be updated (**update_traj**).

5 - If the trajectory exceedes the limit time **timax**, it will be deactivated.

6 - After each iteration the calendar is updated.

.. note:: If all the trajectories are deactivated the simulation will be stopped even if the corresponding time step is not the final one set by **intrun**.

This module contains a single subroutine:

.. f:autosubroutine:: loop


mod_pos.F90
-----------

The module **mod_pos** calculates the new position of a trajectory and the time it will take to cross a wall in the gridbox. This module contains three subroutines: **cross_time**, **calc_pos**, and **update_traj**.

* The subroutine **cross_time** computes the time it will take to cross any of the faces determined by the variable **ijk**. The possible values of **ijk** are (1) for the east/west faces, (2) for the north/south faces, and (3) for the up/down faces. For simplicity, let us consider the case of **ijk** =1 with an eastward zonal flow **uflux**.

  .. image:: figs/fig_boxpos.png
      :width: 300px
      :align: center
      :height: 300px
      :alt: Description of mod_pos

  This is how the module works:

  1 - First, the interpolated values of the zonal flow **uflux** in the east wall (**uu**) and in the west wall (**um**) are computed. A linear interpolation is used to obtained the values for **uu** and **um**:

  .. math::

     U_i(t) = \frac{(t-t_0) \ U_i(t_1) + (t_1-t) \ U_i(t_0)}{t_1-t_0} \quad \text{where} \quad t_0 \le t \le t_1.

  In this case :math:`t_0` and :math:`t_1` represent the *n* and *n+1* time step.

  2 - If **uu** is positive the subroutine computes the crossing time through the eastern wall :math:`t_E` (**sp**).

  2.1 - If  **uu** = **um**, the time it will take to the trajectory to cross the east wall is:

  .. math::

     t_{E} = \frac{x_E-x}{U_i(t)}.

  2.2 - If **uu** :math:`\neq` **um**, the velocity field inside the box is linearly interpolated:

  .. math::

     U(x) = (x-x_W)(U(x_E)-U(x_W)) + U(x_W) \quad \text{where} \quad x_W \le x \le x_E,

  where **iam** represents the index for the western wall (:math:`x_W`) and **ia** represents the eastern wall (:math:`x_E`). If :math:`U(x)>0` at the starting position of the particle, the time to reach the eastern wall is given by:

  .. math::

     t_{E} = \frac{1}{U(x_W)-U(x_E)}\log\left(\frac{U(x)}{U_E} \right).

  2.3 - If none of the above conditions is fulfilled the subroutine returns the value **UNDEF** for **sp**.

  3 - Following a similar procedure, the subroutine computes the crossing time through the western wall (**sn**).

.. note:: The equations used to compute the crossing time considers a different spatial interpolation of :math:`U(x)` for **sn**. The crossing time through the western wall is given by the following equation :math:`t_{W} = \frac{1}{U(x_W)-U(x_E)}\log\left(\frac{U(x)}{U_W} \right)`.

* The subroutine **calc_pos** computes the new position of the trajectory after time **ds** in the direction given by **ijk**. This subroutine works in the following way (let us consider the same case as in the previous example for **cross_time**):

  1 - First, the interpolated values of the zonal flow **uflux** in the east wall (**uu**) and in the west wall (**um**) are computed. A linear interpolation is used to obtained the values for **uu** and **um**.

  2.1 - If  **uu** = **um**, the new position of the trajectory is given by:

  .. math::

     x_1 = x_0 + U(x_E)ds

  2.2 - On the other hand, if **uu** :math:`\neq` **um** the new position is:

  .. math::
     x_1 = \left(x_0 - x_W + \frac{U(x_W)}{U(x_E)-U(x_W)} \right) \exp((U(x_E)-U(x_W))ds) + x_W - \frac{U(x_W)}{U(x_E)-U(x_W)}.

.. warning:: If the trajectory is placed at :math:`U(x)=0` in a divergent field, **calc_pos** is not able to determine the new position (unstable equilibrium).

* The subroutine **update_traj** updates the position of the trajectory after a time step given by **ds** and computes the new values for **x1**, **y1**, and **z1**. The subroutines check if any of the crossing values given by **cross_time** corresponds to the value of **ds** to determine the new position.

                        +---------+----------+---------+--------+----------------+
                        | **ds**  |  **ib**  | **jb**  | **kb** | Crossing wall  |
                        +=========+==========+=========+========+================+
                        |   dse   |  ia + 1  |         |        | Eastern wall   |
                        +---------+----------+---------+--------+----------------+
                        |   dsw   |  ia - 1  |         |        | Western wall   |
                        +---------+----------+---------+--------+----------------+
                        |   dsn   |          |  ja + 1 |        | Northern wall  |
                        +---------+----------+---------+--------+----------------+
                        |   dss   |          |  ja - 1 |        | Northern wall  |
                        +---------+----------+---------+--------+----------------+
                        |   dsu   |          |         | ka + 1 | Upper wall     |
                        +---------+----------+---------+--------+----------------+
                        |   dsd   |          |         | ka - 1 | Lower wall     |
                        +---------+----------+---------+--------+----------------+

If **ds** is smaller than any of the crossing times and equal to the time stepping, or if the trajectory is inside a convergence zone where all the crossing times are **UNDEF**. The trajectory remains inside the box.

This module contains three subroutines:

.. f:autosubroutine:: cross_time

.. f:autosubroutine:: calc_pos

.. f:autosubroutine:: update_traj

mod_print.F90
-------------

The module **mod_print** is responsible for printing the basic information about the run which includes a short summary of the model configuration, the number of trajectories run and a final summary of the number trajectories that are still running, have been deactivated or have errors.

This module includes five subroutines:

.. f:autosubroutine:: print_header_main

.. f:autosubroutine:: writesetup_main

.. f:autosubroutine:: print_start_loop

.. f:autosubroutine:: print_cycle_loop

.. f:autosubroutine:: print_end_loop

mod_seed.F90
------------

The module **mod_seed** defines all the variables and arrays neccesary for the seeding of particles. This modules contains two public subroutines (**init_seed** and **seed**) and a private subroutine (split_grid)

The subroutine **init_seed** defines the grid points and the time steps where the particles are going to be initialised, the wall of the grid where they are going to be placed (**isec**), and their direction (**idir**). There are three options for **isec**: (1) on the east wall of the grid cell, (2) on the north wall of the grid cell, and (3) on the top wall of the grid cell. idir selects the initial direction of the trajectories eastward/northward/upward (**idir = 1**) or westward/southward/downward (**idir = -1**).

.. image:: figs/fig_isec.png
    :width: 389px
    :align: center
    :height: 300px
    :alt: Description of isec on the grid cell

.. note:: If the simulation is backward in time (**nff = -1**), idir represents the last direction of the trajectory to follow. For example, let us consider a eastward flow field. A simulation with **nff = -1** and **idir = 1** will follow trajectories back in time that initially are moving eastward.

The initial seeding location, time, and direction can be defined directly in the namelist or read from a file. This is control by **seedType** and **seedTime**.

* **seedType**: (1) the seeding location is defined by the grid points within the volume described by **(ist2-ist1+1)x(jst2-jst1+1)x(kst2-kst1+1)**, all these trajectories will shared the **idir** and **isec** defined in the namelist, or (2) the seeding location and the direction is read from an external file **seedDir/seedfile**.

* **seedTime**: (1) the seeding happens in the time interval defined between **tst2** and **tst1**, or (2) it is read from a external file **seedDir/timeFile**.


The **seed** subroutine populates the **trajectory** array that contains the position of the trajectories as well as their corresponding volume/mass transport. This module works this way:

1 - The subroutine checks if the current time **ntime** corresponds to a seeding time.

2 - The corresponding flux is chosen according to the value of **isec**. If the direction does not correspond to the value of **idir** the trajectory is not activated.

3 - **num**, the number of trajectories per grid point, is defined. There are different options based on **nqua**: (1) the number of trajectories is defined by **partQuant**, or (2) the particles transport a specific volume/mass transport defined by **partQuant**, the number of particles in the grid is then defined dividing the total volume/mass transport by **partQuant**.

.. image:: figs/fig_nqua.png
    :width: 600px
    :align: center
    :height: 450px
    :alt: Description of nqua

4 - The grid is split in equal parts using the private subroutine **split_grid**. If **num** is a square number the grid cell is divided in equal squares, if **num** is a prime number the grid is split in equal rectangles along one axis (see figure below). For other cases, **split_grid** will divide the square in equal rectangles with similar side lengths.

.. image:: figs/fig_num.png
    :width: 500px
    :align: center
    :height: 200px
    :alt: Description of nqua

5 - The specific volume/mass transport of a trajectory **subvol** is computed from **num**.

6 - The trajectories are placed in the middle of each of the rectangles. This initial position is given by **x1, y1, z1**.

.. warning:: **x1, y1, z1** are computed using the gridbox as a reference.

7 - The position of the trajectory in the gridbox reference system, the trajectory number **ntrac**, the corresponding position index and the mass/volume transported by it is stored in the array **trajectories**.

This module contains two public subroutines:

.. f:autosubroutine:: init_seed

.. f:autosubroutine:: seed

and a private subroutine:

.. f:autosubroutine:: split_grid

mod_vars.F90
------------

**mod_vars.F90** is a collection of different modules that define the required variables for the different components of TRACMASS. This file contains 10 modules:

- **mod_precdef**: defines the precisions of the REAL variables.

- **mod_log**: defines the verbose variables.

- **mod_param**: the general parameters of TRACMASS are defined here.

- **mod_trajdef**: the derived TYPE **trajectory** is defined in this module.

- **mod_loopvars**: the variables used in **mod_loop** are defined here.

- **mod_traj**: the variables to describe a trajectory are defined here.

- **mod_grid**: the grid variables, and the boundary conditions are defined here.

- **mod_time**: defines the variables used by **mod_calendar** and **mod_clock**.

- **mod_domain**: defines the variables to describe the limits of the domain where the trajectory is activated.

- **mod_vel**: the volume/mass fluxes both horizontal and vertical are defined here.

mod_vertvel.F90
---------------

The module **mod_vertvel** computes the vertical volume/mass fluxes. If TRACMASS is setup for two dimensional fields, or the vertical velocity is part of the dataset this module is not activated.

This module contains a single subroutine **vertvel** that computes the vertical flux using the following equation:

.. math::

   W^n_{i,j,k} = W^n_{i,j,k-1} - ( U^n_{i,j,k}-U^n_{i-1,j,k} + V^n_{i,j,k} - V^n_{i,j-1,k})

This equation is integrated from the bottom (ocean) or the TOA (atmosphere) to the level **ka**.

.. f:autosubroutine:: vertvel

mod_write.F90
-------------

The module **mod_write** creates the outfiles where the information of the trajectories is stored. This module is responsible for writing three important files: *_ini.csv* where the initial positions are stored, *_out.csv* where the final positions are stored, and *_run.csv* where the new positions of the trajectory are stored.

.. image:: figs/fig_write.png
  :width: 500px
  :align: center
  :height: 500px
  :alt: Example of a writing frequency

The initial and the final information of the trajectories are always stored. However, the frequency at which data is stored in the *_run.csv* is controlled by **write_frec**: (1) only at GCM time steps, (2) only at GCM and subcycle time steps, (3) only when a trajectory crosses a wall, (4) all time steps, and (5) no data stored.

.. note:: the time format of the output files can also be adjusted with **timeformat**: (0) **tt** is stored, (1) **ts** is stored, (2) the time is saved in YYYY-MM-DD HH format.

This module contains three subroutines:

.. f:autosubroutine:: open_outfiles

.. f:autosubroutine:: write_data

.. f:autosubroutine:: close_outfiles
